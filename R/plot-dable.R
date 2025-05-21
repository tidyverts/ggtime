#' Decomposition plots
#'
#' Produces a faceted plot of the components used to build the response
#' variable of the dable. Useful for visualising how the components contribute
#' in a decomposition or model.
#'
#' @param object A dable.
#' @param .vars The column of the dable used to plot. By default, this will be the response variable of the decomposition.
#' @param scale_bars If `TRUE`, each facet will include a scale bar which represents the same units across each facet.
#' @param level If the decomposition contains distributions, which levels should be used to display intervals?
#' @inheritParams autoplot.tbl_ts
#'
#' @examplesIf requireNamespace("feasts", quietly = TRUE) && requireNamespace("tsibbledata", quietly = TRUE)
#' library(feasts)
#' library(tsibbledata)
#' aus_production %>%
#'   model(STL(Beer)) %>%
#'   components() %>%
#'   autoplot()
#'
#' @importFrom ggplot2 ggplot geom_line geom_rect facet_grid vars ylab labs
#' @export
autoplot.dcmp_ts <- function(object, .vars = NULL, scale_bars = TRUE,
                             level = c(80, 95), ...){
  method <- object%@%"method"
  idx <- index(object)
  keys <- key(object)
  n_keys <- n_keys(object)

  .vars <- enquo(.vars)
  if(quo_is_null(.vars)){
    .vars <- sym(response_vars(object))
  }
  dcmp_str <- dcmp <- (object%@%"aliases")[[expr_name(get_expr(.vars))]]
  if(!is.null(dcmp_str)){
    dcmp_str <- expr_text(dcmp_str)
  }
  object <- as_tsibble(object) %>%
    transmute(!!.vars, !!!syms(all.vars(dcmp))) %>%
    pivot_longer(measured_vars(.), values_to = ".val",
                 names_to = ".var", names_transform = list(.var = ~ factor(., levels = unique(.))))

  if(has_dist <- inherits(object[[".val"]], "distribution")) {
    interval_data <- as_tibble(object)
    interval_data[paste0(level, "%")] <- lapply(level, hilo, x = interval_data[[".val"]])
    interval_data <- tidyr::pivot_longer(
      interval_data, paste0(level, "%"), names_to = NULL, values_to = "hilo"
    )
    intvl_aes <- aes(x = !!idx, hilo = !!sym("hilo"))
    line_aes <- aes(x = !!idx, y = mean(!!sym(".val")))
    if(n_keys > 1){
      line_aes$colour <- intvl_aes$fill <- expr(interaction(!!!keys, sep = "/"))
    }
    dcmp_geom <- list(
      distributional::geom_hilo_ribbon(intvl_aes, ..., data = interval_data),
      geom_line(line_aes, ...)
    )
  } else {
    line_aes <- aes(x = !!idx, y = !!sym(".val"))
    if(n_keys > 1){
      line_aes$colour <- expr(interaction(!!!keys, sep = "/"))
    }
    dcmp_geom <- geom_line(line_aes, ...)
  }

  p <- object %>%
    ggplot() +
    dcmp_geom +
    facet_grid(vars(!!sym(".var")), scales = "free_y") +
    ylab(NULL) +
    labs(
      title = paste(method%||%"A", "decomposition"),
      subtitle = paste(c(expr_text(get_expr(.vars)), dcmp_str), collapse = " = ")
    )

  # Rangebars
  if (scale_bars) {
    xranges <- range(object[[expr_name(idx)]])
    barwidth <- pmax(1, round((1 / 64) * diff(as.double(xranges))))

    # Avoid issues with visible bindings
    ymin <- ymax <- center <- diff <- NULL

    min_fn <- if(has_dist) function(x, ...) min(quantile(x, (100-max(level))/200), ...) else min
    max_fn <- if(has_dist) function(x, ...) max(quantile(x, (100 + max(level))/200), ...) else max

    range_data <- as_tibble(object) %>%
      group_by(!!sym(".var")) %>%
      summarise(ymin = min_fn(!!sym(".val"), na.rm = TRUE), ymax = max_fn(!!sym(".val"), na.rm = TRUE)) %>%
      mutate(
        center = (ymin + ymax) / 2,
        diff = min(ymax - ymin),
        xmin = xranges[1] - barwidth * 2, xmax = xranges[1] - barwidth,
        ymin = center - diff/2, ymax = center + diff/2
      )

    p <- p + geom_rect(data = range_data,
                       aes(ymin = !!sym("ymin"), ymax = !!sym("ymax"),
                           xmin = !!sym("xmin"), xmax = !!sym("xmax")),
                       fill = "gray75", colour = "black", size = 1 / 3
    )
  }

  if(!is_empty(keys)){
    p <- p + guides(colour = guide_legend(paste0(map_chr(keys, expr_name), collapse = "/")))
  }

  p
}
