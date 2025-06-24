#' @export
geom_time_line <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", na.rm = FALSE, orientation = NA,
                      show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTimeLine,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      # orientation = orientation,
      ...
    )
  )
}

#' @format NULL
#' @usage NULL
#' @export
GeomTimeLine <- ggproto(
  "GeomTimeLine", GeomPath,
  draw_panel = function(self, data, panel_params, coord, arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE) {
    # TODO
    # * Add multi-position if value is on the border of time zones.
    # * Add gaps for implicit missing values

    # Check if time scale is civil not absolute
    # if(inherits(x_time, c("POSIXct","POSIXlt")))

    tx <- panel_params$x$get_transformation()
    x_time <- tx$inverse(data$x)

    # Find timezone boundaries for time range plotted
    time_lim <- clock::as_zoned_time(range(x_time))
    tz_shifts <- list()
    tz_time <- time_lim[1]
    while((tz_info <- clock::zoned_time_info(tz_time))$end < time_lim[2]) {
      tz_shifts[[length(tz_shifts) + 1L]] <- tz_info
      tz_time <- tz_info$end
    }
    tz_shifts <- dplyr::bind_rows(tz_shifts, tz_info)

    # Create civil time data

    # needs refactoring for efficiency
    match_before <- function(x, table) {
      vapply(x, function(.) which(.<=table)[1], integer(1L))
    }
    tz_i <- match_before(data$x, unclass(as.POSIXct(tz_shifts$end)))

    # match_between <- function(x, left, right) {
    #   lapply(x, function(.) which(.>=left & .<=right))
    # }
    # match_between(data$x, as.POSIXct(tz_shifts$begin), as.POSIXct(tz_shifts$end))


    # is tz boundary
    tz_boundaries <- match(clock::as_zoned_time(x_time), tz_shifts$begin)
    tz_boundaries_i <- which(!is.na(tz_boundaries))

    # augment data with tz lines
    # if (any(!is.na(tz_boundaries)))
    data <- data[rep(seq_len(nrow(data)), 1L + !is.na(tz_boundaries)),]
    tz_jump_dest <- seq_along(tz_boundaries_i) + tz_boundaries_i
    data$x[tz_jump_dest] <- data$x[tz_jump_dest] - diff(as.integer(tz_shifts$offset[tz_boundaries[tz_boundaries_i] - 0:1]))
    data$linetype[tz_jump_dest-1] <- 2L


    if (!anyDuplicated(data$group)) {
      cli::cli_inform(c(
        "{.fn {class(self[1])}}: Each group consists of only one observation.",
        i = "Do you need to adjust the {.field group} aesthetic?"
      ))
    }

    # must be sorted on group
    # data <- data[order(data$group), , drop = FALSE]
    # data$bkwrd <- c(diff(data$x) < 0, FALSE)
    munched <- ggplot2::coord_munch(coord, data, panel_params)

    # Silently drop lines with less than two points, preserving order
    rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
    munched <- munched[rows >= 2, ]
    if (nrow(munched) < 2) return(ggplot2::zeroGrob())

    # Work out grouping variables for grobs (e.g. rm implicit missing)
    # diff(x) == granularity(x) | irregular | timezone jump

    n <- nrow(munched)
    group_diff <- munched$group[-1] != munched$group[-n]
    start <- c(TRUE, group_diff)
    end <-   c(group_diff, TRUE)

    munched$fill <- arrow.fill %||% munched$colour

    # arrow <- ggplot2::repair_segment_arrow(arrow, munched$group)

    grid::segmentsGrob(
      munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
      default.units = "native", arrow = arrow,
      gp = ggplot2::gg_par(
        col = alpha(munched$colour, munched$alpha)[!end],
        fill = alpha(munched$fill, munched$alpha)[!end],
        lwd = munched$linewidth[!end],
        lty = munched$linetype[!end],
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre
      )
    )
  },
)
