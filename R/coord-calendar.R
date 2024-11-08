#' Calendar coordinates
#'
#' @export
coord_calendar <- function(period = "week", xlim = NULL, ylim = NULL, expand = TRUE,
                           default = FALSE, clip = "on") {
  ggplot2::ggproto(NULL, CoordCalendar,
    period = period,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    default = default,
    clip = clip
  )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordCalendar <- ggplot2::ggproto("CoordCalendar", ggplot2::CoordCartesian,
  aspect = function(details) 1,

  range = function(self, panel_params) {
    list(
      x = panel_params$x.range,
      y = panel_params$y.range
    )
  },

  # Currently only works for x, should work for c("x", "xmin", "xmax", "xend", "xintercept")
  # transform_position is the appropriate helper for this, but this is incompatible with the group hack (as is xmin, xmax, xend)
  transform = function(self, data, panel_params) {
    # Not sure what to do with training panel guides yet
    if(any(is.infinite(data$x))) return(data)

    # Convert x from numeric for handling time
    time <- if (data$x[[1]] > 1e7) {
      structure(data$x, class = c("POSIXct", "POSIXt"))
    } else if (max(data$x) < 1e5) {
      structure(data$x, class = "Date")
    } else {
      # Already done?
      # Need to figure this out so inputs to $transform are consistent
      return(data)
    }

    wrap_points <- cal_gregorian(self$period)(range(time, na.rm = TRUE))
    # data$group <-
    grp <- as.integer(cut(time, wrap_points))
    data$x <- data$x - as.numeric(wrap_points)[grp] #[data$group]

    data
  },

  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    # environment(ggplot2::CoordCartesian$setup_panel_params)$setup_panel_params(self, scale_x, scale_y, params)

    # Find reasonable limits for x
    lim_x <- c(0, max(diff(as.numeric(cal_gregorian(self$period)(self$limits$x %||% scale_x$range$range)))))

    # calculate break information
    out_x <- scale_x$break_info(lim_x)

    # range in coord space has already been calculated
    # needs to be in increasing order for transform_value() to work
    # out_x$range <- range(continuous_ranges$continuous_range_coord)
    out_x <- list(
      # Note that a ViewScale requires a limit and a range that are before the
      # Coord's transformation, so we pass `continuous_range`, not `continuous_range_coord`.
      ggplot2:::view_scale_primary(scale_x, lim_x, lim_x),
      sec = ggplot2:::view_scale_secondary(scale_x, lim_x, lim_x),
      range = out_x$range,
      labels = out_x$labels,
      major = out_x$major_source,
      minor = out_x$minor_source,
      sec.labels = out_x$sec.labels,
      sec.major = out_x$sec.major_source,
      sec.minor = out_x$sec.minor_source
    )
    names(out_x) <- c("x", paste("x", names(out_x)[-1], sep = "."))

    out_x <- ggplot2:::view_scales_from_scale(scale_x, self$limits$x, self$expand)
    out_x$x.range <- lim_x
    c(
      out_x,
      ggplot2:::view_scales_from_scale(scale_y, self$limits$y, self$expand)
    )
  },

  modify_scales = function(self, scales_x, scales_y) {
    # body(scales_x[[1L]]$trans$transform)[[2]] <- rlang::expr(browser())
    # body(scales_x[[1L]]$trans$transform)[[4]] <- rlang::expr(as.numeric(x) %% !!lubridate::period_to_seconds(lubridate::as.period(self$period)))

    invisible()
  }

  # setup_data = function(data, params = list()) {
  #   data
  # },
  #
  # setup_layout = function(layout, params) {
  #   layout
  # },
)
