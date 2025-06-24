#' Position scales for mixtime data
#'
#' These are the default scales for mixtime vectors, which are usually used for
#' a plot automatically. To override the scales behaviour manually, use
#' `scale_*_mixtime`.
#'
#' @inheritParams ggplot2::scale_date
#' @param time_breaks A <duration> giving the distance between breaks like
#' "2 weeks", or "10 years". If both `breaks` and `time_breaks` are specified,
#' `time_breaks` wins.
#' @param time_minor_breaks A <duration> giving the distance between minor breaks like
#' "2 weeks", or "10 years". If both `minor_breaks` and `time_minor_breaks` are
#' specified, `time_minor_breaks` wins.
#' @param wraps Wrap the time scale around a calendrical granularity, one of:
#'   - `NULL` or `waiver()` for no wrapping (the default)
#'   - A `mixtime` vector giving positions of wrapping points
#'   - A function that takes the limits as input and returns wrapping points as
#'     output
#' @param time_wraps A <mixtime::duration> giving the distance between temporal wrapping
#' like "2 weeks", or "10 years". If both `wraps` and `time_wraps` are
#' specified, `time_wraps` wins.
#' @param warps Warp the time scale to have a consistent length, one of:
#'   - `NULL` or `waiver()` for no warping (the default)
#'   - A `mixtime` vector giving positions of warping points
#'   - A function that takes the limits as input and returns warping points as
#'     output
#' @param time_warps A <duration> giving the distance between temporal warping
#' like "2 weeks", or "10 years". If both `warps` and `time_warps` are
#' specified, `time_warps` wins.
#' @param civil_time The positioning of zoned time. If TRUE time is positioned
#' uses the local/wall time (the default), otherwise time will be shown in with
#' absolute/continuous UTC time.
#'
#' @export
#' @rdname scale_mixtime
scale_x_mixtime <- function(name = waiver(),
                         breaks = waiver(),
                         time_breaks = waiver(),
                         labels = waiver(),
                         time_labels = waiver(),
                         minor_breaks = waiver(),
                         time_minor_breaks = waiver(),
                         time_type = c("civil", "absolute"),
                         wraps = waiver(),
                         time_wraps = waiver(),
                         # warps = waiver(),
                         # time_warps = waiver(),
                         limits = NULL,
                         expand = waiver(),
                         oob = scales::censor,
                         guide = waiver(),
                         position = "bottom",
                         sec.axis = waiver()) {

  sc <- mixtime_scale(
    aesthetics = ggplot_global$x_aes,
    transform = "mixtime",
    name = name,
    palette = identity,
    breaks = breaks,
    cal_breaks = cal_breaks,
    labels = labels,
    cal_labels = cal_labels,
    minor_breaks = minor_breaks,
    cal_minor_breaks = cal_minor_breaks,
    timezone = timezone,
    guide = guide,
    limits = limits,
    expand = expand,
    oob = oob,
    position = position
  )

  set_sec_axis(sec.axis, sc)
}


# scale_y_time <- function(name = waiver(),
#                          breaks = waiver(),
#                          cal_breaks = waiver(),
#                          labels = waiver(),
#                          cal_labels = waiver(),
#                          minor_breaks = waiver(),
#                          cal_minor_breaks = waiver(),
#                          timezone = NULL,
#                          limits = NULL,
#                          expand = waiver(),
#                          oob = scales::censor,
#                          guide = waiver(),
#                          position = "left",
#                          sec.axis = waiver()) {
#
#   sc <- datetime_scale(
#     c("y", "ymin", "ymax", "yend", "yintercept",
#       "ymin_final", "ymax_final", "lower", "middle", "upper", "y0"),
#     "time",
#     name = name,
#     palette = identity,
#     breaks = breaks,
#     cal_breaks = cal_breaks,
#     labels = labels,
#     cal_labels = cal_labels,
#     minor_breaks = minor_breaks,
#     cal_minor_breaks = cal_minor_breaks,
#     timezone = timezone,
#     guide = guide,
#     limits = limits,
#     expand = expand,
#     oob = oob,
#     position = position
#   )
#
#   set_sec_axis(sec.axis, sc)
# }

#' @export
#' @keywords internal
mixtime_scale <- function(aesthetics, transform, trans = deprecated(),
                           palette, breaks = pretty_breaks(), minor_breaks = waiver(),
                           labels = waiver(), time_breaks = waiver(),
                           time_labels = waiver(),
                           time_minor_breaks = waiver(), timezone = NULL,
                           guide = "legend", call = caller_call(), ...) {
  call <- call %||% current_call()

  # Backward compatibility
  if (is.character(breaks)) breaks <- breaks_width(breaks)
  if (is.character(minor_breaks)) minor_breaks <- breaks_width(minor_breaks)

  if (!is.waiver(time_breaks)) {
    # check_string(cal_breaks)
    breaks <- breaks_width(time_breaks)
  }
  if (!is.waiver(time_minor_breaks)) {
    # check_string(cal_minor_breaks)
    minor_breaks <- breaks_width(time_minor_breaks)
  }
  if (!is.waiver(time_labels)) {
    check_string(time_labels)
    labels <- function(self, x) {
      tz <- self$timezone %||% "UTC"
      label_date(time_labels, tz)(x)
    }
  }

  # x/y position aesthetics should use ScaleContinuousDate or
  # ScaleContinuousDatetime; others use ScaleContinuous
  if (all(aesthetics %in% c(ggplot2:::ggplot_global$x_aes, ggplot2:::ggplot_global$y_aes))) {
    scale_class <- switch(
      transform,
      date = ggplot2::ScaleContinuousDate,
      time = ScaleContinuousDatetime
    )
  } else {
    scale_class <- ScaleContinuous
  }

  transform <- switch(transform,
                      date = scales::transform_date(),
                      time = scales::transform_time(timezone)
  )

  sc <- ggplot2::continuous_scale(
    aesthetics,
    palette = palette,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    guide = guide,
    transform = transform,
    trans = trans,
    call = call,
    ...,
    super = scale_class
  )
  sc$timezone <- timezone
  sc
}

#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousDatetime <- ggproto(
  "ScaleContinuousDatetime", ScaleContinuous,
  secondary.axis = waiver(),
  timezone = NULL,
  transform = function(self, x) {
    tz <- attr(x, "tzone")
    if (is.null(self$timezone) && !is.null(tz)) {
      self$timezone <- tz
      self$trans <- scales::transform_time(self$timezone)
    }
    if (is_bare_numeric(x)) {
      x <- self$trans$inverse(x)
      cli::cli_warn(c(
        "A {.cls numeric} value was passed to a {.field Datetime} scale.",
        i = "The value was converted to {obj_type_friendly(x)}."
      ), call = self$call)
    }
    ggtime:::calendar_wrap(x)
    # ggproto_parent(ScaleContinuous, self)$transform(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    self$oob(x, limits)
  },
  break_info = function(self, range = NULL) {
    breaks <- ggproto_parent(ScaleContinuous, self)$break_info(range)
    if (!(is.waiver(self$secondary.axis) || self$secondary.axis$empty())) {
      self$secondary.axis$init(self)
      breaks <- c(breaks, self$secondary.axis$break_info(breaks$range, self))
    }
    breaks
  },
  sec_name = function(self) {
    if (is.waiver(self$secondary.axis)) {
      waiver()
    } else {
      self$secondary.axis$name
    }
  },
  make_sec_title = function(self, title) {
    if (!is.waiver(self$secondary.axis)) {
      self$secondary.axis$make_title(title)
    } else {
      ggproto_parent(ScaleContinuous, self)$make_sec_title(title)
    }
  }

)
