set_sec_axis <- function(sec.axis, scale) {
  if (!is.waiver(sec.axis)) {
    if (scale$is_discrete()) {
      if (!identical(.subset2(sec.axis, "trans"), identity)) {
        cli::cli_abort("Discrete secondary axes must have the {.fn identity} transformation.")
      }
    }
    if (is.formula(sec.axis)) sec.axis <- sec_axis(sec.axis)
    if (!is.sec_axis(sec.axis)) {
      cli::cli_abort("Secondary axes must be specified using {.fn sec_axis}.")
    }
    scale$secondary.axis <- ggplot2::sec.axis
  }
  return(scale)
}

# ggplot2:::ggplot_global
ggplot_global <- list(
  x_aes = c("x", "xmin", "xmax", "xend", "xintercept",
            "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
  y_aes = c("y", "ymin", "ymax", "yend", "yintercept",
            "ymin_final", "ymax_final", "lower", "middle", "upper", "y0")
)
