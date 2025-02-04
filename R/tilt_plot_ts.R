#' Plot tilt meter parameters over time
#'
#' @param dat Data frame with tilt meter data in wide or long format.
#'
#' @param pal Optional colour palette. Default is c("#104862", "#62AECA",
#'   "#FFD118") (Indigo dye, Moonstone, Jonquil). For plots with more than 3
#'   variables, these colours are interpolated using
#'   \code{grDevices::colorRampPalette}.
#'
#' @param show_legend Logical argument indicating whether to show figure legend.
#'   Default is \code{FALSE}.
#'
#' @param size Numeric value specifying the line width or point size.
#'
#' @param geom Character string indicating whether to plot line or points.
#'   Options are "point" or "line". Default is "line".
#'
#' @param n_col Number of columns for faceted figure. Default is 1.
#'
#' @param scales Character string indicating how to treat scales of the faceted
#'   plot. Passed to \code{facet_wrap()}. Default is "fixed". Other options are
#'   "free_x" and "free_y".
#'
#' @return Returns a ggplot object of the plotted variables.
#'
#' @importFrom dplyr %>% all_of case_when distinct mutate
#' @importFrom ggplot2 aes element_blank element_text element_rect facet_wrap
#'   geom_line geom_point ggplot guides guide_legend scale_colour_manual
#'   scale_x_datetime scale_y_continuous theme theme_light waiver
#' @importFrom grDevices colorRampPalette
#' @importFrom viridis viridis
#' @importFrom stringr str_detect
#' @importFrom tidyr pivot_longer

#' @export

tilt_plot_ts <- function(
    dat,
  #  vars = NULL,
    geom = "line",
    pal = NULL,
    n_col = 1,
    size = 1,
    scales = "fixed",
    show_legend = FALSE
) {

  if(!("variable" %in% colnames(dat))) {
    dat <- tilt_pivot_longer(dat)
  }


  dat <- dat %>%
    tilt_create_variable_labels()

  vars <- distinct(dat, variable)$variable

  if(is.null(pal)) {

    pal <- viridis(length(vars), option = "F", begin = 0.2, end = 0.8)

  }

  p <- ggplot(dat, aes(timestamp_utc, value, col = variable))

  if(geom == "line") {
    p <- p + geom_line(linewidth = size)
  }

  if(geom == "point") {
    p <- p + geom_point(size = size)
  }

  p <- p +
    scale_x_datetime("Date", date_labels = "%Y-%m-%d") +
    scale_colour_manual("", values = pal) +
    theme_light() +
    theme(
      axis.title.y = element_blank(),
      strip.placement = "outside",
      strip.background = element_rect(colour = NA, fill = NA),
      strip.text = element_text(colour = "black", size = 10)
    ) +
    facet_wrap(
      ~variable,
      ncol = n_col,
      scales = scales,
      strip.position = "left"
    )

  if(isFALSE(show_legend)) {
    p <- p +
      theme(legend.position = "none")
  }

  p
}

