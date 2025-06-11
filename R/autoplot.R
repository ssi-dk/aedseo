#' @importFrom ggplot2 autoplot
#' @rdname autoplot
#' @export
autoplot <- function(object, ...) {
  UseMethod("autoplot")
}
#' Autoplot a `tsd` object
#'
#' @description
#' Generates a complete 'ggplot' object suitable for visualizing time series data in a
#' `tsd`, `tsd_onset` or `tsd_onset_and_burden` object.
#'
#' `autoplot(tsd)`
#' - Generates points for each observation and connects them with a line.
#'
#' `autoplot(tsd_onset)`
#'  - The first plot generates a line connecting the observations.
#'    The transparency of the points reflects if seasonal onset has occurred.
#'  - The second plot presents the growth rate for each observation along with confidence intervals.
#'    The transparency of the points indicates whether a growth warning condition is met.
#'
#' `autoplot(tsd_onset_and_burden)`
#'  - Generates a line connecting the observations in the current season, along with colored regions
#'  representing different burdens levels and a vertical line indicating outbreak start.
#'  The y-axis is scaled with `ggplot2::scale_y_log10` to give better visualisation of the burden levels.

#' @param object A `tsd` object
#' @param line_width `r rd_line_width`
#' @param obs_size `r rd_obs_size`
#' @param time_interval_step `r rd_time_interval_step`
#' @param text_family `r rd_text_family`
#' @param y_label `r rd_y_label`
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the `tsd` data.
#'
#' @aliases autoplot
#'
#' @examples
#' set.seed(345)
#' # Create an example `tsd` object
#' time_series <- generate_seasonal_data()
#' autoplot(time_series)
#'
#' @rdname autoplot
#' @method autoplot tsd
#' @export
autoplot.tsd <- function(
  object,
  line_width = 0.7,
  obs_size = 2,
  text_family = "sans",
  time_interval_step = "5 weeks",
  y_label = "Weekly observations",
  ...
) {
  start_date <- min(object$time)
  end_date <- max(object$time)

  object |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data$time,
        y = .data$observation
      )
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_line(
      linewidth = line_width,
      size = obs_size
    ) +
    time_interval_x_axis(
      start_date = start_date,
      end_date = end_date,
      time_interval_step = time_interval_step
    ) +
    ggplot2::labs(y = y_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 9, color = "black", family = text_family),
      axis.title.x = ggplot2::element_text(size = 11, color = "black", family = text_family),
      axis.title.y = ggplot2::element_text(size = 11, color = "black", family = text_family)
    )
}
#'
#' Autoplot a `tsd_onset` object
#'
#' @param object A `tsd_onset` object
#' @param disease_color `r rd_disease_color`
#' @param line_width `r rd_line_width`
#' @param obs_size `r rd_obs_size`
#' @param alpha_warning A numeric specifying the alpha (transparency) for the observations with a
#' seasonal_onset_alarm (first plot) or significantly positive growth rate (second plot).
#' @param alpha_ribbon A numeric specifying the alpha for the confidence intervals of the growth rate.
#' @param text_family `r rd_text_family`
#' @param legend_position `r rd_legend_position`
#' @param time_interval_step `r rd_time_interval_step`
#' @param y_label `r rd_y_label`
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the `tsd_onset` data.
#'
#' @aliases autoplot
#'
#' @examples
#' # Create an `tsd_onset` object
#' time_series_with_onset <- seasonal_onset(
#'   tsd = time_series,
#'   k = 3,
#'   level = 0.95,
#'   family = "quasipoisson"
#' )
#' autoplot(time_series_with_onset)
#'
#' @rdname autoplot
#' @method autoplot tsd_onset
#' @export
autoplot.tsd_onset <- function(
  object,
  disease_color = "black",
  line_width = 0.7,
  obs_size = 2,
  alpha_warning = 0.2,
  alpha_ribbon = 0.1,
  text_family = "sans",
  legend_position = "bottom",
  time_interval_step = "5 weeks",
  y_label = "Weekly observations",
  ...
) {

  start_date <- min(object$reference_time)
  end_date <- max(object$reference_time)

  # Set growth_warning to FALSE if NA
  object <- object |>
    dplyr::mutate(
      growth_warning = dplyr::case_when(
        is.na(.data$growth_warning) ~ FALSE,
        TRUE ~ .data$growth_warning
      )
    )

  p1 <- object |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data$reference_time,
        y = .data$observation
      )
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        alpha = .data$growth_warning
      ),
      color = disease_color,
      size = obs_size
    ) +
    ggplot2::geom_line(
      color = disease_color,
      linewidth = line_width
    ) +
    ggplot2::scale_alpha_manual(
      name = "Seasonal onset \n alarm",
      values = c("TRUE" = 1, "FALSE" = alpha_warning)
    ) +
    time_interval_x_axis(
      start_date = start_date,
      end_date = end_date,
      time_interval_step = time_interval_step
    ) +
    ggplot2::labs(y = y_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 9, color = "black", family = text_family),
      axis.title.x = ggplot2::element_text(size = 11, color = "black", family = text_family),
      axis.title.y = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.text = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.background = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)
    )

  p2 <- object |>
    dplyr::filter(!is.na(.data$lower_growth_rate)) |>
    ggplot2::ggplot(
      mapping = ggplot2::aes(
        x = .data$reference_time,
        y = .data$growth_rate,
        ymin = .data$lower_growth_rate,
        ymax = .data$upper_growth_rate
      )
    ) +
    ggplot2::geom_point(
      mapping = ggplot2::aes(
        alpha = .data$growth_warning
      ),
      color = disease_color,
      size = obs_size
    ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        x = .data$reference_time,
        ymin = .data$lower_growth_rate,
        ymax = .data$upper_growth_rate
      ),
      color = disease_color,
      alpha = alpha_ribbon,
      inherit.aes = FALSE
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      linetype = "dashed"
    ) +
    ggplot2::scale_alpha_manual(
      name = "Growth warning",
      values = c("TRUE" = 1, "FALSE" = alpha_warning)
    ) +
    time_interval_x_axis(
      start_date = start_date,
      end_date = end_date,
      time_interval_step = time_interval_step
    ) +
    ggplot2::labs(y = "Growth rate estimates") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 9, color = "black", family = text_family),
      axis.title.x = ggplot2::element_text(size = 11, color = "black", family = text_family),
      axis.title.y = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.text = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.background = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)
    )
  # save plots
  list(observed = p1, growth_rate = p2)
}
#'
#' Autoplot a `tsd_onset_and_burden` object
#'
#' @param object a `tsd_combined_seasonal_output` object.
#' @param y_lower_bound A numeric specifying the lower bound of the y-axis.
#' @param factor_to_max A numeric specifying the factor to multiply the high burden level for extending the y-axis.
#' @param disease_color `r rd_disease_color`
#' @param season_start,season_end `r rd_season_start_end()`
#' @param time_interval_step `r rd_time_interval_step`
#' @param y_label `r rd_y_label`
#' @param fill_alpha A numeric vector specifying the transparency levels for the fill colors of burden levels.
#' Must match the number of levels.
#' @param text_burden_size A numeric specifying the size of the text labels.
#' @param text_family `r rd_text_family`
#' @param line_color A character specifying the color of the line connecting observations.
#' @param line_type A character specifying the line type for observation line.
#' @param vline_color A character specifying the color of the vertical outbreak start lines.
#' @param vline_linetype A character specifying the line type for outbreak start lines.
#' @param y_scale_labels A function to format y-axis labels.
#' @param theme_custom A function with a ggplot2 theme, specifying the theme to apply to the plot.
#' @param legend_position `r rd_legend_position`
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the `tsd_onset_and_burden` data for the current season.
#'
#' @aliases autoplot
#'
#' @examples
#' # Define `disease_threshold`
#' disease_threshold <- 150
#'
#' # Create a `tsd_onset_and_burden` object
#' tsd_onset_burden <- combined_seasonal_output(
#'   tsd = time_series,
#'   disease_threshold = disease_threshold
#' )
#' autoplot(tsd_onset_burden)
#'
#' @rdname autoplot
#' @method autoplot tsd_onset_and_burden
#' @export
autoplot.tsd_onset_and_burden <- function(
  object,
  y_lower_bound = 5,
  factor_to_max = 2,
  disease_color = "royalblue",
  season_start = 21,
  season_end = season_start - 1,
  time_interval_step = "3 weeks",
  y_label = "Weekly observations",
  text_burden_size = 10 / 2.8,
  fill_alpha = c(0.45, 0.6, 0.75, 0.89, 1),
  text_family = "sans",
  line_color = "black",
  line_type = "solid",
  vline_color = "red",
  vline_linetype = "dashed",
  y_scale_labels = scales::label_comma(big.mark = ".", decimal.mark = ","),
  theme_custom = ggplot2::theme_bw(),
  legend_position = "right",
  ...
) {

  # Check input arguments
  coll <- checkmate::makeAssertCollection()
  checkmate::assert_class(object, "tsd_onset_and_burden", add = coll)
  checkmate::assert_numeric(y_lower_bound, lower = 0, len = 1, add = coll)
  checkmate::assert_numeric(factor_to_max, lower = 0, len = 1, add = coll)
  checkmate::assert_integerish(season_start, lower = 1, upper = 53, add = coll)
  checkmate::assert_integerish(season_end, lower = 1, upper = 53, add = coll)

  # Extract burden data
  virus_levels_df <- object$burden_output
  if (all(sapply(virus_levels_df, is.list))) {
    virus_levels_df <- dplyr::last(unclass(virus_levels_df))
  }
  checkmate::assert_numeric(fill_alpha, lower = 0, upper = 1,
                            len = length(virus_levels_df$values) + 1, add = coll)
  checkmate::reportAssertions(coll)

  # Extract onset data
  virus_df <- object$onset_output |>
    dplyr::filter(.data$season == max(.data$season))

  # Current week
  cur_week <- max(virus_df$reference_time)

  # Determine last year and date
  last_year <- stringr::str_split(epi_calendar(cur_week), "/")[[1]][2]
  date_last_week_in_season <- ISOweek::ISOweek2date(paste0(last_year, "-W", sprintf("%02d", season_end), "-1"))

  # Extend y-axis
  very_high <- max(virus_levels_df$values) * factor_to_max
  y_levels <- pretty(c(0, very_high))
  virus_levels_df$values <- append(virus_levels_df$values, stats::setNames(max(y_levels), "very high"))

  levels_df <- data.frame(
    level = names(virus_levels_df$values),
    ymin = c(y_lower_bound, virus_levels_df$values[-5]),
    ymax = virus_levels_df$values
  )

  # Assign colors with transparency
  levels_df$color <- scales::alpha(disease_color, fill_alpha)

  # Calculate y_tics
  y_tics_log10 <- pretty(c(log10(y_lower_bound), log10(max(y_levels))))
  y_tics_levels <- 10^(y_tics_log10)

  # For each tic, find the closest magnitude to round correctly
  round_to_nearest <- function(x) {
    magnitude <- 10^floor(log10(x))
    plyr::round_any(x, accuracy = magnitude)
  }
  y_tics <- sapply(y_tics_levels, round_to_nearest)
  y_tics[1] <- y_lower_bound
  levels_df$ymax[length(levels_df$ymax)] <- dplyr::last(y_tics)

  # Plot
  virus_df |>
    ggplot2::ggplot(ggplot2::aes(x = .data$reference_time,
                                 y = pmax(.data$observation, y_lower_bound))) +
    theme_custom +
    ggplot2::geom_rect(
      data = levels_df,
      ggplot2::aes(
        xmin = min(virus_df$reference_time) - 4,
        xmax = date_last_week_in_season + 4,
        ymin = .data$ymin,
        ymax = .data$ymax,
        fill = .data$color
      ),
      inherit.aes = FALSE
    ) +
    ggplot2::geom_text(
      data = levels_df,
      ggplot2::aes(
        x = min(virus_df$reference_time) + 15,
        y = sqrt(.data$ymax * .data$ymin),
        label = .data$level
      ),
      hjust = 0,
      inherit.aes = FALSE,
      color = "white",
      size = text_burden_size,
      family = text_family
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_line(
      ggplot2::aes(group = 1, linetype = "Observations"),
      color = line_color
    ) +
    ggplot2::geom_vline(
      data = virus_df |> dplyr::filter(.data$seasonal_onset == TRUE),
      ggplot2::aes(xintercept = .data$reference_time,
                   color = "Outbreak"),
      linetype = vline_linetype,
    ) +
    ggplot2::scale_y_log10(
      expand = ggplot2::expansion(mult = 0, add = 0),
      breaks = y_tics,
      limits = range(y_tics),
      labels = y_scale_labels
    ) +
    ggplot2::scale_linetype_manual(
      name = "",
      values = c(
        "Observations" = line_type
      )
    ) +
    ggplot2::scale_color_manual(
      name = "",
      values = c(
        "Outbreak" = vline_color
      )
    ) +
    ggplot2::labs(y = y_label) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 9, color = "black", family = text_family),
      axis.title.x = ggplot2::element_text(size = 11, color = "black", family = text_family),
      axis.title.y = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.text = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.background = ggplot2::element_blank(),
      legend.position = legend_position,
      legend.box.margin = ggplot2::margin(0, 0, 0, 0)
    ) +
    time_interval_x_axis(
      start_date = min(virus_df$reference_time),
      end_date = date_last_week_in_season,
      time_interval_step = time_interval_step
    )
}
#' Autoplot a `tsd_growth_warning` object
#'
#' @param object A `tsd_onset` object
#' @param k An integer specifying the window size used to create the `tsd_onset` object.
#' @param skip_current_season A logical. Do you want to skip your current season?
#' @param line_width `r rd_line_width`
#' @param text_family `r rd_text_family`
#' @param legend_position `r rd_legend_position`
#' @param breaks_y_axis A numeric specifying how many breaks to show on the y-axis.
#' @param ... Additional arguments (not used).
#'
#' @return A 'ggplot' object for visualizing the `tsd_growth_warning` data.
#'
#' @aliases autoplot
#'
#' @examples
#' # Create an `tsd_onset` object
#' tsd_onset <- seasonal_onset(
#'   tsd = time_series,
#'   k = 5,
#'   family = "quasipoisson",
#'   season_start = 21,
#'   only_current_season = FALSE
#' )
#'
#' tsd_growth_warning <- consecutive_growth_warnings(tsd_onset)
#'
#' autoplot(tsd_growth_warning)
#'
#' @rdname autoplot
#' @method autoplot tsd_growth_warning
#' @export
autoplot.tsd_growth_warning <- function(
  object,
  k = 5,
  skip_current_season = TRUE,
  line_width = 1,
  text_family = "sans",
  legend_position = "bottom",
  breaks_y_axis = 8,
  ...
) {

  if (skip_current_season) {
    object <- object |>
      dplyr::filter(.data$season != max(object$season))
  }

  object$season <- factor(
    object$season,
    levels = sort(unique(object$season), decreasing = FALSE, method = "auto")
  )

  object |>
    dplyr::filter(!is.na(.data$significant_counter)) |>
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$sum_of_cases / k)) +
    ggplot2::geom_line(
      mapping = ggplot2::aes(
        y = .data$significant_counter,
        group = .data$groupID,
        color = .data$season
      ),
      linewidth = line_width
    ) +
    ggplot2::scale_color_discrete(
      name = "season",
      breaks = unique(object$season),
      labels = unique(object$season)
    ) +
    ggplot2::scale_x_log10(
      breaks = scales::log_breaks(base = 10, n = 10),
      labels = scales::label_comma()
    ) +
    ggplot2::scale_y_continuous(
      breaks = scales::breaks_extended(breaks_y_axis)
    ) +
    ggplot2::labs(
      y = "Number of subsequent significant cases",
      x = paste("Rolling", k, "week mean of positive cases")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 9, color = "black", family = text_family),
      axis.title.x = ggplot2::element_text(size = 11, color = "black", family = text_family),
      axis.title.y = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.text = ggplot2::element_text(size = 11, color = "black", family = text_family),
      legend.background = ggplot2::element_blank()
    )

}
