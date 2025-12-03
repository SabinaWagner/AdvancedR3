#' create descriptive table
#'
#' @param data a dataset containing the columns 'metabolite' and 'value'
#'
#' @returns a tibble with pretty mean and standard deviation for each metabolite
#'
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd, median = median, iqr = IQR))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) format(x, digits = 2))) |>
    dplyr::mutate(
      MeanSD = glue::glue("{value_mean} ± ({value_sd})"),
      MedianIQR = glue::glue("{value_median} ± ({value_iqr})")
    ) |>
    dplyr::select(Metabolite = metabolite, "Mean ± (SD)" = MeanSD, "Median ± (IQR)" = MedianIQR)
}


#' create a plot of histograms for all metabolites
#'
#' @param data a dataset containing the columns 'metabolite' and 'value'
#'
#' @returns a ggplot2 object with histograms for each metabolite
#'
create_plot_distributions <- function(data) {
  data |>
    ggplot2::ggplot(ggplot2::aes(x = value)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_wrap(ggplot2::vars(metabolite), scales = "free") +
    ggplot2::theme_classic() +
    ggplot2::labs(
      x = "Value",
      y = "Count"
    )
}

#' Clean data by averaging duplicate entries
#'
#' @param data dataframe with a "value" column
#'
#' @returns dataframe with duplicates averaged

clean <- function(data) {
  data |>
    dplyr::group_by(dplyr::pick(-value)) |>
    dplyr::summarise(value = mean(value), .groups="keep") |>
    dplyr::ungroup()
}


#' Preprocess data by scaling values and converting class to factor
#'
#' @param data a dataframe with columns "class" and "value"
#'
#' @returns a dataframe with scaled "value" and "class" as factor
#'
preprocess <- function(data) {
  data |>
    dplyr::mutate(
      class = as.factor(class),
      value = scale(value)
    )
}
