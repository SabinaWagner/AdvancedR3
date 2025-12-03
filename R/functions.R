
#' create descriptive table
#'
#' @param data a dataset containing the columns 'metabolite' and 'value'
#'
#' @returns a tibble with pretty mean and standard deviation for each metabolite
#'
create_table_descriptive_stats <- function(data) {
  data |>
    dplyr::group_by(metabolite) |>
    dplyr::summarise(dplyr::across(value, list(mean = mean, sd = sd))) |>
    dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) format(x, digits = 2))) |>
    dplyr::mutate(MeanSD = glue::glue("{value_mean} ± ({value_sd})")) |>
    dplyr::select(Metabolite = metabolite, "Mean ± (SD)" = MeanSD)
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
    ggplot2::theme_minimal()
}
