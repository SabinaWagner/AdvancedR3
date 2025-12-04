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
    dplyr::summarise(value = mean(value), .groups = "keep") |>
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

#' fit a logistic regression model
#'
#' @param data dataframe with columns "value", "class", and "metabolite"
#' @param model a formula for the logistic regression model
#'
#' @returns a tidy tibble with model results as odds ratio

fit_model <- function(data, model) {
  glm(
    formula = model,
    data = data,
    family = binomial
  ) |>
    broom::tidy(exponentiate = TRUE) |>
    dplyr::mutate(
      metabolite = unique(data$metabolite),
      model = format(model),
      .before = tidyselect::everything()
    )
}



#' preprocess data and use it to fit model and get clean results
#'
#' @param data dataframe with columns "value", "class", and "metabolite"
#'
#' @returns a tidy tibble with model results as odds ratio

create_model_results <- function(data) {
  data |>
    dplyr::filter(metabolite == "Cholesterol") |>
    preprocess() |>
    fit_model(class ~ value)
}

#' fit several models for one dataset
#'
#' @param data a dataframe with one metabolite's data
#'
#' @returns a tibble with model results for all specified models
fit_all_models <- function(data) {
  list(
    class ~ value,
    class ~ value + gender + age
  ) |>
    purrr::map(\(model_name) fit_model(data, model = model_name)) |>
    purrr::list_rbind()
}
