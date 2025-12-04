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



#' split by metabolite, preprocess data and use it to fit models and get clean results for all metabolites
#'
#' @param data dataframe with columns "value", "class", and "metabolite"
#'
#' @returns a tidy tibble with model results  for all specified models for all metabolites as odds ratio

create_model_results <- function(data) {
  data |>
    dplyr::group_split(metabolite) |>
    purrr::map(preprocess) |>
    purrr::map(fit_all_models) |>
    purrr::list_rbind()
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

#' Make a point and whiskers plot of model results
#'
#' @param results a dataframe with model results including 'term', 'estimate', 'std.error', 'metabolite', and 'model' columns
#'
#' @returns a ggplot2 object visualizing the model estimates for each metabolite with error bars
#'
create_plot_model_results <- function(results) {
  results |>
    dplyr::filter(term == "value", std.error <= 2, estimate <= 5) |>
    dplyr::select(metabolite, model, estimate, std.error) |>
    ggplot2::ggplot(ggplot2::aes(
      x = estimate,
      y = metabolite,
      xmin = estimate - std.error,
      xmax = estimate + std.error
    )) +
    ggplot2::geom_pointrange() +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed") +
    ggplot2::facet_grid(cols = ggplot2::vars(model))
}
