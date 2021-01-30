#' Summarise survey data
#'
#' This function provides a summary of survey data based on the design provided,
#' as well as checks for significance of results
#'
#' @param .data Dataframe or survey data to be summarised
#' @param var Variable within dataframe to be summarised
#' @param ... Variables to group by
#' @param stat One of "mean", "median" or "total". Defaults to mean if not correctly provided 
#' @param compare Stat to use for significance tests of survey results. Must be provided in "group::response" format. Defaults to the overall value
#' @param survey.design List of survey design inputs to be applied to dataframe, if not survey data of class 'tbl_svy'. Please see '?survey::svydesign' to identify required inputs
#' @param ci_level confidence level to be summarised for
#' @param simplify Should the survey results be simplified from decimals to numbers?
#' 
#' @return A dataframe of summarised results
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
summariser_base <- function(.data,
                       var,
                       ...,
                       stat = c("mean", "median", "total"),
                       compare = "overall",
                       survey.design = NULL,
                       ci_level = 0.95,
                       simplify = TRUE) {

  # Running method summariser_base
  UseMethod("summariser_base")
}

#' @rdname summariser_base
#' @export
summariser_base.default <- function(.data,
                       var,
                       ...,
                       # allowed stats to ask for
                       stat = c("mean", "median", "total"),
                       # metric to compare significance against
                       compare = "overall",
                       # Optional list of survey design inputs
                       survey.design = NULL,
                       # CI level to be used
                       ci_level = 0.95,
                       # Simplify results by converting to percentage
                       simplify = TRUE) {

  # List of checks for inputs ----
  # 1 | Data or the variable are not missing
  if (missing(.data) | missing(var))
    rlang::abort("Please provide a valid data or variable input to summarise")

  # Enquoting selected variable
  enquo_var <- rlang::enquo(var)

  # 2 | Check if variable is present in data
  tmp <- try(tidyselect::eval_select(enquo_var,
            data = tibble::as_tibble(.data)),
            silent = TRUE)
  if (class(tmp) == "try-error")
    rlang::abort(glue::glue("Column {rlang::quo_text(enquo_var)} does not exist in the dataframe"))

  enquo_var <- names(tmp)
  rm(tmp)

  # Quoting grouping variables
  grouped_vars <- rlang::expr(c(...))

  # 3 | Check if group variables are present in data
  tmp <- try(tidyselect::eval_select(grouped_vars,
            data = tibble::as_tibble(.data)),
            silent = TRUE)
  if (class(tmp) == "try-error")
    rlang::abort(glue::glue("Grouping vars provided do not exist in the dataframe. Please check again"))

  grouped_vars <- names(tmp)
  rm(tmp)

  # 4 | Check if argument for stat matches default values
  stat <- match.arg(stat)

  # 5 | Check if compare is a string
  if (!purrr::is_character(compare))
    rlang::abort("`compare` can only take a string of the form {group}::{response} in order to estimate significance")

  # 6 | Set up survey if design is not null
  if (!is.null(survey.design)) {

    # 6a | Check if variables provided in survey.design match required inputs
    argsList <- c("ids", "probs", "strata", "fpc", 
                "weights", "nest", "check.strata")
    if (all(names(survey.design) %in% argsList))
      rlang::abort("List of inputs provided in survey design do not match inputs. Please see '?survey::svydesign' to identify required inputs")

    rm(argsList)

    # 6b | Check if variables provided in survey.design are present in the dataframe
    # TODO

    # Set up .data based on survey design
    .data <- do.call(srvyr::as_survey,
                    c(list(.data),
                      survey.design))

  }

  # 7 | Check if data inherits class accepted by srvyr data
  if (!inherits(.data, "tbl_svy"))
    rlang::abort("Data is not defined as a survey data of class 'tbl_svy'. Please provide a survey.design, or data set up as a survey")

  # 8 | Checks for significance numbers and values
  # 8a | Is the CI level numeric
  if (!is.numeric(ci_level)) {
    rlang::warn("Please provide a numeric value for ci_level. Defaulting to 0.95")

    ci_level <- 0.95
  }

  # 8b | Is the CI level less than or equal to one
  if (any(ci_level > 1)) {
    rlang::warn("CI levels is greater than 1. Defaulting to 0.95")

    ci_level <- 0.95
  }

  # 8c | Has only one CI level been provided
  if (length(ci_level) > 1) {
    rlang::warn("More than one CI levels are provided. Defaulting to first value")

    ci_level <- ci_level[1]
  }

  # Define local variables ----
  # 1 | Is the variable numeric or logical
  IS_NUMERIC <- .data %>%
    dplyr::pull(enquo_var) %>%
    inherits(c("integer", "numeric", "logical"))

  # Summarising variables ----
  if (IS_NUMERIC) {
    .data <- summariser_numeric(.data,
                                 enquo_var,
                                 grouped_vars,
                                 stat,
                                 ci_level)
  } else {
    .data <- summariser_character(.data,
                                   enquo_var,
                                   grouped_vars,
                                   stat,
                                   ci_level)
  }

  # Test significance ----
  .data <- test_significance(.data, compare)

  # Simplify ----
  if (simplify){
    # 1 | if is character, multiply by 100
    if (!IS_NUMERIC &
          stat != "total") {
      .data <- .data %>%
        dplyr::mutate(dplyr::across(starts_with("value"), ~ percent(.x, digits = 2)))
    } else {
      .data <- .data %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~round(.x, 2)))
    }
  }

  # Add a class of svyt_df to the output
  class(.data) <- c("svyt_df", class(.data))

  # Return values ----
  return(.data)
}

#' @importFrom magrittr "%>%"
summariser_numeric <- function(.data, var, group, stat, level) {
  # Summarises variables if they are numeric or logical in nature

  # Check if the variable is logical
  lgl <- .data %>%
    dplyr::pull(var) %>%
    inherits(c("logical"))

  if (lgl) {
    # If logical, convert it to numeric and print out a message
    .data <- .data %>%
      dplyr::mutate(!!rlang::sym(var) := !!rlang::sym(var) * 1)

    rlang::inform(glue::glue("Variable {var} has been converted from logical to numeric. Please force as
                       character if you wish to keep both categories"))
  }

  # creating summary function
  sum_fnc <- switch (stat,
    "mean"   = srvyr::survey_mean,
    "median" = srvyr::survey_median,
    "total"  = srvyr::survey_total,
    srvyr::survey_mean
  )

  # Ungrouped summary
  summary_data <- .data %>%
    srvyr::summarise(value = sum_fnc(!!rlang::sym(var),
                              vartype = c("se", "ci"),
                              na.rm = TRUE,
                              level = level,
                              # If the variable is converted from logical, yes else no
                              proportion = lgl),
              N = srvyr::unweighted(dplyr::n())) %>%
    srvyr::mutate(group = "Overall",
           response = "Overall") %>%
    srvyr::select(group, response, N, value, everything()) %>% 
    dplyr::arrange(-value)

  # Grouped summary
  if (length(group) > 0) {
    # If a group is present, do a grouped summary
    grouped_data <- purrr::map_dfr(group, function(group_var) {
      .data %>%
        srvyr::group_by(!!rlang::sym(group_var), .add = FALSE) %>%
        srvyr::summarise(value = sum_fnc(!!rlang::sym(var),
                                  vartype = c("se", "ci"),
                                  na.rm = TRUE,
                                  level = level,
                                  # If the variable is converted from logical, yes else no
                                  proportion = lgl),
                  N = srvyr::unweighted(dplyr::n())) %>%
        srvyr::mutate(group = group_var,
               response = !!rlang::sym(group_var)) %>%
        srvyr::select(group, response, N, value, everything(), -!!rlang::sym(group_var))
      }
    )

    summary_data <- dplyr::bind_rows(summary_data,
                              grouped_data)
  }

  return(summary_data)
}

#' @importFrom magrittr "%>%"
summariser_character <- function(.data, var, group, stat, level) {
  # Summarises variable if they are character or factor in nature

  # creating summary function
  sum_fnc <- switch (stat,
                     "mean"  = srvyr::survey_mean,
                     # There is no median for characters or factors. Defaulting to mean
                     "total" = srvyr::survey_total,
                     srvyr::survey_mean
  )

  # If stat is median, inform saying that it is not gonna happen
  if (stat == "median")
    rlang::warn("`stat` median is not allowed with character vectors, providing survey mean instead")

  # Sanity checks
  # 1. Converting NAs in var and group var to character
  .data <- .data %>%
    srvyr::mutate(dplyr::across(dplyr::any_of(c(var, group)), stringr::str_replace_na))

  # Ungrouped summary
  # Creating list of responses
  list_of_responses <- .data %>%
    srvyr::pull(var) %>%
    unique()

  # Iterating over list of responses, with 1 / 0 variables
  summary_data <- purrr::map_dfr(list_of_responses, function(x) {
    .data %>%
      srvyr::mutate(`____TEMPVAR____` = (!!rlang::sym(var) == x) * 1) %>%
      srvyr::summarise(value = sum_fnc(`____TEMPVAR____`,
                                vartype = c("se", "ci"),
                                na.rm = TRUE,
                                level = level,
                                proportion = TRUE),
                N = srvyr::unweighted(sum(`____TEMPVAR____`))) %>%
      srvyr::mutate(group = "Overall",
             response = x) %>%
      srvyr::select(group, response, N, value, everything())
  }) %>% 
    dplyr::arrange(-value)

  # Grouped summary
  if (length(group) > 0) {
    grouped_data <- purrr::map_dfr(group, function(group_var) {

      list_of_responses <- .data %>%
        dplyr::pull(var) %>%
        unique()

      # Summarising
      purrr::map_dfr(list_of_responses, function(x) {
        .data %>%
          srvyr::group_by(!!rlang::sym(group_var)) %>%
          srvyr::mutate(`____TEMPVAR____` = (!!rlang::sym(var) == x) * 1) %>%
          srvyr::summarise(value = sum_fnc(`____TEMPVAR____`,
                                    vartype = c("se", "ci"),
                                    na.rm = TRUE,
                                    level = level,
                                    proportion = TRUE),
                    N = srvyr::unweighted(sum(`____TEMPVAR____`))) %>%
          srvyr::mutate(group = case_when(
            # If more than one groups are provided, attach a group name finder, else just the normal works
            length(group) > 1 ~ paste0(group_var, " | ", !!rlang::sym(group_var)),
            TRUE ~ as.character(!!rlang::sym(group_var))
            ),
            response = x) %>%
          srvyr::select(group, response, N, value, everything(), - !!rlang::sym(group_var))
        }
        ) %>%
        dplyr::arrange(group)
      }
    )

    summary_data <- dplyr::bind_rows(summary_data,
                              grouped_data)
  }

  return(summary_data)
}