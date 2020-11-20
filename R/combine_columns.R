#' Combine columns
#'
#' TBU
#'
#' @param .data Dataframe to combine column in
#' @param new_column_name New column name
#' @param pattern pattern to match against
#' @param type one of "starts_with", "ends_with", "matches", "contains", "one_of". Defaults to starts_with 
#' 
#' @return TBU
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
combine_columns <- function(.data, new_column_name, pattern, type = "starts_with") {
    UseMethod("combine_columns")
}

#' @rdname combine_columns
#' @export
combine_columns.default <- function(.data, new_column_name, pattern, type = "starts_with") {
  # Capturing the selected column name and selecting names
  quo_column_name <- rlang::enquo(new_column_name)
  selecting_function <- switch(
    type,
    "starts_with" = dplyr::starts_with,
    "ends_with"   = dplyr::ends_with,
    "matches"     = dplyr::matches,
    "contains"    = dplyr::contains,
    "one_of"      = dplyr::one_of,
    dplyr::starts_with
  )
  
  .data %>%
    dplyr::mutate_at(dplyr::across(selecting_function(pattern), trimws)) %>%
    dplyr::mutate_at(dplyr::across(selecting_function(pattern), ~ ifelse(is.na(.), "", .))) %>%
    tidyr::unite(!!quo_column_name, 
          selecting_function(pattern), 
          sep = "|", 
          remove = F) %>%
    dplyr::mutate(!!quo_column_name := gsub("\\|+", "|", !!quo_column_name),
           !!quo_column_name := trimws(!!quo_column_name),
           !!quo_column_name := gsub("\\|$", "", !!quo_column_name),
           !!quo_column_name := stringr::str_remove_all(!!quo_column_name, "^\\|"),
           !!quo_column_name := stringr::str_remove_all(!!quo_column_name, "^\\| ")
    )
}