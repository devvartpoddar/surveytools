#' Grouper
#'
#' Wrapper around count functions
#'
#' @param .data Dataframe to group and count by
#'
#' @return Dataframe of columns and counts
#'
#' @importFrom magrittr "%>%"
#'
#' @export
grouper <- function(.data) {
  .data %>%
    dplyr::group_by_all() %>%
    dplyr::count()
}

#' Percent
#'
#' Percent function from formattable package
#'
#' @importFrom formattable percent
#' @export percent
#' @name percent
NULL

#' Crosstabs
#'
#' Function to allow creating cross tabs of outputs from summariser
#' @export
#'

cross_tab <- function(.data,
                      row_var,
                      col_var,
                      value,
                      ...,
                      combine = "sgnf") {

  # Capture all variables as symbols
  eq_row <- rlang::enquo(row_var)
  eq_col <- rlang::enquo(col_var)
  eq_val <- rlang::enquo(value)
  eq_com <- rlang::expr(c(...))
  tmp <- rlang::sym(combine)

  # keep only the variables that are present as row, col, value or eq
  .data <- .data %>%
    dplyr::mutate(sgnf = dplyr::case_when(
      sgnf %in% c("-----", "<<--|") ~ "",
      TRUE ~ sgnf
    )) %>%
    dplyr::select(!!eq_row, !!eq_col, !!eq_val, !!eq_com, !!tmp) %>%
    dplyr::unite(`_____TEMP VAL COL_____`, !!tmp, !!eq_val, sep = " ", na.rm = TRUE) %>% # nolint
    dplyr::mutate(`_____TEMP VAL COL_____` = trimws(`_____TEMP VAL COL_____`))


  # spread the col variable out, and keep the row
  .data <- .data %>%
    tidyr::spread(!!eq_col, `_____TEMP VAL COL_____`, drop = TRUE)
ta
  tibble::as_tibble(.data)
}