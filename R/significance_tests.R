#' @importFrom magrittr "%>%"
test_significance <- function(.data,
                              compare = compare) {

  # Function to test significance of a given mean against comparable or default values. Uses welch t test
  # TODO: Change default compare row by type; if numeric use the overall row, and if categorical,
  # use the row that meets the response

  # CHECK if data is 0 rows, return data
  if (nrow(.data) == 0) return(.data)

  # convert compare to lowercase, and trim ws
  compare <- trimws(compare) %>%
    tolower()

  # if compare is overall, pre-pend with "overall::"
  if (compare == "overall") {
    compare <- .data %>%
      dplyr::filter(group == "Overall") %>%
      dplyr::pull(response) %>%
      unique() %>%
      trimws() %>%
      tolower() %>%
      paste0("overall::", .)
  }

  # compare <- str_split(compare, "::") %>%
  #   unlist()

  # comparison row
  compare_row <- .data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(c("group", "response")), ~tolower(trimws(.)))) %>%
    dplyr::mutate(tmp = glue::glue("{group}::{response}")) %>%
    dplyr::filter(tmp %in% compare) %>%
    dplyr::select(tmp_response = response,
           compareN = N, compareV = value, compareVSE = value_se)

  # if compare row is null, push out a warning and use overall instead
  if (nrow(compare_row) == 0) {
    rlang::warn(glue::glue("The selected comparision does not exist in the data, defaulting to overall"))

    return(test_significance(.data, "overall"))
  }

  # If only one row is selected, it should be matched to all. If not then match by response
  if (nrow(compare_row) == 1) {
    # If only one single row is slected to be compared againgst
    significance_data <- .data %>%
      dplyr::mutate(compareN = compare_row$compareN,
             compareV = compare_row$compareV,
             compareVSE = compare_row$compareVSE) %>%
      dplyr::mutate(tmp_group = tolower(group),
             tmp_response = tolower(response),
             tmp_match = tolower(glue::glue("{tmp_group}::{tmp_response}")))
  } else {
    significance_data <- .data %>%
      dplyr::mutate(tmp_group = tolower(group),
             tmp_response = tolower(response),
             tmp_match = tolower(glue::glue("{tmp_group}::{tmp_response}"))) %>%
      dplyr::left_join(compare_row, by = c("tmp_response"))
  }

  significance_data <- significance_data %>%
    dplyr::mutate(pvalue = purrr::pmap_dbl(list(N,
                                  value,
                                  value_se,
                                  compareN,
                                  compareV,
                                  compareVSE), function(n, m, se, n0, m0, se0) {
      tmp <- welch_ttest(
        m1 = m,
        m2 = m0,
        s1 = se,
        s2 = se0,
        n1 = n,
        n2 = n0
      )

      tmp$p_value
      }),
      sgnf = dplyr::case_when(
        # Highlight row being compared against
        tmp_match %in% compare ~ "<<--|",

        # Significance stars
        pvalue < 0.001 ~ "[***]",
        pvalue < 0.05  ~ "[ **]",
        pvalue < 0.1   ~ "[  *]",
        TRUE           ~ "-----"
      )
    ) %>%
    dplyr::select(-tmp_group,
                -tmp_response,
                -tmp_match,
                -compareN,
                -compareV,
                -compareVSE)

  return(significance_data)
}

welch_ttest <- function(m1,
                         m2,
                         s1,
                         s2,
                         n1,
                         n2,
                         m0 = 0,
                         equal.variance = FALSE) {

  # correct for providing SE instead of SD
  s1 <- s1 * sqrt(n1)
  s2 <- s2 * sqrt(n2)

  if (equal.variance == FALSE) {
    se <- sqrt( (s1^2/n1) + (s2^2/n2) )
    # welch-satterthwaite df
    df <- ((s1^2/n1 + s2^2/n2)^2)/((s1^2/n1)^2/(n1 - 1) + (s2^2/n2)^2/(n2 - 1))
  } else {
    # pooled standard deviation, scaled by the sample sizes
    se <- sqrt((1/n1 + 1/n2) * ((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2) )
    df <- n1 + n2 - 2
  }

  t <- (m1 - m2 - m0)/se

  dat = tibble::tibble(
    difference_of_means = (m1 - m2),
    std_error = se,
    t_value = t,
    p_value = 2 * pt(-abs(t), df)
  )

  return(dat)
}
