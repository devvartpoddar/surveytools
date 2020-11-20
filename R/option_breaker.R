#' Option breaker
#'
#' TBU
#'
#' @param .data Dataframe 
#' @param str TBU
#' @param to_check TBU
#' @param group TBU 
#' 
#' @return TBU
#' 
#' @importFrom magrittr "%>%"
#' 
#' @export
option_breaker <- function(.data, str, to_check, group = "") {
    UseMethod("option_breaker")
}

#' @rdname option_breaker
#' @export
option_breaker.default <- function(.data, str, to_check, group = ""){
  if (group == ""){
    .data$group <- "All"
  } else {
    .data$group <- .data[, group]
  }
  
  list_of_response <- .data %>%
    dplyr::select(dplyr::matches(paste0(str,"o[0-9]+"))) %>%
    names() %>%
    stringr::str_replace(str, "")
  
  summary <- purrr::map_dfr(list_of_response, function(x) {
    .data %>% 
      dplyr::mutate(temp = .[[paste0(str, x)]]) %>%
      combine_columns(combined, c("group", "temp")) %>%
      # TODO: Remove summariser and add in count and percentage
      summariser(combined) %>% 
      dplyr::filter(combined != "Overall") %>% 
      dplyr::separate(combined, c("group", "temp"), sep = "\\|") %>% 
      dplyr::filter(temp == to_check) %>% 
      dplyr::mutate(response = x) %>% 
      dplyr::select(-temp)
  }) %>%
    dplyr::mutate(temp = perc_to_dec(perc_overall)) %>%
    dplyr::arrange(group, -temp) %>%
    dplyr::select(group, response, everything(), -temp)
  
  summary <- purrr::map_dfr(list_of_response, function(x) {
    .data %>% 
      dplyr::mutate(temp = .[[paste0(str, x)]]) %>%
      # TODO: Remove summariser and add in count and percentage
      summariser(temp) %>%
      dplyr::mutate(group = "Overall") %>%
      dplyr::filter(temp == to_check) %>%
      dplyr::mutate(response = x) %>%
      dplyr::select(-temp)
  }) %>%
    dplyr::mutate(temp = perc_to_dec(perc_overall)) %>%
    dplyr::arrange(group, -temp) %>%
    dplyr::select(group, response, everything(), -temp) %>%
    dplyr::bind_rows(summary)
  
  summary <- .data %>% 
    dplyr::select(group) %>%
    grouper() %>% 
    dplyr::rename(tempSum = n) %>% 
    dplyr::bind_rows(list("Overall", sum(.$tempSum))) %>% 
    dplyr::full_join(summary, by = "group") %>% 
    dplyr::mutate(perc_overall_corrected = dec_to_perc(overall/tempSum)) %>%
    dplyr::select(-tempSum)
  
  rm(.data)
  
  if (group == ""){
    summary <- summary %>% 
        dplyr::filter(group == "Overall")
  }
  
  return(summary)
}