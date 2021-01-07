#' Logging
#'
#' Initialising a new logging env to save outputs in
.log_env <- new.env(parent = emptyenv())

.log_env$log_name <- "default"
.log_env$default  <- list(
    path = ".",
    text = "---\n title: 'Default log'\n ---"
    )

#' Initialise logging
#' 
#' Initialise html log by provide path to log. No extension is needed - will automate to html. 
#' 
#' @param path path / filename of the log to initialise
#' 
#' @export
log_init <- function(path) {

    # Force path to exist inorder to commence logging
    force(path)

    # Convert path to character, and find the name for the file
    cfile <- as.character(path) %>% 
        stringr::str_split("/") %>% 
        unlist()
    
    file_name <- cfile[length(cfile)]
    path_name <- do.call(file.path, as.list(cfile[-length(cfile)]))

    # if path is not given, assume current working directory
    if (length(path_name) == 0) {
        path_name <- "."
    }

    # check if file_name ends with .html or not, and remove if it does
    ends_html <- stringr::str_detect(file_name, ".html$")

    if (ends_html) {
        file_name <- sub(".html$", "", file_name)
    }

    # Create a log list in the .log_env, to hold the file name and string
    log_list <- list(
        path = path_name,
        text = glue::glue("---\n title: '{file_name}'\n ---")
    )

    # Creating list with path and text data
    assign(glue::glue("{file_name}"), log_list, .log_env) 

    # changing log_name value to initialised list
    assign("log_name", file_name, .log_env)

    # returning cfile
    invisible(log_list)
}  

#' Render log
#' 
#' Render the definied log file using rmarkdown with default options
#' 
#' @param log_name Name of the log to render. Defaults to last called log file
#' 
log_render <- function(log_name = "default") {

    # Get the list of text and path name based on the log_name called
    log_list <- get(log_name, envir = .log_env)

    # create a tempfile to send the text from the log to
    temp_file <- tempfile(tmpdir = log_list$path)

    crd_file <- file.create(temp_file)
    if (crd_file) {

        # Cat the text to the temporary file
        cat(log_list$text, file = temp_file)

        rmarkdown::render(temp_file,
            output_file = log_name,
            output_format = rmarkdown::html_document(
                toc = TRUE,
                toc_float = TRUE
            ),
            output_dir = log_list$path,
            encoding="UTF-8-BOM"
        )

        # Remove the created temp file
        unlink(temp_file)
    } 
}

#' Log table using formattable
#' 
#' Log a table of results using formattable. Has custom methods for the common summariser outputs as well as a generic method for all tables
#' 
#' @param .data Dataframe or summarised outputs to log to table
#' @param header Header for the table
#' @param ... additional options passed to formattable::format_table()
#' @param log Overwrite default log that gets logged to and rendered
#' 
#' @export
log_table <- function(.data, header = "", ..., log = NULL) {
      UseMethod("log_table")
}

#' @rdname log_table
#' @export
log_table.default <- function(.data, header = "", ..., log = NULL) {
    # Force data
    force(.data)

    # if not missing, check if the log is character, if not replace with default
    if (!missing(log)) {
        if(!is.character(log)) {
            warning("Provided log file name is not in a character format. Will default to last log name")
            log <- NULL
        }
    }

    tmp <- formattable::format_table(.data, ...)

    # convert to character and paste below text
    tmp <- as.character(tmp)

    # Get name and text of current log 
    if (is.null(log)) {
        log_name <- get("log_name", envir = .log_env)
    } else {
        log_name <- log
    }

    log_list <- get(log_name, envir = .log_env)

    log_list$text <- paste(log_list$text, glue::glue("### {header}"), tmp, sep = "\n\n")

    # assign the value back to log list in the original env
    assign(glue::glue("{log_name}"), log_list, .log_env) 

    # render the temp file
    suppressMessages(log_render(log_name))

    # Return the original table invisibly
    invisible(.data)
}

#' @rdname log_table
#' @export
log_table.svyt_df <- function(.data, header = "", ..., log = NULL) {
    # Special class for summariser outputs

    # 1. Modify the data to remove value se, and merge value_low and value_upp into a single piece
    .data <- .data %>%
        dplyr::mutate(value_range = glue::glue("[{value_low} - {value_upp}]"),
            pvalue = ifelse(is.na(pvalue) | is.nan(pvalue), 1, pvalue),
            sgnf = dplyr::case_when(
                sgnf %in% c("-----") ~ "",
                sgnf %in% c("<<--|") ~ "   <|",
                TRUE ~ sgnf
                )
            ) %>% 
        dplyr::select(group, response, N, value, value_range, pvalue, sgnf) 
    
    # 2. Send to log_table default with additional formats
    log_table.default(.data, 
        header = header,

        # List of formatters for formattable package
        formatters = list(
            value = formattable::normalize_bar("#6CA5BF"),
            formattable::area(row = -1, col = "N")       ~ formattable::color_tile("#f8c2da9c", "#f069a69c"),
            pvalue = formattable::formatter("span", 
                style = x ~ formattable::style(color = ifelse(x <= 0.01, "#45e0aafa", "#D9D6D6"), 
                    font.weight = ifelse(x <= 0.01, "bold", ""))
            ),
            sgnf = formattable::formatter("span", 
                style = ~ formattable::style(color = ifelse(pvalue <= 0.01, "#45e0aafa", "#D9D6D6"), 
                    font.weight = ifelse(pvalue <= 0.01, "bold", ""))
            )
        )
    )
}

#' Log text 
#' 
#' Log text using rmarkdown. Since it uses rmarkdown to render, a whole range of markdown as well as html are supported
#' 
#' @param ... text to be rendered using rmarkdown
#' @param log Overwrite default log that gets logged to and rendered
#' 
#' @export
log_text <- function(..., log = NULL) {

    # Capture text and paste using sep
    text = list(...) %>% 
        unlist() %>% 
        paste(collapse = "\n")
    
    # if not missing, check if the log is character, if not replace with default
    if (!missing(log)) {
        if(!is.character(log)) {
            warning("Provided log file name is not in a character format. Will default to last log name")
            log <- NULL
        }
    }

    # Get name and text of current log 
    if (is.null(log)) {
        log_name <- get("log_name", envir = .log_env)
    } else {
        log_name <- log
    }

    log_list <- get(log_name, envir = .log_env)

    log_list$text <- paste(log_list$text, text, sep = "\n\n")

    # assign the value back to log list in the original env
    assign(glue::glue("{log_name}"), log_list, .log_env) 

    # render the temp file
    suppressMessages(log_render(log_name))

    invisible(text)
}
