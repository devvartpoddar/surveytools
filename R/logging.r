#' Logging
#'
#' Initialising a new logging env to save outputs in
.log_env <- new.env(parent = emptyenv())

.log_env$log_name <- "default"
.log_env$default  <- list(
    path = ".",
    text = "---\n title: 'Default log'\n ---"
    )

#' Function to initialise a new logger - defines the output file and options available, if any
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
    return(log_list)
}  

#' Function to render a log based on the input file
#' 
#' @export
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
            output_dir = log_list$path
        )

        # Remove the created temp file
        unlink(temp_file)
    } 
}

#' Function to render a data table using formattable
#' 
#' @export
log_table <- function(.data, ..., log = NULL) {
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

    log_list$text <- paste(log_list$text, tmp, sep = "\n")

    # assign the value back to log list in the original env
    assign(glue::glue("{log_name}"), log_list, .log_env) 

    # render the temp file
    log_render(log_name)

    # Return the original table invisibly
    invisible(.data)
}

#' Function to add text to output
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
    print(log_list$text)

    # assign the value back to log list in the original env
    assign(glue::glue("{log_name}"), log_list, .log_env) 

    # render the temp file
    log_render(log_name)

    return(text)
}
