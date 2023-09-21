
#' Open a modeltuner cheatsheet
#' 
#' `modeltuner_cheatsheet()` opens a pdf file (\dQuote{cheatsheet}) summarizing 
#' important concepts and functions in the `modeltuner` package.
#'
#' @param open Logical: If `TRUE`, a cheatsheet in pdf format is opened (via shell command);
#' If `FALSE`, the path to the pdf file is returned as a character string.
#' 
#' @return Path to the cheatsheet in pdf format
#' 
#' @examples
#' \dontrun{modeltuner_cheatsheet()}
#' 
#' # Display path to pdf:
#' modeltuner_cheatsheet(open = FALSE)
#' 
#' @export
modeltuner_cheatsheet <- function(open = TRUE){
  chsheet <- system.file("Misc/cheatsheet.pdf", package = "modeltuner")
  if (open){
    system(paste0("open \"", normalizePath(chsheet), "\""))
    invisible(chsheet)
  } else {
    chsheet
  }
}

