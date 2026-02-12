#' Select Moira Phrases
#'
#' @param text_list A character vector of text items, included with this package
#' @param n Number of items to select.
#' @param replace Sample with replacement? (Default: FALSE)
#'
#' @return A randomly selected character vector.
#' @export
moirasays <- function(text_list = moira_list, n = 1, replace = FALSE) {
  if (!is.character(text_list)) {
    stop("text_list must be a character vector")
  }
  sample(text_list, size = n, replace = replace)
}
