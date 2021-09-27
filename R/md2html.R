#' Prepare markdown-like syntax for display in Shiny
#'
#' @param s character string containing simple markdown/latex-math
#'
#' @return  Another character string containing the equivalent of s in HTML.
#' This is suitable for an htmlOutput() display in Shiny. You'll need to
#' escape the HTML by using the html() function in your shiny::renderText()
#' @export
md2html <- function(s) {
  backtick <- "`([^`]*)`"
  doubledollar   <- "\\${2}([^\\$]+)\\${2}"
  dollar   <- "\\${1}([^\\$]+)\\${1}"
  bold     <- "\\*{2}([^\\*]*)\\*{2}"
  italics  <- "\\*{1}([^\\*]*)\\*{1}"
  s <- gsub(backtick, "<code>\\1</code>", s, perl=TRUE)
  s <- gsub(doubledollar, "QQQQQ\\1QQQQQ", s, perl=TRUE)  # re-encode the $$
  s <- gsub(dollar, "\\\\\\(\\1\\\\)", s, perl=TRUE)
  s <- gsub(bold, "<strong>\\1</strong>", s, perl=TRUE)
  s <- gsub(italics, "<em>\\1</em>", s, perl=TRUE)
  s <- gsub("QQQQQ", "\\$\\$", s, perl=TRUE) # put back the $$

  s
}

