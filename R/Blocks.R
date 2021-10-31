#' Variations on RMarkdown blocks
#'
#' Creates from an inline chunk output that resembles a regular
#' RMarkdown chunk, but with more compact formatting.
#'
#' @param cmd The expression to be run
#' @param digits How many digits to round to
#' @param ignore_cmd If TRUE, just typeset the result
#' @param phrase Character string separating code from output
#' @param display If TRUE, use $$ rather than single $
#' @param inline If TRUE, typeset the output next to the command
#' @param width Character string such as "40%" saying how big the command
#' block should be.
#'
#' @export

matrix_block <- function(cmd, digits=4, ignore_cmd=FALSE, phrase="      ",
                         display=FALSE, inline=TRUE, width="40%") {
  raw <- substitute(cmd)
  result <- eval(raw)
  dollars <- ifelse(display, "$$", "$")
  result <- round(result, digits)
  if (is.matrix(result) && prod(dim(result)) > 1) {
    result = matrix2latex(result)
  }
  browser()
  if (ignore_cmd) {
    paste0(phrase, dollars, result, dollars)
  } else if (inline) {
    paste0("<div><span><pre class='r'  style='width:",width,";overflow:auto;float:left;'>",
           paste(as.character(raw[-1]), collapse="\n"),
           "</pre><span>", " ",phrase," ",
           dollars, result, dollars, "</div></br>")
  } else {
    paste0("<pre class='r'  style='width:",width,";overflow:auto;'>",
           paste(as.character(raw[-1]), collapse="\n"),
           "</pre>", " ",phrase," ",
           dollars, result, dollars, "</br>")
  }
}

#' helper function for typesetting matrices.
matrix2latex <- function(matr) {

  printmrow <- function(x) {
    paste0(paste0(x,collapse=" & "),"\\\\")
  }

  body <- paste(apply(matr,1,printmrow), collapse="\n")
  # it doesn't matter how many columns, so long as there are enough
  paste0("\\left[\\strut\\begin{array}{rrrrrrrrrrrrrrrrrrrrrrrrrrrrr}", body, "\\end{array}\\right]")
}
