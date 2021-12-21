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
  result <- eval(raw, envir=.GlobalEnv)
  dollars <- ifelse(display, "$$", "$")
  result <- round(result, digits)
  if (is.matrix(result) && prod(dim(result)) > 1) {
    result = matrix2latex(result)
  }
  if (ignore_cmd) {
    paste0(phrase, dollars, result, dollars)
  } else if (inline) {
    paste0("<div><span><pre class='sourceCode'  style='width:",width,";overflow:auto;float:left;'>",
           paste(as.character(raw[-1]), collapse="\n"),
           "</pre>", " ",phrase," ",
           dollars, result, dollars, "</span><br></div>")
  } else {
    paste0("<pre class='sourceCode'  style='width:",width,";overflow:auto;'>",
           paste(as.character(raw[-1]), collapse="\n"),
           "</pre>", " ",phrase," ",
           dollars, result, dollars, "<br>")
  }
}

#' @export

oneline_block <- function(cmd, digits=4, phrase="\\(\\color{blue}{\\longrightarrow}\\)",
                          comment="",
                          width="40%", inline=TRUE) {
  raw <- substitute(cmd)
  result <- eval(raw, envir = knitr::knit_global())
  if (inherits(raw, "<-")) {
    # It's an assignment, so put it in the global environment
    vname <- all.names(raw[[2]])
    assign(vname, result, envir=knitr::knit_global())
  }
  if (inline) result <- round(result, digits)
  if (nchar(comment) > 0) comment <- paste("#", comment)

    if (!inline) {
    result <- ""
  } else if (is.matrix(result)) {
    result <- paste(capture.output(result), collapse="\n")
  } else if (is.data.frame(result)) {
    result <- knitr::kable(result) %>%
      kable_paper("hover", html_font="Courier New", full_width = FALSE)
  } else {
    result <- paste0("<code class='sourceCode' style=\"color: blue;\">",
                     result, "</code></p>")
  }




  paste0("<code class='sourceCode'  style='width:",width,";overflow:auto;float:left;'>",
           paste(paste(" ", as.character(raw[-1])), comment, collapse="\n"),
           "</code>", " ", phrase, result)
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
