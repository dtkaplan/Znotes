#' Formats tables to show top and bottom rows.
#'
#' @param data A data frame to display
#' @param top An integer. How many of the first rows to show.
#' @param bottom Like `top`, for for the bottom rows.
#' @param message Character string to place between the top and bottom rows.
#' @param format Either `"html"` or `"latex"`.
#' @param row.names If `TRUE`, will include row.names in table
#' @param position  can be "center", "left", "right", "float_left", or "float_right"
#' @param full_width If `FALSE`, table will be condensed
#' @param options For compatibility with knitr `render=` chunk option
#'
#' For Rmd documents, the format will be detected automatically if you don't use the
#' format argument.`
#'
#' @export
and_so_on <- function(data, top=3, bottom=2,
                      message=ifelse(bottom==0,
                                     "... until row {nrows}",
                                     "... {nrows} rows in total ..."),
                      format = c(NULL, "html", "latex"),
                      row.names = FALSE,
                      full_width = TRUE,
                      position = c("center", "left", "right", "float_left", "float_right"),
                      options) {
  format <- match.arg(format)
  position = match.arg(position)
  if (is.null(format)) {
    if (knitr::is_html_output()) "html"
    else if (knitr::is_latex_output()) "latex"
  }
  ncolumns <- length(data)
  nrows <- nrow(data)
  if (top + bottom >= nrows) {
    top <- nrows
    bottom <- 0
    message <- ""
  }
  message <- glue::glue(message)
  Top <- knitr::kable(head(data, top),
                      format=format, row.names=row.names, booktabs=TRUE) |>
    kableExtra::kable_classic("hover", full_width=full_width, position=position) |>
    strsplit("\n") |> unlist()
  Bottom <- knitr::kable(tail(data, bottom),
                         format=format, row.names=row.names, booktabs=TRUE) |>
    strsplit("\n") |> unlist()

  if (format == "html") {
    topdrop <- grep("</table>|</tbody>", Top)
    bottomdrop <- grep("<table|<thead>|</thead>|</th>|<tbody>", Bottom)
    #Bottom <- gsub("\\<thead\\>.*\\<\\thead>", "", Bottom)
    middle <- glue::glue("  <tr><td colspan={ncolumns} align=\"center\">{message}</td></tr>")
    res <- paste(c(Top[-topdrop], middle, Bottom[-bottomdrop]), collapse="\n")

    structure(res, format = "html", class = "knitr_kable")
  } else if (format == "latex") {
    bottomdrop <- 1:5
    topdrop <- c(length(Top) - 0:1)
    # Note the double slashes added to the end of the string ...
    middle <- glue::glue("\\multicolumn{{{ncolumns}}} {{c}} {{\\itseries {message}}}\\\\")
    res <- paste(c(Top[-topdrop], middle, Bottom[-bottomdrop]), collapse="\n")
  } else {
    stop(paste("Unrecognized table format argument:", format))
  }

  knitr::asis_output(res)
}

#' @export
call_and_so_on <- function(...) {
  args <- list(1, ...) # 1 is a placeholder for the data frame
  function(dat, ...) {
    if (length(args) > 1) {
      args[[1]] <- dat
      do.call(and_so_on, args)
    }
    else and_so_on(dat)
  }
}
