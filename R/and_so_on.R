#' Formats tables to show top and bottom rows.
#'
#' @param data A data frame to display
#' @param top An integer. How many of the first rows to show.
#' @param bottom Like `top`, for for the bottom rows.
#' @param message Character string to place between the top and bottom rows.
#'
#' @export
and_so_on <- function(data, top=3, bottom=2, message="... and so on ...") {
  ncolumns <- length(data)
  Top <- knitr::kable(head(data, top), format="html", row.names=FALSE) |>
    strsplit("\n") |> unlist()
  Bottom <- knitr::kable(tail(data, bottom), format="html", row.names=FALSE) |>
    strsplit("\n") |> unlist()
  topdrop <- grep("</table>|</tbody>", Top)
  bottomdrop <- grep("<table>|<thead>|</thead>|</th>|<tbody>", Bottom)
  #Bottom <- gsub("\\<thead\\>.*\\<\\thead>", "", Bottom)
  middle <- glue::glue("  <tr><td colspan={ncolumns} align=\"center\">{message}</td></tr>")
  res <- paste(c(Top[-topdrop], middle, Bottom[-bottomdrop]), collapse="\n")

  structure(res, format = "html", class = "knitr_kable")
}
