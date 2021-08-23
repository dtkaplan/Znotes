#' Read a data file from the Znotes repository
#'
#' The `{Znotes}` repository on GitHub has an associated web site. Authorized
#' instructors can add CSV files to that site. These data files can then
#' be used in the Sandbox or in the R console.
#'
#' @param dname the complete name of the .csv file, e.g. `"USAFA/hawaii.csv"`
#' @param ... other arguments to readr::read_csv()
#'
#' @examples
#' hawaii <- Zdata("USAFA/hawaii.csv")
#'
#' @export
Zdata <- function(dname, ...) {
  suppressMessages(
    readr::read_csv(
      paste0("https://dtkaplan.github.io/Znotes/", dname), ...)
  )
}
