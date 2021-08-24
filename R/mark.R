#' Places position marks in bookdown file
#'
#' Add a milestone markers to the text of the bookdown document
#' making it easier to readers to refer to a particular place in the
#' document.
#'
#' @param id character or numeric. Id for the milestone. For proper functionality
#' this should be unique within the document.
#'
#' @examples
#' \dontrun{
#' `r mark(4510)`  # place in Rmd file
#' }
#'
#' @export
mark <- function(id) {
  id <- as.character(substitute(id))
  glue::glue('<span style="float: right; padding-left: 50px;"><a name="{id}" href="#{id}"><img src="www/icons8-signpost.png" title="Location: {id}" width="12px"/></a><span style="color: red; font-size: 6pt;">{id}</red></span>')
}

#' @rdname mark
#' @export
exercise_navpoint <- function(num, perm_id, fname="no file specified") {
  glue::glue('**Exercise {num}**: <span><a name="File: {fname}" href="#{perm_id}"><img src="www/icons8-signpost.png" title="Location: {fname}" width="12px"/></a><span style="color: red; font-size: 9pt;">{perm_id}</red></span>')
}

