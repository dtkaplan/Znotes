#' Utilities for laying out exercises
#'
#' @param Roster A data frame with a `fname` column
#' containing the file name of the exercise
#' @param show_answers Logical: whether to show the answers
#' @param askMC function name for the askMC() typesetting
#' @param template Glue string setting formatting for each exercise
#' @export
format_exercises <- function(
    Roster,
    label_format = c("numeric", "alpha", "ALPHA", "roman", "ROMAN"),
    template = "## Exer. {number}\n\n **Exercise {number}**\n\n{contents}\n\n",
    askMC = Znotes::askMC) {

  # label_format <- match.arg(label_format)
  # # Generate a sequence of labels, either numeric or alpha
  # if (nrow(Roster) > 3000) stop("Maximum of 3000 exercises in one call.")
  # letters[12] <- "L" # Don't use lowercase L
  # labels <- switch(label_format,
  #                  numeric = 1:3000,
  #                  alpha = c(letters,
  #                            c(outer(letters, 1:99, paste0)),
  #                            c(outer(letters, letters, paste0))),
  #                  ALPHA = c(LETTERS,
  #                            c(outer(LETTERS, 1:99, paste0)),
  #                            c(outer(LETTERS, LETTERS, paste0))),
  #                  roman = tolower(utils::as.roman(1:3000)),
  #                  ROMAN = utils::as.roman(1:3000)
  # )
  # start_num <- which(labels == start_num)
  # labels <- paste0(chap, ".", labels)

  Res <- character(nrow(Roster))

  for (k in 1:nrow(Roster)) {
    markup <- try(add_exercise(
      Roster$fname[k],
      #paste0(Roster$block[k],"/", Roster$fname[k]),
      Roster$number[k],
      template=template,askMC=askMC)
      )


    Res[k] <- if (inherits(markup, "try-error")) {
      glue::glue("\n\nProblem with {Roster$block[k]} {Roster$fname[k]}\n\n")
    } else {
      markup
    }
  }

  paste(Res, collapse="\n\n")
}


add_exercise <- function(file_name, number,
                         template="## Exer. {number}\n\n {contents}\n\n",
                         # typesetter for individual exercises
                         askMC=Znotes::askMC) {
  where <- new.env()
  where$askMC <- askMC
  contents <- {
    Znotes:::MC_counter$reset()
    knitr::knit_child(file_name, quiet=TRUE)
  }
  glue::glue(template)
}

