#' Formatting multiple-choice questions for the book
#' @export
askMC <- function (prompt = "The question prompt", ..., id = NULL, right_one = NULL,
                   inline = FALSE, random_answer_order = FALSE, allow_retry = TRUE,
                   correct = "Right!", incorrect = "Sorry.", message = NULL,
                   post_message = NULL, submit_button = "Check answer", try_again_button = "Try again",
                   allow_multiple_correct = FALSE, show_feedback=TRUE,
                   out_format=c("PDF", "Markdown", "GradeScope"),
                   item_label = "Part ",
                   show_answers=FALSE) {
  out_format <- match.arg(out_format)

  out <- paste(prompt, "\n\n")

  raw_labels <- c("i", "ii", "iii", "iv", "v", "vi", "vii", "viii", "ix", "x")
  answer_labels <- c(raw_labels,
                     paste0("x", raw_labels),
                     paste0("xx", raw_labels),
                     paste0("xl", raw_labels),
                     paste0("l", raw_labels),
                     letters, LETTERS, paste0(letters, "2"))
  answer_table <- dots_to_answers(..., right_one = right_one,
                                           allow_multiple_correct = allow_multiple_correct)


  ## GradeScope output module
  if (out_format == "GradeScope") {

    answers <- paste0("(", ifelse(answer_table$correct, "x ", " "), ")  ",
                      fix_dollar_signs(answer_table$item), collapse="\n")

    feedback_for_correct <- answer_table$feedback[answer_table$correct]
    if (nchar(gsub(" *", "", feedback_for_correct)) == 0)
      feedback_for_correct <- random_success()

    feedback <- paste0("[[",
                       paste(fix_dollar_signs(feedback_for_correct),
                             collapse = "\n"),
                       "]]\n")

    total <- paste(fix_dollar_signs(out), answers, feedback, sep="\n\n")

    Res <- knitr::asis_output(paste0("<pre>",  total, "\n</pre>\n"))

    return(Res)
  }
  ## End of GradeScope module

  ## latex/PDF output module
  if (out_format == "PDF") {
    # choices <- format_answers_markdown(answer_table)
    choices <- format_answers_PDF(answer_table, width=40,
                                  seed=ifelse(random_answer_order, 435, NA),
                                  show_answers=show_answers)
    Res <- knitr::asis_output(paste0(
      "\n",
      "\t**", item_label, MC_counter$get(), "**  ", out, "\n",
      paste0(choices, collapse="\n"), "\n"))

    return(Res)
  }
  ## End of latex/PDF module


  # make all feedback strings the same length, so items will be
  # evenly spaced
  raw_feedback <- answer_table$feedback
  # raw_feedback <- stringr::str_pad(raw_feedback,
  #                                  max(nchar(raw_feedback)),
  #                                  side="right", pad=".") # pad="‥")


  place_inline <- inline || (sum(nchar(answer_table$item) + nchar(raw_feedback)) < 80)

  if (place_inline) {
    answer_labels <- paste0(rep("    ", nrow(answer_table)))
    newline <- "   "
    success <- "$\\heartsuit\\ $"
    container <- "span"
  } else {
    answer_labels <- paste0(answer_labels, ". ")[1:nrow(answer_table)]
    newline <- "     \n"
    success <- paste0(random_success(), " ")
    container <- "span"

  }

  if (show_feedback) {
    feedback <- paste0("<", container, " class='mcanswer'>",
                      ifelse(answer_table$correct, success, "︎✘ "),
                      raw_feedback) # haven't yet closed <span>
    feedback <- paste0(feedback, "</", container, "></span>") # close it up
  } else {
    feedback <- ""
  }


  answers <- paste0(answer_labels[1:nrow(answer_table)],
                    "<span class='Zchoice'>",
                    answer_table$item,
                    feedback,
                    collapse = newline)

  knitr::asis_output(paste0(
    "**Question ", MC_counter$get(), "**  ",
    out, answers))
}


# For Gradescope output
#' @rdname askMC
#' @export
askGS <- function(...) {
  askMC(..., out_format = "GradeScope")
}
#' @export
askPDF <- function(...) {
  askMC(..., out_format = "PDF")
}
#' @rdname askMC
#' @export
#'
# fix the dollar signs for GradeScope
fix_dollar_signs <- function(str) {
  str <- gsub("\\${1}", "\\$\\$", str)
  str <- gsub("\\${4}", "\\$\\$\\$", str)
  str
}

format_answers_PDF <- function(answer_table, width=40, seed=NA, show_answers=FALSE) {
    Ans <- tibble::tibble(
      text = answer_table$item
    )
    if (!is.na(seed)) {
      set.seed(seed)
      Ans <- sample_n(Ans, size=nrow(Ans))
    }
    if (max(nchar(Ans$text), na.rm = TRUE) > width/2) {
      # lay them out one to a line
      paste(paste0(letters[1:nrow(Ans)], ". ", Ans$text), collapse="\n")
    } else {
      paste(Ans$text, collapse="\\hspace{3em}")
    }
}

# split_rows <- function(nchars, max_width = 25) {
#   row <- 1
#   rows <- rep(1, length(nchars))
#   sofar <- 0
#   for (k in 1:length(nchars)) {
#     if (sofar + nchars[k] < max_width) {
#       sofar <- sofar + nchars[k]
#       rows[k] <- row
#     } else {
#       row <- row+1
#       sofar <- nchars[k]
#       rows[k] <- row
#     }
#   }
#   rows
# }

# format_answers_markdown <- function(answer_table, width=40, seed=NA, padding=2) {
#   Ans <- tibble::tibble(
#     text = answer_table$item
#   )
#   cell_width <- max(nchar(Ans$text)) + 2
#   Ans <- Ans %>% mutate(nletters = cell_width) # making them all equally wide
#   if (!is.na(seed)) {
#     set.seed(seed)
#     Ans <- sample_n(Ans, size=nrow(Ans))
#   }
#   if (any(Ans$nletters > width/2)) {
#     # lay them out one to a line
#     paste(paste0(letters[1:nrow(Ans)], ". ", Ans$text), collapse="\n")
#   } else {
#     # break them up into groups according to length
#     Ans$group=split_rows(Ans$nletters, width)
#
#     Ans <- Ans %>% group_by(group) |>
#       mutate(col=row_number()) |>
#       ungroup()
#     Mat <- matrix("", ncol=max(Ans$col), nrow=max(Ans$group))
#     for (k in 1:nrow(Ans)) {
#       Mat[(Ans$group[k]), (Ans$col[k])] <- Ans$text[k]
#     }
#     if (knitr::is_latex_output()) format <- "latex"
#     else if (knitr::is_html_output()) format <- "html"
#     else format <- "latex"
#     row_width <- paste0(cell_width, "em")
#     Res <- knitr::kable(Mat, format=format, align="c") %>%
#       kableExtra::kable_paper("hover", full_width = FALSE)
#     if (knitr::is_latex_output()) {
#       # strip off the tabular environment
#       Res %>%
#         gsub("\\end{table}", "", ., fixed=TRUE) %>%
#         gsub("\\begin{table}", "", ., fixed=TRUE) %>%
#         gsub("\\centering", "", ., fixed=TRUE)
#     } else {
#       Res %>%
#         kableExtra::column_spec(1:ncol(Mat), width = row_width, border_left=TRUE)
#     }
#
#   }
# }

# return a data frame with one row for each element of ...
dots_to_answers <- function(..., right_one = "",
                            allow_multiple_correct = FALSE) {
  dots <- list(...)
  if (length(dots) == 1) {
    if (is.list(dots[[1]])) choices <- dots[[1]]
    else if (is.vector(dots[[1]])) {
      # it's a character or numerical vector
      choices <- as.list(rep("", length(dots[[1]])))
      names(choices) <- dots[[1]]
    }
  } else {
    choices <- dots
  }

  display <- names(choices)
  no_feedback <- if (is.null(display)) {
    # no names whatsoever
    display <- unlist(choices)
    choices <- as.list(rep("", length(display)))
    names(choices) <- display
    NULL
  } else which(display == "")
  # if it's not named, use the value as the name
  if (length(no_feedback) > 0) {
    display[no_feedback] <- choices[no_feedback]
    choices[no_feedback] <- "" # blank feedback
  }
  names(choices) <- display # update the names
  feedback <- unlist(choices)
  names(feedback) <- NULL
  # store as a data frame
  answers <- tibble(item=names(choices), feedback=feedback)
  if (!is.null(right_one)) answers$correct <- answers$item %in% right_one
  else answers$correct <- grepl("^\\+.*\\+$", answers$item)
  answers$item[answers$correct] <-
    gsub("^\\+(.*)\\+$", "\\1", answers$item[answers$correct])
  if (sum(answers$correct) == 0)
    stop("Must provide one correct answer.")
  if (sum(answers$correct) > 1 && !allow_multiple_correct)
    stop("Must give only one correct answer.")

  answers
}

#' @rdname askMC
#' @export
format_latex_answers <- function(AT, linechars=50) {

}


#' @rdname askMC
#' @export
letter_counter <- function() {
  counter <- 0
  uppercase <- c(LETTERS, paste0(LETTERS, 1), paste0(LETTERS, 2),
             paste0(LETTERS, 3), paste0(LETTERS, 4), paste0(LETTERS, 5))
  lowercase <- tolower(uppercase)
  numbers <- 1:5000
  ROMAN <- utils::as.roman(numbers)
  roman <- tolower(ROMAN)
  names <- uppercase
  res <- list()
  res$reset <- function(s = 0, labels=c("uppercase", "lowercase", "numbers", "ROMAN", "roman")) {
    labels <- match.arg(labels)
    names <<- parent.env(environment())[[labels]]
    counter <<- s
  }
  res$get   <- function() {
    counter <<- counter+1
    names[counter %% length(names)] # never run out
  }
  res
}
