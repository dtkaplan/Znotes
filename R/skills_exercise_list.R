#' Format exercise-by-exercise skill list
#'
#' @export
skills_exercise_list <- function(fname = "Skill_list.csv",
                                 doc_link="http://www.mosaic-web.org/MOSAIC-Calculus/") {
  Exercises <- readr::read_csv(fname)
  Exercises$exercise <- as.character(Exercises$exercise)

  # Get the skills data base
  Skills_data_base <- Znotes::skills_table()
  # Go, skill by skill (in order of the exercises)
  skills_in_order <- Exercises$skill |> unique()
  # Prepare a blank result for filling in
  Results <- character(length(skills_in_order))

  counter <- 0
  for (skill in skills_in_order) {
    counter <- counter + 1
    skill_num <- which(skill == Skills_data_base$ID)

    if (length(skill_num) == 0) {
      ID <- glue::glue("{skill} missing from database")
      skill_text <- "[ ]"
      skill_topic <- "[ ]"
      skill_subtopic <- "[ ]"
    } else {
      ID <- skill
      skill_text     <- Skills_data_base$text[skill_num]
      skill_topic    <- Skills_data_base$topic[skill_num]
      skill_subtopic <- Skills_data_base$subtopic[skill_num]
    }

    # Pull out the matching exercises
    These_exercises <- Exercises[skill == Exercises$skill,]

    exercise_links <-
      with(These_exercises,
           glue::glue("    - [{exercise}]({doc_link}{link}#{hash}) {file}")
      ) |> paste( collapse="\n")

    Results[counter] <-
      glue::glue('\n\n1. {ID}: {skill_text}\n{exercise_links}\n\n')


  }

  return(Results)
}
