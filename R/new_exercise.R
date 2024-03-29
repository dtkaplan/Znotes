#' RStudio addin to make a new CalcZ book exercise
#'
#' @export
new_calcZ_exercise <- function(directory = ".", exercise_id = NULL, save=TRUE) {
  # Does the directory exist
  if (!(grepl("/$", directory) || directory == "." || directory == ".."))
    stop("Directory name must terminate in a forward slash /.")
  tmp <- list.dirs(path = directory)
  if (length(tmp) == 0)
    stop("No directory <", directory, "> in which to create the file.")

  if (is.null(exercise_id))  exercise_id <- make_random_id()
  count = 0
  while (count < 10 && save && file.exists(paste0(exercise_id, ".Rmd"))) {
    exercise_id <- make_random_id()
    count = count + 1
  }
  if (count >= 10) stop("Can't find a random name for the new etude. Use exercise_id argument to make one of your own.")

  global_id <- new_exercise_hash() # A globally unique ID for the exercise

  contents <- glue::glue(
    "---
    id: \"{exercise_id}\"
    created: \"{date()}\"
    global_id: \"{global_id}\"
    ---"
  )

  project_dir <- gsub("^.*/", "", here::here())
  this_dir <- getwd()
  relative_path <- gsub(glue::glue("^.*{project_dir}/?", ""), "", this_dir)

  target_path <- if (nchar(relative_path) == 0) exercise_id
  else paste(relative_path, exercise_id, sep="/")

  insert_exercise <- glue::glue("`r insert_calcZ_exercise(\"XX.XX\", \"{global_id}\", \"{target_path}.Rmd\")`")

  contents <- paste0( contents, "\n\n", insert_exercise, "\n\n")
  new_file_name <- paste0(exercise_id, ".Rmd")

  tmp <- list.files(path = new_file_name)
  if (length(tmp) == 0) { # clear to create the file
    writeLines(contents, con = new_file_name)
    if (!rstudioapi::isAvailable())
      return()
    if (!rstudioapi::hasFun("navigateToFile"))
      return()
    rstudioapi::navigateToFile(new_file_name)
  } else {
    stop("File", target_path, "already exists.")
  }
}

MC_counter <- letter_counter()


#' @export
insert_calcZ_exercise <- function(number, hash, file_name, skill="unassigned") {
  nav_point <- exercise_navpoint(number, hash, fname=file_name, skill)
  contents <- {
    MC_counter$reset()
    knitr::knit_child(file_name)
  }
  #### place exercise in an aside
  # html_text <- '<details>
  #   <summary>{nav_point}</summary>
  #   {contents}</details>'
  html_text <- "::: {{.column-body-outset-left data-latex=''}}\n{nav_point}\n:::\n  {contents}"

  if (exists("book_file_name")) {
    skill <- strsplit(skill, "[ *|, ]", fixed=FALSE)[[1]]
    skill <- skill[nchar(skill) != 0]
    cat(paste0(paste(book_file_name, number,
              hash, file_name, skill, sep=", "), "\n"),
        file="Skill_list.csv", append = TRUE)
  }
  glue::glue(html_text)
}

# Permanently mark the exercise for navigation, regardless of exercise number
#' @export
exercise_navpoint <- ex.mark <- function(num, perm_id, fname="no file specified", skills="") {
  glue::glue('**Exercise {num}**: <span><img src="www/icons8-signpost.png" title="Location:{fname}" width="12px"/><span id="{perm_id}" style="color: red; font-size: 9pt;">{perm_id} {paste(skills, collapse=",")}</red></span>')
}


