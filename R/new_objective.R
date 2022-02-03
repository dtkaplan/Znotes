#' @export
new_objective <- function() {
  this_doc <- rstudioapi::getActiveDocumentContext()
  contents <- this_doc$contents

  # Grab the ID names from the document:
  IDs <- contents[grep("^[a-zA-Z]+.*:$", contents)]
  IDs <- gsub(":", "", IDs)

  # Generate a new unique hash
  for (k in 1:100) {
    newID <- paste0(sample(LETTERS, 1), "-",
           stringi::stri_rand_strings(1, 5, pattern = "[A-Z0-9]"),
           collapse="")
    if (! newID %in% IDs) break
  }
  path_components <- unlist(strsplit(this_doc$path, "/"))
  # topic <- path_components[length(path_components) - 1]

  fname <- basename(this_doc$path)
  topic <- gsub("\\.y.{1,3}$", "", fname)
  subtopic <- "+"
  yml <- construct_yaml(newID, topic, subtopic, fname)

  rstudioapi::insertText(
    yml,
    id = this_doc$id)
}

#'
construct_yaml <- function(newID, topic, subtopic,fname, text="Indent text") {
  glue::glue(
    '{newID}:\n',
    '- topic: {topic}\n',
    '- subtopic: {subtopic}\n',
    '- text: |\n    {text}\n',
    '- date: \"{date()}\"\n',
    '- author: {Sys.info()["user"]}\n',
    '- file: {fname}\n',
    '- ID: {newID}\n',
    '- successor: none\n\n\n')
}

#' @export
objective2dataframe <- function(fname) {
  raw <- yaml::yaml.load_file(fname)

  IDs <- names(raw)
  result <- data.frame()
  for (nm in IDs) {
    result <- dplyr::bind_rows(as.data.frame(raw[[nm]]), result)
  }

  result
}

#' @export
importZcalcObjectives <- function(fname) {
  Obs <- read.csv(fname)

  Res <- " "
  for (k in 1:nrow(Obs)) {
    ID <- paste0("Z", Obs[k, "Semester"], "-",
           stringi::stri_rand_strings(1, 5, pattern = "[A-Z0-9]"),
           collapse="")
    topic <- glue::glue('S{Obs[k, "Semester"]}-L{Obs[k, "Lesson"]}')
    yml <- construct_yaml(ID, "2021/22", subtopic=topic,
                          "import2021-22.yml", text = Obs[k, "Objective"])
    Res[k] <- yml
  }

  paste(Res, collapse="\n")
}
