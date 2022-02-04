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

#' An addin for putting a skills template in a document.
#' @export
add_skills <- function() {
  this_doc <- rstudioapi::getActiveDocumentContext()
  contents <- this_doc$contents
  ID_raw <- rstudioapi::primary_selection(this_doc$selection)$text
  if (nchar(ID_raw) == 0) stop("Must select text giving a skill ID.")
  skill_text <- check_skill(ID_raw) %>% gsub("\n$", "", .)
  if (nchar(skill_text) > 0) {
    hash <- paste0("Skill-link-",
                   stringi::stri_rand_strings(1, 5, pattern = "[A-Z0-9]"),
                   collapse="")
    text <- glue::glue("`r skill_point(\"{ID_raw}\", hash=\"{hash}\")`\n<!--Skill {ID_raw}:   {skill_text}  -->\n")
    rstudioapi::insertText( text,
                           id = this_doc$id)
  }
}

#' An addin for putting a skills template in a document.
#' @export
add_hash <- function() {
  this_doc <- rstudioapi::getActiveDocumentContext()
  contents <- this_doc$contents
  hash <- paste0(sample(LETTERS, 1), "-",
                 stringi::stri_rand_strings(1, 7, pattern = "[A-Z0-9]"),
                 collapse="")

  rstudioapi::insertText(hash,
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

# Create a data frame of objectives
Skill_env <- new.env()
#' @export
skills_table <- function() {
  # Check if this has already been done
  if ("Skills" %in% names(Skill_env)) return(Skill_env$Skills)
  # If not, read in the files
  yml_files <- dir(system.file("Objectives", package="Znotes"),
                  full.names=TRUE)
  keepers <- yml_files[grepl(".yml$|.yaml$", yml_files)]
  Skills <- tibble()
  for (fname in keepers) {
    tmp <- objective2dataframe(fname)
    Skills <- dplyr::bind_rows(Skills, tmp)
  }
  assign("Skills", Skills, envir = Skill_env)
  Skills
}

#' @export
check_skill <- function(txt) {
  Skills <- skills_table()
  if (txt %in% Skills$ID) {
    rownum <- which(Skills$ID == txt)
    return(Skills[[rownum, "text"]])
  } else {
    best <- stringdist::amatch(txt, Skills$ID ,maxDist=3)
    message("Maybe this one:", Skills$ID[best], "\n", Skills$text[best])
    return("")
  }
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
