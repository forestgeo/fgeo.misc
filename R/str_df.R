#' Convert a string to a dataframe where each string element is a column name.
#'
#' @param str Character string.
#'
#' @return Dataframe with character columns named `str` and one row full of
#'   `NA_character_`.
#'
#' @examples
#' str_df(letters[1:3])
#' @noRd
str_df <- function(str) {
  stopifnot(is.character(str), length(str) > 0)
  dfm <- as.data.frame(matrix(ncol = length(str)), stringsAsFactors = FALSE)
  stats::setNames(dfm, str)
}

key_first_census <- function() {
  list(
    root = c(
      "FormId", "Form Name", "Submitted On", "Form Version", "Submitted By",
      "Submission Id", "Date", "Team", "Quadrat"
    ),
    multi_stems = c(
      "Submission Id", "Quadrat", "Tag", "Stem_Tag", "Species", "Species_Code",
      "DBH", "Status", "Codes", "Notes", "POM"
    ),
    secondary_stems = c(
      "Submission Id", "Section Id", "Quadrat", "Tag", "Stem_Tag", "Species",
      "DBH", "Status", "Codes", "Notes", "POM"
    ),
    single_stems = c(
      "Submission Id", "Quadrat", "Tag", "Stem_Tag", "Species", "Species_Code",
      "DBH", "Status", "Codes", "Notes", "POM"
    )
  )
}

key_recensus <- function(variables) {
  list(
    original_stems = c(
      "Submission Id", "Section Id", "Quadrat", "Tag", "Stem Tag", "Species",
      "lx", "ly", "DBH", "Status", "Codes", "POM", "DBH 2018", "Status 2018",
      "Codes 2018", "Notes", "Data Check", "DBH check"
    ),
    new_secondary_stems = c(
      "Submission Id", "Section Id", "Quadrat", "Tag", "Species", "PX", "PY",
      "Stem Tag", "DBH", "Status", "Codes", "POM", "Notes", "New Stem"
    ),
    recruits = c(
      "Submission Id", "Quadrat", "Tag", "Stem Tag", "Species", "PX", "PY", "DBH",
      "Status", "Codes", "POM", "Notes"
    ),
    root = c(
      "FormId", "Form Name", "Submitted On", "Form Version", "Submitted By",
      "Submission Id", "Quadrat", "Date", "Team", "Stem Count", "View Map", "Map"
    )
  )
}

