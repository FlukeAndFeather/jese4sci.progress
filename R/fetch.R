#' Fetch all jese4sci issues
#'
#' @return `[data.frame]` with columns:
#'   - github_username
#'   - track
#'   - lesson
#'   - url
#'   - is_open
#'
#' @export
fetch_issues <- function() {
  repos <- c("jese4sci-RC", "jese4sci-MOD", "jese4sci-DOC", "jese4sci-VAL")
  purrr::map_dfr(repos, fetch_track_issues)
}

#' Fetch jese4sci issues for one track
#'
#' @param repo `[character(1)]` Which jese4sci track repo to fetch issues from.
#'
#' @return `[data.frame]` See `fetch_issues()`.
#'
#' @keywords internal
fetch_track_issues <- function(repo = c("jese4sci-RC",
                                        "jese4sci-MOD",
                                        "jese4sci-DOC",
                                        "jese4sci-VAL")) {
  repo <- match.arg(repo)
  issue_list <- gh::gh(glue::glue("/repos/FlukeAndFeather/{repo}/issues?state=all"))

  github_username <- purrr::map_chr(issue_list, c("user", "login"))
  lesson <- purrr::map_chr(issue_list, "title") %>%
    stringr::str_extract("[A-Z]+[0-9]{3}")
  url <- purrr::map_chr(issue_list, "html_url")
  open <- purrr::map(issue_list, "closed_at") %>%
    purrr::map_chr(~ !is.null(.x))

  dplyr::tibble(
    github_username,
    track = stringr::str_extract(repo, "[A-Z]+$"),
    lesson,
    url,
    is_open
  ) %>%
    dplyr::filter(is_assignment(lesson, repo))
}

#' Check if issue is an assignment
#'
#' @param issue_title `[character]` Vector ofGitHub issue titles. Must all be
#'   from the same repository.
#' @param repo `[character(1)]` Repository name e.g. `"jese4sci-RC"`
#'
#' @return `[logical]` Same length as `issue_title`, indicating is issue
#'   corresponds to an assignment or not.
#'
#' @keywords internal
is_assignment <- function(issue_title, repo) {
  track <- stringr::str_extract(repo, "[A-Z]+$")
  stringr::str_detect(issue_title, glue::glue("{track}[0-9]{{3}}"))
}
