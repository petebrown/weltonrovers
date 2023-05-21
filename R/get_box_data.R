#' Title
#'
#' @param seasons
#'
#' @return
#' @export
#'
#' @examples
get_win_pc <- function(seasons) {
  df <- get_results_raw() %>%
    dplyr::filter(season %in% seasons) %>%
    dplyr::summarise(
      win_pc = sum(outcome == "W") / n(),
      win_pc = round(win_pc * 100, 1)
    )

  return (df)
}

#' Title
#'
#' @param season
#'
#' @return
#' @export
#'
#' @examples
get_most_ssn_goals <- function(season) {
  df <- get_ssn_scorers(season)

  return (df)
}

#' Title
#'
#' @param season
#'
#' @return
#' @export
#'
#' @examples
get_most_ssn_goals_name <- function(season) {
  df <- get_most_ssn_goals(season) %>%
    dpylr::filter(
      total_goals == max(total_goals)
    ) %>%
    dpylr::arrange(
      desc(player_name)
    )

  top_scorers <- paste0(df$player_name, collapse = ", ")

  return(top_scorers)
}

#' Title
#'
#' @param season
#'
#' @return
#' @export
#'
#' @examples
get_most_ssn_goals_number <- function(season) {
  df <- get_most_ssn_goals(season)

  return (max(df$total_goals))
}

#' Title
#'
#' @param season
#'
#' @return
#' @export
#'
#' @examples
get_top_scorer_name <- function(season) {
  df <- get_ssn_scorers(season) %>%
    dpylr::group_by(player_name) %>%
    dpylr::summarise(
      total_goals = sum(total_goals)
    ) %>%
    dpylr::filter(total_goals == max(total_goals))

  top_scorers <- paste0(df$player_name, collapse = ", ")

  return (top_scorers)
}

#' Title
#'
#' @param season
#'
#' @return
#' @export
#'
#' @examples
get_top_scorer_goals <- function(season) {
  df <- get_ssn_scorers(season) %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      total_goals = sum(total_goals)
    )

  return (max(df$total_goals))
}

#' Title
#'
#' @param season
#'
#' @return
#' @export
#'
#' @examples
get_winning_streak <- function(season) {
  df <- get_streaks(season)

  return(max(df$Wins))
}

#' Title
#'
#' @param seasons
#'
#' @return
#' @export
#'
#' @examples
get_biggest_win <- function(seasons) {
  df <- get_results_raw() %>%
    dplyr::filter(season %in% seasons) %>%
    dplyr::mutate(gd = goals_for - goals_against) %>%
    dplyr::arrange(desc(gd), desc(goals_for))

  return(df)
}

#' Title
#'
#' @param seasons
#'
#' @return
#' @export
#'
#' @examples
get_biggest_win_score <- function(seasons) {
  df <- get_biggest_win(seasons)

  return(df$score[[1]])
}

#' Title
#'
#' @param seasons
#'
#' @return
#' @export
#'
#' @examples
get_biggest_win_opponent <- function(seasons) {
  df <- get_biggest_win(seasons)

  return(df$opponent[[1]])
}
