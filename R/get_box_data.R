#' Title
#'
#' @param seasons
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::summarise
#'
#' @return
#' @export
#'
#' @examples
get_win_pc <- function(seasons) {
  df <- get_results_raw() %>%
    dplyr::filter(season %in% seasons) %>%
    dplyr::summarise(
      win_pc = sum(outcome == "W") / dplyr::n(),
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
#' @param seasons
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::arrange
#' @inheritParams dplyr::case_when
#' @inheritParams stringr::str_glue
#'
#' @return
#' @export
#'
#' @examples
get_most_ssn_goals_name <- function(seasons) {
  df <- get_most_ssn_goals(seasons) %>%
    dplyr::filter(
      total_goals == max(total_goals)
    ) %>%
    dplyr::arrange(
      season, player_name
    ) %>%
    dplyr::mutate(
      player_and_season = dplyr::case_when(
        length(seasons) > 1 ~ stringr::str_glue("{player_name} ({season})"),
        TRUE ~ player_name
      )
    )

  if (length(seasons) > 1) {
    top_scorers <- paste0(df$player_and_season, collapse = ", ")
  } else {
    top_scorers <- paste0(df$player_name, collapse = ", ")
  }

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
#' @inheritParams dplyr::group_by
#' @inheritParams dplyr::summarise
#' @inheritParams source  dplyr::filter
#'
#' @return
#' @export
#'
#' @examples
get_top_scorer_name <- function(season) {
  df <- get_ssn_scorers(season) %>%
    dplyr::group_by(player_name) %>%
    dplyr::summarise(
      total_goals = sum(total_goals)
    ) %>%
    dplyr::filter(total_goals == max(total_goals))

  top_scorers <- paste0(df$player_name, collapse = ", ")

  return (top_scorers)
}


#' Title
#'
#' @param season
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::arrange
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
#' @param season
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::arrange
#'
#' @return
#' @export
#'
#' @examples
get_winning_streak_ssns <- function(season) {
  df <- get_streaks(season) %>%
    dplyr::filter(
      Wins == max(Wins)
    ) %>%
    dplyr::arrange(Season)

  win_streak_ssns <- paste0(df$Season, collapse = ", ")

  return(win_streak_ssns)
}

#' Title
#'
#' @param seasons
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::mutate
#' @inheritParams dplyr::arrange
#'
#' @return
#' @export
#'
#' @examples
get_biggest_win <- function(seasons) {
  df <- get_results_raw() %>%
    dplyr::filter(season %in% seasons) %>%
    dplyr::mutate(gd = goals_for - goals_against) %>%
    dplyr::arrange(
      desc(gd),
      desc(goals_for),
      date
    )

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
#' @inheritParams dplyr::mutate
#' @inheritParams stringr::str_glue
#'
#' @return
#' @export
#'
#' @examples
get_biggest_win_opponent <- function(seasons) {
  df <- get_biggest_win(seasons) %>%
    dplyr::mutate(
      oppo_and_ssn = stringr::str_glue("{opponent} ({venue}), {season}")
    )

  biggest_win_oppo <- df$oppo_and_ssn[[1]]

  return(biggest_win_oppo)
}


#' Title
#'
#' @param seasons
#' @inheritParams dplyr::group_by
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::ungroup
#'
#' @return
#' @export
#'
#' @examples
get_av_league_pts <- function(seasons) {
  df <- get_results_raw() %>%
    dplyr::group_by(season) %>%
    dplyr::filter(
      season %in% seasons,
      game_type == "league",
      comp_game_no == max(comp_game_no)
    ) %>%
    dplyr::ungroup()

  av_pts <- round(mean(df$ssn_pts), 1)

  return(av_pts)
}
