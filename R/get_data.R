#' Title
#'
#' @return
#' @export
#'
#' @examples
get_chart_options <- function() {
  charts <- c(
    "Point Accumulation" = "pts",
    "Points-per-game" = "ppg"
  )
  return(charts)
}

#' Title
#'
#' @return
#' @export
#' @inheritParams readr::read_csv
#'
#' @examples
get_results_raw <- function() {
  results <- readr::read_csv("./data/results.csv", show_col_types = FALSE)
  return(results)
}


#' Title
#'
#' @return
#' @export
#' @inheritParams dplyr::arrange
#' @inheritParams dplyr::select
#'
#' @importFrom magrittr %>%
#'
#' @examples
get_results <- function() {
  results <- get_results_raw() %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::select(
      season,
      game_no,
      date,
      opponent,
      venue,
      score,
      outcome,
      scorers,
      competition,
      cup_round,
      attendance,
      manager,
      referee,
      manager
    )

  return(results)
}


#' Title
#'
#' @param seasons
#'
#' @return
#' @export
#' @inheritParams dplyr::filter
#'
#' @examples
filter_results <- function(seasons) {
  results <- get_results() %>%
    dplyr::filter(season %in% seasons)
  return(results)
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
get_season_list <- function() {
  results <- get_results()
  seasons <- sort(unique(results$season), decreasing = TRUE)

  return(seasons)
}

#' Title
#'
#' @return
#' @export
#' @inheritParams readr::read_csv
#'
#' @examples
get_scorers <- function() {
  scorers <- readr::read_csv("./data/scorers.csv", show_col_types = FALSE)

  return(scorers)
}

#' Title
#'
#' @param seasons
#' @inheritParams dplyr::right_join
#'
#' @return
#' @export
#'
#' @examples
filter_scorers <- function(seasons) {
  scorers <- get_scorers()
  results <- filter_results(seasons)
  df <- scorers %>%
    dplyr::right_join(results, by = "date", relationship = "many-to-many")
  return(df)
}

#' Title
#'
#' @param seasons
#' @inheritParams dplyr::group_by
#' @inheritParams dplyr::summarize
#' @inheritParams dplyr::ungroup
#' @inheritParams dplyr::group_by
#' @inheritParams dplyr::slice_max
#' @inheritParams dplyr::ungroup
#' @inheritParams dplyr::arrange
#'
#' @return
#' @export
#'
#' @examples
get_ssn_scorers <- function(seasons) {
  df <- filter_scorers(seasons) %>%
    dplyr::group_by(season, player_name) %>%
    dplyr::summarize(
      total_goals = sum(goals_scored),
      .groups = "keep"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(season) %>%
    dplyr::slice_max(
      n = 3,
      order_by = total_goals
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(
      season,
      desc(total_goals),
      desc(player_name)
    )

  return(df)
}
