#' Title
#'
#' @param seasons
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::arrange
#' @inheritParams dplyr::group_by
#' @inheritParams dplyr::mutate
#' @inheritParams dplyr::rename
#' @inheritParams dplyr::summarize
#'
#' @return
#' @export
#'
#' @examples
get_streaks <- function(seasons) {
  streaks <- get_results_raw() %>%
    dplyr::filter(season %in% seasons) %>%
    dplyr::arrange(season, date) %>%
    dplyr::group_by(season) %>%
    dplyr::mutate(
      wins = ifelse(outcome == "W", 1, 0),
      unbeaten = ifelse(outcome != "L", 1, 0),
      losses = ifelse(outcome == "L", 1, 0),
      winless = ifelse(outcome != "W", 1, 0),
      draws = ifelse(outcome == "D", 1, 0),
      cs = ifelse(goals_against == 0, 1, 0),
      goalless = ifelse(goals_for == 0, 1, 0),
      w_streak = ifelse(wins == 0, 0, sequence(rle(as.character(wins))$lengths)),
      unbeaten_streak = ifelse(unbeaten == 0, 0, sequence(rle(as.character(unbeaten))$lengths)),
      losing_streak = ifelse(losses == 0, 0, sequence(rle(as.character(losses))$lengths)),
      winless_streak = ifelse(winless == 0, 0, sequence(rle(as.character(winless))$lengths)),
      d_streak = ifelse(draws == 0, 0, sequence(rle(as.character(draws))$lengths)),
      clean_sheets = ifelse(cs == 0, 0, sequence(rle(as.character(cs))$lengths)),
      goalless_streak = ifelse(goalless == 0, 0, sequence(rle(as.character(goalless))$lengths)),
    ) %>%
    dplyr::rename(Season = season) %>%
    dplyr::summarize(
      "Wins" = max(w_streak),
      "Unbeaten" = max(unbeaten_streak),
      "Losses" = max(losing_streak),
      "Winless" = max(winless_streak),
      "Draws" = max(d_streak),
      "Clean sheets" = max(clean_sheets),
      "Didn't score" = max(goalless_streak)
    ) %>%
    dplyr::arrange(desc(Season))

  return(streaks)
}
