#' Title
#'
#' @param seasons
#' @param input_venue
#' @inheritParam dplyr::filter
#' @inheritParam dplyr::case_when
#' @inheritParam dplyr::group_by
#' @inheritParam dplyr::summarise
#' @inheritParam dplyr::mutate
#' @inheritParam dplyr::select
#' @inheritParam dplyr::rename
#'
#' @return
#' @export
#'
#' @examples
get_ssn_records <- function(seasons, input_venue = "all") {
  df <- get_results_raw() %>%
    dplyr::filter(
      season %in% seasons,
      game_type == "league",
      dplyr::case_when(
        input_venue == "all" ~ venue %in% c("H", "A", "N"),
        TRUE ~ venue == toupper(input_venue))
    ) %>%
    dplyr::group_by(season) %>%
    dplyr::summarise(
      P = dplyr::n(),
      W = sum(outcome == "W"),
      D = sum(outcome == "D"),
      L = sum(outcome == "L"),
      GF = sum(goals_for),
      GA = sum(goals_against)
    ) %>%
    dplyr::mutate(
      GD = GF - GA,
      GF_GA = paste(GF, GA, sep="-"),
      W_pc = round((W/P) * 100, 2),
      Pts = (W * 3) + D,
      PPG = round((Pts / P), 2)
    ) %>%
    dplyr::select(season, P, W, D, L, GF, GA, GD, W_pc, Pts, PPG) %>%
    dplyr::rename(
      Season = season
    )
  # %>%
  #   rename_with(.cols = W:ppg,
  #               .fn = ~ paste0(input_venue, .x)
  #   )

  return (df)
}
