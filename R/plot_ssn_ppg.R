#' Title
#'
#' @param seasons
#'
#' @inheritParams dplyr::filter
#' @inheritParams dplyr::mutate
#' @inheritParams lubridate::wday
#' @inheritParams ggplot2::ggplot
#' @inheritParams ggplot2::aes
#' @inheritParams ggplot2::geom_line
#' @inheritParams ggplot2::geom_point
#' @inheritParams ggplot2::theme_bw
#' @inheritParams ggplot2::scale_y_continuous
#' @inheritParams ggplot2::scale_color_brewer
#' @inheritParams ggplot2::labs
#' @inheritParams ggplot2::element_text
#' @inheritParams ggplot2::theme
#' @inheritParams lubridate::wday
#' @inheritParams plotly::ggplotly
#' @inheritParams plotly::layout
#'
#' @return
#' @export
#'
#' @examples
plot_ssn_ppg <- function(seasons) {
  df <- get_results_raw() %>%
    dplyr::filter(
      game_type == "league",
      season %in% seasons
    ) %>%
    dplyr::mutate(
      date_str = format(date, format = "%e %B %Y"),
      weekday = lubridate::wday(date, label = TRUE, abbr = FALSE)
    )

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = comp_game_no,
      y = ppg,
      group = 1,
      text = sprintf("Season: %s\nGame No: %.0f\nWeekday: %s\nDate: %s\nOpponent: %s\nVenue: %s\nScore: %s\nScorers: %s\nDivision: %s\nAttendance: %s\nManager: %s\nReferee: %s\nSeason Points: %s\nPPG: %.2f",
                               season,
                               comp_game_no,
                               weekday,
                               date_str,
                               opponent,
                               venue,
                               score,
                               scorers,
                               competition,
                               attendance,
                               manager,
                               referee,
                               ssn_pts,
                               ppg)
    )) +
    ggplot2::geom_line(ggplot2::aes(color = season)) +
    ggplot2::geom_point(ggplot2::aes(color = season)) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(
      limits = c(0, 3),
      breaks = c(0, 1, 2, 3)
    ) +
    ggplot2::scale_color_brewer(
      palette = "Greens",
      name = ""
    ) +
    ggplot2::labs(
      x = NULL,
      y = NULL
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = "Helvetica Neue"
      )
    )

  plotly:ggplotly(p, tooltip = "text") %>% plotly::layout(hoverlabel = list(align = "left"))
}
