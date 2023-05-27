#' Title
#'
#' @param seasons
#'
#' @inheritParam dplyr::arrange
#' @inheritParam dplyr::mutate
#' @inheritParam ggplot2::ggplot
#' @inheritParam ggplot2::aes
#' @inheritParam ggplot2::geom_col
#' @inheritParam ggplot2::geom_text
#' @inheritParam ggplot2::labs
#' @inheritParam ggplot2::facet_wrap
#' @inheritParam ggplot2::scale_fill_gradient
#' @inheritParam ggplot2::scale_x_discrete
#' @inheritParam ggplot2::scale_y_continuous
#' @inheritParam ggplot2::theme_classic
#' @inheritParam ggplot2::theme
#' @inheritParam ggplot2::coord_flip
#' @inheritParam ggplot2::expansion
#' @inheritParam ggplot2::element_blank
#' @inheritParam ggplot2::element_text
#' @inheritParam ggplot2::element_rect
#' @inheritParam plotly::ggplotly
#' @inheritParams grid::unit
#'
#' @return
#' @export
#'
#' @examples
plot_ssn_scorers <- function(seasons) {
  df <- get_ssn_scorers(seasons) %>%
    dplyr::arrange(
      season,
      total_goals,
      player_name
    ) %>%
    dplyr::mutate(
      ordered = paste0(season, total_goals, player_name) %>%
        forcats::fct_inorder()
    )
  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(x = ordered, y = total_goals)
  ) +
  ggplot2::geom_col(
    ggplot2::aes(
      fill = total_goals
      )
    ) +
  ggplot2::geom_text(ggplot2::aes(x = ordered, y = total_goals, label = total_goals),
            color = "white",
            hjust = "left",
            nudge_y = -0.275) +
  ggplot2::labs(
    x = NULL,
    y = NULL
  ) +
  ggplot2::facet_wrap(
    ~season,
    scales = "free_y",
    ncol = 2
  ) +
  ggplot2::scale_fill_gradient(
    low = "green4",
    high = "darkgreen"
  ) +
  ggplot2::scale_x_discrete(
    labels = setNames(df$player_name, df$ordered),
    expand = ggplot2::expansion(mult = c(0, 0), add = c(0, 0))
  ) +
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(mult = c(0, 0), add = c(0, 0))
   ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    legend.position = "none",
    text = ggplot2::element_text(
      family = "Helvetica Neue"
    ),
    strip.text.x = ggplot2::element_text(
      hjust = 0.5,
      face = "bold"
    ),
    strip.background = ggplot2::element_rect(
      fill = "white",
      color="white"
    ),
    panel.border = ggplot2::element_blank(),
    line = ggplot2::element_blank(),
    axis.text.x = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_blank(),
    axis.ticks = ggplot2::element_blank(),
    panel.spacing = grid::unit(1.0, "lines")
  ) +
  ggplot2::coord_flip()

  plotly::ggplotly(p, tooltip = "text")
}
