#' Title
#'
#' @param seasons
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
    aes(x = ordered, y = total_goals)
  ) +
    ggplot2::geom_col(
      aes(
        fill = total_goals
      )
    ) +
    ggplot2::geom_text(aes(x = ordered, y = total_goals, label = total_goals),
              color = "white",
              hjust = 1,
              nudge_y = -0.2) +
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
      expand = expansion(mult = c(0, 0), add = c(0, 0))
    ) +
    ggplot2::scale_y_continuous(
      expand = expansion(mult = c(0, 0), add = c(0, 0))
    ) +
    ggplot2::theme_classic(base_size = 15) +
    ggplot2::theme(
      legend.position = "none",
      text = element_text(
        family = "Helvetica Neue"
      ),
      strip.text.x = element_text(
        hjust = 0.5,
        face = "bold"
      ),
      strip.background = element_rect(
        fill = "white",
        color="white"
      ),
      panel.border = element_blank(),
      line = element_blank(),
      axis.text.x = element_blank(),
      axis.line.x = element_blank(),
      axis.ticks = element_blank(),
      panel.spacing = unit(1.5, "lines")
    ) +
    ggplot2::coord_flip()

  p
}
