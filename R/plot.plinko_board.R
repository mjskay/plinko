#' Plot a Plinko board as a ggplot2 plot
#'
#' Plots a single frame from a Plinko board animation.
#'
#' @inheritParams ggplot2::autoplot
#' @param object A [plinko_board()] object
#' @param frame Which frame to plot (an integer). If `NULL` (the default), plots
#'   the final frame (i.e. all the balls in their final positions).
#' @param show_paths Should ball paths be shown?
#' @param show_dist Should an overlay of the binomial distribution for this
#'   Plinko board be shown?
#'
#' @import ggplot2
#' @importFrom rlang %||%
#' @export
autoplot.plinko_board = function(object, ..., frame = NULL, show_paths = TRUE, show_dist = TRUE) {
  if (is.null(frame)) {
    balls_df = balls(object)
  } else {
    balls_df = frames(object) %>%
      filter(frame_id == frame)
  }

  .plot_plinko_board(object, balls_df = balls_df, show_paths = show_paths, show_dist = show_dist)
}

.plot_plinko_board = function(board, balls_df, show_paths = show_paths, show_dist = show_dist) {
  dist_df = with(board, {
    dist_x = seq(-n_bin/2, n_bin/2) * bin_width + center
    dist_x = dist_x[min(slot_edges) < dist_x & dist_x < max(slot_edges)]
    data.frame(
      x = dist_x,
      y = n_ball * ball_width * dbinom(
        round((dist_x - center)/bin_width + n_bin/2),
        size = n_bin,
        prob = 0.5
      )
    )
  })

  # build customizable layers
  layers = lapply(board$ggplot_layers, function(layer) do.call(eval(layer[[1]]), layer[-1]))
  if (!show_paths) layers[["paths"]] = NULL
  if (!show_dist) layers[["dist"]] = NULL

  ggplot() +
    layers +
    do.call(coord_fixed, board$ggplot_coord) +
    ylab("") +
    scale_y_continuous(breaks = NULL) +
    expand_limits(y = board$total_height) +
    theme(
      axis.line.y = element_blank(),
      axis.line.x = element_line(color = "gray75", size = 1)
    ) +
    board$ggplot_user_layers
}
