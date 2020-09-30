#' Add ggplot layers to a Plinko board
#'
#' Add ggplot layers to a Plinko board.
#'
#' @param board A [plinko_board()] object
#' @param ... Objects that can be added to a ggplot object using `+`.
#'
#' @export
add_layers = function(board, ...) {
  board$ggplot_user_layers = c(board$ggplot_user_layers, list(...))
  board
}
