#' Modify coordinate system used to display a Plinko board
#'
#' Modify the coordinate system used to display a [plinko_board()].
#' [plinko_board()] objects use a `coord_fixed()` coordinate system
#' with several default parameters based on the properties of the board;
#' this function allows you to override those parameters.
#'
#' @param board A [plinko_board()] object
#' @param ... Arguments to `coord_fixed()` you wish to change.
#'
#' @export
modify_coord = function(board, name, mapping = aes(), ...) {
  new_args = list(...)

  board$ggplot_coord[names(new_args)] = new_args

  board
}
