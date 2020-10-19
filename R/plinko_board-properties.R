#' Get properties of a Plinko board
#'
#' Functions to get different properties of a [plinko_board()].
#'
#' @name plinko_board-properties
#' @param board A [plinko_board()]
#' @return A data frame containing the requested properties of the Plinko board.
NULL

#' @describeIn plinko_board-properties Slot edges
#' @export
slot_edges = function(board) {
  board$slots_df
}

#' @describeIn plinko_board-properties Pin locations
#' @export
pins = function(board) {
  board$pins_df
}

#' @describeIn plinko_board-properties Ball paths from drop point to final location
#' @export
paths = function(board) {
  board$paths_df %>%
    ungroup() %>%
    select(ball_id, move_id, bin, pin, x, y, width, region)
}

#' @describeIn plinko_board-properties Ball final locations
#' @export
balls = function(board) {
  paths(board) %>%
    filter(move_id == max(move_id))
}

#' @describeIn plinko_board-properties Frames in the animation giving ball locations
#' @export
frames = function(board) {
  board$frames_df %>%
    select(frame_id, ball_id, move_id, bin, pin, x, y, width, region, stopped)
}

#' @describeIn plinko_board-properties Number of frames in the Plinko board.
#' @export
n_frames = function(board) {
  max(board$frames_df$frame_id)
}