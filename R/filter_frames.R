#' Filter frames in a Plinko board
#'
#' Filter the frames in Plinko board. Useful for testing animations without
#' needing to render the full animation.
#'
#' @param board A [plinko_board()] object
#' @param ... A filtering expression (like one passed to `filter()`) that uses
#'   columns of `frames(board)` to select the frames to keep.
#'
#' @export
filter_frames = function(board, ...) {
  board$frames_df = filter(board$frames_df, ...)
  #board$frames_df[board$frames_df$frame_id %in% frames,]
  # renumber frames
  board$frames_df$frame_id = match(board$frames_df$frame_id, unique(board$frames_df$frame_id))
  board
}
