#' @export
print.plinko_board = function(x, ...) {
  cat(sep = "",
    "A Plinko Board with ", x$n_ball, " balls and ", x$n_bin, " bins centered at ", x$center, "\n"
  )
}
