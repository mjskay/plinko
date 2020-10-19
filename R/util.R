# testing utilities -------------------------------------------------------

deparse_tibble = function(x) {
  chunks = lapply(seq_along(x), function(i) {
    paste0("  ", names(x)[[i]], " = ", deparse1(x[[i]]))
  })
  paste0("tibble(\n", paste0(chunks, collapse = ",\n"), "\n)\n")
}

clip_tibble = function(x) {
  clipr::write_clip(deparse_tibble(x))
}

test_board_election = function() {
  x = c(213, 243, 266, 279, 290, 304, 310, 319, 323, 333, 339, 345,  352, 357, 363, 372, 374, 385.125, 399, 415)
  n_bin = 6
  bin_width = 41
  set.seed(1234)
  plinko_board(
    x, n_bin, bin_width, center = 330.5,
    limits = c(0, 538)
  )
}

