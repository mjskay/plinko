context("autoplot")

x = c(213, 243, 266, 279, 290, 304, 310, 319, 323, 333, 339, 345,  352, 357, 363, 372, 374, 385.125, 399, 415)
n_bin = 6
bin_width = 41
set.seed(1234)
board = plinko_board(
  x, n_bin, bin_width, center = 330.5,
  limits = c(0, 538)
)

test_that("Basic plot works", {
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("autoplot", autoplot(board))
  vdiffr::expect_doppelganger("autoplot frame 1", autoplot(board, frame = 1))
  vdiffr::expect_doppelganger("autoplot frame 10", autoplot(board, frame = 10))
  vdiffr::expect_doppelganger("autoplot frame 40", autoplot(board, frame = 40))
  vdiffr::expect_doppelganger("autoplot frame 86", autoplot(board, frame = 86))
})
