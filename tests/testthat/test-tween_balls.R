context("tween_balls")

x = c(213, 243, 266, 279, 290, 304, 310, 319, 323, 333, 339, 345,  352, 357, 363, 372, 374, 385.125, 399, 415)
n_bin = 6
bin_width = 41
set.seed(1234)
board = plinko_board(
  x, n_bin, bin_width, center = 330.5,
  limits = c(0, 538)
)

test_that("tween_balls works", {
  board_tweened = tween_balls(board, frame_mult = 4)

  expect_equal(n_frames(board_tweened), 340)

  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("tweened frame 1", autoplot(board_tweened, frame = 1))
  vdiffr::expect_doppelganger("tweened frame 2", autoplot(board_tweened, frame = 2))
  vdiffr::expect_doppelganger("tweened frame 3", autoplot(board_tweened, frame = 3))
  vdiffr::expect_doppelganger("tweened frame 4", autoplot(board_tweened, frame = 4))
  vdiffr::expect_doppelganger("tweened frame 5", autoplot(board_tweened, frame = 5))
  vdiffr::expect_doppelganger("last tweened frame", autoplot(board_tweened, frame = n_frames(board_tweened)))
})
