library(dplyr)

context("filter_frames")

x = c(213, 243, 266, 279, 290, 304, 310, 319, 323, 333, 339, 345,  352, 357, 363, 372, 374, 385.125, 399, 415)
n_bin = 6
bin_width = 41
set.seed(1234)
board = plinko_board(
  x, n_bin, bin_width, center = 330.5,
  limits = c(0, 538)
)

test_that("filter_frames() works", {
  board_ball_5 = filter_frames(board, ball_id == 5)

  expect_equal(frames(board_ball_5), frames(board) %>% filter(ball_id == 5) %>% mutate(frame_id = 1:n()))
  expect_equal(n_frames(board_ball_5), 70)

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("ball 5 in filtered board", autoplot(board_ball_5, frame = 70))
})
