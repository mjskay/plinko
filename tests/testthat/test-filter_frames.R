library(dplyr)

context("filter_frames")

test_that("filter_frames() works", {
  board = test_board_election()
  board_ball_5 = filter_frames(board, ball_id == 5)

  expect_equal(frames(board_ball_5), frames(board) %>% filter(ball_id == 5) %>% mutate(frame_id = 1:n()))
  expect_equal(n_frames(board_ball_5), 70)

  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("ball 5 in filtered board", autoplot(board_ball_5, frame = 70))
})
