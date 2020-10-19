context("tween_balls")

test_that("tween_balls works", {
  board = test_board_election()
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
