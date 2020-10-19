context("autoplot")

test_that("Basic plot works", {
  skip_if_not_installed("vdiffr")

  board = test_board_election()

  vdiffr::expect_doppelganger("autoplot", autoplot(board))

  vdiffr::expect_doppelganger("autoplot no dist or paths", autoplot(board, show_paths = FALSE, show_dist = FALSE))

  vdiffr::expect_doppelganger("autoplot frame 1", autoplot(board, frame = 1))
  vdiffr::expect_doppelganger("autoplot frame 10", autoplot(board, frame = 10))
  vdiffr::expect_doppelganger("autoplot frame 40", autoplot(board, frame = 40))
  vdiffr::expect_doppelganger("autoplot frame 86", autoplot(board, frame = 86))
})
