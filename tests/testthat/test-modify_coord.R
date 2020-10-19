context("modify_coord")

test_that("modify_coord() works", {
  skip_if_not_installed("vdiffr")

  board = test_board_election()
  board_modified = modify_coord(board, ylim = c(0, 1500), xlim = c(-100, 638))

  vdiffr::expect_doppelganger("board with modified coord", autoplot(board_modified))
})
