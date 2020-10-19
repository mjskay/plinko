context("modify_layer")

test_that("modify_layer() works", {
  skip_if_not_installed("vdiffr")

  board = test_board_election()
  board_modified = modify_layer(board, "balls", aes(fill = x), color = "black")

  vdiffr::expect_doppelganger("board with modified layer", autoplot(board_modified))
})
