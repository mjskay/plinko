context("add_layers")

test_that("add_layers() works", {
  skip_if_not_installed("vdiffr")

  board = test_board_election()
  board_modified = add_layers(
    board,
    geom_vline(xintercept = 269, color = "black", alpha = 0.15, size = 1),
    annotate("label", x = 269, y = 1500, label = "269", hjust = 0.5, color = "gray50")
  )

  vdiffr::expect_doppelganger("board with additional layers", autoplot(board_modified))
})
