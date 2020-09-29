#' @importFrom gganimate animate
#' @export
gganimate::animate

#' Animate a Plinko board
#'
#' Animate a `plinko_board()` object. Follows the syntax for `gganimate::animate()`
#' with a few exceptions/additions noted below.
#'
#' @inheritParams gganimate::animate
#' @inheritParams autoplot.plinko_board
#' @param plot A [plinko_board()] object
#' @param device A device function used to render images. Default `ragg::agg_png`
#'   is faster and produces higher-quality output than the base `png` function.
#' @param width,height One of either `width` or `height` must be provided. The
#'   other dimension is automatically determined based on the aspect ratio of
#'   the Plinko board.
#' @export
animate.plinko_board = function(plot,
  fps = 10,
  renderer = gganimate::gifski_renderer(),
  device = ragg::agg_png,

  width = NULL,
  height = 800,
  res = 100,

  show_dist = FALSE, ...
) {
  board = plot

  # determine image dimensions
  if (is.null(width)) {
    width = with(board, (x_max - x_min)/total_height * height)
  }
  if (is.null(height)) {
    height = with(board, total_height/(x_max - x_min) * width)
  }

  # create temporary directory to render frames into
  png_dir = tempfile("png")
  dir.create(png_dir, showWarnings = FALSE)
  unlink(list.files(png_dir, pattern = "*.png", full.names = TRUE), expand = FALSE)

  tryCatch({
    render_frames(png_dir, board, width, height, res, show_dist = show_dist, ...)
    renderer(list.files(png_dir, pattern = "*.png", full.names = TRUE), fps)
  },
  finally = {
    unlink(list.files(png_dir, pattern = "*.png", full.names = TRUE), expand = FALSE)
    unlink(png_dir, expand = FALSE)
  })
}

render_frames = function(png_dir, board, width, height, res, show_dist, ...) {
  frame_dfs = group_split(frames(board), frame_id)

  cat("Rendering frames...\n")
  pb = txtProgressBar(max = length(frame_dfs), style = 3)

  for (i in seq_along(frame_dfs)) {
    frame_df = frame_dfs[[i]]
    outfile = sprintf("%s/%04i.png", png_dir, i)
    agg_png(outfile, width = width, height = height, res = res, ...)

    tryCatch({
      print(.plot_plinko_board(board, frame_df, show_dist = show_dist, show_paths = FALSE))
    },
    finally = {
      invisible(dev.off())
    })

    setTxtProgressBar(pb, i)
  }

  close(pb)
}
