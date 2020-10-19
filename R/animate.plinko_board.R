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
#' @param start_pause,end_pause Number of times to repeat the first and last
#'   frame in the animation (default is `0` for both)
#' @param width,height One of either `width` or `height` must be provided. The
#'   other dimension is automatically determined based on the aspect ratio of
#'   the Plinko board.
#' @param res The resolution of the device. This setting will govern how device
#'   dimensions given in inches, centimeters, or millimeters will be converted
#'   to pixels. Further, it will be used to scale text sizes and linewidths
#' @param progress Output progress bar and messages? Default is to only output
#'   during interactive sessions.
#'
#' @importFrom grDevices dev.off
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @export
animate.plinko_board = function(
  plot,
  fps = 10,
  renderer = gganimate::gifski_renderer(),
  device = ragg::agg_png,
  start_pause = 0,
  end_pause = 0,

  width = NULL,
  height = 800,
  res = 100,

  progress = interactive(),

  show_paths = FALSE,
  show_dist = FALSE,
  show_target_dist = FALSE,
  ...
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
    frame_dfs = group_split(frames(board), frame_id)

    # repeat frames for start and end pauses
    start_frames = rep(frame_dfs[1], start_pause)
    end_frames = rep(frame_dfs[length(frame_dfs)], end_pause)
    frame_dfs = c(start_frames, frame_dfs, end_frames)

    # render individual frames to png files
    if (progress) {
      cat("Rendering frames...\n")
      pb = txtProgressBar(max = length(frame_dfs), style = 3)
    }
    for (i in seq_along(frame_dfs)) {
      frame_df = frame_dfs[[i]]
      outfile = sprintf("%s/%04i.png", png_dir, i)
      device(outfile, width = width, height = height, res = res, ...)

      tryCatch({
        print(.plot_plinko_board(board, frame_df, show_paths = show_paths, show_dist = show_dist, show_target_dist = show_target_dist))
      },
        finally = {
          invisible(dev.off())
        })

      if (progress) setTxtProgressBar(pb, i)
    }
    if (progress) close(pb)

    # combine frames into animation
    animation = renderer(list.files(png_dir, pattern = "*.png", full.names = TRUE), fps)
    gganimate:::set_last_animation(animation)
    animation
  },
  finally = {
    unlink(list.files(png_dir, pattern = "*.png", full.names = TRUE), expand = FALSE)
    unlink(png_dir, expand = FALSE)
  })
}
