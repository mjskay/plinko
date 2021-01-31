#' @importFrom gganimate animate
#' @export
gganimate::animate

#' Animate a Plinko board
#'
#' Animate a `plinko_board()` object. Follows the syntax for `gganimate::animate()`
#' with a few exceptions/additions noted below.
#'
#' @inheritParams autoplot.plinko_board
#' @param plot A [plinko_board()] object
#' @param fps The framerate of the animation in frames/sec (default `10`).
#' @param renderer The function used to render the generated frames into an animation.
#'   Gets a vector of paths to images along with the framerate. (default `gganimate::gifski_renderer()`)
#' @param device A device function used to render images. Default [`ragg::agg_png`]
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
#' @param cores **(experimental)** How many cores to use to render animation frames.
#' @param ... Arguments passed on to the device. See [`ragg::agg_png`] for the
#'   arguments for the default device. For available device arguments to other
#'   devices, see the corresponding documentation (e.g. [grDevices::png()]).
#'
#' @importFrom grDevices dev.off
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @importFrom foreach foreach `%dopar%` `%do%`
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
  cores = getOption("cores", 1),
  ...
) {
  board = plot

  # determine image dimensions
  x_min = min(board$x_min, board$slot_edges)
  x_max = max(board$x_max, board$slot_edges)
  if (is.null(width)) {
    width = (x_max - x_min)/board$total_height * height
  }
  if (is.null(height)) {
    height = board$total_height/(x_max - x_min) * width
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

    # set up progress bar
    if (progress) {
      cat("Rendering frames...\n")
      pb = txtProgressBar(max = length(frame_dfs), style = 3)
    }

    # render individual frames to png files
    tryCatch({
      # for parallel execution, set up cluster
      snow_opts = list()
      `%do_impl%` = if (cores > 1) {
        cl = snow::makeCluster(cores)
        doSNOW::registerDoSNOW(cl)
        # need to save ggplot theme and reapply it on each worker since workers
        # may not be in the same process
        ggplot_theme = theme_get()

        `%dopar%`
      } else {
        `%do%`
      }

      i = NULL # avoid no visible binding warning from CHECK
      foreach(
          i = seq_along(frame_dfs),
          .combine = function(...) {
            n = sum(...)
            setTxtProgressBar(pb, n)
            n
          },
          .multicombine = TRUE,
          .inorder = FALSE
      ) %do_impl% {
        frame_df = frame_dfs[[i]]
        outfile = sprintf("%s/%04i.png", png_dir, i)
        device(outfile, width = width, height = height, res = res, ...)

        if (cores > 1) {
          theme_set(ggplot_theme)
        }

        tryCatch({
          print(.plot_plinko_board(board, frame_df, show_paths = show_paths, show_dist = show_dist, show_target_dist = show_target_dist))
        },
        finally = {
          invisible(dev.off())
        })

        if (cores == 1 && progress) setTxtProgressBar(pb, i)
        1
      }
      if (progress) close(pb)
    },
    finally = {
      if (cores > 1) snow::stopCluster(cl)
    })

    # combine frames into animation
    animation = renderer(list.files(png_dir, pattern = "*.png", full.names = TRUE), fps)
    utils::getFromNamespace("set_last_animation", "gganimate")(animation)
    animation
  },
  finally = {
    unlink(list.files(png_dir, pattern = "*.png", full.names = TRUE), expand = FALSE)
    unlink(png_dir, expand = FALSE)
  })
}
