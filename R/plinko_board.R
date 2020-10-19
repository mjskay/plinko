# TODO: might want to chuck the overuse of `within` and such and get rid of these
globalVariables(c(
  ".frame", "ball_id", "ball_width", "bin", "bin_width", "board_height", "center",
  "frame_id", "frames_till_drop", "height", "move", "move_id", "n_balanced_move",
  "n_ball", "n_bin", "n_fixed_move", "n_move", "paths_df", "pin", "region",
  "row_height", "slot_height", "stopped", "visible_move_id", "width", "x",
  "x_max", "x_min", "y"
))

#' Construct a Plinko board
#'
#' Constructs a semi-deterministic Plinko board. The board is semi-deterministic
#' in that the final values are predetermined, but the paths to get from the
#' drop point to the values are random.
#'
#' @param x The final values that the Plinko board will "sediment" to.
#' @param n_bin The number of bins used in the Binomial approximation of `x`.
#' @param bin_width The width of the bins used in the Binomial approximation of `x`
#' @param n_ball The number of balls to drop. Defaults to `length(x)`.
#' @param center The location at which to drop balls. Defaults to `mean(x)`.
#' @param limits The minimum and maximum value shown on the board. Defaults to
#'   `range(x)`.
#' @param row_ratio The ratio between the width of the slots and the height of
#'   a single row. Defaults to `2`
#' @param frames_till_drop The number of frames in between each drop of a ball.
#'   Defaults to `4`.
#' @param slot_height Height of the slots. If `NULL` (the default), determined
#'   automatically based on the height of the highest bin plus a bit of
#'   overhead.
#' @param sampling For `plinko_board.distribution`, how to "sample" balls from the
#'   distribution. Use `"quantiles"`, uses quantiles of the distribution; if
#'   `"random"`, samples balls randomly.
#' @param ... Arguments passed on to lower-level implementations of `plinko_board()`.
#'   Currently all implementations call down to `plinko_board.numeric()`, so `...`
#'   may include arguments to that function not otherwise defined.
#'
#' @return An object of class `c("plinko_board", "list")`.
#'
#' @importFrom rlang %||%
#' @importFrom purrr map_dfr
#' @importFrom dplyr tibble mutate group_by ungroup select %>% bind_rows case_when filter n
#' @importFrom tidyr unnest
#' @importFrom ggforce geom_circle
#' @export
plinko_board = function(x, ...) {
  UseMethod("plinko_board")
}

#' @rdname plinko_board
#' @export
plinko_board.numeric = function(
  x, n_bin, bin_width,
  n_ball = NULL, center = NULL, limits = NULL,
  row_ratio = 2,
  frames_till_drop = 4,
  slot_height = NULL,
  ...
) {
  # TODO: either bin_width or n_bin should be determined automatically if the
  # other is omitted

  n_ball = n_ball %||% length(x)
  center = center %||% mean(x)
  x_min = limits[[1]] %||% min(x)
  x_max = limits[[2]] %||% max(x)

  # convert x values into bin locations
  bin_values = round((x - center)/bin_width + n_bin/2)
  # randomize bin order so that when balls are dropped they don't fall into
  # bins in order from left to right
  # can't use sample(bin_values, n_ball) b/c bin_values may be length 1 (see ?sample)
  bin_values = bin_values[sample.int(length(bin_values), n_ball)]

  board = structure(
    list(
      bin_values = bin_values,
      n_bin = n_bin,
      bin_width = bin_width,
      n_ball = n_ball,
      x_min = x_min,
      x_max = x_max,
      center = center,
      row_ratio = row_ratio,
      frames_till_drop = frames_till_drop
    ),
    class = c("plinko_board", "list")
  )

  # determine derived board parameters
  board$row_height = board$bin_width * board$row_ratio
  # ball width is just a bit smaller than the bins
  board$ball_width = board$bin_width * 0.9
  # slot height needs to accommodate the tallest bin in the distribution plus some leeway
  board$slot_height = slot_height %||% ((max(table(board$bin_values)) + 2) * board$ball_width)
  board$board_height = board$slot_height + board$n_bin * board$row_height
  board$total_height = board$board_height + 11 * board$bin_width

  board = create_slots(board)
  board = create_pins(board)
  board = create_paths(board)
  board = create_frames(board)

  # customizable pre-defined layers (can be changed with modify_layer())
  board$ggplot_layers = list(
    "slot_edges" = list(geom = quote(geom_segment), data = quote(slot_edges(board)),
      mapping = aes(x = x, y = 0, xend = x, yend = height), colour = "gray75", size = 1
    ),
    "pins" = list(geom = quote(geom_point), data = quote(pins(board)),
      mapping = aes(x = x, y = y), shape = 18, colour = "#e41a1c", size = 1
    ),
    "paths" = list(geom = quote(geom_path), data = quote(paths(board)),
      mapping = aes(x = x, y = y, group = ball_id), alpha = 1/4, colour = "gray50", size = 1
    ),
    "balls" = list(geom = quote(geom_circle), data = quote(balls_df),
      mapping = aes(x0 = x, y0 = y, r = width/2), fill = "#1f78b4", colour = NA
    ),
    "dist" = list(geom = quote(geom_step), data = quote(dist_df),
      mapping = aes(x = x, y = y), colour = "black", alpha = 0.75, size = 1, direction = "mid"
    )
  )

  # coord_fixed parameters (can be modified with `modify_coord()`)
  board$ggplot_coord = list(expand = FALSE, clip = "off")

  # user-defined layers (can be added to with `+`)
  board$ggplot_user_layers = list()

  board
}

#' @rdname plinko_board
#' @importFrom distributional variance generate
#' @importFrom stats quantile ppoints
#' @importFrom ggdist stat_dist_slab
#' @export
plinko_board.distribution = function(
  x, n_bin = NULL, bin_width = NULL,
  n_ball = 50,
  sampling = c("quantiles", "random"),
  ...
) {
  sampling = match.arg(sampling)
  mean_x = mean(x)
  var_x = variance(x)

  if (!is.null(n_bin)) {
    if (!is.null(bin_width)) {
      stop(
        "To construct a plinko board from a distributional object, you must\n",
        "provide either `n_bin` or `bin_width`, but not both."
      )
    }

    # determine distribution parameters based on n_bin
    bin_width = sqrt(4 * var_x / n_bin)
  } else {
    if (is.null(bin_width)) {
      stop(
        "To construct a plinko board from a distributional object, you must\n",
        "provide either `n_bin` or `bin_width`."
      )
    }

    # determine distribution parameters based on bin_width
    n_bin = round(4 * var_x / bin_width^2)
  }

  x_samples = switch(sampling,
    quantiles = sapply(ppoints(n_ball), function(p) quantile(x, p)),
    random = unlist(generate(x, n_ball))
  )

  board = plinko_board(x_samples, n_bin = n_bin, bin_width = bin_width, center = mean_x, ...)

  # add additional layer showing the target distribution (only for boards built with distributional)
  board$dist = dist
  board$ggplot_layers$target_dist = list(
    geom = quote(stat_dist_slab), data = quote(tibble(dist = board$dist)),
    mapping = aes(dist = dist, y = 0, thickness = stat(f) * board$n_ball * board$ball_width * board$bin_width),
    colour = "#d95f02", fill = NA, alpha = 0.75, normalize = "none", scale = 1, size = 1
  )

  class(board) = c("plinko_board_dist", class(board))

  board
}


# helpers -----------------------------------------------------------------

# Create slots the balls will fall into
create_slots = function(board) { within(board, {
  slot_edges = seq(-(n_bin + 1)/2, (n_bin + 1)/2) * bin_width + center

  # restrict ourselves to the predefined min/max x, if necessary
  slot_edges = slot_edges[x_min - bin_width < slot_edges & slot_edges < x_max + bin_width]

  # extend out the left and right edges to predefined min/max x, if necessary
  slot_edges = c(
    rev(seq(min(slot_edges), x_min - bin_width, by = -bin_width)),
    slot_edges[-c(1, length(slot_edges))],
    seq(max(slot_edges), x_max + bin_width, by = bin_width)
  )

  # make the slot edges at the ends of the board
  # go all the way up the height of the board
  slot_heights = rep(slot_height, length(slot_edges))
  slot_heights[[1]] = board_height
  slot_heights[[length(slot_heights)]] = board_height

  slots_df = tibble(
    x = slot_edges,
    height = slot_heights
  )
})}

# Create the grid of pins
create_pins = function(board) { within(board, {
  pins_df = tibble()
  for (i in 1:n_bin) {
    y = slot_height + (n_bin - i) * row_height

    xs = slot_edges
    if (i %% 2 != n_bin %% 2) {
      xs = xs + bin_width/2
    }

    # restrict ourselves to the predefined min/max x
    xs = xs[min(slot_edges) + bin_width/2 < xs & xs < max(slot_edges) - bin_width/2]

    pins_df = bind_rows(pins_df, tibble(x = xs, y = y))
  }
})}

# Create the paths of the balls
create_paths = function(board) {
  # Instead of simulating the ball's path through the pins with a physics engine
  # determine random paths that could have led to the distribution we have

  # since there are twice as many pins as bins, the mean (center) pin is at n_bin / 2 * 2
  mean_pin = board$n_bin

  within(board, {
    # first, let's determine the final positions of each ball
    final_balls_df = with(board,
      tibble(
        ball_id = 1:n_ball,
        bin = bin_values,
        pin = bin * 2, # pin locations are every half-bin
        x = (bin - n_bin/2) * bin_width + center,
        move_id = n_bin + 2,
      ) %>%
      group_by(x) %>%
      mutate(y = 1:n() * ball_width - ball_width/2)
    )

    # Now we need to come up with paths for each ball that would cause them to end up in their final locations.
    # Given the starting pin at the mean (`m`) and the final pin (`k`), we can use the fact that a ball in a bin
    # centered at pin `k` ended up in that bin if and only if it traveled `k - m` pins over from the starting pin
    # (I'll dub these *fixed moves*), plus an equal number of left and right movements (i.e. movements which
    # cancel themselves out; I'll dub these *balanced moves*), *in any order* (modulo hitting the edge, which
    # we're just gonna ignore for now). So we'll just figure out what set of moves are needed for each ball and
    # then randomize the order of those moves to make a path.
    paths_df = final_balls_df %>%
      group_by(ball_id) %>%
      mutate(
        # number of fixed moves (negative if fixed moves are to the left)
        n_fixed_move = pin - mean_pin,
        # number of balanced moves (half of these will be to the right and half to the left)
        n_balanced_move = n_bin - abs(n_fixed_move),
        n_move = abs(n_fixed_move) + n_balanced_move,
        # list of moves where each move is -1 (left) or +1 (right) or 0 (start)
        move = list(c(
          0,
          sample(
            c(rep(sign(n_fixed_move), abs(n_fixed_move)), rep(-1, n_balanced_move/2), rep(1, n_balanced_move/2)),
            size = n_move
          )
        ))
      ) %>%
      unnest(move) %>%
      # determine actual positions at each step based on the accumulation of moves
      mutate(
        move_id = 1:n(),
        x = cumsum(move * bin_width/2) + center,
        y = move_id * -row_height + board_height + ball_width/2
      ) %>%
      # add final positions
      bind_rows(final_balls_df)

    #add initial ball positions at the drop location
    paths_df = paths_df %>%
      filter(move_id == 1) %>%
      mutate(
        move_id = 0,
        y = y + bin_width * 10
      ) %>%
      bind_rows(paths_df) %>%
      mutate(
        move_id = move_id + 1,
        width = ball_width,
        # what region of the board is the ball in? useful for custom plotting
        region = case_when(
          move_id == 1 ~ "start",
          move_id >= max(move_id) - 1 ~ "slot",
          TRUE ~ "pin"
        )
      )
  })
}

create_frames = function(board) {
  # we construct a dataframe of animation frames by determining on each frame which
  # move for each ball is visible (if any)

  # total leading space before last ball is dropped + number of bins it must
  # traverse + 4 (for the initial and final frames)
  board$n_frame = (board$n_ball - 1) * board$frames_till_drop + board$n_bin + 4

  paths_df = ungroup(board$paths_df)
  max_move_id = max(paths_df$move_id)

  board$frames_df = map_dfr(1:board$n_frame, function(i) {
    paths_df$frame_id = i
    paths_df$visible_move_id = i - (paths_df$ball_id - 1) * board$frames_till_drop
    paths_df$stopped = (paths_df$move_id == max_move_id) & paths_df$move_id < paths_df$visible_move_id
    paths_df[(paths_df$move_id == paths_df$visible_move_id) | paths_df$stopped,]
  })

  board
}
