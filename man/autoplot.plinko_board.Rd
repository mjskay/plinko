% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.plinko_board.R
\name{autoplot.plinko_board}
\alias{autoplot.plinko_board}
\title{Plot a Plinko board as a ggplot2 plot}
\usage{
\method{autoplot}{plinko_board}(
  object,
  ...,
  frame = NULL,
  show_paths = TRUE,
  show_dist = TRUE,
  show_target_dist = TRUE
)
}
\arguments{
\item{object}{A \code{\link[=plinko_board]{plinko_board()}} object}

\item{...}{other arguments passed to specific methods}

\item{frame}{Which frame to plot (an integer). If \code{NULL} (the default), plots
the final frame (i.e. all the balls in their final positions).}

\item{show_paths}{Should ball paths be shown?}

\item{show_dist}{Should an overlay of the binomial distribution for this
Plinko board be shown?}

\item{show_target_dist}{For \code{plinko_board()} objects constructed from a
\href{https://pkg.mitchelloharawild.com/distributional/}{distributional} object
(like \code{\link[=dist_normal]{dist_normal()}}, etc), should an overlay of the target distribution
for this Plinko board be shown?}
}
\description{
Plots a single frame from a Plinko board animation.
}
