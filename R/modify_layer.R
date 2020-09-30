#' Modify layers/geoms used to display a Plinko board
#'
#' Modify the geoms on layers used to display the slots, pins, paths, balls,
#' or distribution in a [plinko_board()]. [plinko_board()] objects use several
#' geoms to produce the plot of the board; this function allows you to modify
#' arguments and aesthetic mappings passed to those geometries.
#'
#' @param board A [plinko_board()] object
#' @param name Name corresponding to the geom to modify: one of `"slot_edges"`,
#'   `"pins"`, `"balls"`, `"paths"`, or `"dist"`.
#' @param mapping An `aes()` object containing entries in the aesthetic mapping
#'   for this geom that you wish to change. Missing entries are left as-is.
#' @param ... Arguments to the geom you wish to change.
#'
#' @export
modify_layer = function(board, name, mapping = aes(), ...) {
  new_args = list(...)

  # standardize names
  names(mapping) = standardise_aes_names(names(mapping))
  names(new_args) = standardise_aes_names(names(new_args))

  # update aesthetics
  board$ggplot_layers[[name]]$mapping[names(mapping)] = mapping
  # must remove any use of those aesthetics from parameters so that the
  # aesthetics are acutally used by the geom
  board$ggplot_layers[[name]][names(mapping)] = NULL

  # update parameters

  board$ggplot_layers[[name]][names(new_args)] = new_args

  board
}
