deparse_tibble = function(x) {
  chunks = lapply(seq_along(x), function(i) {
    paste0("  ", names(x)[[i]], " = ", deparse1(x[[i]]))
  })
  paste0("tibble(\n", paste0(chunks, collapse = ",\n"), "\n)\n")
}

clip_tibble = function(x) {
  clipr::write_clip(deparse_tibble(x))
}
