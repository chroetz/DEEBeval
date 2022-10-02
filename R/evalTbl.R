evalTbl <- function(meta, plotFuns, infoList = list()) {

  meta$results <- lapply(
    seq_len(nrow(meta)),
    \(i) evalOne(c(meta[i,], infoList), plotFuns))

  meta <-
    meta |>
    rowwise() |>
    mutate(
      quants = list(unlist(.data$results$quants)),
      plots = list(.data$results$plots),
      results = NULL) |>
    ungroup()

  return(meta)
}
