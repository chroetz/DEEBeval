evalMetaAndWriteToFile <- function(meta, plotFuns, outPath, method) {

  tbl <- evalTbl(meta, plotFuns, infoList = list(method = method))

  quantTbl <-
    tbl |>
    select(-ends_with("Path"), -.data$plots) |>
    tidyr::unnest_wider(.data$quants) |>
    mutate(method = .env$method) |>
    relocate(.data$method, ends_with("Nr"))

  quantsOutFile <- file.path(outPath, paste0("Eval_", method, ".csv"))
  readr::write_csv(
    quantTbl,
    file = quantsOutFile)

  plotTbl <-
    tbl |>
    select(-ends_with("Path"), -.data$quants) |>
    rowwise() |>
    mutate(plots = list(lapply(.data$plots, list))) |>
    ungroup() |>
    tidyr::unnest_wider(.data$plots) |>
    mutate(across(-ends_with("Nr"), ~ lapply(., \(x) ifelse(is.null(x), list(), x)))) |>
    mutate(across(-ends_with("Nr"), ~ unlist(., recursive=FALSE))) |>
    mutate(method = .env$method) |>
    relocate(.data$method, ends_with("Nr"))

  plotsOutFile <- file.path(outPath, paste0("Plots_", method, ".RDS"))
  saveRDS(
    plotTbl,
    file = plotsOutFile)

  return(list(
    quants = quantsOutFile,
    plots = plotsOutFile))
}
