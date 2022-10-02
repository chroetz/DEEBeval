evalMetaAndWriteToFile <- function(meta, plotFuns, outPath, method) {

  tbl <- evalTbl(meta, plotFuns, infoList = list(method = method))

  quantTbl <-
    tbl |>
    select(-ends_with("Path"), -.data$plots) |>
    mutate(method = .env$method)
  quantsOutFiles <- sapply(
    unique(tbl$taskNr),
    \(tnr) {
      taskQuantTbl <-
        quantTbl |>
        filter(.data$taskNr == tnr) |>
        tidyr::unnest_wider(.data$quants) |>
        relocate(.data$method, ends_with("Nr"))
      quantsOutFile <- file.path(outPath, sprintf("Eval_%s_Task%02d.csv", method, tnr))
      readr::write_csv(taskQuantTbl, quantsOutFile)
      quantsOutFile
    })

  plotsPath <- file.path(outPath, "plots")
  if (!dir.exists(plotsPath)) dir.create(plotsPath)
  for (i in 1:nrow(tbl)) {
    row <- tbl[i,]
    plots <- row$plots[[1]]
    for (nm in names(plots)) {
      plt <- plots[[nm]]
      if (length(plt) == 0) next
      fileName <- sprintf(
        "Truth%04dObs%04dTask%02d%s_%s.png",
        row$truthNr, row$obsNr, row$taskNr, method, nm)
      ggsave(file.path(plotsPath, fileName), plt, width = 3, height = 3)
    }
  }

  return(list(
    quants = quantsOutFiles,
    plots = plotsPath))
}
