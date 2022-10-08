evalMetaAndWriteToFile <- function(meta, outPath, method) {

  plotsPath <- file.path(outPath, "plots")
  dir.create(plotsPath, showWarnings=FALSE)
  tbl <- evalTbl(meta, infoList = list(method = method, plotsPath = plotsPath))

  scoreTbl <-
    tbl |>
    select(-ends_with("Path")) |>
    mutate(method = .env$method)
  scoresOutFiles <- sapply(
    unique(tbl$taskNr),
    \(tnr) {
      taskscoreTbl <-
        scoreTbl |>
        filter(.data$taskNr == tnr) |>
        tidyr::unnest_wider(.data$scores) |>
        relocate(.data$method, ends_with("Nr"))
      scoresOutFile <- file.path(outPath, sprintf("Eval_%s_Task%02d.csv", method, tnr))
      readr::write_csv(taskscoreTbl, scoresOutFile)
      scoresOutFile
    })

  return(list(
    scores = scoresOutFiles,
    plots = plotsPath))
}


evalTbl <- function(meta, infoList = list()) {

  meta$scores <- lapply(
    seq_len(nrow(meta)),
    \(i) evalOne(c(meta[i,], infoList)))

  return(meta)
}


evalOne <- function(infoList, verbose=TRUE) {

  infoList <- as.list(infoList)
  info <- new.env(parent=emptyenv())
  appendToEnv(info, infoList)
  loadPathsInInfo(info)

  stopifnot(!is.null(info$truth), !is.null(info$esti))

  info$title <- paste0(info$method, ", Task", info$taskNr, ", Truth", info$truthNr, ", Obs", info$obsNr)
  if (verbose) cat(info$title, "\n")

  scoreFuns <- lapply(info$task$scoreList$list, buildScore)
  names(scoreFuns) <- sapply(info$task$scoreList$list, \(x) x$name)

  scores <- lapply(scoreFuns, do.call, args = list(info = info))

  plots <- createTruthEstiTaskPlots(info)
  for (nm in names(plots)) {
    plt <- plots[[nm]]
    fileName <- sprintf(
      "Truth%04dObs%04dTask%02d%s_%s.png",
      info$truthNr, info$obsNr, info$taskNr, info$method, nm)
    ggsave(file.path(info$plotsPath, fileName), plt, width = 3, height = 3)
  }

  return(scores)
}

