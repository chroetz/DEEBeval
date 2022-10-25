evalMetaAndWriteToFile <- function(meta, outPath, method, verbose=TRUE) {

  plotsPath <- file.path(outPath, "plots")
  if (!dir.exists(plotsPath)) dir.create(plotsPath, recursive=TRUE)
  dir.create(plotsPath, showWarnings=FALSE)

  tbl <- evalTbl(
    meta,
    infoList = list(method = method, plotsPath = plotsPath),
    verbose = verbose)

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
      scoresOutFile <- file.path(outPath, DEEBpath::evalDataFile(method = method, taskNr = tnr))
      readr::write_csv(taskscoreTbl, scoresOutFile)
      scoresOutFile
    })

  DEEBplots::createShowPlots(outPath)

  return(list(
    scores = scoresOutFiles,
    plots = plotsPath))
}


evalTbl <- function(meta, infoList = list(), verbose=TRUE) {

  meta$scores <- lapply(
    seq_len(nrow(meta)),
    \(i) evalOne(c(meta[i,], infoList), verbose))

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

  scoreFuns <- lapply(info$task$scoreList$list, buildScore, verbose=verbose)
  names(scoreFuns) <- sapply(info$task$scoreList$list, \(x) x$name)

  scores <- lapply(scoreFuns, do.call, args = list(info = info))

  plots <- createTruthEstiTaskPlots(info)
  for (nm in names(plots)) {
    plt <- plots[[nm]]
    fileName <- DEEBpath::parenthesisFileName(
      truth = info$truthNr,
      obs = info$obsNr,
      task = info$taskNr,
      method = info$method,
      plot = nm,
      .ending = "png")
    ggsave(file.path(info$plotsPath, fileName), plt, width = 3, height = 3)
  }

  return(scores)
}

