evalMetaAndWriteToFile <- function(
    meta,
    outPath,
    plotsPath,
    method,
    createPlots = TRUE,
    scoreFilter = NULL,
    verbose = TRUE
) {

  if (!dir.exists(outPath)) dir.create(outPath, recursive=TRUE)

  if (createPlots) {
    if (!dir.exists(plotsPath)) dir.create(plotsPath, recursive=TRUE)
    dir.create(plotsPath, showWarnings=FALSE)
  }

  tbl <- evalTbl(
    meta,
    infoList = list(method = method, plotsPath = plotsPath),
    createPlots = createPlots,
    scoreFilter = scoreFilter,
    verbose = verbose)

  scoreTbl <-
    tbl |>
    select(-ends_with("Path")) |>
    mutate(method = .env$method)
  scoresOutFiles <- sapply(
    unique(tbl$taskNr),
    \(tnr) {
      taskScoreTbl <-
        scoreTbl |>
        filter(.data$taskNr == tnr) |>
        tidyr::unnest_wider(.data$scores) |>
        relocate(.data$method, ends_with("Nr"))
      scoresOutFile <- file.path(outPath, DEEBpath::evalDataFile(method = method, taskNr = tnr))
      if (file.exists(scoresOutFile)) {
        oldScores <- readr::read_csv(scoresOutFile, col_types = readr::cols())
        taskScoreTbl <- updateScores(oldScores, taskScoreTbl)
      }
      readr::write_csv(taskScoreTbl, scoresOutFile)
      scoresOutFile
    })

  if (createPlots) {
    DEEBplots::createShowPlots(outPath)
  }

  return(list(
    scores = scoresOutFiles,
    plots = plotsPath))
}


updateScores <- function(old, new) {
  key <- c("method", "truthNr", "obsNr", "taskNr")
  retainRows <- dplyr::anti_join(old, new, by = key)
  res <- dplyr::bind_rows(new, retainRows[names(new)])
  retainColNames <- setdiff(names(old), names(new))
  if (length(retainColNames) > 0) {
    retainCols <- old[c(key, retainColNames)]
    res <- dplyr::left_join(res, retainCols, by = key)
  }
  dplyr::arrange(res, .data$method, .data$truthNr, .data$obsNr, .data$taskNr)
}


evalTbl <- function(
    meta,
    infoList = list(),
    createPlots = TRUE,
    scoreFilter = NULL,
    verbose = TRUE
) {

  meta$scores <- lapply(
    seq_len(nrow(meta)),
    \(i) evalOne(
      c(meta[i,], infoList),
      createPlots = createPlots,
      scoreFilter = scoreFilter,
      verbose = verbose))

  return(meta)
}


evalOne <- function(
    infoList,
    createPlots = TRUE,
    scoreFilter = NULL,
    verbose = TRUE
) {

  infoList <- as.list(infoList)
  info <- new.env(parent=emptyenv())
  appendToEnv(info, infoList)
  loadPathsInInfo(info)

  stopifnot(!is.null(info$truth), !is.null(info$esti))

  info$title <- paste0(info$method, ", Task", info$taskNr, ", Truth", info$truthNr, ", Obs", info$obsNr)
  if (verbose) cat(info$title, "\n")

  scoreNames <- sapply(info$task$scoreList$list, \(x) x$name)
  if (is.null(scoreFilter)) {
    selScores <- TRUE
  } else {
    selScores <- scoreNames %in% scoreFilter
  }
  scoreList <- info$task$scoreList$list[selScores]
  scoreFuns <- lapply(scoreList, buildScore, verbose=verbose)
  names(scoreFuns) <- scoreNames[selScores]

  # calculate scores
  scores <- lapply(scoreFuns, do.call, args = list(info = info))

  if (createPlots) {
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
      DEEBplots::writePlot(plt, file.path(info$plotsPath, fileName))
    }
  }

  return(scores)
}

