evalMetaAndWriteToFile <- function(
    meta,
    outPath,
    method,
    createPlots = TRUE,
    scoreFilter = NULL,
    verbose=TRUE
) {

  plotsPath <- file.path(outPath, "plots")
  if (!dir.exists(plotsPath)) dir.create(plotsPath, recursive=TRUE)
  dir.create(plotsPath, showWarnings=FALSE)

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
        retainedScores <- dplyr::anti_join(
          oldScores,
          taskScoreTbl,
          by = c("method", "truthNr", "obsNr", "taskNr"))
        taskScoreTbl <- bind_rows(
          taskScoreTbl,
          retainedScores) |>
          dplyr::arrange(
            .data$method, .data$truthNr, .data$obsNr, .data$taskNr)
      }
      readr::write_csv(taskScoreTbl, scoresOutFile)
      scoresOutFile
    })

  DEEBplots::createShowPlots(outPath)

  return(list(
    scores = scoresOutFiles,
    plots = plotsPath))
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
      ggsave(file.path(info$plotsPath, fileName), plt, width = 3, height = 3)
    }
  }

  return(scores)
}

