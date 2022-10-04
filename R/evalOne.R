evalOne <- function(infoList, plotFuns = list()) {

  infoList <- as.list(infoList)
  info <- new.env(parent=emptyenv())
  appendToEnv(info, infoList)
  loadPathsInInfo(info)

  if (is.null(info$truth)) return(list(quants = NULL, plots = NULL))

  if (is.null(info$esti)) {
    info$esti <- makeTrajsStateConst(info$truth, mean)
  }

  info$title <- paste0(info$method, ", Task", info$taskNr, ", Truth", info$truthNr, ", Obs", info$obsNr)

  scoreFuns <- lapply(
    info$task$scoreList$list,
    buildScore,
    timeRange = info$task$predictionTime)
  names(scoreFuns) <- sapply(info$task$scoreList$list, \(x) x$name)

  quants <- lapply(scoreFuns, do.call, args = list(info = info))

  # TODO:
  if (is.null(info$timeWarp)) {
    warning("time warp not found, removing plots")
    plotFuns <- plotFuns[!names(plotFuns) %in% c("timeDependenceWarped", "timeDiff")]
  }
  plots <- lapply(plotFuns, do.call, args = list(info = info))

  return(list(quants = quants, plots = plots))
}

