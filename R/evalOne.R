evalOne <- function(infoList, plotFuns, verbose=TRUE) {

  infoList <- as.list(infoList)
  info <- new.env(parent=emptyenv())
  appendToEnv(info, infoList)
  loadPathsInInfo(info)

  stopifnot(!is.null(info$truth), !is.null(info$esti))

  info$title <- paste0(info$method, ", Task", info$taskNr, ", Truth", info$truthNr, ", Obs", info$obsNr)
  if (verbose) cat(info$title, "\n")

  scoreFuns <- lapply(
    info$task$scoreList$list,
    buildScore,
    timeRange = info$task$predictionTime)
  names(scoreFuns) <- sapply(info$task$scoreList$list, \(x) x$name)

  quants <- lapply(scoreFuns, do.call, args = list(info = info))
  plots <- createTruthEstiTaskPlots(info)

  return(list(quants = quants, plots = plots))
}


createTruthEstiTaskPlots <- function(info) {
  taskClass <- getClassAt(info$task, 2)
  switch(
    taskClass,
    "estiObsTrajs" = {
      list(
        stateSpace = DEEBplots::plotStateSpace(
          info$truth, esti = info$esti, obs = NULL, title = info$title),
        timeState = DEEBplots::plotTimeState(
          info$truth, esti = info$esti, obs = NULL, title = info$title)
      )
    },
    "newTrajs" = {
      list(
        stateSpace = DEEBplots::plotStateSpace(
          info$truth, esti = info$esti, obs = NULL, title = info$title),
        timeState = DEEBplots::plotTimeState(
          info$truth, esti = info$esti, obs = NULL, title = info$title)
      )
    },
    "velocity" = {
      list(
        vectorFieldEsti = DEEBplots::plotVectorField(info$esti, title = info$title),
        vectorFieldDiff = {
          if (any(info$esti$state - info$esti$state != 0)) stop("states don't match")
          diff <- info$truth
          diff$deriv <- diff$deriv - info$esti$deriv
          DEEBplots::plotVectorField(diff, title = info$title)
        }
      )
    },
    stop("Unknown task class ", taskClass)
  )
}
