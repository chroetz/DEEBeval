createTruthEstiTaskPlots <- function(info) {
  taskClass <- getClassAt(info$task, 2)
  plots <- switch(
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
  if ("timeWarp" %in% names(info)) {
    plots <- append(plots, list(
      timeWarp = DEEBplots::plotTimeState(
      info$timeWarp$truth, esti = info$timeWarp$esti, obs = NULL, title = info$title)))
  }
  if ("followTime" %in% names(info)) {
    plots <- append(plots, list(
      followTime = DEEBplots::plotFollowTime(info$followTime, title = info$title)))
  }
  return(plots)
}
