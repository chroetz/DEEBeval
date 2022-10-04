plotFunList <- list(
  stateSpace = \(info) DEEBplots::plotStateSpace(
    info$truth, info$esti, info$obs,
    timeRange = info$task$predictionTime,
    title = info$title),
  timeState = \(info) DEEBplots::plotTimeState(
    info$truth, info$esti, info$obs,
    timeRange = info$task$predictionTime,
    title = info$title),
  timeStateWarped = \(info) DEEBplots::plotTimeState(
    info$timeWarp$truth, info$timeWarp$esti, obs = NULL,
    timeRange = info$task$predictionTime,
    title = info$title)
)
