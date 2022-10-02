estiPlotFuns <- list(
  loc = \(info) DEEBplots::getStateSpacePlot(
    info$truth, info$esti, info$obs,
    timeRange = info$task$predictionTime,
    title = info$title),
  timeLoc = \(info) DEEBplots::getTimeDependencePlot(
    info$truth, info$esti, info$obs,
    timeRange = info$task$predictionTime,
    title = info$title),
  timeWarpedLoc = \(info) DEEBplots::getTimeDependencePlot(
    info$timeWarp$truth, info$timeWarp$esti, obs = NULL,
    timeRange = info$task$predictionTime,
    title = info$title),
  timeDiff = \(info) DEEBplots::getTimeWarpDiffPlot(
    info$timeWarp$estiIdxes, info$timeWarp$truth$time, info$title)
)
