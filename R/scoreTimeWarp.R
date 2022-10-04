scoreTimeWarp <- function(trajs1, trajs2, timeRange, opts, info=NULL) {
  opts <- asOpts(opts, c("TimeWarp", "TimeState", "Score"))
  times <- seq(timeRange[1], timeRange[2], length.out = opts$timeSteps)
  trajs1 <- interpolateTrajs(trajs1, times)
  trajs2 <- interpolateTrajs(trajs2, times)
  if (is.null(info)) info <- new.env(parent = emptyenv())
  calculateTimeWarp(trajs1, trajs2, opts, info)
  return(info$timeWarp$distance)
}

calculateTimeWarp <- function(esti, truth, opts, info) {
  warp <- dtw::dtw(
    esti$state, truth$state,
    dist.method = opts$distMethod,
    window.type = opts$windowType,
    open.end = opts$openEnd,
    open.begin = opts$openBegin)
  timesWarped <- seq(min(truth$time), max(truth$time), length.out = length(warp$index1))
  truthWarped <- makeTrajs(
    time = timesWarped,
    state = truth$state[warp$index2,],
    trajId = truth$trajId[1])
  estiWarped <- makeTrajs(
    time = timesWarped,
    state = esti$state[warp$index1,],
    trajId = esti$trajId[1])
  appendToEnv(info, list(timeWarp = list(
    esti = estiWarped,
    truth = truthWarped,
    distance = warp$distance
  )))
}
