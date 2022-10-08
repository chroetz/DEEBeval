scoreTimeWarp <- function(trajs1, trajs2, opts, info=NULL) {
  opts <- asOpts(opts, c("TimeWarp", "TimeState", "Score"))
  stopifnot(isTimeEqual(trajs1, trajs2))
  if (is.null(info)) info <- new.env(parent = emptyenv())
  calculateTimeWarp(trajs1, trajs2, opts, info)
  return(info$timeWarp$distance)
}

warpOneTraj <- function(est, trth, opts) {
  warp <- dtw::dtw(
    est$state, trth$state,
    dist.method = opts$distMethod,
    window.type = opts$windowType,
    open.end = opts$openEnd,
    open.begin = opts$openBegin)
  timesWarped <- seq(min(trth$time), max(trth$time), length.out = length(warp$index1))
  truthWarped <- makeTrajs(
    time = timesWarped,
    state = trth$state[warp$index2,],
    trajId = trth$trajId[1])
  estiWarped <- makeTrajs(
    time = timesWarped,
    state = est$state[warp$index1,],
    trajId = est$trajId[1])
  list(
    esti = estiWarped,
    truth = truthWarped,
    distance = warp$normalizedDistance)
}

calculateTimeWarp <- function(esti, truth, opts, info) {
  res <- apply2TrajId(esti, truth, opts=opts, fun = warpOneTraj)
  estiWarped <- bindTrajs(lapply(res, \(x) x$esti))
  truthWarped <- bindTrajs(lapply(res, \(x) x$truth))
  totalDistance <- sum(sapply(res, \(x) x$distance))
  appendToEnv(info, list(timeWarp = list(
    esti = estiWarped,
    truth = truthWarped,
    distance = totalDistance
  )))
}
