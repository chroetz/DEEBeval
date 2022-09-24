lpErr <- function(trajs1, trajs2, timeRange, p) {
  distTraj <- compareTrajStates(
    trajs1, trajs2,
    \(x, y) abs(x-y),
    timeRange = timeRange)
  if (is.finite(p)) {
    mean(rowSums(distTraj$state^p))^(1/p)
  } else {
    max(distTraj$state)
  }
}

getRefTraj <- function(info, p) {
  if (p == 1) {
    ref <- makeTrajsStateConst(info$truth, stats::median)
  } else if (p == 2) {
    ref <- makeTrajsStateConst(info$truth, mean)
  } else if (p == Inf) {
    ref <- makeTrajsStateConst(info$truth, \(x) mean(range(x)))
  } else {
    stop()
  }
  return(ref)
}

timeWarpError <- function(info, p=2, nBasePoints=1e3) {

  times <- seq(min(info$obs$time), max(info$obs$time), length.out = nBasePoints)
  truth <- interpolateTrajs(info$truth, times)
  esti <- interpolateTrajs(info$esti, times)
  appendToEnv(info, list(truthIp = truth, estiIp = esti))

  fitWarp <- fitTimeWarp(truth, esti)
  selNa <- is.na(fitWarp$idxes)
  naRate <- mean(selNa)
  warpedEstiCutoff <- fitWarp$trajs[!selNa,]
  warpedIdxesCutoff <- fitWarp$idxes[!selNa]
  appendToEnv(info, list(estiWarped = warpedEstiCutoff, timeWarpIdexes = warpedIdxesCutoff))

  timeIdxDiff <- warpedIdxesCutoff - seq_along(warpedIdxesCutoff)
  dst <- abs(timeIdxDiff) / length(warpedIdxesCutoff)
  timeErr <- mean(dst) * (1-naRate)
  missingTimeErr <- dst[length(dst)] * naRate
  spaceErr <- lpErr(truth, warpedEstiCutoff, range(warpedEstiCutoff$time), p = p) * (1-naRate)
  missingSpaceErr <- lpErr(info$truth, getRefTraj(info, p=p), range(info$obs$time), p=p) * naRate

  list(
    time = timeErr,
    space = spaceErr,
    missingSpace = missingSpaceErr,
    missingTime = missingTimeErr)
}
