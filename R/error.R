lpErr <- function(trajs1, trajs2, timeRange, p) {

  trajs1 <- fillNaState(trajs1, applyToTrajStateCols(trajs1, mean, na.rm=TRUE))
  trajs2 <- fillNaState(trajs2, applyToTrajStateCols(trajs2, mean, na.rm=TRUE))

  distTraj <- compareTrajStates(
    trajs1, trajs2,
    \(x, y) abs(x-y),
    timeRange = timeRange)

  norms <- sqrt(rowSums(distTraj$state^2))

  if (is.finite(p)) {
    mean(norms^p)^(1/p)
  } else {
    max(norms)
  }
}


timeWarpLpErr <- function(info, timeRange, p) {
  if (is.null(info$timeWarp)) calculateTimeWarp(info, timeRange)
  lpErr(info$timeWarp$esti, info$timeWarp$truth, timeRange, p)
}


timeWarpCosts <- function(info, timeRange, p) {
  if (is.null(info$timeWarp)) calculateTimeWarp(info, timeRange)
  timeDiff <- abs(info$timeWarp$estiIdxes - seq_along(info$timeWarp$estiIdxes))
  timeDiff[is.na(timeDiff)] <- max(timeDiff, na.rm=TRUE)
  mean(timeDiff / length(info$timeWarp$estiIdxes) * 2)
}
