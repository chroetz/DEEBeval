# TODO: make score work with new system
scoreFollowTime <- function(truth, esti, obs, timeRange, nBasePoints=1e3, windowSize=200, refScale=0.9) {
  times <- seq(timeRange[1], timeRange[2], length.out = nBasePoints)
  esti <- interpolateTrajs(esti, times)
  truth <- interpolateTrajs(truth, times)

  refDists <- sqrt(rowSums(
    (rep(colMeans(obs$state), each=nrow(truth$state)) - truth$state)^2))
  meanRefDist <- mean(refDists)
  dists <- sqrt(rowSums((esti$state - truth$state)^2))

  away <- dists > meanRefDist * refScale
  m <- length(dists) - windowSize + 1
  res <- double(m)
  for (i in seq_len(m)) {
    res[i] <- mean(away[i:(i+windowSize-1)])
  }
  idx <- which(res > 0.5*refScale)[1]
  if (is.na(idx)) return(1)
  idxPrecise <- idx - 1 + which(away[idx:(idx+windowSize-1)])[1]
  return(1 - idxPrecise / length(dists))
}
