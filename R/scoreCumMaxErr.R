scoreCumMaxErr <- function(follower, target, opts) {
  opts <- asOpts(opts, c("CumMaxErr", "TimeState", "Score"))
  if (!isTimeEqual(follower, target)) {
    warning("unequal time -> interpolating", immediate. = TRUE)
    follower <- DEEBtrajs::interpolateTrajs(follower, targetTimes = target$time)
  }
  res <- apply2TrajId(follower, target, scoreCumMaxErrOne, opts=opts)
  total <- sum(sapply(res, \(x) x$score))
  return(total)
}

scoreCumMaxErrOne <- function(follower, target, opts) {
  n <- nrow(target$state)
  meanState <- colMeans(target$state)
  targetStateDemeaned <- target$state - rep(meanState, each = n)
  scale <- sqrt(mean(rowSums(targetStateDemeaned)^2))
  targeStateNormed <- targetStateDemeaned / scale
  followerStateNormed <- (follower$state - rep(meanState, each = n)) / scale
  err <- sqrt(rowSums((targeStateNormed - followerStateNormed)^2))
  err[is.na(err)] <- opts$bound
  score <- mean(pmin(opts$bound, cummax(err)))
  return(lst(score))
}

