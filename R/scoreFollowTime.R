scoreFollowTime <- function(follower, target, opts, info) {
  opts <- asOpts(opts, c("FollowTime", "TimeState", "Score"))
  stopifnot(isTimeEqual(follower, target))
  res <- apply2TrajId(follower, target, scoreFollowTimeOne, opts=opts)
  appendToEnv(info, list(followTime = res))
  total <- sum(sapply(res, \(x) x$followTime))
  return(total)
}

scoreFollowTimeOne <- function(follower, target, opts) {
  dst <- minDistTimeState(follower$state, target$state, target$time, opts$timeScale)
  iLoose <- which(dst > opts$radius)[1]
  if (is.na(iLoose)) {
    tm <- max(follower$time)
  } else if (iLoose == 1) {
    tm <- min(follower$time)
  } else {
    tm <- follower$time[iLoose-1]
  }
  return(list(
    followTime = tm,
    time = target$time,
    distance = dst,
    timeScale = opts$timeScale,
    radius = opts$radius
  ))
}

