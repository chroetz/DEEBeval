scoreFollowTime <- function(follower, target, opts, info) {
  opts <- asOpts(opts, c("FollowTime", "TimeState", "Score"))
  stopifnot(isTimeEqual(follower, target))
  res <- apply2TrajId(follower, target, scoreFollowTimeOne, opts=opts)
  appendToEnv(info, list(followTime = res))
  total <- sum(sapply(res, \(x) x$followTime))
  return(total)
}

scoreFollowTimeOne <- function(follower, target, opts) {
  normalization <- calculateNormalization(target)
  targetNormed <- normalization$normalize(target)
  followerNormed <- normalization$normalize(follower)
  dst <- minDistTimeState(followerNormed$state, targetNormed$state, target$time, opts$timeScale)
  iLoss <- which(dst > opts$radius)[1]
  if (is.na(iLoss)) {
    tm <- max(target$time)
  } else {
    tm <- target$time[iLoss]
  }
  return(list(
    followTime = tm,
    time = target$time,
    distance = dst,
    radius = opts$radius
  ))
}

