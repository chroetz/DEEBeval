scoreFollowTime <- function(follower, target, opts, info) {
  opts <- asOpts(opts, c("FollowTime", "TimeState", "Score"))
  if (!isTimeEqual(follower, target)) {
    warning("unequal time -> interpolating", immediate. = TRUE)
    follower <- DEEBtrajs::interpolateTrajs(follower, targetTimes = target$time)
  }
  res <- apply2TrajId(follower, target, scoreFollowTimeOne, opts=opts)
  appendToEnv(info, list(followTime = res))
  total <- sum(sapply(res, \(x) x$score))
  return(total)
}

scoreFollowTimeOne <- function(follower, target, opts) {
  normalization <- calculateNormalization(target)
  targetNormed <- normalization$normalize(target)
  followerNormed <- normalization$normalize(follower)
  dst <- rep(NA_real_, n)
  lastValidIdx <- which(rowSums(is.na(follower)) > 0)[1]
  if (is.na(lastValidIdx)) lastValidIdx <- nrow(follower)
  sel <- 1:lastValidIdx
  dst[sel] <- DEEButil::minDistTimeState(
    followerNormed$state[sel,, drop=FALSE],
    targetNormed$state[sel,, drop=FALSE],
    target$time[sel],
    opts$timeScale)
  iLoss <- which(dst > opts$radius)[1]
  if (is.na(iLoss)) {
    tm <- target$time[lastValidIdx]
  } else {
    tm <- target$time[iLoss]
  }
  return(list(
    followTime = tm,
    score = tm - target$time[1],
    time = target$time,
    distance = dst,
    radius = opts$radius
  ))
}

