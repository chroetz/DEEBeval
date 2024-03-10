scoreFollowTime <- function(follower, target, opts, info) {
  opts <- asOpts(opts, c("FollowTime", "TimeState", "Score"))
  if (!isTimeEqual(follower, target)) {
    warning("unequal time -> interpolating", immediate. = TRUE)
    follower <- DEEBtrajs::interpolateTrajs(follower, tragetTimes = target$time)
  }
  if (any(is.na(follower$state)) || any(is.na(target$state))) return(NA)
  res <- apply2TrajId(follower, target, scoreFollowTimeOne, opts=opts)
  appendToEnv(info, list(followTime = res))
  total <- sum(sapply(res, \(x) x$score))
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
    score = tm - target$time[1],
    time = target$time,
    distance = dst,
    radius = opts$radius
  ))
}

