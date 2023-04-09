scoreValidTime <- function(follower, target, opts, info) {
  opts <- asOpts(opts, c("ValidTime", "TimeState", "Score"))
  stopifnot(isTimeEqual(follower, target))
  if (any(is.na(follower$state)) || any(is.na(target$state))) return(NA)
  res <- apply2TrajId(follower, target, scoreValidTimeOne, opts=opts)
  appendToEnv(info, list(validTime = res))
  total <- sum(sapply(res, \(x) x$score))
  return(total)
}

scoreValidTimeOne <- function(follower, target, opts) {
  normalization <- calculateNormalization(target, opts$normalization)
  targetNormed <- normalization$normalize(target)
  followerNormed <- normalization$normalize(follower)
  dst <- sqrt(rowSums((followerNormed$state - targetNormed$state)^2))
  idxFirstInvalid <- which(dst >= opts$threshold)[1]
  if (is.na(idxFirstInvalid)) {
    tm <- max(target$time)
  } else {
    tm <- target$time[idxFirstInvalid]
  }
  return(list(
    followTime = tm,
    score = tm - target$time[1],
    time = target$time,
    distance = dst,
    radius = opts$radius
  ))
}

