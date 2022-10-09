scoreFollowTime <- function(follower, target, opts, info) {
  opts <- asOpts(opts, c("FollowTime", "TimeState", "Score"))
  stopifnot(isTimeEqual(follower, target))
  res <- apply2TrajId(follower, target, scoreFollowTimeOne, opts=opts)
  appendToEnv(info, list(followTime = res))
  total <- sum(sapply(res, \(x) x$followTime))
  return(total)
}

scoreFollowTimeOne <- function(follower, target, opts) {
  n <- nrow(target$state)
  refMean <- applyToTrajStateCols(target, mean)$value
  refSd <- applyToTrajStateCols(target, stats::var)$value |> sum() |> sqrt()
  diffStateFollower <- (follower$state - target$state)^2 |> rowSums() |> sqrt()
  diffStateConst <- (rep(refMean, each=n) - target$state)^2 |> rowSums() |> sqrt()
  diffStateFollowerConst <- (rep(refMean, each=n) - follower$state)^2 |> rowSums() |> sqrt()
  betterThanFractionConst <- diffStateFollower <= opts$constFraction*diffStateConst
  bothCloseToConst <-
    diffStateConst <= opts$sdFraction*refSd &
    diffStateFollowerConst <= opts$sdFraction*refSd
  good <- betterThanFractionConst | bothCloseToConst
  z <- 0
  cumStepsLost <- double(n)
  for (i in seq_along(good)) {
    if (good[i] && z > 0) z <- z-1
    if (!good[i]) z <- z+1
    cumStepsLost[i] <- z
  }
  cumTimeLost <- cumStepsLost * getTimeStepTrajs(target)
  followTime <- target$time[which(cumTimeLost > opts$lostTimeThreshold)[1]]
  if (is.na(followTime)) followTime <- target$time |> max()
  return(list(
    followTime = followTime,
    diffConst = diffStateConst,
    diffFollower = diffStateFollower,
    time = target$time,
    cumTimeLost = cumTimeLost,
    closeToConstThreshold = opts$sdFraction*refSd,
    constFraction = opts$constFraction,
    lostTimeThreshold = opts$lostTimeThreshold
  ))
}

