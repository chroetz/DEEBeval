buildScore <- function(opts, timeRange) {
  opts <- asOpts(opts, "Score")
  name <- getClassAt(opts, 2)
  switch(
    name,
    StateSpace = buildStateSpaceScore(opts, timeRange),
    TimeState = buildTimeStateScore(opts, timeRange),
    stop("Unknown Score ", name)
  )
}


buildStateSpaceScore <- function(opts, timeRange) {
  opts <- asOpts(opts, c("StateSpace", "Score"))
  name <- getClassAt(opts, 3)
  switch(
    name,
    Wasserstein = \(info) scoreWasserstein(
      info$esti, info$truth,
      timeRange = timeRange,
      opts),
    stop("Unknown StateSpaceScore ", name)
  )
}

buildTimeStateScore <- function(opts, timeRange) {
  opts <- asOpts(opts, c("TimeState", "Score"))
  name <- getClassAt(opts, 3)
  switch(
    name,
    Distance = \(info) scoreDistance(info$esti, info$truth, timeRange, opts),
    TimeWarp = \(info) scoreTimeWarp(info$esti, info$truth, timeRange, opts, info),
    FollowTime = \(info) stop("FollowTime score is not implemented yet"),
    stop("Unknown TimeStateScore ", name)
  )
}


