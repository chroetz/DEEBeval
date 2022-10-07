buildScore <- function(opts, timeRange, verbose=TRUE) {
  cat(classString(opts), "\n")
  opts <- asOpts(opts, "Score")
  name <- getClassAt(opts, 2)
  scoreFun <- switch(
    name,
    StateSpace = buildStateSpaceScore(opts, timeRange),
    TimeState = buildTimeStateScore(opts, timeRange),
    VelocityField = buildVelocityFieldScore(opts),
    stop("Unknown Score ", name)
  )
  if (verbose) {
    verboseScoreFun <- \(info) {
      cat("start", paste(oldClass(opts), collapse="_"), "... ")
      pt <- proc.time()
      res <- scoreFun(info)
      cat("end after ", (proc.time()-pt)[3], "s\n", sep="")
      return(res)
    }
    return(verboseScoreFun)
  }
  return(scoreFun)
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
    FollowTime = \(info) {warning("FollowTime score is not implemented yet. Returning 0.");0},
    stop("Unknown TimeStateScore ", name)
  )
}

buildVelocityFieldScore <- function(opts) {
  opts <- asOpts(opts, c("VelocityField", "Score"))
  name <- getClassAt(opts, 3)
  switch(
    name,
    Distance = \(info) scoreFieldDistance(info$esti, info$truth, opts),
    stop("Unknown VelocityFieldScore ", name)
  )
}


