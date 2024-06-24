buildScore <- function(opts, verbose=TRUE) {
  if (verbose) cat(classString(opts), "\n")
  opts <- asOpts(opts, "Score")
  name <- getClassAt(opts, 2)
  scoreFun <- switch(
    name,
    StateSpace = buildStateSpaceScore(opts),
    TimeState = buildTimeStateScore(opts),
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


buildStateSpaceScore <- function(opts) {
  opts <- asOpts(opts, c("StateSpace", "Score"))
  name <- getClassAt(opts, 3)
  switch(
    name,
    Wasserstein = \(info) scoreWasserstein(info$esti, info$truth, opts),
    stop("Unknown StateSpaceScore ", name)
  )
}

buildTimeStateScore <- function(opts) {
  opts <- asOpts(opts, c("TimeState", "Score"))
  name <- getClassAt(opts, 3)
  switch(
    name,
    CumMaxErr = \(info) scoreCumMaxErr(info$esti, info$truth, opts),
    Distance = \(info) scoreDistance(info$esti, info$truth, opts),
    TimeWarp = \(info) scoreTimeWarp(info$esti, info$truth, opts, info),
    FollowTime = \(info) scoreFollowTime(info$esti, info$truth, opts, info),
    ValidTime = \(info) scoreValidTime(info$esti, info$truth, opts, info),
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


