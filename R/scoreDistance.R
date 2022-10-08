scoreDistance <- function(trajs1, trajs2, timeRange, opts) {
  opts <- asOpts(opts, c("Distance", "TimeState", "Score"))
  if (length(trajs1$time) != length(trajs2$time) || any(trajs1$time != trajs2$time)) {
    stop("Times are not equal")
  }
  dists <- do.call(
    proxy::dist,
    c(
      list(
        x = trajs1$state,
        y = trajs2$state,
        method = opts$method,
        pairwise = TRUE
      ),
      opts$methodArgs
    )
  )
  return(mean(dists))
}


scoreFieldDistance <- function(trajs1, trajs2, opts) {
  opts <- asOpts(opts, c("Distance", "VelocityField", "Score"))
  dists <- do.call(
    proxy::dist,
    c(
      list(
        x = trajs1$deriv,
        y = trajs2$deriv,
        method = opts$method,
        pairwise = TRUE
      ),
      opts$methodArgs
    )
  )
  return(mean(dists))
}

