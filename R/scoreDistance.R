scoreDistance <- function(trajs1, trajs2, opts) {
  opts <- asOpts(opts, c("Distance", "TimeState", "Score"))
  if (!isTimeEqual(trajs1, trajs2)) {
    warning("unequal time -> interpolating", immediate. = TRUE)
    trajs1 <- DEEBtrajs::interpolateTrajs(trajs1, tragetTimes = trajs2$time)
  }
  if (any(is.na(trajs1$state)) || any(is.na(trajs2$state))) return(NA)
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
  stopifnot(max(abs(trajs1$state - trajs2$state)) < sqrt(.Machine$double.eps))
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

