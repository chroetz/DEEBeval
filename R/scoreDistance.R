scoreDistance <- function(trajs1, trajs2, opts) {
  opts <- asOpts(opts, c("Distance", "TimeState", "Score"))
  if (!isTimeEqual(trajs1, trajs2)) {
    warning("unequal time -> interpolating", immediate. = TRUE)
    trajs1 <- DEEBtrajs::interpolateTrajs(trajs1, targetTimes = trajs2$time)
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
  if (opts$normalize == "norm") {
    zero <- matrix(0, nrow = nrow(trajs1$state), ncol = ncol(trajs1$state))
    d1 <- do.call(
      proxy::dist,
      c(
        list(
          x = trajs1$state,
          y = zero,
          method = opts$method,
          pairwise = TRUE
        ),
        opts$methodArgs
      )
    )
    d2 <- do.call(
      proxy::dist,
      c(
        list(
          x = trajs2$state,
          y = zero,
          method = opts$method,
          pairwise = TRUE
        ),
        opts$methodArgs
      )
    )
    dists <- dists/((d1+d2)/2)
  }
  dst <- opts$factor*mean(dists)
  return(dst)
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

