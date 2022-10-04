scoreDistance <- function(trajs1, trajs2, timeRange, opts) {
  opts <- asOpts(opts, c("Distance", "TimeState", "Score"))
  times <- seq(timeRange[1], timeRange[2], length.out = opts$timeSteps)
  trajs1 <- interpolateTrajs(trajs1, times)
  trajs2 <- interpolateTrajs(trajs2, times)
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
