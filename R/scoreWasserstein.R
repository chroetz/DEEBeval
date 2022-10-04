scoreWasserstein <- function(trajs1, trajs2, timeRange, opts) {
  opts <- asOpts(opts, c("Wasserstein", "StateSpace", "Score"))
  times <- seq(timeRange[1], timeRange[2], length.out = opts$timeSteps)
  trajs1 <- interpolateTrajs(trajs1, times)
  trajs2 <- interpolateTrajs(trajs2, times)
  wss <- transport::wasserstein(
    transport::pp(trajs1$state),
    transport::pp(trajs2$state),
    p = opts$p)
  return(wss)
}
