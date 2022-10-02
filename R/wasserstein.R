wassersteinDistance <- function(trajs1, trajs2, p=2, timeRange, nBasePoints=1e3) {
  # TODO: multiple trajIds
  times <- seq(timeRange[1], timeRange[2], length.out = nBasePoints)
  trajs1 <- interpolateTrajs(trajs1, times)
  trajs2 <- interpolateTrajs(trajs2, times)
  wss <- transport::wasserstein(
    transport::pp(trajs1$state),
    transport::pp(trajs2$state),
    p = p)
  return(wss)
}
