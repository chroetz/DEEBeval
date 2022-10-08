scoreWasserstein <- function(trajs1, trajs2, timeRange, opts) {
  opts <- asOpts(opts, c("Wasserstein", "StateSpace", "Score"))
  if (length(trajs1$time) != length(trajs2$time) || any(trajs1$time != trajs2$time)) {
    stop("Times are not equal")
  }
  res <- apply2TrajId(trajs1, trajs2, simplify=TRUE, function(t1, t2) {
    transport::wasserstein(
      transport::pp(t1$state),
      transport::pp(t2$state),
      p = opts$p)
  })
  totalDistance <- sum(res)
  return(totalDistance)
}
