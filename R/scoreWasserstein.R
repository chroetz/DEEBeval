scoreWasserstein <- function(trajs1, trajs2, opts) {
  opts <- asOpts(opts, c("Wasserstein", "StateSpace", "Score"))
  stopifnot(isTimeEqual(trajs1, trajs2))
  if (any(is.na(trajs1$state)) || any(is.na(trajs2$state))) return(NA)
  res <- apply2TrajId(trajs1, trajs2, simplify=TRUE, function(t1, t2) {
    transport::wasserstein(
      transport::pp(t1$state),
      transport::pp(t2$state),
      p = opts$p)
  })
  totalDistance <- sum(res)
  return(totalDistance)
}
