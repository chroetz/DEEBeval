calculateTimeWarp <- function(info, timeRange, nBasePoints=1e3) {
  times <- seq(timeRange[1], timeRange[2], length.out = nBasePoints)
  truth <- interpolateTrajs(info$truth, times)
  esti <- interpolateTrajs(info$esti, times)
  fitWarp <- fitTimeWarp(truth, esti)
  appendToEnv(info, list(timeWarp = list(
    esti = fitWarp$trajs,
    truth = truth,
    estiIdxes = fitWarp$idxes
  )))
}

distPointMat <- function(point, mat) {
  mat <- matrix(mat, ncol = length(point))
  sqrt(rowSums((rep(point, each = nrow(mat)) - mat)^2))
}

fitTimeWarpGen <- function(trajFix, trajWarp, maxDeltaI = 10) {
  if (length(unique(trajFix$trajId)) > 1) stop("Multiple Traj IDs not implemented yet.")
  warpedIdxes <- integer(nrow(trajFix))
  n <- nrow(trajWarp$state)
  iWarp <- 1
  for (iFix in seq_len(nrow(trajFix))) {
    di <- min(max(maxDeltaI, iFix - iWarp), n - iWarp)
    dst <- distPointMat(trajFix$state[iFix,], trajWarp$state[iWarp + 0:di, ])
    iWarpCandidates <- iWarp + which(dst == min(dst)) - 1 # multiple minima
    iWarp <- iWarpCandidates[which.min(abs(iWarpCandidates-iFix))]
    warpedIdxes[iFix] <- iWarp
  }

  warpedIdxes[which(warpedIdxes == n)[-c(1:2)]] <- NA

  warped <- trajWarp
  warped$state <- trajWarp$state[warpedIdxes,]

  list(trajs = warped, idxes = warpedIdxes)
}

fitTimeWarp <- function(trajFix, trajWarp) { # linear time warp
  if (length(unique(trajFix$trajId)) > 1) stop("Multiple Traj IDs not implemented yet.")
  m1 <- 40
  as <- 2^seq(-1, 1, len=m1)
  # round 1: find some rough estimate of the time warp factor
  res <- sapply(
    as,
    \(a) {
      x <- trajWarp
      x$time <- x$time * a
      y <- interpolateTrajs(x, trajFix$time)
      sel <- y$time <= max(x$time)
      max((y$state[sel,] - trajFix$state[sel,])^2)
    })
  i <- which.min(res[-c(1,m1)])+1

  # round 2: increase accuracy
  m2 <- 10
  as <- as[i-1]*(as[i+1]/as[i-1])^seq(0, 1, len=m2)
  res <- sapply(
    as,
    \(a) {
      x <- trajWarp
      x$time <- x$time * a
      y <- interpolateTrajs(x, trajFix$time)
      sel <- y$time <= max(x$time)
      max((y$state[sel,] - trajFix$state[sel,])^2)
    })
  i <- which.min(res)
  x <- trajWarp
  x$time <- x$time * as[i]
  warped <- interpolateTrajs(x, trajFix$time)
  list(trajs = warped, idxes = round(seq_len(nrow(trajFix)) * as[i]))
}
