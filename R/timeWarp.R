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

fitTimeWarp <- function(trajFix, trajDial, maxDeltaI = 5) {
  if (length(unique(trajFix$trajId)) > 1) stop("Multiple Traj IDs not implemented yet.")
  warpedIdxes <- integer(nrow(trajFix))
  n <- nrow(trajDial$state)
  iDial <- 1
  for (iFix in seq_len(nrow(trajFix))) {
    di <- min(maxDeltaI, n - iDial)
    dst <- distPointMat(trajFix$state[iFix,], trajDial$state[iDial + 0:di, ])
    iDialCandidates <- iDial + which(dst == min(dst)) - 1 # multiple minima
    iDial <- iDialCandidates[which.min(abs(iDialCandidates-iFix))]
    warpedIdxes[iFix] <- iDial
  }

  warpedIdxes[which(warpedIdxes == n)[-c(1:2)]] <- NA

  warped <- trajDial
  warped$state <- trajDial$state[warpedIdxes,]

  list(trajs = warped, idxes = warpedIdxes)
}
