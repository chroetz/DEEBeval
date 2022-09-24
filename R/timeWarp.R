distPointMat <- function(point, mat) {
  mat <- matrix(mat, ncol = length(point))
  sqrt(rowSums((rep(point, each = nrow(mat)) - mat)^2))
}

fillNaState <- function(trajs, state) {
  sel <- apply(trajs$state, 1, \(x) any(is.na(x)))
  trajs$state[sel, ] <- rep(state, each = sum(sel))
  trajs
}

fitTimeWarp <- function(trajFix, trajDial, maxDeltaI = 5) {
  if (length(unique(trajFix$trajId)) > 1) stop("Multiple Traj IDs not implemented yet.")
  warpedIdxes <- integer(nrow(trajFix))
  iDial <- 1
  for (iFix in seq_len(nrow(trajFix))) {
    di <- min(maxDeltaI, nrow(trajDial$state) - iDial)
    dst <- distPointMat(trajFix$state[iFix,], trajDial$state[iDial + 0:di, ])
    iDial <- iDial + which.min(dst) - 1
    warpedIdxes[iFix] <- iDial
  }
  warpedIdxes[which(warpedIdxes == max(warpedIdxes))[-c(1:2)]] <- NA

  warped <- trajDial
  warped$state <- trajDial$state[warpedIdxes,]

  list(trajs = warped, idxes = warpedIdxes)
}
