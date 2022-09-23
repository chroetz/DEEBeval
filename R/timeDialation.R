distPointMat <- function(point, mat) {
  mat <- matrix(mat, ncol = length(point))
  sqrt(rowSums((rep(point, each = nrow(mat)) - mat)^2))
}

fillNaState <- function(trajs, state) {
  sel <- apply(trajs$state, 1, \(x) any(is.na(x)))
  trajs$state[sel, ] <- rep(state, each = sum(sel))
  trajs
}

fitTimeDialation <- function(trajFix, trajDial, maxDeltaI = 5) {
  dialatedIdxes <- integer(nrow(trajFix))
  iDial <- 1
  for (iFix in seq_len(nrow(trajFix))) {
    di <- min(maxDeltaI, nrow(trajDial$state) - iDial)
    dst <- distPointMat(trajFix$state[iFix,], trajDial$state[iDial + 0:di, ])
    iDial <- iDial + which.min(dst) - 1
    dialatedIdxes[iFix] <- iDial
  }
  dialatedIdxes[which(dialatedIdxes == max(dialatedIdxes))[-1]] <- NA

  dialated <- trajDial
  dialated$state <- trajDial$state[dialatedIdxes,]

  list(trajs = dialated, idxes = dialatedIdxes)
}
