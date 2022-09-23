lpErr <- function(trajs1, trajs2, timeRange, p) {
  distTraj <- compareTrajStates(
    trajs1, trajs2,
    \(x, y) abs(x-y),
    timeRange = timeRange)
  if (is.finite(p)) {
    mean(rowSums(distTraj$state^p))^(1/p)
  } else {
    max(distTraj$state)
  }
}


calcError <- function(errorFun, model, truthNr, method, db, example) {
  info <- expandInfo(db, example, model, truthNr, method)
  errorFun(info$truth, info$esti, info$obs)
}


calcConstError <- function(model, truthNr, db, example, p) {
  info <- expandInfo(db, example, model, truthNr)
  if (p == 1) {
    ref <- makeTrajsStateConst(info$truth, stats::median)
  } else if (p == 2) {
    ref <- makeTrajsStateConst(info$truth, mean)
  } else if (p == Inf) {
    ref <- makeTrajsStateConst(info$truth, \(x) mean(range(x)))
  } else {
    stop()
  }
  lpErr(info$truth, ref, range(info$obs$time), p=p)
}
