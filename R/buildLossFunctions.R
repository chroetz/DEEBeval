buildLossFunction <- function(name, predictionTime) {
  switch(
    name,
    L1 = \(info) lpErr(info$truth, info$esti, predictionTime, p=1),
    L2 = \(info) lpErr(info$truth, info$esti, predictionTime, p=2),
    Linf = \(info) lpErr(info$truth, info$esti, predictionTime, p=Inf),
    L1AfterTimeWarp = \(info) timeWarpLpErr(info, predictionTime, p=1),
    L2AfterTimeWarp = \(info) timeWarpLpErr(info, predictionTime, p=2),
    LinfAfterTimeWarp = \(info) timeWarpLpErr(info, predictionTime, p=Inf),
    TimeWarpCosts = \(info) timeWarpCosts(info, predictionTime),
    stateDistriW2 = \(info) stateWasserstein(info, predictionTime, p=2),
    timeTillFail  = \(info) 0,
    stop("Unknown loss name ", name))
}
