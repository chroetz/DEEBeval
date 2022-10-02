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
    stateDistriW2 = ,
    Wasserstein2 = \(info) wassersteinDistance(info$esti, info$truth, timeRange=predictionTime, p=2),
    timeTillFail = ,
    FollowTime = \(info) followTime(info$truth, info$esti, info$obs, predictionTime),
    stop("Unknown loss name ", name))
}
