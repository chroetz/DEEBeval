plotTimeDiff <- function(idxes, time) {
  dialatedIdxesCutoff <- idxes[!is.na(idxes)]
  timeDiff <- (dialatedIdxesCutoff - seq_along(dialatedIdxesCutoff)) * getTimeStep(time)
  timeCutoff <- time[seq_along(timeDiff)]
  plot(
    NA,
    xlim = range(time),
    ylim = c(
      min(timeDiff)-diff(range(timeDiff))*0.1,
      max(timeDiff)+diff(range(timeDiff))*0.1),
    xlab = "time", ylab = "time difference")
  graphics::grid()
  graphics::abline(a = max(time), b = -1, col=2, lwd=2)
  graphics::abline(h = 0, col=1, lwd=2)
  graphics::lines(timeCutoff, timeDiff, col=3, lwd=2)
}
