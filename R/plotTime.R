getTimeWarpDiffPlot <- function(idxes, time, title) {
  timeDiff <- (idxes - seq_along(idxes)) * getTimeStep(time)
  timeCutoff <- time[seq_along(timeDiff)]
  pltData <- tibble(
    time = timeCutoff,
    diff = timeDiff)
  x0 <- max(pltData$time)-max(time)*0.05
  plt <- ggplot(pltData, aes(x = .data$time, y = .data$diff)) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = max(time), color = "gray") +
    geom_line() +
    xlab("time") + ylab("time difference") +
    ggtitle(title)
  if (last(pltData$diff) > 0) {
    plt <- plt +
      annotate(
        "segment",
        x = max(pltData$time), xend = max(time),
        y = last(pltData$diff), yend = last(pltData$diff),
        color = "blue") +
      annotate(
        "segment",
        x = x0, xend = max(time),
        y = max(time)-x0, yend = 0,
        color = "red")
  }
  return(plt)
}
