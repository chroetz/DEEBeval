isTimeEqual <- function(trajs1, trajs2) {
  length(trajs1$time) == length(trajs2$time) && all(trajs1$time == trajs2$time)
}
