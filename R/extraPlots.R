createExtraPlots <- function(path, method, obsNrFilter = NULL, truthNrFilter = NULL) {

  methodEstiPath <- file.path(path$esti, method)
  smoothMeta <-
    DEEBpath::getMetaGeneric(
      c(path$truth, path$obs, methodEstiPath),
      tagsFilter = c("esti", "truth", "obs", "smooth"),
      # TODO: tagFileFilter = ...?
      nrFilters = list(
        obsNr = obsNrFilter,
        truthNr = truthNrFilter
      ),
      removeNa = TRUE)
  if (!"smoothPath" %in% colnames(smoothMeta)) return(NULL)

  for (i in seq_len(nrow(smoothMeta))) {

    info <- smoothMeta[i,]
    smooth <- readTrajs(info$smoothPath)
    truth <- readTrajs(info$truthPath)
    obs <- readTrajs(info$obsPath)
    esti <- searchObsEsti(path, method, info)

    plt <- DEEBplots::plotTimeState(
      truth = truth, esti = esti, smooth = smooth, obs = obs,
      title = paste0(method, ", Truth", info$truthNr, ", Obs", info$obsNr))
    fileName <- DEEBpath::parenthesisFileName(
      truth = info$truthNr,
      obs = info$obsNr,
      method = method,
      plot = "smoothTimeState",
      .ending = "png")
    DEEBplots::writePlot(plt, file.path(path$plots, fileName))

    plt <- DEEBplots::plotStateSpace(
      truth = truth, esti = esti, smooth = smooth, obs = obs,
      title = paste0(method, ", Truth", info$truthNr, ", Obs", info$obsNr))
    fileName <- DEEBpath::parenthesisFileName(
      truth = info$truthNr,
      obs = info$obsNr,
      method = method,
      plot = "smoothStateSpace",
      .ending = "png")
    DEEBplots::writePlot(plt, file.path(path$plots, fileName))

    plt <- DEEBplots::plotDistances(
      truth = truth, esti = esti, smooth = smooth, obs = obs,
      title = paste0(method, ", Truth", info$truthNr, ", Obs", info$obsNr))
    fileName <- DEEBpath::parenthesisFileName(
      truth = info$truthNr,
      obs = info$obsNr,
      method = method,
      plot = "L2-distance",
      .ending = "png")
    DEEBplots::writePlot(plt, file.path(path$plots, fileName))
  }

  DEEBplots::createShowPlots(path$eval)
}


searchObsEsti <- function(path, method, info) {
  meta <-
    DEEBpath::getMetaGeneric(
      c(path$truth, path$obs, file.path(path$esti, method), path$task),
      tagsFilter = c("esti", "truth", "obs", "smooth", "task"),
      tagFileFilter = list(
            c("truth", "obs", "task", "esti"),
            c("task", "truth"),
            "task"),
      nrFilters = list(
        obsNr = info$obsNr,
        truthNr = info$truthNr
      ),
      removeNa = TRUE)
  taskNr <- NULL
  for (i in seq_len(nrow(meta))) {
    nfo <- meta[i,]
    task <- readOptsBare(nfo$taskPath)
    if (ConfigOpts::getClassAt(task, 2) == "estiObsTrajs" &&
        task$predictionTime[1] == 0) {
      taskNr <- nfo$taskNr
      break
    }
  }
  if (is.null(taskNr)) return(NULL)
  nfo <- meta[meta$taskNr == taskNr,]
  return(readTrajs(nfo$estiPath))
}

