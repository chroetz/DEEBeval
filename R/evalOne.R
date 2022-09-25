evalOneInfo <- function(info, quantFuns, plotFuns) {
  expandInfoEnv(info)
  if (is.null(info$truth)) return(list(quants = NULL, plots = NULL))
  quants <- lapply(quantFuns, do.call, args = list(info = info))
  plots <- lapply(plotFuns, do.call, args = list(info = info))
  return(list(quants = quants, plots = plots))
}

evalOne <- function(db, example, model, truthNr, method = NULL, quantFuns = list(), plotFuns = list()) {
  info <- new.env(parent=emptyenv())
  info$db <- db
  info$example <- example
  info$model <- model
  info$truthNr <- truthNr
  info$method <- method
  evaluation <- evalOneInfo(info, quantFuns, plotFuns)
  list(evaluation = evaluation, info = info)
}

