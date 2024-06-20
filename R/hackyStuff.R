#' @export
hackCreateSmapeSeries <- function(dbPath) {
  opts <- ConfigOpts::makeOpts(
    c("Distance", "TimeState", "Score"),
    name = "sMAPE",
    method = "L1",
    optimalValue = 0,
    normalize = "norm",
    factor = 100
  )
  models <- DEEBpath::getModels(dbPath)
  for (model in models) {
    cat("Model:", model, "\n")
    paths <- DEEBpath::getPaths(dbPath, model)
    methods <- list.dirs(paths$esti, recursive=FALSE)
    estiTable <- DEEBpath::getMetaGeneric(methods, tagsFilter=c("truth", "obs", "task", "esti"))
    for (i in seq_len(nrow(estiTable))) {
      truth <- DEEBtrajs::readTrajs(file.path(paths$truth, DEEBpath::taskTruthFile(estiTable[i, ])))
      estiPath <- estiTable$estiPath[[i]]
      esti <- DEEBtrajs::readTrajs(estiPath)
      res <- scoreDistance(esti, truth, opts, aggregateFun = force)
      errorTrajs <- DEEBtrajs::makeTrajs(trajId = truth$trajId, time = truth$time, state = matrix(res, ncol=1))
      outPath <- file.path(dirname(estiPath), paste0("sMAPE_", basename(estiPath)))
      DEEBtrajs::writeTrajs(errorTrajs, outPath)
    }
  }
}
