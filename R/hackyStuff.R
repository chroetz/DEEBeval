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


#' @export
hackCreateSmapeSeriesPlot <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  data <-
    lapply(models, \(model) {
      cat("Model:", model, "\n")
      paths <- DEEBpath::getPaths(dbPath, model)
      methods <- list.dirs(paths$esti, recursive=FALSE)
      lapply(methods, \(method) {
        smapePath <- file.path(method, "sMAPE_truth0001obs0001task01esti.csv")
        DEEBtrajs::readTrajs(smapePath) |>
          rename(smape = state) |>
          mutate(smape = as.vector(smape)) |>
          mutate(method = basename(method))
      }) |>
        bind_rows() |>
        mutate(model = model)
    }) |>
      bind_rows()
  data |>
    ggplot(aes(x = time, y = smape, color = method)) +
    geom_line() +
    facet_wrap(vars(model), scales="free")
  le <- read_csv(file.path(dbPath, "_info/LyapunovExponents.csv"))
  data |>
    left_join(le, join_by(model)) |>
    group_by(model, method) |>
    mutate(time = lyapunovExponent*(time - time[1])) |>
    summarize(max(time))
  data |>
    left_join(le, join_by(model)) |>
    group_by(model, method) |>
    #mutate(time = lyapunovExponent*(time - time[1])) |>
    summarize(max(time))
  data |>
    summarise(smape = smape[10], .by = c("model", "method")) |>
    ggplot(aes(x = method, y = smape, color = model)) + geom_point()
}


