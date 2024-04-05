#' @export
runEval <- function(
  dbPath = ".",
  models = DEEBpath::getModels(dbPath),
  methodsFilter = NULL,
  obsNrFilter = NULL,
  truthNrFilter = NULL,
  taskNrFilter = NULL,
  scoreFilter = NULL,
  createPlots = TRUE,
  verbose = FALSE,
  onlyNew = FALSE,
  writeScoreHtml = TRUE,
  createSummary = FALSE
) {

  for (model in models) {

    cat("MODEL: ", model, "\n")
    pt <- proc.time()

    path <- DEEBpath::getPaths(dbPath, model)

    methods <- list.dirs(path$esti, full.names = FALSE, recursive = FALSE)
    if (!is.null(methodsFilter)) methods <- intersect(methods, methodsFilter)

    for (method in methods) {
      cat("\t", method)
      ptMethod <- proc.time()
      methodEstiPath <- file.path(path$esti, method)
      if (!dir.exists(methodEstiPath)) next
      meta <-
        DEEBpath::getMetaGeneric(
          c(path$truth, path$obs, methodEstiPath, path$task),
          nrFilters = list(
            obsNr = obsNrFilter,
            truthNr = truthNrFilter,
            taskNr = taskNrFilter
          ),
          tagFileFilter = list(
            c("truth", "obs", "task", "esti"),
            c("task", "truth"),
            "task"),
          removeNa = TRUE)
      if (!"estiPath" %in% colnames(meta)) {
        cat(" No estimation file found.\n")
        next
      }
      evalMetaAndWriteToFile(
        meta,
        path$eval,
        path$plots,
        method,
        createPlots = createPlots,
        scoreFilter = scoreFilter,
        verbose = verbose)

      if (createPlots)
        createExtraPlots(path, method, obsNrFilter, truthNrFilter)

      cat(" took ", format((proc.time()-ptMethod)[3]), "s\n", sep="")
    }

    cat(model, " took ", format((proc.time()-pt)[3]), "s", sep="")
  }

  if (writeScoreHtml) {
    for (model in models) runScoreHtml(dbPath, model)
  }

  if (createSummary) createSummary(dbPath)
}


#' @export
runEvalTbl <- function(
    dbPath,
    tbl, # use getUnevaled() to get a suitable tibble with columns c("truthNr", "obsNr", "taskNr", "method", "model")
    scoreFilter = NULL,
    createPlots = TRUE,
    verbose = FALSE,
    writeScoreHtml = TRUE,
    createSummary = FALSE
) {
  models <- tbl$model |> unique()
  methods <- tbl$method |> unique()

  # TODO: remove code duplications with runEval()

  for (model in models) {
    cat("MODEL:", model, "\n")
    pt <- proc.time()
    path <- DEEBpath::getPaths(dbPath, model)
    for (method in methods) {
      cat("\t", method)
      ptMethod <- proc.time()
      methodEstiPath <- file.path(path$esti, method)
      if (!dir.exists(methodEstiPath)) {
        cat(" methodEstiPath does not exist\n")
        next
      }
      meta <-
        DEEBpath::getMetaGeneric(
          c(path$truth, path$obs, methodEstiPath, path$task),
          tagFileFilter = list(
            c("truth", "obs", "task", "esti"),
            c("task", "truth"),
            "task"),
          removeNa = TRUE)
      if (length(meta) == 0) {
        cat(" no files found\n")
        next
      }
      filteredTbl <-
        tbl |> filter(
          .data$method == .env$method,
          .data$model == .env$model)
      if (NROW(meta) == 0 || NROW(tbl) == 0) {
        cat(" nothing at all to evaluate\n")
        next
      }
      meta <-
        meta |>
        semi_join(
          filteredTbl,
          by = c("truthNr", "obsNr", "taskNr"))
      if (nrow(meta) == 0) {
        cat(" nothing to evaluate\n")
        next
      }
      evalMetaAndWriteToFile(
        meta,
        path$eval,
        path$plots,
        method,
        createPlots = createPlots,
        scoreFilter = scoreFilter,
        verbose = verbose)
      if (createPlots) createExtraPlots(path, method)
      cat(" took ", format((proc.time()-ptMethod)[3]), "s\n", sep="")
    }

    cat(model, " took ", format((proc.time()-pt)[3]), "s\n", sep="")
  }

  if (writeScoreHtml) {
    for (model in models) runScoreHtml(dbPath, model)
  }

  if (createSummary) createSummary(dbPath)
}


#' @export
runScoreHtml <- function(dbPath, model) {
  cat("Writing ScoreHTML for model", model, "...")
  pt <- proc.time()
  paths <- DEEBpath::getPaths(dbPath, model)
  writeDoc(
    paths$eval,
    "scores",
    path = paths$eval,
    reference = "ConstMean",
    best = "Truth")
  cat(" done after ", format((proc.time()-pt)[3]), "s\n", sep="")
}
