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
  writeScoreHtml = TRUE,
  createSummary = TRUE,
  onlySummarizeScore = FALSE
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

    cat(model, " took ", format((proc.time()-pt)[3]), "s\n", sep="")
  }

  if (writeScoreHtml) {
    for (model in models) runScoreHtml(dbPath, model)
  }

  if (createSummary) createSummary(
    dbPath,
    collectScores = TRUE,
    collectHyper = !onlySummarizeScore,
    renderSummary = !onlySummarizeScore,
    renderHyper = !onlySummarizeScore)
}


#' @export
runEvalTbl <- function(
    dbPath,
    tbl, # a suitable tibble with columns c("truthNr", "obsNr", "taskNr", "method", "model")
    scoreFilter = NULL,
    createPlots = TRUE,
    verbose = FALSE,
    writeScoreHtml = TRUE,
    createSummary = TRUE,
    onlySummarizeScore = FALSE,
    autoId = NULL
) {

  if (NROW(tbl) > 0) {

    tblModelMethod <-
      tbl |>
      select(model, method) |>
      distinct() |>
      arrange(model, method)

    # TODO: remove code duplications with runEval()

    for (i in seq_len(nrow(tblModelMethod))) {
      model <- tblModelMethod$model[i]
      method <- tblModelMethod$method[i]
      cat(model, method)
      pt <- proc.time()
      path <- DEEBpath::getPaths(dbPath, model)

      methodEstiPath <- file.path(path$esti, method)
      if (!dir.exists(methodEstiPath)) {
        cat(" methodEstiPath does not exist:", methodEstiPath,"\n")
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
      cat(" took ", format((proc.time()-pt)[3]), "s\n", sep="")
    }

    if (writeScoreHtml) {
      models <- unique(tbl$model)
      for (model in models) runScoreHtml(dbPath, model)
    }

    if (onlySummarizeScore) {
      collectAutoScores(dbPath, tblModelMethod, autoId = autoId)
    }
  } else {
    cat("table has no rows\n")
  }

  if (createSummary) {
    createSummary(
      dbPath,
      collectScores = TRUE,
      collectHyper = !onlySummarizeScore,
      renderSummary = !onlySummarizeScore,
      renderHyper = !onlySummarizeScore)
  }

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


#' @export
runPlotting <- function(dbPath, model) {
  cat("Start creating plots for model", model, "...")
  pt <- proc.time()
  paths <- DEEBpath::getPaths(dbPath, model)
  if (!dir.exists(paths$plots)) dir.create(paths$plots, recursive=TRUE)

  createObsPlots(paths)

  methodsTbl <- DEEBpath::getMethods(dbPath, model)
  for (k in seq_len(nrow(methodsTbl))) {
    estiPattern <- "^truth(\\d{4})obs(\\d{4})task(\\d{2})esti\\.csv$"
    methodEstiPath <- methodsTbl$path[k]
    estiFileNames <- list.files(methodEstiPath, estiPattern)
    if (length(estiFileNames) == 0) next
    mat <- stringr::str_match(estiFileNames, estiPattern)
    colnames(mat) <- c("fileName", "truthNr", "obsNr", "taskNr")
    tbl <- tibble::as_tibble(mat)
    data <-
      tbl |>
      mutate(
        truthNr = as.integer(truthNr),
        obsNr = as.integer(obsNr),
        taskNr = as.integer(taskNr),
        filePath = file.path(methodEstiPath, fileName)
      )
    baseInfo <- list(
      model = model,
      method = methodsTbl$method[k],
      plotsPath = paths$plots
    )
    for (i in seq_len(nrow(data))) {
      info <- c(
        as.list(data[i,]),
        baseInfo)
      info <- c(
        info,
        list(
          truth = DEEBtrajs::readTrajsOrDerivTrajs(file.path(paths$truth, DEEBpath::taskTruthFile(info))),
          task = ConfigOpts::readOptsBare(file.path(paths$task, DEEBpath::taskFile(info, ending=TRUE))),
          esti = DEEBtrajs::readTrajsOrDerivTrajs(info$filePath),
          obs = NULL))
      }
      createPlotsOne(info)
  }

  DEEBplots::createShowPlots(paths$eval)

  cat(" done after ", format((proc.time()-pt)[3]), "s\n", sep="")
}

createPlotsOne <- function(info) {
  stopifnot(!is.null(info$truth), !is.null(info$esti))

  info$title <- paste0(info$method, ", Task", info$taskNr, ", Truth", info$truthNr, ", Obs", info$obsNr)

  plots <- createTruthEstiTaskPlots(info)
  for (nm in names(plots)) {
    plt <- plots[[nm]]
    fileName <- DEEBpath::parenthesisFileName(
      truth = info$truthNr,
      obs = info$obsNr,
      task = info$taskNr,
      method = info$method,
      plot = nm,
      .ending = "png")
    DEEBplots::writePlot(plt, file.path(info$plotsPath, fileName))
  }
}


createObsPlots <- function(paths) {
  metaObs <-
    DEEBpath::getMetaGeneric(
      c(paths$truth, paths$obs),
      tagsFilter = c("truth", "obs")
    ) |>
    tidyr::drop_na()

  for (i in seq_len(nrow(metaObs))) {
    info <- metaObs[i,]
    plts <- DEEBdata::createTruthObsPlots(info)
    for (nm in names(plts)) {
      fileName <- DEEBpath::parenthesisFileName(truth = info$truthNr, obs = info$obsNr, plot = nm, .ending = "png")
      ggplot2::ggsave(file.path(paths$plots, fileName), plts[[nm]], width = 3, height = 3)
    }
  }

  metaTruth <-
    DEEBpath::getMetaGeneric(
      c(paths$truth, paths$task),
      tagsFilter = c("truth", "task")
    ) |>
    tidyr::drop_na()

  if (!"taskNr" %in% colnames(metaTruth)) metaTruth <- metaTruth[0,]

  for (i in seq_len(nrow(metaTruth))) {
    info <- metaTruth[i,]
    plts <- DEEBdata::createTruthTaskPlots(info)
    for (nm in names(plts)) {
      fileName <- DEEBpath::parenthesisFileName(truth = info$truthNr, task = info$taskNr, plot = nm, .ending = "png")
      ggplot2::ggsave(file.path(paths$plots, fileName), plts[[nm]], width = 3, height = 3)
    }
  }
}
