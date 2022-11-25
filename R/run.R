#' @export
runEval <- function(
    dbPath = ".",
    models = list.dirs(path = dbPath, full.names = FALSE, recursive = FALSE),
    example = FALSE,
    methodsFilter = NULL,
    obsNrFilter = NULL,
    truthNrFilter = NULL,
    taskNrFilter = NULL,
    scoreFilter = NULL,
    createPlots = TRUE,
    verbose = FALSE
) {

  for (model in models) {

    message("MODEL: ", model)
    pt <- proc.time()

    path <- DEEBpath::getPaths(dbPath, model, example=example)

    methods <- list.dirs(path$esti, full.names = FALSE, recursive = FALSE)
    if (!is.null(methodsFilter)) methods <- intersect(methods, methodsFilter)

    for (method in methods) {
      cat(method, "\n")
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
          removeNa = TRUE)
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

    writeDoc(
      path$eval,
      "scores",
      paths = DEEBpath::getScoreFiles(path$eval))

    message(model, " took ", format((proc.time()-pt)[3]), "s")
  }
}

createExtraPlots <- function(path, method, obsNrFilter = NULL, truthNrFilter = NULL) {
  methodEstiPath <- file.path(path$esti, method)
  smoothMeta <-
    DEEBpath::getMetaGeneric(
      c(path$truth, path$obs, methodEstiPath),
      tagsFilter = c("esti", "truth", "obs", "smooth"),
      nrFilters = list(
        obsNr = obsNrFilter,
        truthNr = truthNrFilter
      ),
      removeNa = TRUE)
  for (i in seq_len(nrow(smoothMeta))) {
    info <- smoothMeta[i,]
    smooth <- readTrajs(info$smoothPath)
    truth <- readTrajs(info$truthPath)
    obs <- readTrajs(info$obsPath)
    plt <- DEEBplots::plotTimeState(
      truth, smooth, obs,
      title = paste0(method, ", Truth", info$truthNr, ", Obs", info$obsNr))
    fileName <- DEEBpath::parenthesisFileName(
      truth = info$truthNr,
      obs = info$obsNr,
      method = method,
      plot = "smooth",
      .ending = "png")
    DEEBplots::writePlot(plt, file.path(path$plots, fileName))
  }
  DEEBplots::createShowPlots(path$eval)
}

#' @export
runEvalTbl <- function(
    dbPath,
    tbl, # use getUnevaled() to get a suitable tibble with columns c("truthNr", "obsNr", "taskNr", "method", "model", "example")
    scoreFilter = NULL,
    createPlots = TRUE,
    verbose = FALSE
) {
  models <- tbl$model |> unique()
  methods <- tbl$method |> unique()
  examples <- tbl$example |> unique()

  # TODO: remove code duplications with runEval()

  for (example in examples) {
    for (model in models) {
      message("MODEL: ", model)
      if (example) message("example")
      pt <- proc.time()
      path <- DEEBpath::getPaths(dbPath, model, example=example)
      for (method in methods) {
        cat(method)
        ptMethod <- proc.time()
        methodEstiPath <- file.path(path$esti, method)
        if (!dir.exists(methodEstiPath)) next
        meta <-
          DEEBpath::getMetaGeneric(
            c(path$truth, path$obs, methodEstiPath, path$task),
            removeNa = TRUE)
        meta <-
          meta |>
          dplyr::semi_join(
            tbl |> dplyr::filter(
              .data$method == .env$method,
              .data$model == .env$model,
              .data$example == .env$example),
            by = c("truthNr", "obsNr", "taskNr"))
        if (nrow(meta) == 0) next
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

      writeDoc(
        path$eval,
        "scores",
        paths = DEEBpath::getScoreFiles(path$eval))

      message(model, " took ", format((proc.time()-pt)[3]), "s")
    }
  }
}

