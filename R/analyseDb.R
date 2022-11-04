getUniqueDbEntries <- function(dbPath, example) {
  # TODO: move to DEEBpath
  models <- list.dirs(path = dbPath, full.names = FALSE, recursive = FALSE)
  modelPaths <- file.path(dbPath, models, if (example) "example" else "")
  methods <- unique(unlist(lapply(
    file.path(modelPaths, "estimation"),
    list.dirs,
    full.names = FALSE, recursive = FALSE)))
  truthFiles <- unique(unlist(lapply(
    file.path(modelPaths, "truth"),
    list.files,
    pattern = "obs_truth\\d+\\.csv",
    full.names = FALSE, recursive = FALSE)))
  truthNrs <- unique(as.integer(substr(truthFiles, 10, 13)))
  obsFiles <- unique(unlist(lapply(
    file.path(modelPaths, "observation"),
    list.files,
    pattern = "truth\\d+obs\\d+\\.csv",
    full.names = FALSE, recursive = FALSE)))
  obsNrs <- unique(as.integer(substr(obsFiles, 13, 16)))
  taskFiles <- unique(unlist(lapply(
    file.path(modelPaths, "task"),
    list.files,
    pattern = "task\\d+\\.json",
    full.names = FALSE, recursive = FALSE)))
  taskNrs <- unique(as.integer(substr(taskFiles, 5, 6)))
  scoreFunctions <- taskFiles <- unique(unlist(lapply(
    file.path(modelPaths, "task"),
    \(path) {
      files <- list.files(path, pattern = "task\\d+\\.json", full.names = TRUE, recursive = FALSE)
      sapply(files, \(fl) sapply(
        jsonlite::read_json(fl)$scoreList$list,
        \(x) x$name))
    })))

  return(list(
    models = models,
    methods = methods,
    truthNrs = truthNrs,
    obsNrs = obsNrs,
    taskNrs = taskNrs,
    scoreFunctions = scoreFunctions
  ))
}

#' @export
getNew <- function(dbPath, example=FALSE) {
  # TODO: move to DEEBpath (package change for interact when calling DEEBeval::getNew(...))
  models <- list.dirs(path = dbPath, full.names = FALSE, recursive = FALSE)
  unevaled <- lapply(models, \(model) {
    path <- DEEBpath::getPaths(dbPath, model, example=example)
    methods <- list.dirs(path$esti, full.names = FALSE, recursive = FALSE)
    meta <- lapply(methods, \(method) {
      methodEstiPath <- file.path(path$esti, method)
      if (!dir.exists(methodEstiPath)) return(NULL)
      meta <- DEEBpath::getMetaGeneric(methodEstiPath)
      meta$method <- method
      meta$estiPath <- NULL
      return(meta)
    }) |>
      dplyr::bind_rows()
    scoreFiles <- DEEBpath::getScoreFiles(path$eval)
    scores <-
      lapply(scoreFiles, \(sf) {
        scores <- readr::read_csv(sf, col_types = readr::cols())
        scores[c("method", "truthNr", "obsNr", "taskNr")]
      }) |>
      dplyr::bind_rows()
    unevaled <- anti_join(meta, scores, by = c("truthNr", "obsNr", "taskNr", "method"))
    unevaled$model <- model
    unevaled
  }) |>
    dplyr::bind_rows()
  unevaled$example <- example
  return(unevaled)
}
