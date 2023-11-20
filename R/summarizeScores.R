#' @export
createSummary <- function(dbPath = ".", collectScores = TRUE, collectHyper = TRUE) {
  summaryDir <- DEEBpath::summaryDir(dbPath)
  if (collectScores) {
    scores <- collectScores(dbPath)
    if (!dir.exists(summaryDir)) dir.create(summaryDir)
    readr::write_csv(scores, DEEBpath::summaryTablePath(dbPath))
  }
  if (collectHyper) {
    hyperOpts <- collectHyper(dbPath)
    if (!dir.exists(summaryDir)) dir.create(summaryDir)
    for (i in seq_len(nrow(hyperOpts))) {
      info <- hyperOpts[i,]
      readr::write_csv(info$opts[[1]], DEEBpath::summaryHyperPath(dbPath, info$model, info$methodBase))
    }
  }
  writeDoc(
    summaryDir,
    "summary",
    dbPath = normalizePath(dbPath, mustWork=TRUE))
  writeDoc(
    summaryDir,
    "summaryHyper",
    dbPath = normalizePath(dbPath, mustWork=TRUE))
}

collectHyper <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  res <-
    lapply(
      models,
      \(model) {
        collectHyperOfModel(dbPath, model) |>
          dplyr::mutate(model = model, .before = 1)
      }
    )
  res <-
    res|>
    dplyr::bind_rows()
  return(res)
}


collectHyperOfModel <- function(dbPath, model) {
  paths <- DEEBpath::getPaths(dbPath, model)
  dirNames <- list.files(paths$esti, pattern = "^[^_]", include.dirs=TRUE)
  files <- tibble::tibble(
    dirName = dirNames[sapply(file.path(paths$esti, dirNames), dir.exists)],
    dirFull = normalizePath(file.path(paths$esti, dirName)),
    hyperFile = lapply(dirFull, \(dir) list.files(dir, pattern = "^Opts_HyperParms.*\\.json$")))
  files <-
    files |>
    dplyr::filter(sapply(hyperFile, \(fls) length(fls) == 1)) |>
    dplyr::mutate(
      hyperFile = sapply(hyperFile, \(fls) fls[[1]]),
      hasVariations = stringr::str_detect(dirName, "_[0-9a-f]{32}$")) |>
    dplyr::filter(hasVariations) |>
    dplyr::mutate(
      methodBase = stringr::str_sub(dirName, end=-34),
      hash = stringr::str_sub(dirName, start=-32))
  filesWithOpts <-
    files |>
    dplyr::summarize(
      opts = list(
        getDistinguishingOptsTable(file.path(dirFull, hyperFile), hash, removeNames="name")),
      .by = methodBase) |>
    mutate(
      methodBase = as.character(methodBase),
      opts = as.list(opts))
  return(filesWithOpts)
}

getDistinguishingOptsTable <- function(filePaths, ids, removeNames = NULL) {
  # TODO: need to extend to list of lists ...
  stopifnot(length(filePaths) == length(ids))
  if (length(filePaths) <= 1) return(tibble::tibble(id = ids))
  opts <- lapply(filePaths, ConfigOpts::readOptsBare)
  names <- lapply(opts, names) |> unlist() |> unique()
  names <- setdiff(names, removeNames)
  isIdentical <- sapply(names, \(nm) allSame(lapply(opts, \(opt) opt[[nm]])))
  distinguishingNames <- names[!isIdentical]
  res <- lapply(distinguishingNames, \(nm) sapply(opts, \(opt) opt[[nm]]))
  if (!"id" %in% distinguishingNames) {
    res <- c(list(ids), res)
    distinguishingNames <- c("id", distinguishingNames)
  }
  names(res) <- distinguishingNames
  return(tibble::as_tibble(res))
}

allSame <- function(lst) {
  reduced <- Reduce(\(x, y) if (identical(x,y)) x else NULL, lst)
  return(!is.null(reduced))
}



collectScores <- function(dbPath, aggregationFunction = mean) {
  models <- DEEBpath::getModels(dbPath)
  res <-
    lapply(
      models,
      \(model) {
        collectScoresOfModel(dbPath, model, aggregationFunction = mean) |>
          dplyr::mutate(model = model, .before = 1)
      }
    ) |>
    dplyr::bind_rows()
  return(res)
}


collectScoresOfModel <- function(dbPath, model, aggregationFunction = mean) {
  paths <- DEEBpath::getPaths(dbPath, model)
  scoreTablePattern <- "^task(\\d{2})(.+)_eval\\.csv$"
  fileNames <- list.files(paths$eval, pattern = scoreTablePattern)
  scores <- lapply(
    fileNames,
    \(flnm) {
        readr::read_csv(file.path(paths$eval, flnm), col_types=readr::cols()) |>
        dplyr::summarise(
          across(-truthNr, aggregationFunction),
          .by = c(method, obsNr, taskNr))
    }
  ) |>
    dplyr::bind_rows()
  tbl <-
    scores |>
    dplyr::mutate(
      hasVariations = stringr::str_detect(method, "_[0-9a-f]{32}$"),
      methodFull = method,
      methodBase = ifelse(hasVariations, stringr::str_sub(method, end=-34), method),
      hash = ifelse(hasVariations, stringr::str_sub(method, start=-32), NA),
      method = NULL,
      hasVariations = NULL
    ) |>
    dplyr::relocate(
      methodFull, methodBase, hash, obsNr, taskNr
    ) |>
    tidyr::pivot_longer(
      -c(methodFull, methodBase, hash, obsNr, taskNr),
      names_to = "scoreName",
      values_to = "scoreValue")
  return(tbl)
}
