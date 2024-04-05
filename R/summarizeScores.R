#' @export
createSummary <- function(dbPath = ".", collectScores = TRUE, collectHyper = TRUE) {

  cat("Writing Summary...")
  pt <- proc.time()

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

  cat(" done after", format((proc.time()-pt)[3]), "s\n")
}

collectHyper <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  res <-
    lapply(
      models,
      \(model) {
        collectHyperOfModel(dbPath, model) |>
          mutate(model = model, .before = 1)
      }
    )
  res <-
    res|>
    bind_rows()
  return(res)
}


collectHyperOfModel <- function(dbPath, model) {
  paths <- DEEBpath::getPaths(dbPath, model)
  dirNames <- list.files(paths$esti, pattern = "^[^_]", include.dirs=TRUE)
  files <- tibble(
    dirName = dirNames[sapply(file.path(paths$esti, dirNames), dir.exists)],
    dirFull = normalizePath(file.path(paths$esti, dirName)),
    hyperFile = lapply(dirFull, \(dir) list.files(dir, pattern = "^Opts_HyperParms.*\\.json$")))
  files <-
    files |>
    filter(sapply(hyperFile, \(fls) length(fls) == 1)) |>
    mutate(
      hyperFile = sapply(hyperFile, \(fls) fls[[1]]),
      hasVariations = str_detect(dirName, "_[0-9a-f]{32}$")) |>
    filter(hasVariations) |>
    mutate(
      methodBase = str_sub(dirName, end=-34),
      hash = str_sub(dirName, start=-32))
  filesWithOpts <-
    files |>
    summarize(
      opts = list(
        getDistinguishingOptsTable(file.path(dirFull, hyperFile), hash, removeNames="name")),
      .by = methodBase) |>
    mutate(
      methodBase = as.character(methodBase),
      opts = as.list(opts))
  return(filesWithOpts)
}

getDistinguishingOptsTable <- function(filePaths, ids, removeNames = NULL) {
  stopifnot(length(filePaths) == length(ids))
  if (length(filePaths) <= 1) return(tibble(id = ids))
  opts <- lapply(filePaths, ConfigOpts::readOptsBare)
  names <- lapply(opts, nestedNames) |> unlist() |> unique()
  names <- setdiff(names, removeNames)
  isIdentical <- sapply(names, \(nm) allSame(lapply(opts, selectNestedName, nestedName = nm)))
  distinguishingNames <- names[!isIdentical]
  res <- lapply(
    distinguishingNames,
    \(nm) sapply(opts, \(o) asString(selectNestedName(o, nm)))) # TODO: It seems quite hacky to distinguish objects by their string representation...
  if (!"id" %in% distinguishingNames) {
    res <- c(list(ids), res)
    distinguishingNames <- c("id", distinguishingNames)
  }
  names(res) <- distinguishingNames
  return(as_tibble(res))
}

nestedNames <- function(lst, prefix = "") {
  nms <- names(lst)
  nmLst <- lapply(nms, \(nm) {
    pre <- paste0(prefix, nm)
    if (is.null(names(lst[[nm]]))) pre else nestedNames(lst[[nm]], paste0(pre, "$"))
  })
  return(unlist(nmLst))
}

selectNestedName <- function(lst, nestedName) {
  nameLst <- stringr::str_split_1(nestedName, pattern="\\$")
  return(lst[[nameLst]])
}

asString <- function(x) {
  if (is.null(x)) return("")
  utils::capture.output(dput(x, control = c("keepNA", "niceNames", "showAttributes")))
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
          mutate(model = model, .before = 1)
      }
    ) |>
    bind_rows()
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
        summarise(
          across(-truthNr, aggregationFunction),
          .by = c(method, obsNr, taskNr))
    }
  ) |>
    bind_rows()
  tbl <-
    scores |>
    mutate(
      hasVariations = stringr::str_detect(method, "_[0-9a-f]{32}$"),
      methodFull = method,
      methodBase = ifelse(hasVariations, stringr::str_sub(method, end=-34), method),
      hash = ifelse(hasVariations, stringr::str_sub(method, start=-32), NA),
      method = NULL,
      hasVariations = NULL
    ) |>
    relocate(
      methodFull, methodBase, hash, obsNr, taskNr
    ) |>
    pivot_longer(
      -c(methodFull, methodBase, hash, obsNr, taskNr),
      names_to = "scoreName",
      values_to = "scoreValue")
  return(tbl)
}
