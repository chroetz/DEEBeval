#' @export
createOverall <- function(dbPath = ".") {
  ptThis <- proc.time()
  cat("Render Hyper...\n")
  writeDoc(
    DEEBpath::summaryDir(dbPath),
    "overall",
    dbPath = normalizePath(dbPath, mustWork=TRUE))
  cat("... took", (proc.time()-ptThis)[3], "s\n")
}


#' @export
createSummary <- function(dbPath = ".", collectScores = TRUE, collectHyper = TRUE, renderSummary = TRUE, renderHyper = TRUE) {

  cat("Writing Summary...")
  pt <- proc.time()

  summaryDir <- DEEBpath::summaryDir(dbPath)
  if (collectScores) {
    ptThis <- proc.time()
    cat("Collect Scores...")
    scores <- collectScores(dbPath)
    if (!dir.exists(summaryDir)) dir.create(summaryDir)
    write_csv(scores, DEEBpath::summaryTablePath(dbPath), progress = FALSE)
    cat("took", (proc.time()-ptThis)[3], "s\n")
  }
  if (collectHyper) {
    ptThis <- proc.time()
    cat("Collect Hyper...")
    hyperOpts <- collectHyper(dbPath)
    if (!dir.exists(summaryDir)) dir.create(summaryDir)
    for (i in seq_len(nrow(hyperOpts))) {
      info <- hyperOpts[i,]
      write_csv(info$opts[[1]], DEEBpath::summaryHyperPath(dbPath, info$model, info$methodBase), progress = FALSE)
    }
    cat("took", (proc.time()-ptThis)[3], "s\n")
  }
  if (renderSummary) {
    ptThis <- proc.time()
    cat("Render Summary...\n")
    writeDoc(
      summaryDir,
      "summary",
      dbPath = normalizePath(dbPath, mustWork=TRUE))
    cat("... took", (proc.time()-ptThis)[3], "s\n")
  }
  if (renderHyper) {
    ptThis <- proc.time()
    cat("Render Hyper...\n")
    writeDoc(
      summaryDir,
      "summaryHyper",
      dbPath = normalizePath(dbPath, mustWork=TRUE))
    cat("... took", (proc.time()-ptThis)[3], "s\n")
  }

  cat(" done after", format((proc.time()-pt)[3]), "s\n")
}


#' @export
collectAutoScores <- function(dbPath, tblModelMethod, autoId) {

  cat("collect scores auto...\n")
  pt <- proc.time()

  summaryDir <- DEEBpath::summaryDir(dbPath)

  scores <- collectScores(dbPath, tblModelMethod = tblModelMethod)
  if (!dir.exists(summaryDir)) dir.create(summaryDir)
  if (file.exists(DEEBpath::summaryTablePath(dbPath, autoId))) {
    scoresOld <- readr::read_csv(DEEBpath::summaryTablePath(dbPath, autoId), col_types = readr::cols())
  } else {
    scoresOld <- NULL
  }
  data <- bind_rows(scoresOld, scores) |> distinct()
  outFilePath <- DEEBpath::summaryTablePath(dbPath, autoId)
  cat("Writing", nrow(data), "lines to", outFilePath, "\n")
  write_csv(
    data,
    outFilePath,
    progress = FALSE)
  cat("All done after", format((proc.time()-pt)[3]), "s\n")
}


collectHyper <- function(dbPath) {
  models <- DEEBpath::getModels(dbPath)
  res <-
    lapply(
      models,
      \(model) {
        hyper <- collectHyperOfModel(dbPath, model)
        if (is.null(hyper)) return(NULL)
        hyper |>
          mutate(model = model, .before = 1)
      }
    ) |>
    bind_rows()
  return(res)
}


collectHyperOfModel <- function(dbPath, model) {
  paths <- DEEBpath::getPaths(dbPath, model)
  dirNames <- list.files(paths$esti, pattern = "^[^_]", include.dirs=TRUE)
  if (length(dirNames) == 0) return(NULL)
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
  opts <- lapply(filePaths, ConfigOpts::readOpts)
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
  res <- tryCatch(
    lst[[nameLst]],
    error = \(cond) {
      stop("selectNestedName: ", paste0(nameLst, collapse=","), ". names(lst):", paste0(names(lst), collapse=","))
    })
  return(res)
}

asString <- function(x) {
  if (is.null(x)) return("")
  utils::capture.output(dput(x, control = c("keepNA", "niceNames", "showAttributes")))
}

allSame <- function(lst) {
  x <- lst[[1]]
  for (y in lst[-1]) {
    if (!identical(x, y)) return(FALSE)
  }
  return(TRUE)
}



collectScores <- function(dbPath, tblModelMethod = NULL) {
  if (hasValue(tblModelMethod)) {
    models <- tblModelMethod$model |> unique()
  } else {
    models <- DEEBpath::getModels(dbPath)
  }
  res <-
    lapply(
      models,
      \(model) {
        scores <- collectScoresOfModel(dbPath, model, tblModelMethod = tblModelMethod)
        if (is.null(scores)) return(NULL)
        scores |>
          mutate(model = model, .before = 1)
      }
    ) |>
    bind_rows()
  return(res)
}


collectScoresOfModel <- function(dbPath, model, tblModelMethod = NULL) {
  paths <- DEEBpath::getPaths(dbPath, model)
  scoreTablePattern <- "^task(\\d{2})(.+)_eval\\.csv$" # TODO: move to DEEBpath
  fileNames <- list.files(paths$eval, pattern = scoreTablePattern)
  if (hasValue(tblModelMethod)) {
    methodNames <- str_extract(fileNames, scoreTablePattern, group = 2)
    methods <- tblModelMethod |> filter(.data$model == .env$model) |> pull(method) |> unique()
    fileNames <- fileNames[methodNames %in% methods]
  }
  scoresPlain <- lapply(
    fileNames,
    \(flnm) readr::read_csv(file.path(paths$eval, flnm), col_types=readr::cols())
  ) |>
    bind_rows()
  scoresAgg <-
    scoresPlain |>
    pivot_longer(
      -c(method, truthNr, obsNr, taskNr),
      names_to = "scoreName",
      values_to = "scoreValue") |>
    summarize(
      scoreMean = mean(scoreValue),
      scoreMeanNoNa = mean(scoreValue, na.rm=TRUE),
      nTruths = dplyr::n(),
      nNA = sum(is.na(scoreValue)),
      .by = -c(scoreValue, truthNr))
  if (NROW(scoresAgg) == 0) return(NULL)
  tbl <-
    scoresAgg |>
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
    )
  return(tbl)
}
