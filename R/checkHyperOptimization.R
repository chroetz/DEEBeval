#' @export
getStateOfHyperParmOptimization <- function(dbPath, methodTable) {
  bestHyperCubePaths <- getNextFreeBestHyperCubeNumber(dbPath)
  if (!dir.exists(bestHyperCubePaths$dirPath)) dir.create(bestHyperCubePaths$dirPath, recursive=TRUE)
  resList <- lapply(seq_len(nrow(methodTable)), \(i) getStateOfHyperParmOptimizationOne(dbPath, methodTable[i, ], bestHyperCubePaths$dirPath))
  return(resList)
}


#' @export
getStateOfHyperParmOptimizationHasScore <- function(dbPath, methodTable, outDir) {
  bestHyperCubePaths <- getNextFreeBestHyperCubeNumber(dbPath)
  if (!dir.exists(bestHyperCubePaths$dirPath)) dir.create(bestHyperCubePaths$dirPath, recursive=TRUE)
  resList <- lapply(seq_len(nrow(methodTable)), \(i) getStateOfHyperParmOptimizationHasScoreOne(dbPath, methodTable[i, ], bestHyperCubePaths$dirPath))
  res <- bind_rows(resList)
  if (!dir.exists(outDir)) dir.create(outDir, showWarnings=TRUE, recursive=TRUE)
  outFilePath <- tempfile("checkScores_", tmpdir=outDir, fileext=".csv")
  write_csv(res, outFilePath)
  return(invisible())
}


getStateOfHyperParmOptimizationOne <- function(dbPath, methodInfo, bestHyperCubeDirPath) {
  initCheck <- checkMethodFileState(dbPath, methodInfo$methodBaseFile, methodInfo$model, methodInfo$obs)
  optimCheck <- checkOptimizationState(dbPath, methodInfo, bestHyperCubeDirPath)
  return(
    c(
      methodInfo,
      lst(
        initCheck,
        optimCheck
      )))
}


getStateOfHyperParmOptimizationHasScoreOne <- function(dbPath, methodInfo, bestHyperCubeDirPath) {
  initCheck <- checkMethodFileStateHasScore(dbPath, methodInfo$methodBaseFile, methodInfo$model, methodInfo$obs)
  optimCheck <- checkOptimizationStateHasScore(dbPath, methodInfo, bestHyperCubeDirPath)
  return(
    c(
      methodInfo,
      lst(
        initCheckMissing = if (is.null(initCheck)) NA_integer_ else sum(!initCheck),
        initCheckTotal = if (is.null(initCheck)) NA_integer_ else length(initCheck),
        optimCheckMissing = if (is.null(optimCheck)) NA_integer_ else sum(!optimCheck),
        optimCheckTotal = if (is.null(optimCheck)) NA_integer_ else length(optimCheck)
      )))
}


checkMethodFileState <- function(dbPath, methodFilePath, model, obsName) {
  obsNr <- DEEBpath::getObsNrFromName(dbPath, model, obsName)
  paths <- DEEBpath::getPaths(dbPath, model)
  hyperParmsList <- DEEBesti::loadAsHyperParmsList(dbPath, methodFilePath)
  hyperParmsList$list |>
    lapply(checkStateOfHyperParms, dbPath=dbPath, paths=paths, obsNr=obsNr, model=model) |>
    bind_rows()
}


checkMethodFileStateHasScore <- function(dbPath, methodFilePath, model, obsName) {
  obsNr <- DEEBpath::getObsNrFromName(dbPath, model, obsName)
  scoresCsvFilePath <- DEEBpath::summaryTablePath(dbPath)
  if (file.exists(scoresCsvFilePath)) {
    scores <- read_csv(scoresCsvFilePath, col_types=readr::cols())
    hyperParmsList <- DEEBesti::loadAsHyperParmsList(dbPath, methodFilePath)
    res <- hyperParmsList$list |> sapply(checkStateOfHyperParmsHasScore, scores=scores, obsNr=obsNr, model=model)
  } else {
    res <- rep(FALSE, length(hyperParmsList$list))
  }
  return(res)
}


checkStateOfHyperParms <- function(dbPath, hyperParms, paths, obsNr, model) {
  estiPattern <- "^truth(\\d{4})obs(\\d{4})task(\\d{2})esti\\.csv$"
  estiDirPath <- file.path(paths$esti, hyperParms$name)
  estiDirExists <- dir.exists(estiDirPath)
  if (estiDirExists) {
    estiFiles <- list.files(estiDirPath, pattern=estiPattern)
    mat <- stringr::str_match(estiFiles, estiPattern)
    colnames(mat) <- c("fileName", "truthNr", "obsNr", "taskNr")
    estiFileTable <- mat |>
      tibble::as_tibble() |>
      mutate(
        truthNr = as.integer(.data$truthNr),
        obsNr = as.integer(.data$obsNr),
        taskNr = as.integer(.data$taskNr))
  } else {
    estiFileTable <- tibble(
      fileName = character(),
      truthNr = integer(),
      obsNr = integer(),
      taskNr = integer())
  }
  evalPattern <- paste0("^task(\\d{2})", hyperParms$name, "_eval\\.csv$")
  evalDirExists <- dir.exists(paths$eval)
  if (evalDirExists) {
    evalFiles <- list.files(paths$eval, pattern=evalPattern)
    mat <- stringr::str_match(evalFiles, evalPattern)
    colnames(mat) <- c("fileName", "taskNr")
    evalFileTable <- mat |>
      tibble::as_tibble() |>
      mutate(taskNr = as.integer(.data$taskNr))
  } else {
    evalFileTable <- tibble(
      fileName = character(),
      taskNr = integer())
  }
  if (NROW(evalFileTable) > 0) {
    scoresEvalDir <- lapply(evalFileTable$fileName, \(fn) readr::read_csv(file.path(paths$eval, fn), col_types = readr::cols())) |>
      bind_rows() |>
      pivot_longer(-c("method", "truthNr", "obsNr", "taskNr"), names_to="scoreName", values_to="score_value")
  } else {
    scoresEvalDir <- NULL
  }

  scoresCsvFilePath <- DEEBpath::summaryTablePath(dbPath)
  if (file.exists(scoresCsvFilePath)) {
    scoreSummaryDir <-
      scoresCsvFilePath |>
      readr::read_csv(col_types = readr::cols()) |>
      filter(methodFull == hyperParms$name, model == .env$model, obsNr == .env$obsNr)
  } else {
    scoreSummaryDir <- NULL
  }
  return(
    lst(
      estiDirExists,
      estiFileTable = list(estiFileTable),
      evalDirExists,
      evalFileTable = list(evalFileTable),
      scoresEvalDir = list(scoresEvalDir),
      scoreSummaryDir = list(scoreSummaryDir)))
}



checkStateOfHyperParmsHasScore <- function(scores, hyperParms, obsNr, model) {
  scoreSummaryDir <-
    scores |>
    filter(methodFull == hyperParms$name, model == .env$model, obsNr == .env$obsNr)
  return(NROW(scoreSummaryDir) > 0)
}



checkOptimizationState <- function(dbPath, methodInfo, bestHyperCubeDirPath) {
  filePath <- list.files(
    path = DEEBpath::hyperDir(dbPath),
    pattern = paste0("^", methodInfo$methodBaseFile, "\\.json$"),
    full.names = TRUE)[1]
  if (is.na(filePath)) {
    cat("Did not find base file for", methodInfo$methodBaseFile, "\n")
    return(NULL)
  }
  optsProto <- ConfigOpts::readOptsBare(filePath)
  isGenerative <- ConfigOpts::hasGenerativeExpands(optsProto)
  if (!isGenerative) {
    return(NULL)
  }
  info <- getBests(dbPath, onlyHashed = TRUE, methodTable = methodInfo)
  if (NROW(info) != 1) {
    warning("Was not able to collect specific best method.\n", immediate.=TRUE)
    return(NULL)
  }
  optsBest <- ConfigOpts::readOpts(info$filePath)
  found <- FALSE
  for (iProto in seq_along(optsProto$list)) {
    proto <- ConfigOpts::asOpts(optsProto$list[[iProto]])
    if (ConfigOpts::isOfPrototype(optsBest, proto)) {
      found <- TRUE
      break
    }
  }
  if (!found) stop(filePath, " did not match prototype for ", optsBest$name)
  optsProto$list <- list(ConfigOpts::replaceExpandValues(optsProto$list[[iProto]], optsBest))
  optsProto$name <- info$methodBase
  outFile <- getFreeFilePath(bestHyperCubeDirPath, info$methodBase, "json")
  ConfigOpts::writeOpts(
    optsProto,
    file = outFile$filePath,
    validate = FALSE)
  optiState <- checkMethodFileState(dbPath, file.path(basename(dirname(outFile$methodFile)), basename(outFile$methodFile)), methodInfo$model, methodInfo$obs)
  return(optiState)
}


checkOptimizationStateHasScore <- function(dbPath, methodInfo, bestHyperCubeDirPath) {
  filePath <- list.files(
    path = DEEBpath::hyperDir(dbPath),
    pattern = paste0("^", methodInfo$methodBaseFile, "\\.json$"),
    full.names = TRUE)[1]
  if (is.na(filePath)) {
    cat("Did not find base file for", methodInfo$methodBaseFile, "\n")
    return(NULL)
  }
  optsProto <- ConfigOpts::readOptsBare(filePath)
  isGenerative <- ConfigOpts::hasGenerativeExpands(optsProto)
  if (!isGenerative) {
    return(NULL)
  }
  info <- getBests(dbPath, onlyHashed = TRUE, methodTable = methodInfo)
  if (NROW(info) != 1) {
    warning("Was not able to collect specific best method.\n", immediate.=TRUE)
    return(NULL)
  }
  optsBest <- ConfigOpts::readOpts(info$filePath)
  found <- FALSE
  for (iProto in seq_along(optsProto$list)) {
    proto <- ConfigOpts::asOpts(optsProto$list[[iProto]])
    if (ConfigOpts::isOfPrototype(optsBest, proto)) {
      found <- TRUE
      break
    }
  }
  if (!found) stop(filePath, " did not match prototype for ", optsBest$name)
  optsProto$list <- list(ConfigOpts::replaceExpandValues(optsProto$list[[iProto]], optsBest))
  optsProto$name <- info$methodBase
  outFile <- getFreeFilePath(bestHyperCubeDirPath, info$methodBase, "json")
  ConfigOpts::writeOpts(
    optsProto,
    file = outFile$filePath,
    validate = FALSE)
  optiState <- checkMethodFileStateHasScore(dbPath, file.path(basename(dirname(outFile$methodFile)), basename(outFile$methodFile)), methodInfo$model, methodInfo$obs)
  return(optiState)
}

