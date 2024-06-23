#' @export
generateBestHyperCube <- function(dbPath, methodTablePath=NULL, autoId=NULL) {

  bestHyperCubePaths <- getNextFreeBestHyperCubeNumber(dbPath, autoId)

  if (!dir.exists(bestHyperCubePaths$dirPath))
    dir.create(bestHyperCubePaths$dirPath, recursive=TRUE)

  if (hasValue(methodTablePath)) {
    methodTable <- readr::read_csv(methodTablePath, col_types=readr::cols())
  } else {
    methodTable <- NULL
  }
  bests <- getBests(dbPath, onlyHashed = TRUE, methodTable = methodTable)
  if (NROW(bests) == 0) {
    cat("Was not able to collect any best method.\n")
    return(NULL)
  }

  bests$bestCubePath <- NA_character_
  bests$bestProtoPath <- NA_character_
  bests$nr <- NA_real_
  for (i in seq_len(nrow(bests))) {
    info <- bests[i, ]
    filePath <- list.files(
      path = DEEBpath::hyperDir(dbPath),
      pattern = paste0("^", info$methodBase, "\\.json$"),
      full.names = TRUE)[1]
    if (is.na(filePath)) {
      cat("Did not find base file for", info$methodBase, "\n")
      next
    }

    optsProto <- ConfigOpts::readOptsBare(filePath)
    if (!ConfigOpts::hasGenerativeExpands(optsProto)) {
      cat(info$methodBase, "does not have generative elemets. Skipping.\n")
      next
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
    if (!found) stop(filePath, " did not matching prototype for ", optsBest$name)
    optsProto$list <- list(ConfigOpts::replaceExpandValues(optsProto$list[[iProto]], optsBest))

    outFile <- getFreeFilePath(bestHyperCubePaths$dirPath, info$methodBase, "json")

    optsProto$name <- info$methodBase
    ConfigOpts::writeOpts(
      optsProto,
      file = outFile$filePath,
      validate = FALSE)
    bests$bestCubePath[i] <- outFile$filePath
    bests$bestProtoPath[i] <- optsProto$name
    bests$nr[i] <- outFile$nr
  }

  methodCsv <-
    bests |>
    drop_na(bestCubePath, nr) |>
    mutate(methodFile = file.path(
      bestHyperCubePaths$dir,
      paste0(methodBase, "_", nr))
    ) |>
    group_by(model) |>
    mutate(obs = DEEBpath::getObsNameFromNr(dbPath, model, obsNr)) |>
    ungroup() |>
    select(model, methodFile, obs)

  write_csv(methodCsv, bestHyperCubePaths$csvFilePath, progress = FALSE)

  cat("Created:", bestHyperCubePaths$csvFilePath, "\n")
  return(invisible())
}


getFreeFilePath <- function(dirPath, fileName, ending) {
  for (nr in 1:1e5) {
    filePath <- file.path(dirPath, paste0(fileName, "_", nr, ".", ending))
    if (!file.exists(filePath)) break
  }
  stopifnot(nr < 1e5)
  return(lst(
    dirPath,
    fileName = paste0(fileName, "_", nr, ".", ending),
    filePath,
    nr))
}


getNextFreeBestHyperCubeNumber <- function(dbPath, autoId = NULL) {
  if (hasValue(autoId)) {
    basePath <- DEEBpath::autoIdDir(dbPath, autoId)
  } else {
    basePath <- DEEBpath::hyperDir(dbPath)
  }
  for (nr in 1:1e5) {
    csvFilePath <- file.path(basePath, paste0("methods_BestCube_", nr, ".csv"))
    dirName <- paste0("methods_BestCube_", nr)
    dirPath <- file.path(basePath, dirName)
    if (!file.exists(csvFilePath) && !dir.exists(dirPath)) break
  }
  stopifnot(nr < 1e5)
  if (hasValue(autoId)) {
    dir <- file.path(DEEBpath::autoIdDirRelativeToHyper(autoId), dirName)
  } else {
    dir <- dirName
  }
  return(lst(
    csvFilePath,
    dirPath,
    dir,
    nr))
}



getBestMethod <- function(dbPath, data, model, obsNr, methodBase = NULL) {

  methodData <-
    data |>
    filter(model == .env$model, obsNr == .env$obsNr)

  if (!is.null(methodBase)) {
    methodData <- methodData |> filter(methodBase == .env$methodBase)
  }

  if (NROW(methodData) == 0) return(NULL)

  targetInfo <- getTargetInfo(dbPath, model)

  bestMethod <-
    methodData |>
    filter(taskNr == targetInfo$taskNr, scoreName == targetInfo$scoreName) |>
    filter(scoreValue == targetInfo$fun(scoreValue))

  if (nrow(bestMethod) == 0) return(NA_character_)

  bestMethod <- bestMethod[1,]


  return(bestMethod$methodFull)
}


getTargetInfo <- function(dbPath, model) {
  if (length(model) == 0) return(NULL)
  targetMethodAndScore <- DEEBpath::getTargetTaskAndScore(dbPath)
  modelTarget <- targetMethodAndScore |> filter(str_detect(.env$model, .data$model))
  stopifnot(nrow(modelTarget) == 1)
  tnr <- modelTarget |> pull(taskNr)
  lss <- modelTarget |> pull(scoreName)
  targetFun <- switch(
    modelTarget |> pull(target),
    max = \(x) max(x, na.rm = TRUE),
    min = \(x) min(x, na.rm = TRUE))
  return(list(
    taskNr = tnr,
    scoreName = lss,
    fun = targetFun))
}


.getMethodOptsOne <- function(methodOpts, model, methodBase) {

  methodOptsOne <-
    filter(methodOpts, model == .env$model, method == .env$methodBase) |>
    mutate(opts = list(readr::read_csv(filePath, col_types = readr::cols()))) |>
    select(-c(model, method, fileName, filePath)) |>
    tidyr::unnest(opts)

  # MathJax can break $ if not escaped.
  names(methodOptsOne) <- str_replace_all(names(methodOptsOne), stringr::fixed("$"), "\\$")

  return(methodOptsOne)
}


