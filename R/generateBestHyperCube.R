#' @export
generateBestHyperCube <- function(dbPath, methodTablePath=NULL, autoId=NULL, cubeId=NULL) {

  if (hasValue(methodTablePath)) {
    methodTable <- readr::read_csv(methodTablePath, col_types=readr::cols())
  } else {
    methodTable <- NULL
  }
  bests <- getBests(dbPath, onlyHashed = TRUE, methodTable = methodTable, autoId = autoId)
  if (NROW(bests) == 0) {
    stop("Was not able to collect any best method.\n")
  }

  bestHyperCubePaths <- getBestHyperCubePaths(dbPath, autoId, bests, cubeId=cubeId)

  bests$methodFilePath <- NA_character_
  bests$methodFileRelPath <- NA_character_
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

    optsProto$name <- info$methodBase
    outFileName <- DEEButil::getUniqueFileName(
      dirPath = bestHyperCubePaths$dirPath,
      prefix = optsProto$name,
      fileExtension = "",
      identifyingObject = optsProto,
      fullPath = FALSE)
    outFilePath <- file.path(bestHyperCubePaths$dirPath, paste0(outFileName, ".json"))
    ConfigOpts::writeOpts(
      optsProto,
      file = outFilePath,
      validate = FALSE)
    bests$methodFilePath[i] <- outFilePath
    bests$methodFileRelPath[i] <- file.path(bestHyperCubePaths$dirPathRel, outFileName)
  }

  methodCsv <-
    bests |>
    drop_na(methodFilePath) |>
    group_by(model) |>
    mutate(obs = DEEBpath::getObsNameFromNr(dbPath, model, obsNr)) |>
    ungroup() |>
    rename(methodFile = methodFileRelPath) |>
    select(model, methodFile, obs)

  write_csv(methodCsv, bestHyperCubePaths$csvFilePath, progress = FALSE)

  cat("Created:", bestHyperCubePaths$csvFilePath, "\n")
  return(invisible())
}


getBestHyperCubePaths <- function(dbPath, autoId, identifyingObject, cubeId=NULL) {
  if (hasValue(autoId)) {
    basePath <- DEEBpath::autoIdDir(dbPath, autoId)
    basePathRel <- file.path("auto", autoId)
  } else {
    basePath <- file.path(DEEBpath::hyperDir(dbPath), "BestCube")
    basePathRel <- "BestCube"
  }
  bestHyperCubeDirName <- DEEButil::getUniqueFileName(
    dirPath = basePath,
    prefix = paste0(c("BestCube", cubeId), collapse="_"),
    fileExtension = "",
    identifyingObject = identifyingObject,
    fullPath = FALSE)
  bestHyperCubeDirPath <- file.path(basePath, bestHyperCubeDirName)
  if (!dir.exists(bestHyperCubeDirPath)) dir.create(bestHyperCubeDirPath, recursive=TRUE)
  bestHyperCubeCsvFilePath <- paste0(bestHyperCubeDirPath, ".csv")
  list(
    dirPath = bestHyperCubeDirPath,
    dirPathRel = file.path(basePathRel, bestHyperCubeDirName),
    csvFilePath = bestHyperCubeCsvFilePath)
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
    filter(scoreMean == targetInfo$fun(scoreMean))

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


