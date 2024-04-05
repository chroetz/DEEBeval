#' @export
generateBestHyperCube <- function(dbPath, timeInMinutes = 60) {

  bestHyperCubePaths <- getNextFreeBestHyperCubeNumber(dbPath)

  data <-
    read_csv(DEEBpath::summaryTablePath(dbPath), col_types = readr::cols()) |>
    filter(!is.na(hash))

  meta <-
    data |>
    select(model, obsNr, methodBase) |>
    distinct() |>
    drop_na()

  res <-
    meta |>
    rowwise() |>
    mutate(bestMethod = getBestMethod(dbPath, data, model, obsNr, methodBase))
  bests <-
    res |>
    drop_na() |>
    mutate(filePath = DEEBpath::getRanMethodOptsPath(dbPath, model, bestMethod))

  bests$bestCubePath <- NA_character_
  bests$nr <- NA_real_
  for (i in seq_len(nrow(bests))) {
    info <- bests[i, ]
    filePath <- list.files(
      path = file.path(dbPath, "_hyper"),
      pattern = paste0("^", info$methodBase),
      full.names = TRUE)[1]
    if (is.na(filePath)) {
      cat("Did not find base file for", info$methodBase, "\n")
      next
    }

    optsProto <- ConfigOpts::readOptsBare(filePath)
    optsProto$list <- optsProto$list[1]
    optsBest <- ConfigOpts::readOpts(info$filePath)
    optsProto$list[[1]] <- replaceExpandValues(optsProto$list[[1]], optsBest)

    outFile <- getFreeFilePath(
      bestHyperCubePaths$dirPath,
      paste0(info$methodBase, "_BestCube"),
      "json")

    ConfigOpts::writeOpts(
      optsProto,
      file = outFile$filePath,
      validate = FALSE)
    bests$bestCubePath[i] <- outFile$filePath
    bests$nr[i] <- outFile$nr
  }

  methodCsv <-
    bests |>
    drop_na(bestCubePath, nr) |>
    mutate(method = file.path(
      bestHyperCubePaths$dir,
      paste0(methodBase, "_BestCube_", nr))
    ) |>
    group_by(model) |>
    mutate(obs = DEEBpath::getObsNameFromNr(dbPath, model, obsNr)) |>
    ungroup() |>
    select(model, method, obs) |>
    mutate(timeInMinutes = .env$timeInMinutes)

  write_csv(methodCsv, bestHyperCubePaths$csvFilePath)

  cat("Created:", outFile$filePath, "\n")
  return(invisible())
}


getFreeFilePath <- function(dirPath, fileName, ending) {
  for (nr in 1:1e4) {
    filePath <- file.path(dirPath, paste0(fileName, "_", nr, ".", ending))
    if (!file.exists(filePath)) break
  }
  stopifnot(nr < 1e4)
  return(lst(
    dirPath,
    fileName = paste0(fileName, "_", nr, ".", ending),
    filePath,
    nr))
}


getNextFreeBestHyperCubeNumber <- function(dbPath) {
   basePath <- file.path(dbPath, "_hyper")
   for (nr in 1:1e4) {
    csvFilePath <- file.path(basePath, paste0("methods_BestCube_", nr, ".csv"))
    dirPath <- file.path(basePath, paste0("methods_BestCube_", nr))
    if (!file.exists(csvFilePath) && !dir.exists(dirPath)) break
  }
  stopifnot(nr < 1e4)
  return(lst(
    csvFilePath,
    dirPath,
    dir = basename(dirPath),
    nr))
}



getBestMethod <- function(dbPath, data, model, obsNr, methodBase = NULL) {

  methodData <-
    data |>
    filter(model == .env$model, obsNr == .env$obsNr)

  if (!is.null(methodBase)) {
    methodData <- methodData |> filter(methodBase == .env$methodBase)
  }

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
  targetMethodAndScore <- DEEBpath::getTargetTaskAndScore(dbPath)
  tnr <- targetMethodAndScore |> filter(model == .env$model) |> pull(taskNr)
  lss <- targetMethodAndScore |> filter(model == .env$model) |> pull(scoreName)
  targetFun <- switch(
    targetMethodAndScore |> filter(model == .env$model) |> pull(target),
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


replaceExpandValues <- function(proto, replacement) {
  if (inherits(proto, "expansion")) {
    if (!"generate" %in% names(proto)) return(proto)
    proto$values <- generateExpansionValues(replacement, proto$generate)
    return(proto)
  }
  if (is.list(proto)) {
    stopifnot(is.list(replacement))
    if (!is.null(names(proto))) {
      stopifnot(all(names(proto) %in% names(replacement)))
      for (nm in names(proto)) {
        proto[[nm]] <- replaceExpandValues(proto[[nm]], replacement[[nm]])
      }
    } else {
      stopifnot(length(proto) == length(replacement))
      for (i in seq_along(proto)) {
        proto[[i]] <- replaceExpandValues(proto[[i]], replacement[[i]])
      }
    }
    return(proto)
  }
  return(proto)
}


generateExpansionValues <- function(value, generateInfo) {
  out <- switch(
    generateInfo$kind,
    multiply = generateExpansionValuesMultiply(value, generateInfo$value),
    add = generateExpansionValuesAdd(value, generateInfo$value),
    stop("Unknown generate kind ", kind))
  out <- out[out >= generateInfo$min & out <= generateInfo$max]
  return(out)
}

generateExpansionValuesMultiply <- function(value, factor) {
  c(value / factor, value, value * factor)
}

generateExpansionValuesAdd <- function(value, summand) {
  c(value - summand, value, value + summand)
}
