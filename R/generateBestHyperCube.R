#' @export
generateBestHyperCube <- function(dbPath, timeInMinutes = 60) {

  data <-
    read_csv(DEEBpath::summaryTablePath(dbPath), col_types = readr::cols()) |>
    filter(!is.na(hash))

  methodOpts <- getMethodOpts(dbPath)

  res <-
    data |>
    select(model, obsNr, methodBase) |>
    distinct() |>
    rowwise() |>
    mutate(
      paramCube = list(getParamCube(data, methodOpts, model, obsNr, methodBase)),
      length = length(paramCube)
    )
  res <-
    res |>
    ungroup() |>
    mutate(nJobs = sapply(paramCube, getNumberOfJobs))

  cat("Best Cube: Up to ", sum(res$nJobs, na.rm=TRUE), "Jobs.\n")

  res$bestCubePath <- NA_character_
  res$nr <- NA_real_
  for (i in seq_len(nrow(res))) {
    info <- res[i, ]
    if (info$length == 0) next
    filePath <- list.files(
      path = file.path(dbPath, "_hyper"),
      pattern = paste0("^", info$methodBase),
      full.names = TRUE)[1]
    if (is.na(filePath)) next
    opts <- ConfigOpts::readOptsBare(filePath)
    opts$list <- opts$list[1]
    allGood <- TRUE
    for (nm in names(info$paramCube[[1]])) {
      if (!inherits(opts$list[[1]][[nm]], "expansion")) {
        allGood <- FALSE
        break
      }
      params <- info$paramCube[[1]][[nm]]
      opts$list[[1]][[nm]]$values <- params
    }
    if (!allGood) next

    outFile <- getFreeFilePath(
      file.path(dbPath, "_hyper"),
      paste0(info$methodBase, "_BestCube"),
      "json")

    ConfigOpts::writeOpts(
      opts,
      file = outFile$filePath,
      validate = FALSE)
    res$bestCubePath[i] <- outFile$filePath
    res$nr[i] <- outFile$nr
  }

  methodCsv <-
    res |>
    drop_na(bestCubePath, nr) |>
    mutate(method = paste0(methodBase, "_BestCube_", nr)) |>
    group_by(model) |>
    mutate(obs = DEEBpath::getObsNameFromNr(dbPath, model, obsNr)) |>
    ungroup() |>
    select(model, method, obs) |>
    mutate(timeInMinutes = .env$timeInMinutes)

  outFile <- getFreeFilePath(
    file.path(dbPath, "_hyper"),
    "methods_BestCube",
    "csv")
  write_csv(methodCsv, outFile$filePath)

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


getParamCube <- function(data, methodOpts, model, obsNr, methodBase) {

  best <- getBestMethod(data, methodOpts, model, obsNr, methodBase)
  if (is.null(best)) return(NULL)

  paramNames <- names(best) |> setdiff("id") |> setdiff(names(data))

  paramList <- lapply(
    paramNames,
    \(name) {
      v <- sort(unique(methodOptsOne[[name]]))
      if (!is.numeric(v)) {
        kind <- "unclear"
      } else {
        if (max(diff(diff(v/mean(abs(v))))) < sqrt(.Machine$double.eps)) {
          kind <- "additive"
          delta <- abs(mean(diff(v)))
        } else if (min(v) > 0 && max(diff(diff(log(v)))) < sqrt(.Machine$double.eps)) {
          kind <- "multiplicative"
          delta <- exp(abs(mean(diff(log(v)))))
        } else {
          kind <- "unclear"
        }
      }
      v0 <- best[[name]]
      params <- switch(
        kind,
        additive = c(v0 - delta, v0, v0 + delta),
        multiplicative = c(v0 / delta, v0, v0 * delta),
        unclear = v0)
      if (is.numeric(params)) params <- as.double(params)
      return(params)
    }
  )
  names(paramList) <- paramNames
  return(paramList)
}

getNumberOfJobs <- \(paramList) {
  x <- sapply(paramList, length)
  if (is.numeric(x)) return(prod(x))
  return(NA_real_)
}

getBestMethod <- function(data, methodOpts, model, obsNr, methodBase) {

  targetMethodAndScore <- DEEBpath::getTargetMethodAndScore(dbPath)

  methodData <-
    data |>
    filter(model == .env$model, methodBase == .env$methodBase, obsNr == .env$obsNr)

  methodOptsOne <-
    filter(methodOpts, model == .env$model, method == .env$methodBase) |>
    mutate(opts = list(readr::read_csv(filePath, col_types = readr::cols()))) |>
    select(-c(model, method, fileName, filePath)) |>
    tidyr::unnest(opts)

  # MathJax can break $ if not escaped.
  names(methodOptsOne) <- str_replace_all(names(methodOptsOne), stringr::fixed("$"), "\\$")

  tnr <- targetMethodAndScore |> filter(model == .env$model) |> pull(taskNr)
  lss <- targetMethodAndScore |> filter(model == .env$model) |> pull(scoreName)
  targetFun <- switch(
    targetMethodAndScore |> filter(model == .env$model) |> pull(target),
    max = \(x) max(x, na.rm = TRUE),
    min = \(x) min(x, na.rm = TRUE))

  bestMethod <-
    methodData |>
    filter(taskNr == tnr, scoreName == lss) |>
    filter(scoreValue == targetFun(scoreValue))

  if (nrow(bestMethod) == 0) return(NULL)

  bestMethod <- bestMethod[1,]
  best <- bestMethod |> left_join(methodOptsOne, join_by(id == hash))

  return(best)
}
