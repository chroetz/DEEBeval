expandInfo <- function(db, example, model, truthNr, obsNr, method = NULL) {

  if (example) {
    truthFile <- sprintf("~/%s/%s/example/truth%04d.csv", db, model, truthNr)
    obsFile <- sprintf("~/%s/%s/example/truth%04dobs0001.csv", db, model, truthNr)
    if (!is.null(method))estiFile <- sprintf("~/%s/%s/example/%s/truth%04dobs0001esti.csv", db, model, method, truthNr)
  } else {
    truthFile <- sprintf("~/%s/%s/truth/truth%04d.csv", db, model, truthNr)
    obsFile <- sprintf("~/%s/%s/observation/truth%04dobs%04d.csv", db, model, truthNr, obsNr)
    if (!is.null(method)) estiFile <- sprintf("~/%s/%s/estimation/%s/truth%04dobs0001esti.csv", db, model, method, truthNr)
  }

  title <- NULL
  if (!is.null(method)) title <- paste(truthNr, method, sep = ", ")

  truth <- NULL
  obs <- NULL
  esti <- NULL

  if (file.exists(truthFile)) truth <- readTrajs(truthFile)
  if (!is.null(method) && file.exists(estiFile)) esti <- readTrajs(estiFile)
  if (file.exists(obsFile)) obs <- readTrajs(obsFile)

  if (!hasTrajId(truth)) {
    if (file.exists(truthFile)) truth <- setTrajId(truth, 0)
    if (!is.null(method) && file.exists(estiFile))  esti <- setTrajId(esti, 0)
    if (file.exists(obsFile)) obs <- setTrajId(obs, 0)
  }

  list(
    truth = truth,
    obs = obs,
    esti = esti,
    title = title
  )
}

expandInfoEnv <- function(info) {
  newInfo <- expandInfo(info$db, info$example, info$model, info$truthNr, info$obsNr, info$method)
  addNewToEnv(info, newInfo)
}

addNewToEnv <- function(env, newLst) {
  envNames <- names(env)
  newLstNames <- names(newLst)
  isList <- sapply(env, \(x) is.list(x) && is.vector(x))
  listNames <- envNames[isList]
  for (nm in intersect(listNames, newLstNames)) {
    assign(nm, addNewToList(get(nm, envir = env), newLst[[nm]]), envir = env)
  }
  newNames <- setdiff(newLstNames, envNames)
  for (nm in newNames) {
    assign(nm, newLst[[nm]], envir = env)
  }
  return(newNames)
}

addNewToList <- function(lst, newLst) {
  lstNames <- names(lst)
  newLstNames <- names(newLst)
  isList <- sapply(lst, \(x) is.list(x) && is.vector(x))
  lstListNames <- lstNames[isList]
  for (nm in intersect(lstListNames, newLstNames)) {
    lst[[nm]] <- addNewToList(lst[[nm]], newLst[[nm]])
  }
  newNames <- setdiff(newLstNames, lstNames)
  lst[newNames] <- newLst[newNames]
  return(lst)
}

appendToEnv <- function(env, newLst) {
  for (nm in names(newLst)) {
    assign(nm, newLst[[nm]], envir = env)
  }
  return(invisible(NULL))
}

evalUnknownNames <- function(..., knownNames) {
  names <- ...names()
  isNameSeen <- names %in% knownNames
  unseenIdxes <- which(!isNameSeen)
  res <- list()
  for (i in unseenIdxes) {
    res[[names[i]]] <- ...elt(i)
  }
  res
}

getFromEnv <- function(env, names) {
  res <- list()
  for (nm in names) {
    res[[nm]] <- get(nm, envir = env)
  }
  res
}

getInfo <- function(info, ..., simplify = TRUE) {
  res <- evalUnknownNames(..., names(info))
  appendToEnv(info, res)
  namesToGet <- ...names()
  if (simplify && length(namesToGet) == 1) {
    return(get(namesToGet, envir = info))
  }
  getFromEnv(info, namesToGet)
}


