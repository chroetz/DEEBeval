loadPathsInInfo <- function(info) {
  selPath <- endsWith(names(info), "Path")
  for (nm in names(info)[selPath]) {
    path <- info[[nm]]
    if (!is.character(path) || !length(path) == 1) next
    if (is.na(path)) stop("Path in ", nm, " is NA!")
    nmWoPath <- substr(nm, 1, nchar(nm)-4)
    if (!is.null(info[[nmWoPath]])) next
    if (endsWith(path, ".csv")) {
      info[[nmWoPath]] <- readTrajs(path)
    } else if (endsWith(path, ".json")) {
      info[[nmWoPath]] <- readOptsBare(path)
    }
  }
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


