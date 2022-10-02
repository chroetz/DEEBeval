getFileMeta <- function(path, pattern, ...) {
  fileNames <-
    path |>
    dir() |>
    stringr::str_subset(pattern)
  if (length(fileNames) == 0) return(NULL)
  parts <- stringr::str_match(fileNames, pattern)[,-1,drop=FALSE]
  partsLst <- apply(parts, 2, asNumericConditional, simplify=FALSE)
  names(partsLst) <- c(...)
  meta <- dplyr::bind_cols(
    path = normalizePath(file.path(path, fileNames)),
    tibble::as_tibble(partsLst))
  return(meta)
}


asNumericConditional <- function(x) {
  z <- suppressWarnings(as.numeric(x))
  if (any(is.na(z))) return(x)
  zi <- as.integer(z)
  if (all(abs(zi - z) <= 2*.Machine$double.eps)) return(zi)
  return(z)
}

getMetaGenericOne <- function(path, tagsFilter) {

  fileNames <-
    path |>
    dir() |>
    stringr::str_subset("([:alpha:]+\\d+)+([:alpha:]+)?\\.(csv|json)")

  tags <- stringr::str_extract_all(fileNames |> stringr::str_extract("\\w+"), "[:alpha:]+")

  sel <- sapply(tags, \(tg) all(tg %in% tagsFilter))
  fileNames <- fileNames[sel]
  tags <- tags[sel]

  nums <- stringr::str_extract_all(fileNames |> stringr::str_extract("\\w+"), "\\d+")

  tagsConcat <- sapply(tags, paste, collapse="_")
  ids <- unique(tagsConcat)

  metaList <- lapply(ids, \(id) {
    sel <- tagsConcat == id
    fullPaths <- normalizePath(file.path(path, fileNames[sel]))
    tg <- tags[sel][[1]]
    nm <- nums[sel] |> unlist() |> as.integer() |> matrix(ncol = sum(sel)) |> t() |> as.data.frame()
    meta <- cbind(fullPaths, nm)
    colnames(meta) <- c(paste0(tg[length(tg)], "Path"), paste0(tg[1:ncol(nm)], "Nr"))
    meta <- as_tibble(meta)
    return(meta)
  })

  return(metaList)
}


getMetaGeneric <- function(paths, tagsFilter = c("task", "esti", "truth", "obs")) {
  paths <- normalizePath(paths, mustWork=TRUE)
  paths <- unique(paths)
  metaList <- lapply(paths, getMetaGenericOne, tagsFilter=tagsFilter) |> unlist(recursive=FALSE)
  colCount <- sapply(metaList, ncol)
  meta <- Reduce(fullJoinMeta, metaList[order(colCount, decreasing=TRUE)])
  meta <- dplyr::relocate(meta, ends_with("Nr"), ends_with("Path"))
  return(meta)
}

fullJoinMeta <- function(x, y) {
  common <- intersect(names(x), names(y))
  if (length(common) == 0) {
    dplyr::bind_cols(
      x[rep(seq_len(nrow(x)), each = nrow(y)), ],
      y[rep(seq_len(nrow(y)), times = nrow(x)), ])
  } else {
    dplyr::full_join(x, y, by = common)
  }
}
