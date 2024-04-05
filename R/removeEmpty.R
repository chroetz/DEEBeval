#' @export
removeEmptyMethodFolders <- function(dbPath, removeSingleFileFolders = FALSE) {
  pt <- proc.time()
  models <- DEEBpath::getModels(dbPath)
  for (model in models) {
    cat("MODEL:", model, "\n")
    paths <- DEEBpath::getPaths(dbPath, model)
    allDirs <- list.dirs(paths$esti)
    for (dir in allDirs) {
      files <- list.files(dir)
      if (length(files) == 0) {
        cat("Folder", dir, "is empty. Removing... ")
        unlink(dir, recursive = TRUE, force = TRUE)
        if (dir.exists(dir)) cat("failed") else cat("ok")
        cat("\n")
      }
      if (length(files) == 1) {
        cat("Folder", dir, "only contains", files, ".")
        if (removeSingleFileFolders) {
          cat(" Removing ...")
          unlink(dir, recursive = TRUE, force = TRUE)
          if (dir.exists(dir)) cat("failed") else cat("ok")
        }
        cat("\n")
      }
    }
  }
  cat("Removing empty method folders took", (proc.time()-pt)[3], "s\n")
}
