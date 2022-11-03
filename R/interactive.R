#' @export
interact <- function() {
  # module load R/4.2.0/gcc-mkl
  # module load compiler/gnu/10.2.0
  # module load pandoc/2.14.2
  # R
  # > dir
  # > .libPaths("~/R/x86_64-pc-linux-gnu-library/4.2")
  # > .libPaths("~/R/x86_64-pc-linux-gnu-library/4.2")
  # Rscript -e 'DEEBeval::interact()'
  dbPath <- getwd()
  askUserWhatToEval(dbPath)
}

askUserWhatToEval <- function(dbPath = ".") {
  dbPath <- normalizePath(dbPath, winslash="/", mustWork=TRUE)
  cat("Scaning for new estimation files...\n")
  analysis <- analyseDb(dbPath)
  if (nrow(analysis$newEsti) == 0) {
    cat("No new estimation files detected.\n")
  } else {
    cat("Found", nrow(analysis$newEsti), "new estimation files. The first three are:\n")
    print(tbl[1:min(3,nrow(analysis$newEsti)),])
  }
  choice <- getUserInput("Choose what to do", c("new" = "evaluate new", "choose" = "choose what to (re-)evaluate"))
  if (choice == "new") {
    startComp(rlang::expr_text(rlang::expr(
      DEEBeval::runEvalNew(!!dbPath))))
  } else {
    # TODO: get what to choose from
    example <- getUserInputYesNo(
      "From the example folders?",
      default = "No")
    models <- getUserInput(
      "Choose model(s)",
      analysis$models,
      multi = TRUE,
      default = "all")
    methodsFilter <- getUserInput(
      "Choose method(s)",
      analysis$methods,
      multi = TRUE,
      default = "all")
    truthNrFilter <- getUserInputNrs(
      "Choose truthNr(s)",
      analysis$truthNrs,
      multi = TRUE,
      default = "all")
    obsNrFilter <- getUserInputNrs(
      "Choose obsNr(s)",
      analysis$obsNrs,
      multi = TRUE,
      default = "all")
    taskNrFilter <- getUserInputNrs(
      "Choose taskNr(s)",
      analysis$taskNrs,
      multi = TRUE,
      default = "all")
    scoreFilter <- getUserInput(
      "Choose scoreFunction(s)",
      analysis$scoreFunctions,
      multi = TRUE,
      default = "all")
    createPlots <- getUserInputYesNo(
      "Should plots be (re-)created?",
      default = "Yes")
    readyToStart <- getUserInputYesNo(
      "Ready to start?",
      default = "Yes")
    if (readyToStart) {
      startComp(
        rlang::expr_text(rlang::expr(
          DEEBeval::runEval(
            dbPath = !!dbPath,
            models = !!models,
            example = !!example,
            methodsFilter = !!methodsFilter,
            obsNrFilter = !!obsNrFilter,
            truthNrFilter = !!truthNrFilter,
            taskNrFilter = !!taskNrFilter,
            scoreFilter = !!scoreFilter,
            createPlots = !!createPlots,
            verbose = FALSE
          )
        ))
      )
    } else {
      cat("Abort.\n")
    }
  }
}

startComp <- function(cmdStr) {
  if (isSlurmAvailable()) {
    jobName <- "DEEBeval"
    cat("Starting SLURM job", jobName, "\n")
    clcom <- paste0(
      "sbatch ",
      " --qos=short",
      " --job-name=", jobName,
      " --output=", jobName, "-%j.out",
      " --error=", jobName, "-%j.err",
      " --mail-type=END",
      " --wrap=\"Rscript -e '", gsub("\"", "\\\\\"", cmdStr), "'\"")
    cat(clcom, "\n")
    system(clcom)
  } else {
    cat("Evaluating following R expression:\n", cmdStr, "\n", sep="")
    eval(rlang::parse_expr(cmdStr))
  }
}

isSlurmAvailable <- function() {
  return(suppressWarnings(system2("srun", stdout = FALSE, stderr = FALSE) != 127))
}

getUserInputNrs <- function(title, options, multi = FALSE, default = NULL) {
  getUserInput(title, options, multi = multi, default = default, onlyNrs = TRUE)
}

getUserInputYesNo <- function(title, default) {
  input <- getUserInput(title, c("Yes", "No"), multi = FALSE, default = default, onlyNrs = FALSE)
  switch(
    input,
    Yes = TRUE,
    No = FALSE)
}

getUserInput <- function(title, options, multi = FALSE, default = NULL, onlyNrs = FALSE) {
  cat("\n", title, "\n", sep="")

  possibleChoices <- NULL
  possibleChoicesNr <- NULL
  if (multi) {
    allText <- " 0: all"
    if (default == "all") allText <-  crayon::cyan(allText)
    cat(allText, "\n")
    possibleChoices <- c(possibleChoices, "all")
    possibleChoicesNr <- c(possibleChoicesNr, 0)
  }
  if (onlyNrs) {
    cat(" ")
    cat(ifelse(options %in% default, crayon::cyan(options), options), sep = ",")
    cat("\n")
    possibleChoices <- c(possibleChoices, options)
    possibleChoicesNr <- c(possibleChoicesNr, options)
  } else {
    numList <- paste0(" ", seq_along(options), ": ", options)
    cat(ifelse(options %in% default, crayon::cyan(numList), numList), sep = "\n")
    possibleChoices <- c(possibleChoices, options)
    possibleChoicesNr <- c(possibleChoicesNr, seq_along(options))
  }

  if (!is.null(default)) cat(
    "Leave empty for ",
    crayon::cyan("cyan"),
    " (", paste(default, collapse = ", "), ").\n", sep="")
  if (multi) {
    cat("Choose one or more options, e.g., '2', '3:6', '1,4,7':\n")
  } else {
    cat("Choose one option!:\n")
  }

  input <- getLine()
  ids <- as.numeric(eval(parse(text = paste("c(", input, ")"))))
  if (!multi && length(ids) > 1) stop("Choose only one element!")
  if (any(!ids %in% possibleChoicesNr)) {
    stop("Choose numbers from ", paste(possibleChoicesNr, collapse=","), "!")
  }
  if (length(ids) == 0) {
    ids <- possibleChoicesNr[possibleChoices %in% default]
  }
  if (multi && 0 %in% ids) {
    chosenIdx <- seq_along(options)
  } else {
    if (onlyNrs) {
      chosenIdx <- which(options %in% ids)
    } else {
      chosenIdx <- ids
    }
  }
  cat("You chose: ", paste(options[chosenIdx], collapse = ","), "\n", sep = "")
  if (is.null(names(options))) {
    chosenElements <- options[chosenIdx]
  } else {
    chosenElements <- names(options)[chosenIdx]
  }
  return(chosenElements)
}

# Get one line of user input regardless of whether running interactively or not (via Rscript).
# (base::readline does not wait for user input when running via Rscript.)
getLine <- function() {
  if (interactive()) {
    # needed for e.g. RStudio and R in jupyter
    return(readline())
  }
  return(readLines(withr::local_connection(file("stdin")), n = 1))
}

analyseDb <- function(dbPath = ".") {
  x1 <- getNew(dbPath, TRUE)
  x1$example <- TRUE
  x2 <- getNew(dbPath, FALSE)
  x2$example <- FALSE
  newEsti <- dplyr::bind_rows(x1, x2)

  uniques <- getUniqueDbEntries(dbPath, FALSE)
  uniquesEx <- getUniqueDbEntries(dbPath, TRUE) # TODO: use

  return(c(
    list(newEsti = newEsti),
    uniques))
}

getUniqueDbEntries <- function(dbPath, example) {
  # TODO: move to DEEBpath
  # TODO: react to `example` arg
  models <- list.dirs(path = dbPath, full.names = FALSE, recursive = FALSE)
  methods <- unique(unlist(lapply(
    file.path(dbPath, models, "estimation"),
    list.dirs,
    full.names = FALSE, recursive = FALSE)))
  truthFiles <- unique(unlist(lapply(
    file.path(dbPath, models, "truth"),
    list.files,
    pattern = "obs_truth\\d+\\.csv",
    full.names = FALSE, recursive = FALSE)))
  truthNrs <- unique(as.integer(substr(truthFiles, 10, 13)))
  obsFiles <- unique(unlist(lapply(
    file.path(dbPath, models, "observation"),
    list.files,
    pattern = "truth\\d+obs\\d+\\.csv",
    full.names = FALSE, recursive = FALSE)))
  obsNrs <- unique(as.integer(substr(obsFiles, 13, 16)))
  taskFiles <- unique(unlist(lapply(
    file.path(dbPath, models, "task"),
    list.files,
    pattern = "task\\d+\\.json",
    full.names = FALSE, recursive = FALSE)))
  taskNrs <- unique(as.integer(substr(taskFiles, 5, 6)))
  scoreFunctions <- taskFiles <- unique(unlist(lapply(
    file.path(dbPath, models, "task"),
    \(path) {
      files <- list.files(path, pattern = "task\\d+\\.json", full.names = TRUE, recursive = FALSE)
      sapply(files, \(fl) sapply(
        jsonlite::read_json(fl)$scoreList$list,
        \(x) x$name))
    })))

  return(list(
    models = models,
    methods = methods,
    truthNrs = truthNrs,
    obsNrs = obsNrs,
    taskNrs = taskNrs,
    scoreFunctions = scoreFunctions
  ))
}

getNew <- function(dbPath, example) {
  models <- list.dirs(path = dbPath, full.names = FALSE, recursive = FALSE)

  unevaled <- lapply(models, \(model) {
    path <- DEEBpath::getPaths(dbPath, model, example=example)
    methods <- list.dirs(path$esti, full.names = FALSE, recursive = FALSE)
    meta <- lapply(methods, \(method) {
      methodEstiPath <- file.path(path$esti, method)
      if (!dir.exists(methodEstiPath)) next
      meta <- DEEBpath::getMetaGeneric(methodEstiPath)
      meta$method <- method
      meta$estiPath <- NULL
      return(meta)
    }) |>
      dplyr::bind_rows()
    scoreFiles <- DEEBpath::getScoreFiles(path$eval)
    scores <-
      lapply(scoreFiles, \(sf) {
        scores <- readr::read_csv(sf, col_types = readr::cols())
        scores[c("method", "truthNr", "obsNr", "taskNr")]
      }) |>
      dplyr::bind_rows()
    unevaled <- anti_join(meta, scores, by = c("truthNr", "obsNr", "taskNr", "method"))
    unevaled$model <- model
    unevaled
  }) |>
    dplyr::bind_rows()
}
