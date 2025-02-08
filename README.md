# DEEBeval

A package for evaluating the results of estimation methods for dynamical systems. This package is typically used via the command line interface provide by the package DEEBcmd.

Alternatively, a typicall useage of this package looks like this:
```r
DEEBeval::runEvalTbl(
    dbPath, # Path to a DEEB database
    DEEBpath::getNew(dbPath, model), # collects all runs that have not been evaluated
    scoreFilter = NULL,
    createPlots = FALSE,
    verbose = FALSE,
    writeScoreHtml = FALSE,
    createSummary = FALSE,
    onlySummarizeScore = FALSE,
    autoId = NULL
)
```
