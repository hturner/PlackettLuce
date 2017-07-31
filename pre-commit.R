#' License: CC0 (just be nice and point others to where you got this)
#' Author: Heather Turner <ht@heatherturner.net>, github.com/hturner
#' Author: Robert M Flight <rflight79@gmail.com>, github.com/rmflight
#' Author: Seth S Wenchel <wenchel@gmail.com>, github.com/restonslacker
#'
#' This is an R script associated with a pre-commit hook that checks whether
#' there are files to be committed. If there are and the minor version is
#' formatted as four digits (signalling a development version), the script
#' increments the package version and sets today's date in the DESCRIPTION file.
#' Under the convention that master development versions start with 0, master
#' development versions are not incremented when on a branch (allowing version
#' to be set before merging back to master).
#'
#' To install it, simply copy this into the top level directory of your git
#' repository. Then, edit the ".git/hooks/pre-commit" file of your git repo to
#' be
#'
#'   #!/bin/bash
#'   Rscript pre-commit.R
#'
#' On Windows, the path to Rscript (which is the same as that to R itself)
#' should be in the PATH system environment variable.
#'

# define path to DESCRIPTION
DESCRIPTION <- "DESCRIPTION"

# check if development version
currDCF <- read.dcf(DESCRIPTION)
currVersion <- currDCF[1,"Version"]
splitVersion <- strsplit(currVersion, ".", fixed=TRUE)[[1]]
nVer <- length(splitVersion)
currEndVersion <- splitVersion[nVer]

# don't increment master development version on branch
# must have path to git.exe on Windows PATH
branch <- system("git branch", intern = TRUE)
doIncrement <- nchar(currEndVersion) == 4 &
    (("* master" %in% branch) | substring(currEndVersion, 1, 1) != "0")

# check that there are files that will be committed
# - don't want to increment version if there won't be a commit
fileDiff <- system("git diff HEAD --name-only", intern=TRUE)

if ((length(fileDiff) > 0) & doIncrement){
    currEndVersion <- as.integer(currEndVersion)
    newEndVersion <- sprintf("%04d", currEndVersion + 1)
    splitVersion[nVer] <- newEndVersion
    newVersion <- paste(splitVersion, collapse=".")
    currDCF[1,"Version"] <- newVersion
    if (any(colnames(currDCF) == "Date")){
        currDCF[1, "Date"] <- strftime(as.POSIXlt(Sys.Date()), "%Y-%m-%d")
    } else currDCF <- cbind(currDCF,
                            Date = strftime(as.POSIXlt(Sys.Date()), "%Y-%m-%d"))
    write.dcf(currDCF, DESCRIPTION)
    system(paste("git add", DESCRIPTION))
    cat("Incremented package version and added to commit!\n")
}
