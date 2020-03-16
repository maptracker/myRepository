#' myRepo File Handlers
#'
#' Utility functions to manipulate files
#'
#' @name myRepoFileHandlers
#'
#' @details
#'
#' Internal functions to manipulate files within the repository,
#' generally supporting files used by this package
#'
#' \itemize{
#' \item \link{.extractedDESCRIPTION} - Pull DESCRIPTION from tar.gz package
#' \item \link{.extractedPDF} - Use Rd2pdf to make a PDF file of documentation
#' \item \link{.copyTemplateFile} - Copy a template file and swap in values
#' }
#'
#' @examples
#' 
#' # It is not advised that you utilize these methods yourself. If you
#' # feel the need to, use the ::: operator to access them, eg:
#'
#' \donttest{
#' td <- tempdir()
#' myRepository:::.copyTemplateFile('ResourcesREADME.md', td,
#'                                  list(NAME='Foodle'))
#' message("template copied to: ", td)
#' }
NULL

#' Extracted Description
#'
#' Extracts and caches the DESCRIPTION file from a .tar.gz package
#'
#' @name dotExtractedDESCRIPTION
#'
#' @param tgz Required, the versioned name of a .tar.gz package file,
#'     eg "mypackage_1.4.2.tar.gz"
#' @param as.url Default is FALSE if it is detected that you're
#'     running on a system where the repo is locally accessible,
#'     otherwise TRUE. If TRUE then the path to the cached DESCRIPTION
#'     file is returned as a URL rather than a file path.
#'
#' @importFrom utils untar
#' @importFrom CatMisc file.rename2
#'
#' @keywords internal
#'

.extractedDESCRIPTION <- function (tgz, as.url=ifelse(.opt('localOk'),FALSE,TRUE)) {
    subDir  <- "extracted_DESCRIPTION"
    rPath   <- .opt('Path')
    dRoot   <- ifelse(as.url, .opt('URL'), rPath) # URI as URL/file path
    descDir <- file.path(dRoot, "resources", subDir)
    dFile   <- file.path(descDir, gsub('.tar.gz$','.txt', tgz))
    if (!.opt('localOk')) return( dFile )
    
    ## Local file system is available, we can fill in missing or old files
    locDir  <- file.path(rPath, "resources", subDir)
    tmpDir  <- tempdir()
    for (name in tgz) {
        ## Full path to tgz file:
        src  <- file.path(rPath, "src", "contrib", name)
        ## Full path to local description file:
        file <- file.path(locDir, gsub('.tar.gz$','.txt', name))
        ## If the extracted description file exists and is newer
        ## than the source, then we do not need to refresh it:
        if (file.exists(file) && file.mtime(file) >= file.mtime(src)) next
        ## Find the DESCRIPTION file:
        contents <- untar(src, list=TRUE)
        found    <- contents[ grepl('^[^/]+/DESCRIPTION$', contents) ]
        if (length(found) == 0) {
            .wrapMsg("Failed to find DESCRIPTION in archive",src,color='red')
            next
        } else if (length(found) != 1) {
            .wrapMsg("Multiple DESCRIPTION files in archive!", found,
                     "*very odd*", src, color='red')
            next
        }
        ## We have one and only one DESCRIPTION
        ## Surgically extract the single file:
        ##   https://stackoverflow.com/a/7151322
        
        tmpDesc <- file.path(tmpDir, found[1])
        if (file.exists(tmpDesc)) file.remove(tmpDesc)
        if (file.exists(tmpDesc)) {
            .wrapMsg("Can not parse DESCRIPTION - prior temporary file can't be removed", tmpDesc, color='red')
            next
        }

        untar(src, files=found[1], exdir=tmpDir)
        if (!file.exists(tmpDesc)) {
            .wrapMsg("Failed to extract DESCRIPTION from archive:", src,
                     "File should have been here, but was not:",
                     tmpDesc, color='red')
            next
        }

        ## Move from temporary location to resources/ :
        if (file.exists(file)) file.remove(file)
        if (file.exists(file)) {
            .wrapMsg("Failed to remove stale DESCRIPTION file:", file,
                     "Replacement file is here:",
                     tmpDesc, color='red')
            next
        }
        if (!CatMisc::file.rename2(tmpDesc, file)) {
            .wrapMsg("Failed to move extracted DESCRIPTION to proper directory", color='red')
        }
    }
    
    dFile
}

#' Extracted PDF
#'
#' Maintain the extracted_PDF folder on the repository website
#'
#' @name dotExtractedPDF
#'
#' @details
#'
#' This is a small wrapper around \link{makePDF}, intended to maintain
#' the \code{extracted_PDF} subdirectory on the repository website's
#' file system.
#'
#' @param tgz Required, file path to the .tar.gz package file
#' @param as.url Default is FALSE if it is detected that you're
#'     running on a system where the repo is locally accessible,
#'     otherwise TRUE. If TRUE then the path to the cached DESCRIPTION
#'     file is returned as a URL rather than a file path.
#'
#' @seealso \link{makePDF}, which actually generates the PDF files
#'
#' @keywords internal

.extractedPDF <- function (tgz, as.url=ifelse(.opt('localOk'),FALSE,TRUE)) {
    subDir  <- "extracted_PDF"
    pdfDir  <- file.path(.opt('Path'), "resources", subDir)
    rv  <- NULL
    if (.opt('localOk')) {
        ## We can fill in missing or old files, and get their size
        srcFile <- file.path(.opt('Path'), "src", "contrib", tgz)
        rv <- makePDF( srcFile, dir=pdfDir, quiet=TRUE)
    }
    if (as.url || !.opt('localOk')) {
        ## Either we want returned value as URLs, or we can't get local data
        sz <- attr(rv, "size")
        pBase   <- gsub('.tar.gz$','.pdf', basename(tgz))
        rv <- file.path(.opt('URL'), "resources", subDir, pBase)
        ## Add the size attribute in - expected by getMyManifest
        attr(rv, 'size') <- if (is.null(sz)) {
            ## Just toss in NA
            rep(NA, length(tgz))
        } else {
            sz
        }
    }
    rv
}

#' Make PDF
#'
#' Create PDF files from one or more .tar.gz pacakages
#'
#' @details
#'
#' Given a .tar.gz file, extract it and generate a monolithic PDF
#' document of the help topics using \code{Rd2pdf}
#'
#' @param tgzs Required, a vector of file paths to tar.gz archives
#'     representing R packages
#' @param dir Default \code{.}, the directory to generate PDF files in
#' @param quiet Default \code{FALSE}. If TRUE, will supress a
#'     couple messages reporting success.
#' @param clobber Default \code{TRUE}, which will overwrite PDF files
#'     that are older than their source tar.gz files. PDF files that
#'     are newer will never be overwritten (if you wish to do so,
#'     delete the target PDF yourself)
#'
#' @seealso \link{dotExtractedPDF}, \link{buildAndCheck}
#'
#' @importFrom crayon blue red
#' @importFrom callr rcmd
#' @importFrom utils untar
#' 
#' @export

makePDF <- function(tgzs, dir='.', quiet=FALSE, clobber=TRUE) {
    tmp     <- file.path(tempdir(), "makePDF")
    if (!dir.exists(tmp)) dir.create(tmp)
    rv <- character()
    thisDir <- getwd()
    for (tgz in tgzs) {
        base    <- basename(tgz)
        pdf     <- gsub('.tar.gz$','.pdf', base)
        if (pdf == base) {
            message(".makePDF(): File ", .fileCol(base)," does not appear to be a .tar.gz")
            rv <- c(rv, NA)
            next
        }
        pdf       <- file.path(dir, pdf)
        if (file.exists(pdf)) {
            if (file.mtime(pdf) > file.mtime(tgz)) {
                if (!quiet) message(".makePDF(): File ", .fileCol(tgz),
                                    " already has up-to-date PDF at:\n  ",
                                    .fileCol(pdf), " - keeping existing file")
                rv <- c(rv, pdf)
                next
            } else if (!clobber) {
                message(".makePDF(): File ", .fileCol(tgz),
                        " has existing, stale PDF:\n  ", .fileCol(pdf),
                        "\n  but clobber is FALSE - No action taken")
                ## Record this as NA, since it was not regenerated
                rv <- c(rv, NA)
                next
            }
            ## We will regenerate - delete the file
            file.remove(pdf)
        }
        pkgName   <- gsub('_[0-9].+$', "", base)
        expectDir <- file.path(tmp, pkgName)
        ## Make sure we have a fresh extraction directory:
        if (dir.exists(expectDir)) unlink(expectDir, recursive=TRUE)

        ## untar in R: https://stackoverflow.com/a/7151322
        utils::untar(tgz, exdir=tmp)
        if (!dir.exists(expectDir)) {
            message(".makePDF(): File ", .fileCol(tgz)," failed to generate expected directory:\n  ", .fileCol(expectDir), " - no action taken")
            rv <- c(rv, NA)
            next
        }
        seFile <- file.path(tmp, gsub('.tar.gz', '.STDERR.txt', base))
        soFile <- file.path(tmp, gsub('.tar.gz', '.STDOUT.txt', base))
        ## According to ?Rd2pdf this utility is part of base, but
        ## needs to be run with R CMD, so using a system call to
        ## execute. Also, it sometimes creates irritating temp files
        ## in the current directory, so move to tempdir for the call:
        setwd(tmp)
        callr::rcmd("Rd2pdf", c("--no-preview", "--force","-o", pdf, expectDir),
                    stderr=seFile, stdout=soFile)
        setwd(thisDir)

        if (file.exists(pdf)) {
            if (!quiet) message("Created ", .fileCol(pdf),"\n    ",
                    crayon::blue(seFile), " + ", crayon::blue(soFile))
            rv <- c(rv, pdf)
        } else {
            ie <- .ignoredErrors()
            if (is.na(ie[pkgName]))
                message("Attempt to generate ", pdf, " appears to have failed.",
                        "\n  See: ", crayon::red(seFile), " + ", crayon::red(soFile))
            rv <- c(rv, NA)
        }
    }
    attr(rv, 'size') <- file.info(ifelse(is.na(rv), "", rv))$size
    rv
}

#' Copy Template File
#'
#' Copy and optionally modify a template file to another location
#'
#' @name dotCopyTemplateFile
#'
#' @details
#'
#' The \code{inst/templates} folder contains "boilerplate" files that
#' can be copied to new locations. These are primarily used when
#' setting up a new repository file system. The files can contain
#' replaceable tokens of format <TAG>, which can be substituted with
#' values defined in the \code{replace} parameter.
#'
#' @param name Required. The 'base' name of the template file. A value
#'     of 'foobar' will look for a template at
#'     \code{inst/templates/template_foobar}
#' @param dest Required. The folder the file should be copied to
#' @param replace Default an empty list. An optional list of tag/value
#'     pairs to swap out. For example, a value of
#'     \code{list(FooBar='apple')} would find all occurances of
#'     '<FooBar>' in the template and replace them with 'apple'
#' @param newname Default \code{NULL}. If NULL, then the template file
#'     will be used as the name, absent the 'template_' prefix (so
#'     'template_puddle.txt' will be copied as
#'     'puddle.txt'). Otherwise, the filename will be the value
#'     provided by newname.
#' @param clobber Default \code{NULL}, which will cause existing files
#'     to be quietly overwritten. If \code{TRUE}, the file will be
#'     overwritten with a warning message. If \code{FALSE}, the file
#'     will not be overwritten, instead it will be written to a
#'     temporary location. The paths to both the temporary and
#'     intended destination will be reported to the user.
#' @param as.is Default \code{FALSE}, which will result in the file
#'     being parsed to substitute placeholder values specified in the
#'     \code{replace} parameter. If TRUE, the file will simply be
#'     copied as-is; This option is useful for binary files.
#'
#' @return The path to the newly created file
#' 
#' @importFrom CatMisc is.something
#' 
#' @keywords internal

.copyTemplateFile <- function (name, dest, replace=list(), newname=NULL,
                               clobber=NULL, as.is=FALSE) {
    srcDir <- .inst.path.package('myRepository')
    src    <- file.path(srcDir, 'templates', paste('template', name, sep='_'))
    if (!file.exists(src)) .wrapMsg("Failed to read template - no such file",
                                    src, color='red', fatal=TRUE)
    if (!is.null(newname)) name <- newname
    trg    <- file.path(dest, name)
    if (file.exists(trg) && !is.null(clobber)) {
        if (clobber) {
            .wrapMsg("Template is overwriting existing file:",
                     .fileCol(trg), color='yellow')
        } else {
            .wrapMsg("File exists; Template has not replaced it:",
                     .fileCol(trg), color='red')
            trg <- file.path(tempdir(), name)
            .wrapMsg("Instead, file has been written to a temporary location:",
                     .fileCol(trg), color='cyan')
        }
    }
    if (as.is) {
        ## Just copy the file as-is:
        file.copy(src, trg)
    } else {
        ## Parse file and substitute values in replace
        if (CatMisc::is.something(replace$fromOpts)) {
            ## lazy way to pull replacement strings in from the
            ## "standard" options:
            defaults <- list(
                NAME=.opt("Name"),
                DESCRIPTION=.opt("Description"),
                PATH=.opt("Path"),
                URL=.opt("URL"),
                CRAN=.quoteVector(.opt("CRAN")),
                GROUP=.opt("Group"),
                RELEASES=.opt("Releases"),
                DOCUMENTATION=.opt("Documentation"),
                CONFSCRIPT=.opt("Configure")
            )
            for (k in names(defaults)) {
                ## Use defaults to set replacement values, but only if not
                ## already set:
                if (!CatMisc::is.something(replace[[k]]) &&
                    CatMisc::is.something(defaults[[k]]))
                    replace[[k]] <- defaults[[k]]
            }
        }
        ## Read the template, replacing placeholder
        lines <- readLines(src)
        for (tag in names(replace)) {
            ## Swap out placeholders
            val <- replace[ tag ]
            if (!is.null(val)) {
                tag   <- paste('<', tag, '>', sep='') # NAME -> <NAME>
                ## replace all instances of <tag>. Note that sub() is not global
                lines <- gsub(tag, val, lines, fixed=TRUE)
            }
        }
        cat(paste(lines, collapse="\n"), file=trg)
    }
    trg
}

#' alterFile
#'
#' Gingerly alter an existing file
#'
#' @details
#'
#' Designed to aid modification of settings or configuration files
#' once and only once. Takes a file path, the text to add and a
#' "check" value to determine if the addition has already been
#' performed.
#' 
#' @param file Required, the path to the file to be modified
#' @param newText Required, a character vector of text to be added to
#'     the file
#' @param check Required, a pattern to look for in the existing
#'     file. If the pattern is found, no modifications will be made.
#' @param what Default "altered", human-readable text to describe what
#'     is being done. For example, could be
#'     "updated to set default units to meters"
#' @param collapse Default "\\n" (newline). The character string to use
#'     when separating values in \code{newText}, the default will
#'     treat each entry as a separate line
#' @param newline Default TRUE, which will assure that the added text
#'     includes a terminal newline.
#'
#' @return A string describing what was (or was not) done
#'
#' @examples
#'
#' \donttest{
#' ## Will put two comments in your .Rprofile
#' alterFile(myRprofile(), c("## Kilroy was here",
#'      "## Above line added by myRepository"), "Kilroy",
#'      what="commented with GI graffiti")
#' }
#' 
#' @importFrom CatMisc is.something
#'
#' @export

alterFile <- function (file, newText, check, what = "altered", collapse="\n",
                       newline=TRUE) {
    rv <- "Unknown outcome"
    msgCol <- "red"
    if (!file.exists(file)) {
        rv <- paste("File not", what, "-", file, "does not exist")
    } else if (!CatMisc::is.something(newText)) {
        msgCol <- "yellow"
        rv <- paste("File not", what, "- no new content provided")
    } else {
        ## Is there an indication that the file already has been modified?
        alreadyThere <- any(grepl(check, readLines(file)))
        if (alreadyThere) {
            ## Looks like alteration has already been made
            msgCol <- "blue"
            rv <- paste("File", file, "already", what)
        } else {
            ## NEED ERROR HANDLING
            addTxt <- paste(newText, collapse=collapse)
            ## By default we will make sure last line has a newline
            if (newline && !grepl("\\n$", addTxt))
                addTxt <- paste(addTxt, "\n", sep='')
            ## Include a newline before new text:
            addTxt <- paste('\n', addTxt, sep='')
            cat(addTxt, file=file, append=TRUE)
            msgCol <- "green"
            rv <- paste("File", file, "succesfully", what)
        }
    }
    .wrapMsg(rv, color=msgCol)
    invisible(rv)
}

#' File Repository Status
#'
#' Determine the status of a file under version control
#'
#' @details
#'
#' Will attempt to determine if a file is not under version control,
#' is controlled but unaltered or controlled and modified relative to
#' the repository.
#' 
#' @param file Required, the path to the file to be checked
#'
#' @return A single character representing the state, where:
#'
#' \itemize{
#'   \item \code{?} - Not under version control or ignored
#'   \item \code{U} - Controlled and unchanged
#'   \item \code{M} - Controlled and modified
#'   \item \code{-} - File does not exist, not in a VC directory, etc
#'   \item \code{#} - This function failed to recognize the returned code
#' }
#'
#' @examples
#'
#' fileRepoStatus("~/.Rprofile")
#' 
#' @importFrom CatMisc is.something
#'
#' @export

fileRepoStatus <- function (file) {
    if (!file.exists(file)) return("-")
    file <- normalizePath(file)
    dir  <- dirname(file) # Directory holding the file
    cpn  <- currentPackageName(dir, quiet=TRUE) # Package status for file
    vct  <- cpn['VcType']
    if (is.na(vct)) return("-") # If we did not identify version control
    ## U : Unchanged
    ## M : Modified
    ## ? : Untracked
    ## 
    if (vct == 'git') {
        ## Ask git what the state of the file is:
        stat <- system2("git", c("-C", cpn['VcDir'], "status",
                                 "--porcelain", file), stdout=TRUE)
        if (length(stat) == 0) {
            ## I can't find a way for git status to explicitly tell me
            ## that a file is tracked and unaltered. It just returns
            ## nothing.
            "U" 
        } else if (grepl('^( M|M |MM|MD|A |AM|AD|D |R |RM|RD| D|MD|AD|RD|CD| R|DR|DD|AU|UD|UA|DU|AA|UU) ', stat[1])) {
            "M"
        } else if (grepl('^(\\?\\?|!!) ', stat[1])) {
            "?"
        } else {
            ## There are a LOT of code combinations
            ## https://git-scm.com/docs/git-status
            "#"
        }
    } else {
        ## Unrecognized version control
        "-"
    }
}
