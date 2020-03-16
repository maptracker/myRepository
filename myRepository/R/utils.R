#' myRepository Utilities
#'
#' Utility functions
#'
#' @name myRepoUtilities
#'
#' @details
#'
#' Internal utility functions. Generally not exported, 
#'
#' \itemize{
#'   \item \link{.opt} - Manage internal options
#'   \item \link{.quoteVector} - serialize a vector to a quoted string
#'   \item \link{.wrapMsg} - Wrapped, colorized message() output
#'   \item \link{.inst.path.package} - Find the location of inst/
#'   \item \link{.localOffline} - Determine if repo file system is mounted
#'   \item \link{.logMessage} - Write an entry to the local log file
#' }
#' 
NULL

packEnv  <- as.environment(-1) # This package's environment, used to set opts
optVar   <- '.opts'            # Variable name we'll use for the options hash

#' Get/Set Internal Options
#'
#' Get or set a repository-related option/parameter
#'
#' @name DOTopt
#'
#' @details
#'
#' An attempt to manage 'persistent' values without having them 'pollute'
#' the global environment.
#'
#' @param key Required, the name of the parameter (eg 'Path')
#' @param val Default \code{NULL}, an optional new value to assign to
#'     the key
#'
#' @seealso Because assign() does not work on list members:
#'     \url{https://stackoverflow.com/a/9561088}
#'
#' @keywords internal

.opt <- function (key, val=NULL) {
    ## Initialize the options list if it's not already assigned
    if (!exists(optVar, envir=packEnv)) assign(optVar, list(), envir=packEnv)
    locOpt <- get(optVar, envir=packEnv)
    if (is.null(val)) {
        ## Simply recover a key
        locOpt[[ key ]]
    } else {
        ## Set a new value
        locOpt[[ key ]] <- val
        ## Apparently we need to reset the entire list, as we can't
        ## touch individual keys with assign:
        ## https://stackoverflow.com/a/9561088
        assign(optVar, locOpt, envir=packEnv)
        val
    }
}

#' Quoted vector string
#'
#' Builds a string appropriate for evaluating characters inside c()
#'
#' @name DOTquoteVector
#'
#' @details
#'
#' Turns c("a","b","c") into the string "'a', 'b', 'c'". Does not
#' attempt to escape contents, does not wrap in "c()". Intended
#' primarily as an aid for writing configuration files.
#' 
#' @keywords internal

.quoteVector <- function(x) {
    paste(sprintf("'%s'", x), collapse=', ')
}

confFile <- "configureRepo.R"  # Setup file for new users

#' Wrapped Message
#'
#' Internal method for pretty-printing messages
#'
#' @name DOTwrapMessage
#'
#' @details
#'
#' Wraps input, indenting all but the first line, with optional color
#' argument. Made to tidy up message reporting
#'
#' @param \dots The character strings to build the message
#' @param color Default 'green', the color to render the message in
#' @param fatal Default \code{FALSE}. If true, the function will
#'     \code{stop()}.
#'
#' @keywords internal
#' @import crayon

.wrapMsg <- function(..., color='green', fatal=FALSE ) {
    lines <- strwrap(c(...))
    ## The relevant color function from package crayon
    cFunc <- get(color, environment(crayon::red))
    txt   <- paste(lines, collapse="\n  ")
    if (fatal) {
        stop(paste(crayon::bgWhite(crayon::black("Fatal myRepository error:")),
                   cFunc(txt), sep="\n"), call.=FALSE)
    } else {
        message(cFunc(txt))
    }
    invisible(txt)
}

## Copied from my AnnotatedMatrix package

#' Development Package Path
#'
#' Get the path to package file, including when the package is locally
#' installed from tar.gz
#'
#' @name DOTinst.path.package
#'
#' @details
#'
#' When developing with the uncompressed R package, some files are in
#' different locations. In particular, exdata/ remains inside the
#' inst/ folder. This method detects the presence of "inst" and
#' includes it in the return path
#'
#' @param pkg Required. Thee name of the package in question
#'
#' @return The path to the package folder, or the inst/ subfolder if
#'     it exists
#'
#' @seealso \link[base]{path.package}
#' @keywords internal

.inst.path.package <- function(pkg) {
    srcDir <- path.package(pkg)
    ## When developing with the uncompressed R package, extdata is
    ## still inside inst/ - detect this scenario and accomodate it:
    if (file.exists(file.path(srcDir, "inst")))
        srcDir <- file.path(srcDir, "inst")
    srcDir
}

#' Log Message
#'
#' Note repository activity to help debug strangeness
#'
#' @name DOTlogMessage
#'
#' @details
#'
#' Will make an entry in the log file specified in \code{.opt('Log')}
#'
#' @param msg The type of activity occuring
#'
#' @importFrom utils packageVersion
#' @keywords internal

.logMessage <- function(msg) {
    cat(paste(Sys.time(), utils::packageVersion("myRepository"),
              Sys.getenv("USER"), Sys.info()["nodename"], msg, sep="\t"),
        file=.opt('Log'), append=TRUE, fill=TRUE)
}

#' Is the local repository offline?
#'
#' Internal method to see if local actions are not possible and provide feedback
#'
#' @name DOTlocalOffline
#'
#' @details
#'
#' Functionally, simply check to see if the precomputed value
#' \code{.opt('localOk')} is true or not, and returns the inverse.
#'
#' Primary utility is to help guide the user towards resolving the
#' issue if it's not. The two general situations are:
#'
#' \itemize{
#'   \item myRepository has not been set up - contact the person who
#'   maintains the repository so they can point the user to the setup
#'   script
#'   \item The repo is defined, but apparently not mounted. Either
#'   work on a system where it is mounted, or get a systems
#'   administrator to help mount it on the local machine.
#' }
#'
#' Initially, the internal flag \code{localOK} was checked only once,
#' when the settings were first read. This was based on the
#' presumption that the repository would be mounted more-or-less
#' "permanently", for example via NFS. However, other filesystems,
#' like sshfs, can be fleeting creatures that come and go. For this
#' reason, the method re-checks the file system (via dir.exists) on
#' each call.
#' 
#' @param action Default "do something with your repository". A
#' message describing what was being attempted that required the
#' function to be checked. Will be included in the warning message if
#' the local path is unavailable. If it is NA, NULL or an empty
#' string, then the message will not be shown.
#' 
#' @return A logical, TRUE if the local system is not available, FALSE
#'     if it is.
#'
#' @importFrom CatMisc is.something
#' @keywords internal

.localOffline <- function (action="do something with your repository") {
    srcContrib <- .opt('Contrib')

    lok   <- FALSE
    issue <- if (CatMisc::is.something(srcContrib)) {
        ## Path is defined
        if (dir.exists(srcContrib)) {
            ## ... and appears to be available
            lok <-TRUE
            ""
        } else {
            ## Path defined but not visible
            paste0("The location is expected at ", srcContrib, ", but can not be found on this machine. Please either re-run addPackageToRepo() from a location that has the location mounted, or talk to your system administrators about mounting the location on this machine. In general, it will likely be better to make submissions on a single machine rather than mounting the repository on many systems.")
        }
    } else {
        ## Path was never defined
        "That location is currently not defined for your R profile. Please ask your organization's R gurus how to specify the location of your repository."
    }
    .opt('localOk', lok)
    if (lok) return( FALSE ) # local directories available
    ## Nope, no local dir. Unless `action` is 'empty', emit a
    ## hopefully helpful message.
    if (CatMisc::is.something(action)) .wrapMsg(paste("In order to", action, "the local file system that 'holds' the packages must be available.", issue), color='red')
    TRUE
}

#' Increment Version String
#'
#' Increment version string
#'
#' @name DOTincVersString
#'
#' @param vers Required, the version string to be changed, eg 1.4.15
#' @param level Default \code{1}, the part of the string to change. 1
#'     or 'patch' is the 'lowest' number (right-most), 2 or 'minor' is
#'     the next level, and 3 or 'major' is the 'highest' level
#'
#' @examples
#'
#' myRepository:::.incVersString("12.34.56")
#' myRepository:::.incVersString("12.34.56", 2)
#' myRepository:::.incVersString("12.34.56", "major")
#'
#' @importFrom stats setNames
#' @keywords internal

.incVersString <- function (vers, level=1) {
    ## Parse the level parameter to determine which part of the
    ## version we will be incrementing:
    lvl <- level[1]
    if (!is.null(lvl)) {
        if (grepl('(1|patch)', lvl, ignore.case=TRUE)) {
            lvl <- 1L
        } else if (grepl('(2|minor)', lvl, ignore.case=TRUE)) {
            lvl <- 2L
        } else if (grepl('(3|major)', lvl, ignore.case=TRUE)) {
            lvl <- 3L
        } else {
            lvl <- NULL
        }
    }
    part <- c("patch","minor","major")[lvl]
    if (is.null(lvl)) {
        message("The level parameter must be one of 1 (patch), 2 (minor) or 3 (major). You can use numbers or text")
        return(NA)
    }
        
    ## Split the version, reversing so lesser parts are "at front"
    vbits <- as.integer(rev(unlist(strsplit(vers, '\\.'))))
    vlen  <- length(vbits)
    if (lvl > vlen) {
        message("You requested to increment the ", part, " version number, but version ", vers, " has only ", vlen," components")
        return(NA)
    }
    ## Increment the requested level:
    vbits[lvl] <- vbits[lvl] + 1
    ## Set the lesser parts to zero:
    for (i in seq_len(lvl - 1)) vbits[i] <- 0
    ## Un-reverse and stitch back together again
    newVers <- paste(rev(vbits), collapse='.')
    stats::setNames(newVers, part)
}

#' Suggest Version Control Checkout
#'
#' Alert the user that a version-controlled file was overwritten
#'
#' @name DOTsuggestVcCheckout
#' 
#' @details
#'
#' This functionality was orignally put in place to warn the user when
#' they overwrote a tar.gz package file. The concern here is that the
#' package was already submitted to the repository, and the programmer
#' continued development without remembering to increment the
#' DESCRIPTION file. The original version of the tar.gz wouldn't be
#' lost if committed (it's version control, after all) but it could be
#' (ie, would *likely* be) overlooked, causing an inappropriate
#' release of that version to get used.
#'
#' The method is now called only after checking the version control
#' status of the file(s). If you see a warning, it means the file in
#' question IS under version control already and HAS been
#' modified. Because these files have explicit version numbers in
#' their names, this is a bad practice.
#'
#' @param files Required, the paths to the version-controlled files
#' @param vcdir Required, the root-level directory of the VC project
#' @param vctype Default \code{"unknown"}, required in order to
#'     provide meaningful advice
#'
#' @return Colorized text designed to be integrated into
#'     \code{message()}, or \code{NULL} if no suggestion to be made
#'
#' @importFrom CatMisc is.something relativePath
#' @keywords internal

.suggestVcCheckout <- function (files, vcdir, vctype='unknown') {
    relpath <- vapply(files, function(x) {
        ## Find relative path to files with leading slash
        ## Assume other files are already relative
        ifelse(grepl('^/',x), CatMisc::relativePath(vcdir,x), x)
    }, "")
    inRepo  <- relpath[ !is.na(relpath) ]
    if (length(inRepo) == 0) return(NULL) ## None of the files are in the repo!
    
    msg <- c("  You likely neglected to ", .actCol("incrementPackageVersion()"),
             "\n  You SHOULD restore from version control (in a shell):\n    ")
    if (vctype == 'git') {
        c(msg, .actCol("git -C '", vcdir, "' checkout"),
          .safeColorNewline(.actCol(inRepo)))
    } else {
        c(msg, "Unfortunately, I don't know what command to suggest for ",
          crayon::yellow(vctype), " version control")
    }
}

#' Suggest Version Control Commit
#'
#' Point out some files that the user may wish to commit before further work
#'
#' @name DOTsuggestVcCommit
#' 
#' @details
#'
#' Used after generating a 'blank' package with
#' \link{createNewPackage}.
#'
#' @param files Required, the paths to the files that could be added
#'     to the repository
#' @param vcdir Required, the root-level directory of the VC project
#' @param vctype Default \code{NULL}, which will exhort the user to
#'     consider setting up their package under version
#'     control. Otherwise used to construct the relevant commands
#' @param full Default \code{FALSE}. If false, then the commit command
#'     shown will commit only the explicit file list. Otherwise, the
#'     commit will be a 'general' commit, and will include any other
#'     additions outside of those being suggested.
#' @param tag Default \code{NULL}. If non-NULL, will suggest a tag to
#'     apply to the commit (generally the pacakge version)
#' @param msg Default
#'     "myRepository suggests committing these files". The commit
#'     message to suggest to the user.
#' @param note Default
#'     "We suggest committing these files to version control:". The
#'     message to show before the version control command.
#'
#' @return Colorized text designed to be integrated into
#'     \code{message()}, or \code{NULL} if no suggestion to be made
#'
#' @importFrom CatMisc is.something relativePath
#' @importFrom crayon magenta yellow
#'
#' @keywords internal

.suggestVcCommit <- function (files, vcdir, vctype=NULL, full=FALSE, tag=NULL,
    msg="myRepository suggests committing these files",
    note="We suggest committing these files to version control:") {
    if (!CatMisc::is.something(files) || length(files) == 0) return(NULL)

    fullMsg <- if (!CatMisc::is.something(vctype)) {
        c(note, "\n", sprintf("  %s\n", .fileCol(files)),
          "... but either your package is not part of a repository, or I do not recognize the VC system being used")
    } else {
        ## Verify the files are within the repository directory structure
        relpath <- vapply(files, function(x) {
            ## Find relative path to files with leading slash
            ## Assume other files are already relative
            ifelse(grepl('^/',x), CatMisc::relativePath(vcdir,x), x)
        }, "")
        inRepo  <- relpath[ !is.na(relpath) ]
        notRepo <- relpath[ is.na(relpath) ]
        mBits   <- character()
        if (length(inRepo) != 0) {
            mBits  <- c(mBits, note, "\n")
            cFiles <- .fileCol(inRepo)
            if (vctype == 'git') {
                cFiles <- .safeColorNewline(cFiles)
                ## Use -C to reliably specify git folder regardles of
                ## where user runs command. git add:
                mBits <- c(mBits, .actCol("  git -C '", vcdir, "' add"), cFiles)
                ## git commit:
                mBits <- c(mBits, .actCol("\n  git -C '", vcdir, "' commit"))
                ## Surgically specify files if full=FALSE
                if (!full) mBits <- c(mBits, .actCol(cFiles))
                mBits <- c(mBits,
                           .actCol(" \\\n    -m \"", .alertCol(msg),"\""))
                ## Add the tagging nomenclature
                if (!is.null(tag)) {
                    mBits <- c(mBits,
                        .actCol("\n  git -C '", vcdir, "' tag -f '", tag, "'",
                                " \\\n    && git -C '", vcdir, "' push",
                                " \\\n    && git -C '", vcdir, "' push --tags"))
                }
            } else {
                mBits <- c(mBits, sprintf("  %s\n", cFiles),
                    crayon::yellow("... but I do not know the commit command for", vctype))
            }
        }
        if (length(notRepo) != 0) {
            mBits <- c(mBits, "\nThese files should also be added, but appeart to be outside your repository file structure:",
                       sprintf("\n  %s", .fileCol(notRepo)))
        }
        mBits
    }
    message(fullMsg)
    invisible(fullMsg)
}

#' Suggest Version Control Cleanup
#'
#' Suggest version control commands to move older files to an 'archive' folder
#'
#' @name DOTsuggestVcArchive
#' @details
#'
#' The tar.gz and pdf folders (default packageReleases and
#' packageDocumentation, respectively) could become quite cluttered
#' during active development. This method detects when a package has
#' "too many" files, and will suggest the relevant version control
#' command to move the older ones to a subfolder.
#'
#' The simplest way to silence this functionality is to remove or
#' rename the \code{archive} folder (For example, capitalizing it to
#' "Archive" will result in it not being recognized).
#'
#' @param package Required, the name of the package being processed.
#' @param dir Required, the path of the directory holding the files
#' @param max Default \code{NULL}, which will result in no operation
#'     taking place. Otherise, the maximum number (integer) of files
#'     to leave in \code{dir} - the rest will be moved to the
#'     \code{archive} subdirectory.
#' @param vcdir Default \code{NULL}, which will result in no
#'     operation. Otherwise the root-level directory of the version
#'     controlled project
#' @param vctype Default \code{"unknown"}, required in order to
#'     provide meaningful advice for VC operations
#'
#' @return If the version control type is known and there are files to
#'     be archived, a colorized string representing the VC command to
#'     run. Otherwise, \code{NULL}.
#' 
#' @importFrom CatMisc is.something relativePath
#' 
#' @keywords internal

.suggestVcArchive <- function (package, dir, max=NULL,
                               vcdir=NULL, vctype='unknown') {
    if (!all(vapply(c(package, dir, max, vcdir), CatMisc::is.something, TRUE))) {
        ## Make sure all arguments are "not blank"
        return(NULL)
    }
    arch <- file.path(dir, "archive")
    ## Do nothing if the archive directory does not exist:
    if (!dir.exists(arch)) return(NULL)
    
    pat   <- paste0('^', package, '_([0-9\\.]+)\\.[a-z.]+?$')
    files <- list.files(dir, pattern=pat)
    if (length(files) <= max) return(NULL) # We are not over the limit

    ## Extract the version numbers from the files
    vers  <- CatMisc::parenRegExp(pat, files)
    ## AARGGG. There must be a better way to custom sort but I'm not
    ## finding it. I want to sort the version numbers according to
    ## compareVersion, but I can't see a way to provide a custom
    ## comparator method to any of the sort/order functions.  So
    ## instead I am going to heavily zero-pad all the version numbers,
    ## and then string sort.
    srt   <- vapply(vers, function(x) paste0(vapply( unlist(strsplit(x, '\\.')),
             function(y) sprintf("%08d", as.integer(y)), ""),collapse='.'), "")
    srt     <- sort(srt, decreasing=TRUE)
    old     <- names(srt[ -seq_len(max) ])
    ## Ok, we didn't really track suffixes, so let's brute force
    ## recover them. Becuase lazy. (and we don't need to supply it as a param)
    oldFiles <- character()
    for (v in old) {
        of <- list.files(dir, pattern=paste0('^', package, '_', v,'\\.[a-z.]+$'))
        oldFiles <- c(oldFiles, of)
    }
    ## Relative path from repository root to directory being analyzed:
    relPath <- CatMisc::relativePath(vcdir, dir)
    ## Make archive path relative, too:
    arch    <- file.path(relPath, "archive/")

    if (vctype == 'git') {
        ## Colorized relative paths to files being moved:
        cFiles <- .safeColorNewline(c(.fileCol(file.path(relPath, oldFiles)),
                                      .actCol(arch)))
        ## Use -C to reliably specify git folder regardles of where
        ## user runs command. git mv:
        paste(c(.actCol("  git -C '", vcdir, "' mv"), cFiles), collapse='')
    } else {
        .wrapMsg(paste("Some older files can be archived, but I do not know how to do so for the ", vctype, " version control system"), color='yellow')
        NULL
    }
}

#' Make 'current' symlink
#'
#' Make an unversioned 'current' symlink pointing to a file just created
#'
#' @name DOTmakeCurrentSymlink
#' @details
#'
#' Designed to help bring some order to \code{packageReleases/} and
#' \code{packageDocumentation} folders, which will by design fill up
#' with multiple versions of the package. The 'current' symlink will
#' point to the most recently generated file, to aid in browsing of
#' the folder.
#'
#' @param file Required, the target of the symlink
#' @param dir Required, where the symlink should be made
#' @param sfx The suffix to associate with the symlink
#'
#' @importFrom CatMisc is.something relativePath
#' 
#' @keywords internal

.makeCurrentSymlink <- function(file, dir, sfx, quiet=TRUE) {
    if (!all(vapply(c(file, dir, sfx), CatMisc::is.something, TRUE))) {
        ## Make sure all arguments are "not blank"
        return(NA)
    }
    if (!file.exists(file)) {
        .wrapMsg("Can not make 'current' symlink to", file,
                 "File does not exist", color='red')
        return(NA)
    }
    relPath <- CatMisc::relativePath(dir, file)
    if (is.na(relPath)) {
        ## Intended to create a link within the same directory structure
        return(NA)
    }
    ## In most cases the directory should contain only files from one
    ## package. Some VC projects may have multiple packages present,
    ## however. In those cases, we will need to segregate symlinks
    ## according to package
    pat   <- paste0('_[0-9\\.]+', sfx, '$')
    files <- list.files(dir, pattern=pat)
    prfx  <- unique(gsub(pat, '', files))
    baseF <- basename(file)

    ## Build the symlink name
    if (!grepl('^\\.', sfx)) sfx <- paste0('.',sfx) # Assure leading '.'
    slName  <- "current"
    if (length(prfx) != 1) {
        ## If there are multiple 'things' sharing the directory, we
        ## need to make sure they have separate 'current' names:
        slPrior <-  file.path(dir, paste0(c(slName, sfx), collapse=''))
        slName  <- c(slName, '_', gsub(pat, '', baseF))
        ## Remove any old 'vanilla' links:
        if (file.exists(slPrior)) file.remove(slPrior)
    }
    ## Name and full path of the symlink:
    slName <- paste0(c(slName, sfx), collapse='')
    slPath <- file.path(dir, slName)
    ## Remove symlink that might already be there
    
    ## NOTE: R does not consider a broken symlink to be "there". In
    ## fact, file tests on a broken link (one that points to a
    ## non-existant target) will report as if the file does not exist
    ## at all.

    ## HOWEVER: If you attempt to create a new symlink "on top of" the
    ## broken one, you will fail with the error "reason 'File
    ## exists'". So we just blindly delete the symlink, whether it is
    ## there or "not":
    suppressWarnings( file.remove(slPath) )
    ## %@$*! R is sometimes failing to detect that it can symlink on
    ## certain file systems. In particular, it believes that a shared
    ## folder mounted in a virtual container is "read-only"; It is
    ## not. So we will suppress warnings and check if linking occured
    suppressWarnings(file.symlink(baseF, slPath))
    if (file.exists(slPath)) {
        slPath
    } else {
        message(.lightYellow("Failed to create 'current' symlink"),
                " : ", .fileCol(baseF))
        NA
    }
}

#### COLOR OUTPUT ####

## I'm using the crayon package to colorize text to help make it
## easier to pick out the more important parts of output. I'm finding
## that while ANSI colors are well supported between various
## environments (gnome-terminal, konsole, even RStudio web UI), the
## "predefined" colors are inconsistently rendered. I am using
## crayon's make_style function to normalize the displayed colors. The
## functions below are for both compact representation, and to allow
## some forms of markup to be more easily modified if need be in the
## future.

.darkBlue    <- crayon::make_style(rgb(0,0,.7))
.darkRed     <- crayon::make_style(rgb(.5,0,0))
.brown       <- crayon::make_style(rgb(.5, .35, .25)) # I use for comments
.magenta     <- crayon::make_style(rgb(1,0,1))
.lightCyan   <- crayon::make_style(rgb(.5,1,1), bg=TRUE)
.lightYellow <- crayon::make_style(rgb(1,1,0), bg=TRUE)
.lightWhite  <- crayon::make_style(rgb(1,1,1), bg=TRUE)
                                
#' Highlight
#'
#' Just make some text stand out: yellow background, blue text
#'
#' @details
#'
#' The konsole terminal (KDE) has a very dark shade of yellow. Use
#' crayon::make_style to generate more consistently legible styling.
#' 
#' @name DOThilite
#'
#' @importFrom crayon make_style bold
#' @keywords internal

.hilite <- function(x) .lightYellow(.darkBlue(crayon::bold(x)))

#' .fileCol
#'
#' Hilite files and directories: Dark blue on cyan background
#'
#' @name DOTfileCol
#' 
#' @details
#'
#' In konsole (KDE) blue is REALLY dark and contrasts poorly on
#' black. Using crayon RGB styles to try to make a more uniform /
#' visible text display. This style used to highlight directories and
#' files.
#'
#' @importFrom crayon make_style bold
#' @keywords internal

.fileCol <- function(x) .lightCyan(.darkBlue(crayon::bold(x)))

#' .actCol
#'
#' Hilite commands: magenta text
#'
#' @name DOTactCol
#'
#' @param \dots Passed on to crayon
#' 
#' @details
#'
#' Trivial function used to style text that represents actions
#'
#' @importFrom crayon make_style
#' @keywords internal

.actCol <- function(...) .magenta(..., sep='')

#' .alertCol
#'
#' Hilite commands: magenta text
#'
#' @name DOTalertCol
#'
#' @param \dots Passed on to crayon
#' 
#' @details
#'
#' Trivial function used to style 'alert' text
#'
#' @importFrom crayon make_style bold
#' @keywords internal

.alertCol <- function(...) .lightWhite(.darkRed(crayon::bold(..., sep='')))

#' Separator Bar
#'
#' Just a graphical bar to visually separate chunks of output
#'
#' @name DOTbar
#'
#' @param say Default \code{FALSE}, if TRUE will cause the bar to be
#'     wrapped in \code{message()}
#' @param newline Default \code{TRUE}, which will add a terminal
#'     newline, unless say is TRUE
#'
#' @importFrom crayon make_style
#' @keywords internal

.bar <- function(say=FALSE, newline=TRUE) {
    rv <- .lightWhite(.darkBlue(paste(rep('-',35), collapse=' ')))
    if (say) {
        message(rv)
    } else if (newline) {
        rv <- paste0(rv, "\n")
    }
    invisible(rv)
}

#' Safe Colored Newline
#'
#' Weird things can happen if ANSI colors 'go across' a newline in a terminal
#'
#' @details
#'
#' FUN FACT: ANSI color codes that span a new line can apparently add
#' explicit whitespace to a terminal. In this case, I am concatenating
#' a set of quoted files, one per line, with command continuity
#' provided by the '\' character. This is part of a git shell command,
#' and I want all the components to be magenta colored to help hint
#' the user that they should 'copy all the purple stuff and paste it
#' into a shell'. If I allow the newline to also be colored, then a
#' terminal-width's worth of spaces comes after it. The trailing
#' whitespace after the slash upsets git, which complains:
#'
#'   \code{fatal: pathspec ' ' did not match any files}
#'
#' So. All this does is build the newline-separated purple string, but
#' without coloring the newline as well.
#' 
#' @param x Required, the things that are going to be written on each
#'     line
#'
#' @keywords internal

.safeColorNewline <- function(x) {
    ## Possibly the dorkiest code I've written, a purple apostrophe:
    .actPos <- .actCol("'")
    ## The assembled sprintf format:
    .actNL  <- paste0(" ", .actCol('\\'), "\n", "    ", .actPos, '%s', .actPos)
    sprintf(.actNL, x)
}
