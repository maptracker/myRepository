## See also:
## https://cran.r-project.org/doc/manuals/R-admin.html#Setting-up-a-package-repository


#' My Repository
#'
#' Utilities aiding maintenance of your own internal R package repository
#'
#' @docType package
#'
#' @details
#'
#' This package is designed to support the maintenance of an internal
#' package repository (that is, your own version of CRAN). It contains
#' scripts to help you set up a new repository (local filestructure),
#' and then share configuration settings with colleagues so that they
#' can transparently install packages from your repository, as well as
#' make contributions to it.
#'
#' While this package could be of utility to hardened R programmers,
#' much of it is targetted at helping those new to the language. 
#'
#' The package relies heavily on frameworks contributed by other R
#' programmers, in particular \pkg{devtools}, \pkg{roxygen2} and
#' \pkg{testthat}. Some methods, such as \code{\link{buildAndCheck}}
#' are relatively thin wrappers around imported methods from these
#' packages.
#'
#' The main functions by end users, in the typical order of their usage, are:
#'
#' \itemize{
#'
#'   \item \code{source("http://example.com/stuff/OurRepo/configureRepo.R")} -
#'   Point your R session to your organization's repository
#'
#'   \item \code{\link{rememberMyRepo}} - Store settings in .Rprofile
#'   so you don't have to keep manually sourcing them.
#'
#'   \item \code{\link{findMyUpdates}} - Show packages from your
#'   internal repository that you've installed but have a newer
#'   version available.
#'
#'   \item If you just plan to \emph{use} the repository, nothing else
#'   needs to be done. The following points are if you also wish to
#'   \emph{develop} new packages
#'
#'   \item \code{\link{createNewPackage}} - Set up the directory
#'   structure to support a new package
#'
#'   \item \code{\link{currentPackageStatus}} - Run before editting a
#'   package, to alert you if the version number should be
#'   incremented.
#'
#'   \item \code{\link{incrementPackageVersion}} - Utility to bump up
#'   your package version number and modified date.
#'
#'   \item \code{\link{buildAndCheck}} - Compile your package into a
#'   tar.gz file, run checks and tests, and generate PDF
#'   documentation.
#'
#'   \item \code{\link{addPackageToRepo}} - Publish your built tar.gz
#'   package to your repository
#'
#'   \item \code{\link{buildRepoIndex}} - Update the repository server
#'   (file system) to include refreshed package indices plus a nice
#'   tabular view of all available packages on the landing URL.
#'
#' }
#'
#' @section Setting up a repository:
#'
#' This section is for R administrators setting up a repository for
#' the first time. Everyone else can ignore it.
#'
#' The function \code{\link{myRepoInitialize}} will establish an empty
#' framework to hold (and serve) your packages. It will also codify
#' your configuration (local file paths, URL, names and descriptions)
#' into a configuration script. This script can be sent to new users
#' to allow them to transparently use the repository.
#'
#' @section Utilizing the repository:
#'
#' When the repository is set up, it will generate an R configuration
#' script that will configure your R session to know the location of
#' the repository. You can then install its packages by simply using
#' \link{install.packages}, without needing to specify the
#' \code{repos} option.
#'
#' A helper utility, \code{\link{rememberMyRepo}}, will inject the
#' source call into your .Rprofile so it is automatically run when you
#' start a new R session.
#'
#' @section Developing new modules:
#'
#' Use the \code{\link{createNewPackage}} function to initialize a new
#' package with a directory structure recognized by
#' \code{myRepository} (includes sibling directories for tar.gz files
#' and PDF documentation).
#'
#' Use \code{\link{currentPackageStatus}} before you start editting
#' code, and then \code{\link{incrementPackageVersion}} if you need to
#' bump up the version number.
#'
#' Run \code{\link{buildAndCheck}} to build the package, run checks,
#' run tests, create PDF documentation, and alert you to any issues.
#'
#' @section Publishing modules:
#'
#' Provided you have the repository's file system locally mounted (or
#' can move your tar.gz to a server where it is), you can then use
#' \code{\link{addPackageToRepo}} to copy it into the repository, and
#' \code{\link{buildRepoIndex}} to update it.
#'
#' \emph{PLEASE} do not manually copy the tar.gz! This is technically
#' possible, but ends up causing significant headache due to
#' permissions mismatches.
#'
#' @section Other exported functions:
#'
#' \itemize{
#'   \item \link{getMyManifest} - Get PACKAGES file as structured list
#'   \item \link{myRepoPath} - Local file system path to your repo
#'   \item \link{shareMyRepo} - Make boilerplate invitation text
#'   \item \link{myRepoURL} - URL to your repository's landing page
#'   \item \link{myRprofile} - Find your .Rprofile file path
#'   \item \link{alterFile} - Utility to conditionally append text to a file
#'   \item \link{packageDir} - Detects R subdirectory in git repo
#'   \item \link{packageDir} - Detects R subdirectory in git repo
#'   \item \link{currentPackageName} - Name of the package for your current dir
#'   \item \link{currentPackageVersion} - Show version number of current package
#'   \item \link{parseDescriptionFile} - key/value pairs from a DESCRIPTION file
#'   \item \link{makePDF} - Make a PDF documentation file from a tar.gz
#'   \item \link{myRepoSettings} - Get a list of settings for this repo
#' }
#'
#'
#' @seealso The primary functions to consider are:
#'     \code{\link{rememberMyRepo}}, \code{\link{createNewPackage}},
#'     \code{\link{currentPackageStatus}},
#'     \code{\link{incrementPackageVersion}}, and
#'     \code{\link{buildAndCheck}}. If you are adding to the repo,
#'     also see \code{\link{addPackageToRepo}} and
#'     \code{\link{buildRepoIndex}}.
#'
#' 
#' @examples
#'
#' \dontrun{
#' # Start by getting the URL of your internal repository (talk to
#' # your colleagues). Visiting that URL in a browser will show you
#' # the commands to run. If you can't visit the site in a browser, it
#' # won't work to serve packages either! You'll then run something
#' # like:
#'
#' source("http://example.com/this/that/AcmeRAN/configureRepo.R")
#'
#' ## If it carried a package called TickTock, you could then just:
#' install.packages("TickTock") # no need for repos=c(...) !
#'
#' ## If you haven't done so already, and want to use more features,
#' ## you can then:
#'
#' install.packages("myRepository")
#'
#' ## To avoid manually sourcing the configuration, run:
#' rememberMyRepo()
#' }
"_PACKAGE"


## Users that can not modify the repository.
forbiddenUsers <- c("root")
## Optional whitelist of users allowed to modify the repository. Not
## sure how to securely manage this as an option
allowedUsers   <- character()

dMode          <- '1777' # Directory mode

#' My Repository Initialization
#'
#' Set up files and configuration scripts for your own R repository
#'
#' @details
#'
#' This method is used to initialize your local repository. It
#' collects various descriptive components and builds them into an
#' options configuration script that can be sent to your users to
#' allow them to transparently install your internal packages.
#'
#' Generally, this method needs to be run only once to set up the
#' repository. It should be run on a system that has the repository
#' file system locally mounted, since it will be establishing
#' directory structure and copying over some template files.
#'
#' @param path Required. The local filesystem path that will 'hold'
#'     your repository.
#' @param url Default NULL, \strong{strongly recommended}. This is the
#'     HTTP-accessible URL that corresponds to \code{path}. It allows
#'     users to install packages over the network, even if they are
#'     not on the local filesystem that stores the repository
#'     (provided you have a properly configured webserver serving the
#'     relevant part of the filesystem). You can run a repository
#'     without this, but it will require your users to have the
#'     repository file system mounted.
#' @param name Default "MyRepo". A short name for your repository,
#'     analagous to "CRAN" used to identify the public R
#'     repository. Should only have letters, numbers, underscores and
#'     dashes.
#' @param description Default "My internal repository". A short
#'     sentance describing your repository in a little more detail
#'     than name.
#' @param cran Default is set to the current 'CRAN' value of your
#'     "repos" option. If this is \code{NA}, will be set to
#'     '@CRAN@'. If it ends up as '@CRAN@', you will be reminded that
#'     it will result in users being asked each session to pick a
#'     specific CRAN URL whenever they install software. In our
#'     experience it is generally more convienent to pick a single
#'     CRAN repository that has performed reliably for your users.
#' @param group Default \code{NULL}. Optional Linux group to assign to
#'     user-uploaded files. Attempts will be made to assure that files
#'     and directories belong to this group, if provided. This can be
#'     neccessary to allow updates to regenerated files by a diverse
#'     set of contributing users. Group membership reassignment does
#'     not always work for reasons that are not yet understood.
#' @param orgdomain Default \code{NULL}. Optional organizational
#'     domain name, eg 'example.com'. Used when building hyperlinks to
#'     open an email to a package maintainer; If the email address
#'     does not match this domain, the email message that opens will
#'     include text in the body warning the sender that the author is
#'     outside their team's domain (that is, don't accidentally reveal
#'     internal secrets to an outsider).
#' @param releases Default 'packageReleases'. An auto-maintained
#'     directory "at the same level" as the package directory that
#'     holds built .tar.gz files.
#' @param documentation Default 'packageDocumentation'.
#'     Auto-maintained directory "at the same level" as the package
#'     directory that holds aggregate PDF man documentatuion.
#'
#' @importFrom CatMisc is.something
#'
#' @examples
#'
#' tmpd <- tempdir()
#' tmpd <- file.path(tmpd,'testRepository')
#' myRepoInitialize( path=tmpd, name='testingRepo' )
#'
#' @export

myRepoInitialize <- function (
    path, url=NULL, name='MyRepo', description='My internal repository',
    cran=getOption("repos")["CRAN"], group=NULL, orgdomain=NULL,
    releases='packageReleases', documentation='packageDocumentation') {

    if (missing(path)) {
        .wrapMsg("You must provide path, the *local* filesystem directory that contains your repository", color="red")
        return(NA)
    }
    if (length(path) != 1) {
        .wrapMsg("The path parameter should be a single value", color="red")
        return(NA)
    }
    if (!dir.exists(path)) {
        ## The requested installation directory does not exist
        par <- dirname(path)
        if (dir.exists(par)) {
            ## The parent does - can we create the request?
            if (dir.create(path)) {
                .wrapMsg("Installation directory created:\n  ",
                         .fileCol(path), color="green")
            } else {
                .wrapMsg("While the parent directory exists, I could not create the installation folder:\n    ", path,
                     "\n  This is likely a permissions issue on the parent, please investigate", color="red")
            return(NA)

            }
        } else {
            .wrapMsg("Neither the requested path nor its parent directory exists:\n    ", path,
                     "\n  Please create either the directory or its parent, or pick a new location", color="red")
            return(NA)
        }
    }

    if (grepl("[^a-z0-9_\\-]", name, ignore.case=TRUE)) {
        .wrapMsg("Please pick a repository name that is only letters, numbers underscores and dashes. You picked:",  name, color="red")
        return(NA)
    }

    cranNames <- "CRAN"
    if (CatMisc::is.something(cran)) {
        nms <- names(cran)
        ## If the user has provided a named vector for CRAN, use
        ## the names provided
        if (!is.null(nms)) {
            cranNames <- nms
        } else if (length(cran) > 1) {
            ## Eh. Two or more 'cran' repos, no distinguishing
            ## names. For lack of a better option, name them CRAN1,
            ## CRAN2, etc
            cranNames <- sprintf("CRAN%d", seq_len(length(cran)))
            .wrapMsg("Multiple CRAN repos have been defined. They have been imaginatively named:", .quoteVector(cranNames), "You can provide a named vector if you wish to define your own names", color='yellow')
        }
    } else {
        ## If the variable is not defined, use the special pick-your-own value
        cran <- '@CRAN@'
    }
    if (all(cran == '@CRAN@')) {
        .wrapMsg("Setting your CRAN repos path to", cran,
                 "This will ask you to pick a CRAN mirror once per session when you install packages. You may wish to define an single explict CRAN mirror to avoid this")
    }

    setupSources <- c(path)
    if (CatMisc::is.something(url)) {
        setupSources <- c(setupSources, url)
    } else {
        .wrapMsg("Setting your local repository to your file path:", path,
                 "This will not allow packages to be installed from the web. For optimal use of your repository, it is advised that you set up a web server and provide the URL 'pointing to' your path folder", color="yellow")
        url <- path
    }

    if (!CatMisc::is.something(group)) {
        .wrapMsg("No group specified. Setting a group allows submitted files to be assigned memberhip in a common Linux group, which can prevent issues in managing permissions of files in the repository. You may need to manually manage permissions of some files (like index.html) without a group being set.", color="blue")
        group <- ""
    }
    if (!CatMisc::is.something(orgdomain)) orgdomain <- ""

    if (!CatMisc::is.something(releases)) {
        .wrapMsg("No release folder specified", color="blue")
        releases <- ""
    }
    if (!CatMisc::is.something(documentation)) {
        .wrapMsg("No documentation folder specified", color="blue")
        documentation <- ""
    }


    optList <- list(NAME=name, DESCRIPTION=description,
                    PATH=path, URL=url, CRAN=.quoteVector(cran),
                    GROUP=group,
                    ORGDOMAIN=orgdomain,
                    RELEASES=releases, DOCUMENTATION=documentation,
                    REPONAMES=.quoteVector(c(name, cranNames)),
                    REPOURLS=.quoteVector(c(url, cran)))

    ## The resources directory holds utility files
    rDir <- file.path(path, "resources")
    if (!dir.exists(rDir)) dir.create(rDir, recursive=TRUE,
                                      showWarnings=FALSE, mode=dMode)

    
    .copyTemplateFile("resourcesIndex.html", rDir, optList, newname="index.html")
    for (subdir in c("extracted_PDF", "extracted_DESCRIPTION")) {
        ## Utility folders holding unpacked DESCRIPTION files and
        ## auto-generated PDF files
        d <- file.path(rDir, subdir)
        if (!dir.exists(d)) dir.create(d, mode=dMode)
    }
    
    ## Archive folder for holding packages removed from src/contrib:
    aDir <- file.path(path, "archive")
    if (!dir.exists(aDir)) dir.create(aDir)
    .copyTemplateFile("README_repoArchive.md", aDir,optList, newname="README.md")
    
    lastSrc <- gsub('/$', '', setupSources[length(setupSources)])

    confFile <- "configureRepo.R"
    optList$CONFSCRIPT <- paste(lastSrc, basename(rDir), confFile, sep='/')

    ## Folder for holding versions of the configuration scripts:
    csSubdir <- "ConfigurationScripts"
    csPath   <- file.path(rDir, csSubdir)
    if (!dir.exists(csPath)) dir.create(csPath, recursive=TRUE,
                                        showWarnings=FALSE, mode=dMode)
    .copyTemplateFile("README_ConfScripts.md", csPath, newname="README.md")
    
    ## Version the script:
    stamp     <- format(Sys.time(), "%Y-%m-%d")
    stampName <- sprintf("configureRepo_%s.R", stamp)
    cf <- .copyTemplateFile(confFile, csPath, optList, clobber=FALSE,
                            newname=stampName)
    ## Now symlink the versioned file to an unversioned one within the
    ## main resources folder:
    nd <- getwd()
    setwd(rDir)
    file.link(file.path(csSubdir, stampName), confFile)
    setwd(nd)

    source(cf)
    myRepoSettings(force=TRUE)
    
    ## Make an initial blank index document.
    .copyTemplateFile("blankIndex.html", path, optList, clobber=FALSE,
                      newname="index.html")

    ## Assure the src/contrib folder exists
    ## Set the option:
    cDir <- .opt('Contrib',  file.path(.opt('Path'), "src", "contrib"))
    if (!dir.exists(cDir)) dir.create(cDir, recursive=TRUE,
                                      showWarnings=FALSE, mode=dMode)
    ## Make a blank log file if it is not already there
    if (!file.exists(.opt('Log'))) cat("Date\tVersion\tUser\tHost\tAction",
                                     file=.opt('Log'), append=TRUE, fill=TRUE)

    ## File with packages flagged for 'silent' processing (complaints,
    ## particularly for PDF generation, will not be shown to user)
    .copyTemplateFile( "ignoreErrors.txt", rDir, optList, clobber=FALSE)
    ## Table header HMTL for package index:
    .copyTemplateFile( "tableHeader.html", rDir, optList, clobber=FALSE)
    
    .wrapMsg("Your repository folder structure has been created here:",
             .fileCol(path),
             "The setup script can be accessed via:",
             .actCol(sprintf("   %s/%s/%s", setupSources, basename(rDir), confFile)),
             "\nsource()ing the script will configure access to the repo")
    invisible(get(optVar, envir=packEnv))
}

#' My Repository Settings
#'
#' Collects / returns options associated with your internal repository
#'
#' @details
#'
#' The 'primary' settigns are extracted from package-specific
#' \code{options()}, which in turn are initially defined by the script
#' created in \code{\link{myRepoInitialize}}.
#'
#' This function will parse the high-level options, generate some
#' derived values (eg subfolder paths like src/contrib) and package
#' them into a list structure.
#'
#' @param force Default \code{FALSE}, which will not re-run
#'     the... uh... the reasons this parameter should be here appear
#'     to have gone away in an update. Yet I have propogated code to
#'     my colleagues that uses the parameter. So it's going to stay,
#'     quietly doing... nothing.
#' @param quiet Default \code{FALSE}. If TRUE, then summary messages
#'     will not be printed. Used when doing snap checks to see if new
#'     options have been provided after loading the library.
#'
#' @return A list
#'
#' @seealso \code{\link{myRepoInitialize}}
#'
#' @importFrom CatMisc is.something
#' @importFrom crayon magenta
#' @export

myRepoSettings <- function(force=FALSE, quiet=FALSE) {
    notSet <- character()
    rv     <- list( )
    myOpts <- getOption("myRepoOptions")
    if (is.null(myOpts)) myOpts <- list()
    for (var in c("Name", "Description", "Path", "URL", "Configure",
                  "CRAN", "Group", "OrgDomain", "Releases", "Documentation")) {
        val <- myOpts[[ var ]]
        if (CatMisc::is.something(val)) {
            rv[var] <- val
        } else {
            notSet <- c(notSet, var)
        }
    }
    path <- .opt('Path', rv$Path)
    if (CatMisc::is.something(path)) {
        ## A local path is defined. Set some derived values. The
        ## 'contrib' folder, where tar.gz files will go. We store that
        ## as an option now, since it will be needed by .localOffline
        .opt("Contrib", rv$Contrib <- file.path(path, "src", "contrib"))
        ## An activity log file
        rv$Log <- file.path(path, "resources", "actionLog.txt")
    }
    rv$localOk <- .localOffline("")
    rv$Package <- 'myRepository'

    if ((is.null(path) || is.null(rv$URL)) && !quiet) {
        .wrapMsg("The following settings were not defined in your options:",
                 paste(notSet, collapse=', '),
                "Your next steps to use the myRepository package are:",
                paste("*  You are an end user: ",
                      crayon::magenta("Contact your in-house R experts")),
                paste("*  You *ARE* the in-house R expert: ",
                      .actCol("?myRepoInitialize")), color="red")
    }
    if (is.null(rv$Name)) rv$Name <- "NameNotSet"
    if (is.null(rv$Description)) rv$Description <- "My Personal Repository"
    ## Set the "package level" variable opts to the calculated value
    for (key in names(rv)) {
        .opt(key, rv[[ key ]])
    }
    invisible(rv)
}

#' On Load
#'
#' onLoad function to instantiate the opts list
#'
#' @name dotOnLoad
#'
#' @details
#'
#' Sets up the \code{opts} structure using \code{myRepoSettings}
#'
#' @seealso Via https://stackoverflow.com/a/37821763
#' 
#' @keywords internal

## Initial configuration on package load
.onLoad <- function(libname, pkgname) {
    myRepoSettings()
}

#' shareMyRepo
#'
#' Builds boilerplate message to set up a colleague to use your repository
#'
#' @details
#' 
#' This just generates a block of R code that you can paste into a
#' message and send to a new R user to help them set up to use your
#' internal repository
#'
#' @param name Default 'colleague', can be set to whatever greeting
#'     you'd like. Since this function is just generating a block of
#'     (editable) text, it's really just a cosmetic parameter.
#' 
#' @examples
#'
#' shareMyRepo( name='Sam' )
#'
#' @export

shareMyRepo <- function( name='colleague') {
    name <- name[1]
    msg <- sprintf("# Hey, %s!,
# Please run this block of code in R to set up your local environment to use
# our internal '%s' repository:

source('%s')

# This will let you install our internal R packages using install.packages()
# with no additional parameters. If you wish to use additional development
# tools, you could next run:

install.packages('myRepository')

# If you wish to have these settings always applied when you start R, run:

rememberMyRepo()
", name, .opt('Name'), .opt('Configure'))

    message(msg)
    invisible(msg)
}

#' addPackageToRepo
#'
#' Add an R package to the repository
#'
#' @details
#'
#' The core action of this function is to copy a tar.gz file
#' containing a built package into the appropriate part of your
#' repository's file structure (src/contrib). The function also
#' performs some sanity checking. Checks include:
#'
#' \itemize{
#'   \item Verifying the local file structure is available
#'   \item The file is at least named as a .tar.gz archive
#'   \item The file is not clobbering a different file with the same name
#' }
#'
#' On succesful copy, the user will be reminded that the next step is
#' \code{updateMyRepo()}
#' 
#' @param tarfile Required. The file path(s) for the .tar.gz packages
#'     generated by \code{R CMD build}
#'
#' @return \code{file.info} data for the file copied to (or already
#'     present in) the repository, or \code{NA} if the copy failed,
#'     structured as a list named after the basename of each request.
#'
#' @seealso \code{\link{buildAndCheck}}, \code{\link{updateMyRepo}}
#'
#' @examples
#'
#' \dontrun{
#' ## Copy an R package to our repository
#' addPackageToRepo("~/Downloads/drivePerformance_2.62.tar.gz")
#' }
#' 
#' @importFrom openssl sha256
#' @export

addPackageToRepo <- function (tarfile) {
    if (.localOffline("add a package to the repository")) return(NA)
    rv <- list()
    for (ti in seq_len(length(tarfile))) {
        tf <- tarfile[ti]
        ## Extract the filename from the tar file
        file     <- basename(tf)
        pkgName  <- gsub("_.+?$", "", file)
        if (!is.null(rv[[ file ]])) {
            .wrapMsg(paste("File submitted twice, ignoring duplicates: ",
                           file), color='yellow')
            next
        }

        if (!grepl('\\.tar\\.gz', tf)) {
            .wrapMsg("The submitted file must be a .tar.gz R package archive. If you have not yet started writing your package, see ?createNewPackage. If you have source code already, see ?buildAndCheck for help in building the archive.", color='red')
            .logMessage(paste("ERR: Attempt to add incorrect format: ", file))
            rv[[file]] <- NA
            next
        }
        
        if (!file.exists(tf)) {
            .wrapMsg("Submitted tarfile does not appear to exist, or is in a different folder than provided. Perhaps double check your current directory (getwd())?", tf, color='red')
            .logMessage(paste("ERR: Non-existent file: ", file))
            rv[[file]] <- NA
            next
        }
        
        ## Where the file will be copied to:
        repoPath <- file.path(.opt('Contrib'), file)

        info <- NA
        if (file.exists(repoPath)) {
            ## A file of the same name is already in the contrib
            ## folder. Does it have the same content? If so, we won't
            ## chastize the user.
            
            ## Hashing files (instead of R objects) is a bit of a
            ## pain. I failed to do so with digest::digest, but
            ## openssl has functions that work well:           
            ##   https://cran.r-project.org/web/packages/openssl/vignettes/crypto_hashing.html
            old <- openssl::sha256(file(repoPath))
            new <- openssl::sha256(file(tf))
            info <- file.info(repoPath)
            if (all(old == new)) {
                .logMessage(paste("Note: Resubmission of archive: ", file))
                .wrapMsg("Submitted (and identical) package is already in the archive",
                         repoPath, sprintf("Last modified: %s", info$mtime),
                         "No action taken (or needed). Next step:",
                         .actCol("  updateMyRepo()"), color='yellow')
            } else {
                ## Note that if identical code is re-built, it will
                ## still be "different" because the package includes
                ## an internal build time stamp (I think).
                .logMessage(paste("ERR: Attempt to replace existing file: ", file))
                .wrapMsg("A package with the same name BUT DIFFERENT CONTENT already exists in the repository.", repoPath, sprintf("Last modified: %s", info$mtime),
                         "No action taken! Maybe you needed to increment the version number before building? See ?incrementPackageVersion for a utility that does so.", color='red')
            }
        } else {
            ## Copy it to the 'src/contrib' sub-directory
            ok <- file.copy( tf, repoPath )
            if (all(ok)) {
                info <- file.info(repoPath)
                message("Package added to repo, next step:\n  ",
                        .actCol("updateMyRepo()"))
                .logMessage(paste("Add package: ", file))
            } else {
                .logMessage(paste("ERR: Failed to add package: ", file))
                .wrapMsg(sprintf("Failed to copy %s package %s to %s\n",
                                 pkgName, tf, .opt('Contrib')), color='red')
            }
        }
        rv[[file]] <- info
    }
    invisible( rv )
}

#' updateMyRepo
#'
#' Update the repository to process all packages stored in the file system
#'
#' @details
#'
#' Will assure that subdirectories ("windows", "macosx", etc) are
#' present, then will run \code{tools::write_PACKAGES()} to properly
#' populate them. Will automatically create the R-version subdirectory
#' of the currently running R process. Will only work if the
#' repository's local filesystem is available!
#'
#' @param clearrds Default \code{TRUE}. At some point (somewhere after
#'     3.3.1, but I'm not sure where exactly) R added a "PACKAGES.rds"
#'     file to the \link[tools]{write_PACKAGES} output, which is generated
#'     in addition to the "PACKAGES" and "PACKAGES.gz" files. If a
#'     user runs \code{updateMyRepo()} on a newer version of R, then
#'     later another user updates using an older version of R, the
#'     .rds file will NOT get updated. This makes the .gz and .rds
#'     manifests unsynchronized. Because the RDS file appears to be
#'     "useful but not neccesary", default behavior is to delete
#'     it. If you're confident that all your users are utilizing a
#'     more modern version of R that generates the RDS file, you can
#'     set the flag to FALSE to preserve the file.
#' 
#' @seealso
#' 
#' Most of this code is taken from an RStudio \code{packrat} HowTo at
#' <URL:https://rstudio.github.io/packrat/custom-repos.html>
#'
#' @examples
#'
#' \dontrun{
#' ## Update the manifest in our repository.
#' updateMyRepo()
#' }
#' 
#' @importFrom tools write_PACKAGES
#' @importFrom CatMisc parenRegExp is.something
#' @importFrom utils old.packages
#' @importFrom crayon red green
#' 
#' @export

updateMyRepo <- function ( clearrds=TRUE ) {
    ## This should be run whenever a package is added to / updated in
    ## the repository
    if (.localOffline("update the repository registry")) return(NA)

    old <- old.packages()
    mrn <- .opt('Package')
    if (is.element(mrn, rownames(old))) {
        ## It looks like an update is available for this package
        info <- old[mrn, c("Installed", "ReposVer", "Repository")]
        locVers <- info["Installed"]
        repVers <- info["ReposVer"]

        ## We are going to use the first two version numbers only for
        ## determining if the package is stale. That is, for "1.2.31"
        ## we will only compare "1.2". Minor version values will be
        ## bumped for changes that alter how the repo file system is
        ## manged; Fixes to repository handling, alterations to
        ## index.html or supporting files, etc.

        lv2 <- CatMisc::parenRegExp("^(\\d+\\.\\d+)\\.", locVers)
        if (!is.na(lv2[1])) locVers <- lv2
        rv2 <- CatMisc::parenRegExp("^(\\d+\\.\\d+)\\.", repVers)
        if (!is.na(rv2[1])) repVers <- rv2
        if (compareVersion(locVers, repVers) == -1) {
            ## Going to require the user to update
            .wrapMsg(sprintf("Your installation of %s needs to be updated from version %s to %s. This is to assure consistency in the configuration of the repository when updated by multiple users.", mrn, locVers, repVers), "Please install the new version using:",
                     .actCol("  install.packages('", mrn,"')"),
                     sprintf("... then %s (or start another R session) and update again:", crayon::red("restart R")),
                     .actCol(sprintf("  library('%s'); updateMyRepo()", mrn)), color='yellow' )
            .logMessage(paste("WARN: Update demand: ", locVers, '->', repVers))
            return(invisible(NA))
        }
    }

    user <- Sys.info()[ "user" ]

    if (is.element(user, forbiddenUsers)) {
        ## Yeah. Things break if root updates the repository.
        .wrapMsg("You can not update the repository as user ", user, color='red')
        .logMessage(paste("ERR: Forbidden user: ", user))
        return(invisible(NA))
    }

    if (length(allowedUsers) > 0 && !is.element(user, allowedUsers)) {
        .wrapMsg("Your user ID is not allowed to update the repository",
                 user, color='red')
        .logMessage(paste("ERR: Unallowed user: ", user))
        return(invisible(NA))
    }
    
    pckRds <- file.path(.opt('Contrib'), "PACKAGES.rds")
    if (file.exists(pckRds) && clearrds) {
        ## Clear the RDS version of PACKAGES to prevent
        ## synchronization issues between newer and older versions of
        ## R
        if (!file.remove(pckRds)) {
            .logMessage("ERR: Could not remove RDS manifest")
            .wrapMsg("MAJOR ERROR! Failed to remove RDS manifest!",
                     pckRds, "This file is not essential, but its presence can cause consistency issues with older versions of R! Failure is likely due to your lacking permissions on the filesystem. You can try to delete the file manually. Alternatively, if you are *CERTAIN* that all your users are utilizing more recent versions of R that recognize PACKAGES.rds, you can update again with the `clearrds` flag set to `FALSE`. If your users complain that they can't 'see' newer versions of packages that you 'know' are present in the repository, it is likely due to this issue.", color='red')
            stop("Halting update")
        }
    }

    ## Before we build the "real" manifest we should first identify
    ## older packages and tuck them away in the archive folder:
    buildRepoArchive()
    
    ## --------------------------- Code via packrat:
    ## https://rstudio.github.io/packrat/custom-repos.html
    ## Put the binary folder structure in place:
    rVersion <- paste(unlist(getRversion())[1:2], collapse = ".")
    macPath  <- "bin/macosx"
    binPaths <- list(
        win.binary = file.path("bin/windows/contrib", rVersion),
        mac.binary = file.path(macPath,"contrib", rVersion),
        mac.binary.mavericks = file.path(macPath,"mavericks/contrib", rVersion),
        mac.binary.leopard = file.path(macPath,"leopard/contrib", rVersion)
    )

    binPaths <- lapply(binPaths, function(x) file.path(.opt('Path'), x))
    lapply(binPaths, function(path) {
        dir.create(path, recursive=TRUE, showWarnings=FALSE, mode=dMode)
    })
    
    ## Write the PACKAGES file for each sub-directory. This file is
    ## used by R’s functions like install.packages() for querying
    ## information about the repository, and what packages are
    ## actually available.

    message(crayon::red("  write_PACKAGES()... may take some time..."))
    tools::write_PACKAGES(.opt('Contrib'), type = "source")
    rv <- lapply(binPaths, function(path) {
        tools::write_PACKAGES(path)
    })
    message(crayon::green("    Done."))
    ## ---------------------------

    ## Now we need to make sure the file and dir groups have
    ## team-friendly permissions, and optionally are assigned to the
    ## group specified in our settings.
    
    if (.Platform$OS.type == 'unix') {
        ## file.mode() can be used to set permissions on files and
        ## directories. However, it was not always working; Failure
        ## was sporadic and no obvious cause was found. So on *nix
        ## systems I am using find to locate files, and Sys.chmod to
        ## change permissions.
        
        filePerm <- "0664"
        fixPerm  <- system2("find", c(.opt('Path'), "-type", "f",
                                      "-not", "-perm", filePerm), stdout=TRUE)
        vapply(fixPerm, Sys.chmod, TRUE, mode=filePerm, use_umask=FALSE)

        dirPerm  <- "0777"
        fixDir   <- system2("find", c(.opt('Path'), "-type", "d",
                                      "-not", "-perm", dirPerm), stdout=TRUE)
        vapply(fixDir, Sys.chmod, FALSE, mode=dirPerm, use_umask=FALSE)

        if (CatMisc::is.something(.opt('Group'))) {
            ## User wants to assure that files and folders are
            ## assigned to a specific group. R does not have methods
            ## to change group assignment built in, so use system
            ## calls. Using `find` to locate files that are not
            ## already in the group and use -exec to chgrp them.
            fixGroup <- system2("find",
                c(.opt('Path'), "-not", "-group", .opt('Group'), "-exec",
                  "chgrp", .opt('Group'), "{} \\;"), stdout=TRUE)
            ## Find files not in a group: https://serverfault.com/a/132715
       }
    } else {
        ## Not really sure how to tackle this in Windows...
        .wrapMsg("Please assure that files and directories in this folder have permissions and group assignments that will allow collaborators to alter them as well.", .opt('Path'), color='yellow')
    }
    
    ## Update the index
    buildRepoIndex(file.path(.opt('Path'),"index.html"))
    
    .wrapMsg("Repository has been updated", color='cyan')
    .logMessage("Update repository")
    invisible(rv)
}


#' PACKAGES URL
#'
#' Return the URL to the PACKAGES file in your repository
#'
#' @name dotPACKAGES_URL
#'
#' @importFrom CatMisc is.something
#' 
#' 
#' @keywords internal

.PACKAGES_URL <- function () {
    ## This is the URL to the PACKAGES file in the repo
    base <- .opt('URL')
    if (CatMisc::is.something(base)) {
        paste(base, "src/contrib/PACKAGES" ,sep='/')
    } else {
        NA
    }
}

#' buildRepoIndex
#'
#' Create an HTML table of all packages currently offered by repository
#' 
#' @details
#'
#' Scans \code{src/contrib/PACKAGES} to find all packages in the
#' repository, and creates an HTML table listing them. The table is
#' built with \code{dynamictable} to allow for dynamic filtering and
#' sorting.
#'
#' If the repository can be accessed via file system, each package is
#' also unpacked to get at the DESCRIPTION file, in order to capture
#' description and maintainer information.
#'
#' @param file Default \code{NA}. The file path where the HTML
#'     document should be written. If NA, then a temporary file will
#'     be created.
#' @param display.url Default is TRUE If file is NA, otherwise
#'     FALSE. If TRUE, then R will attempt to display the resulting
#'     HTML in the user's browser.
#'
#' @return A data.frame with extracted package information
#'
#' @examples
#'
#' \dontrun{
#' buildRepoIndex()
#' }
#' 
#' @importFrom htmltools htmlEscape
#' @importFrom CatMisc is.something
#' @import dynamictable
#' @export

buildRepoIndex <- function (file=NA, display.url=ifelse(is.na(file[1]),TRUE,FALSE)) {
    df <- getMyManifest(cleanVersions=TRUE)
    if (is.null(df)) {
        .wrapMsg("Repo location appears to be undefined", color='red')
        return(NA)
    }
    subFactOpt   <- list(factor=list(min.level=2,
                                     textFilter=TRUE, subfactor='\\s*,\\s*'))
    ## Making sure recovered options aren't NULL or NA (unpleasantness happens):
    myName <- ifelse(CatMisc::is.something(.opt('Name')),
                     .opt('Name')[1], "OurRepo")
    myDesc <- ifelse(CatMisc::is.something(.opt('Description')),
                     .opt('Description')[1], "Our own internal R repository")
    myConf <- ifelse(CatMisc::is.something(.opt('Configure')),
                     .opt('Configure')[1], "ERROR-Conf-Script-Unknown!!")
    myPath <- ifelse(CatMisc::is.something(.opt('Path')),
                     .opt('Path')[1], "/dev/null")

    ## Read the header file, if present
    header <- character()
    hFile  <- file.path(.opt('Path'), "resources", "tableHeader.html")
    if (file.exists(hFile)) header <- readLines(hFile, warn=FALSE)
    ## Add a count of available packages:
    header <- c(header, sprintf("<i style='color: orange'>%d packages are available:</i><br>", nrow(df)))
    ## Collapse with newlines:
    header   <- paste(header, collapse="\n")
    ## For the footer, make links into the underlying file system:
    navFiles <- list.files(myPath, include.dirs=TRUE)
    navFiles <- navFiles[ navFiles != 'index.html' ]
    footer   <- paste(c(paste0("<i>Navigate ",myName," file hierarchy:</i>"),
                      sprintf("<a href='./%s'>%s</a>", navFiles,
                              htmltools::htmlEscape(navFiles))), collapse="<br>")

    ## Write to a temp file to prevent partially-formed files
    tmp <- paste(file, "tmp", sep='.')

### ToDo: Break out favicon as a file
    
    dynamictable(df, show.rownames=FALSE, show.attr=FALSE,file=tmp,
                 title=paste(myName,":", myDesc),
                 header=header, footer=footer, display.url=display.url,
                 options=
                     list(NeedsCompilation = list(name='Compile?',factor=TRUE),
                          Title       = list(truncate=80),
                          Description = list(truncate=50),
                          MD5sum      = list(hide = TRUE),
                          Depends     = subFactOpt,
                          Imports     = subFactOpt,
                          License     = subFactOpt,
                          Maintainer  = subFactOpt,
                          Suggests    = subFactOpt,
                          Enhances    = subFactOpt),
                 favicon="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQAgMAAABinRfyAAAACVBMVEUAAGcAZpn/mTO6g9PAAAAAAXRSTlMAQObYZgAAAAlwSFlzAAALEwAACxMBAJqcGAAAAAd0SU1FB+EDHxEpKUUGkV4AAAAIdEVYdENvbW1lbnQA9syWvwAAADlJREFUCNdjYBANYWBgCA1lYGB0YHRgYF21KoBBZNWqKQwBK7haocSqVaxQAi4GVgJWDNYGNgBkFAAZjhPjnJqxGQAAAABJRU5ErkJggg==")
    ## Move tempfile into expected location
    file.rename(tmp, file)
    invisible(df)
}

#' buildRepoArchive
#'
#' Generate the Meta/archive.rds structure that tracks older package versions
#' 
#' @details
#'
#' CRAN maintains a structure of all package versions named
#' \code{archive.rds} in
#' \url{https://cran.r-project.org/src/contrib/Meta/}. This object is
#' required for \link[devtools]{install_version} to identify older
#' packages that can be installed from a repository. The repository
#' will hold these older packages in a directory at
#' \code{/src/contrib/Archive}, with each package having its own
#' subdirectory, holding \code{.tar.gz} packages.
#'
#' The RDS object is structured as a list of data frames:
#'
#' \itemize{
#' 
#'   \item The names of the list are 'plain' package names, eg 'stilt'
#' 
#'   \item Each row in the data.frame coresponds to an archived package
#'
#'   \item The rownames of the DF are file paths for the
#'     \code{.tar.gz} files, eg \code{stilt/stilt_1.1.0.tar.gz}. Note that
#'     these paths are relative to the \code{Archive} directory.
#'
#'   \item The DF columns are the values returned by \link[base]{file.info}
#' 
#' }
#'
#' Note: For reasons that aren't clear to me, CRAN names its archive
#' folder \code{00Archive}. I don't know what points \code{devtools}
#' to recognize that variant, but on BRAN it goes looking for
#' \code{Archive}.
#'
#' @param clean Default \code{FALSE}. Normally operations are only
#' done on \code{src/contrib}. If clean is TRUE, then \code{Archive}
#' will be scanned to assure that the archived files there are
#' consistent with \code{archive.rds}
#' 
#' @return \code{NA} if no changes were made (or if problems
#' occur). Otherwise, a data.frame with all tar.gz packages that were
#' moved to Archive. All values are returned invisibly.
#'
#' @examples
#'
#' \dontrun{
#' buildRepoArchive()
#' }
#' 
#' @importFrom CatMisc is.something parenRegExp
#' @importFrom utils compareVersion
#' @export

buildRepoArchive <- function(clean=FALSE) {
    if (.localOffline("update the repository archive")) return(NA)

    if (clean) stop("buildRepoArchive(clean=TRUE) functionality not ready")
    
    user <- Sys.info()[ "user" ]
    if (is.element(user, forbiddenUsers)) {
        ## Root shouldn't be doing this!
        .wrapMsg("You can not update the repository as user ",
                 user, color='red')
        .logMessage(paste("ERR: Forbidden user: ", user))
        return(invisible(NA))
    }
    if (length(allowedUsers) > 0 && !is.element(user, allowedUsers)) {
        .wrapMsg("Your user ID is not allowed to update the repository",
                 user, color='red')
        .logMessage(paste("ERR: Unallowed user: ", user))
        return(invisible(NA))
    }

    ## Ok. First we need to assure that the Meta directory is present
    cdir <- .opt('Contrib')
    mdir <- file.path(cdir, "Meta")
    if (!dir.exists(mdir)) {
        dir.create(mdir, recursive=TRUE, showWarnings=FALSE, mode=dMode)
        if (!dir.exists(mdir)) {
            .wrapMsg("Failed to create Meta directory", mdir, color='red')
            .logMessage(paste("ERR: Meta dir create failed: ", mdir))
            return(invisible(NA))
        }
    }

    ## Path to the archive RDS file:
    ards <- file.path(mdir, "archive.rds")
    ## If the archive exists read it in, otherwise start fresh
    arch <- if (file.exists(ards)) { readRDS(file=ards) } else { list() }

    ## Ok. Now we need to read all package tar.gz files, organize them
    ## by pacakge name, identify the most recent version, leave it
    ## alone, and move the rest to archive:
    nowLive <- list.files(cdir, pattern=".*\\.tar\\.gz$")
    pkgVer  <- CatMisc::parenRegExp("^(.+?)_([0-9.\\-]+)\\.tar\\.gz$", nowLive)
    ## Easier to manage as a matrix:
    pkgVer  <- matrix(pkgVer[!is.na(pkgVer)], ncol=2, byrow=TRUE)
    colnames(pkgVer) <- c("Package", "Version")
    byPkg   <- list()
    for (r in seq_len(nrow(pkgVer))) {
        pkg <- pkgVer[r, "Package"]
        ver <- pkgVer[r, "Version"]
        if (is.null(byPkg[[ pkg ]])) {
            ## New package, instantiate with this version
            byPkg[[ pkg ]] <- ver
        } else {
            ## Saw this package already
            byPkg[[ pkg ]] <- c(byPkg[[ pkg ]], ver)
        }
    }
    toArchive <- list()
    kept      <- list()
    for (pkg in names(byPkg)) {
        ## Wow. It does not look like R allows customized sort
        ## functions. I planned to order the list via compareVersion,
        ## but I don't see a way to provide an alternative sort
        ## method. In this case I can make do by just finding the
        ## highest version and excluding it from archiving, but still
        ## ... wow.
        vers <- byPkg[[ pkg ]]
        if (length(vers) < 2) next ## Nothing to do with one version
        top <- vers[1]
        for (ver in vers[-1]) {
            ## If we find a higher version, reset top:
            if (utils::compareVersion(top, ver) == -1) top <- ver
        }
        ## Make note of all but the top version:
        toArchive[[ pkg ]] <- setdiff(vers, top)
        kept[[ pkg ]] <- top
    }

    if (length(toArchive) == 0) {
        ## Nothing to do, return:
        return(invisible(NA))
    }

    ## Ok, we have at least one package that needs archiving. Need to
    ## make sure the archive folder is in place:
    adir <- file.path(cdir, "Archive")
    if (!dir.exists(adir)) {
        dir.create(adir, recursive=TRUE, showWarnings=FALSE, mode=dMode)
        if (!dir.exists(adir)) {
            .wrapMsg("Failed to create Archive directory", adir, color='red')
            .logMessage(paste("ERR: Archive dir create failed: ", adir))
            return(invisible(NA))
        }
    }

    rv <- NULL
    ## ARE YOU READY TO ARRRRCHHIIIVVVEEE?!?!
    for (pkg in names(toArchive)) {
        pdir <- file.path(adir, pkg)
        ## Each package has its own subdir in Archive
        if (!dir.exists(pdir)) {
            dir.create(pdir, recursive=TRUE, showWarnings=FALSE, mode=dMode)
            if (!dir.exists(pdir)) {
                .wrapMsg("Failed to create Archive subdirectory",
                         pdir, color='red')
                .logMessage(paste("ERR: Archive subdir create failed: ", pdir))
                next # We will keep trying the others, though
            }
        }

        vers    <- toArchive[[ pkg ]] ## The versions we want to archive
        base    <- sprintf("%s_%s.tar.gz", pkg, vers) # Basename
        relPath <- sprintf("%s/%s", pkg, base) # rel path = archive.rds rownames
        srcPath <- file.path(cdir, base) # Source of files
        trgPath <- file.path(adir, pkg, base) # Target, where we move them to
            
        ## The data frame held by archive.rds:
        archPkg <- arch[[pkg]]
        if (!is.null(archPkg)) {
            ## We already have some files noted in the archive
            ## Make sure none of them are what we're preparing to move!
            overlap <- intersect(rownames(archPkg), relPath)
            if (length(overlap) != 0) {
                .wrapMsg(paste0("Problem attempting to archive packages from ", pkg, " :"),
                         "The following packages appear to be in both /src/contrib and Archive:",
                         srcPath[ is.element(relPath, overlap) ],
                         "Make sure each version is only in /src/contrib -OR- Archive",
                         "You may also need to clear Meta/archive.rds",
                         color="red")
                next
            }
        }
        ## Ok. It looks like we can cleanly move these packages to Archive
        newRows <- file.info(srcPath)
        rownames(newRows) <- relPath
        
        ok <- file.rename( srcPath, trgPath )
        if (!all(ok)) {
            .wrapMsg(paste0("SIGNIFICANT PROBLEM - Archive rename for", pkg, " :"),
                     "I attempted to move these files:", srcPath,
                     "To this location:", trgPath,
                     "I failed. <skynet.jpg>",
                     "This puts the archive in a weird state. Figure out why the move failed.",
                     "Then try running buildRepoArchive(clean=TRUE)",
                     "The main repo should be unaffected",
                     color="red")
            stop("Halting run")
        }
        if (is.null(archPkg)) {
            ## First time adding this package to the archive:
            arch[[pkg]] <- newRows
        } else {
            ## We already have some versions archived. Bind the new rows:
            arch[[pkg]] <- rbind(archPkg, newRows)
        }
        ## Extend the return value:
        if (is.null(rv)) {
            rv <- newRows
        } else {
            rv <- rbind(rv, newRows)
        }
        .wrapMsg(paste0(pkg, " - Keeping ", kept[[pkg]], " - Archiving:"),
                 base, color="green")

    }
    ## Finally, update the RDS file:
    ## Make sure this stays as version=2 for backward compatibility!
    saveRDS(arch, file=ards, version=2)
    invisible(rv)
}

alreadyCommentedOnBranSize <- FALSE

#' Get My Manifest
#'
#' Parse the PACKAGES file into a data frame
#'
#' @param cleanVersions Default FALSE. If true, then version
#'     requirements will be removed from some columns and moved into a
#'     "<Column> Title" column.
#' @param details Default TRUE, which will cause each .tar.gz in the
#'     manifest to be unpacked and the DESCRIPTION files checked for
#'     additional information (authors, title, description). These
#'     DESCRIPTION files are copied and cached with
#'     \code{.extractedDESCRIPTION()} and parsed with
#'     \code{parseDescriptionFile()}
#' @param quiet Default FALSE. If TRUE, then message reporting will
#'     not occur
#'
#' @return \code{NULL} if there was a problem recovering the manifest,
#'     otherwise a data.frame
#'
#' @seealso \link{parseDescriptionFile}
#' 
#' @importFrom CatMisc parenRegExp is.something
#' @export

getMyManifest <- function( cleanVersions=FALSE, details=TRUE, quiet=FALSE) {
    pkUrl   <- .PACKAGES_URL()
    if (is.na(pkUrl)) {
        return(NULL)
    }
    pkLines <- readLines( pkUrl )
    pkNum   <- sum(grepl('^Package: ', pkLines))
    if (!alreadyCommentedOnBranSize && !quiet)
        message(pkNum, " packages in ", pkUrl)
    alreadyCommentedOnBranSize <- TRUE
    manifest <- list(Package     = character(pkNum))
    if (!.localOffline("")) manifest$SizeKB <- manifest$Doc <-
        manifest$Description <- manifest$Title <- manifest$Updated <-
            manifest$Maintainer <- character(pkNum)
    i <- 0L # Index counting packages
    for (line in pkLines) {
        ## Scan through each line in PACKAGES, building a list of
        ## lists as we go.
        kv <- strsplit(line, ": ")[[1]]
        if (length(kv) > 1) {
            k <- kv[1]
            v <- paste(kv[-1], collapse=': ')
            if (k == 'Package') {
                ## New entry in manifest
                i <- i + 1
                if (i %% 100 == 0 && !quiet) message(sprintf("%6d : %s", i, v))
            }
            if (is.null(manifest[[ k ]])) {
                ## We have not seen this key before - add it as a new
                ## fully-padded character vector.
                manifest[[ k ]] <- character(pkNum)
            }
            manifest[[ k ]][i] <- v
        }
    }
    if (cleanVersions) {
        for (nm in c("Imports","Depends","Suggests","License")) {
            if (is.null(manifest[[nm]])) next
            ## Remove trailing commas
            cleaned <- gsub(', *$', '', manifest[[nm]])
            ## To make display more compact, strip out version numbers,
            ## and capture them in a title pop-up
            mlen   <- length(manifest[[nm]])
            titles <- character(mlen)
            bits   <- strsplit(cleaned,"\\s*,\\s*")
            for (bi in seq_len(mlen)) {
                vals   <- bits[[bi]]
                tibits <- character()
                for (vi in seq_len(length(vals))) {
                    hasVers <- CatMisc::parenRegExp("^(.+)\\s*\\([^\\)]+\\)",
                                                    vals[vi])
                    if (!is.na(hasVers[1])) {
                        ## There appears to be a version
                        ## restriction. Remove it for more compact
                        ## display, but add the version to the title
                        ## column
                        tibits   <- c(tibits, vals[vi])
                        vals[vi] <- hasVers[1]
                    }
                }
                ## Re-concatenate the components with ', '
                bits[[bi]] <- paste(vals, collapse=', ')
                titles[bi] <- paste(tibits, collapse=', ')
            }
            manifest[[nm]] <- unlist(bits)
            manifest[[paste(nm, "Title")]] <- ifelse(titles == "", NA, titles)
        }
        ## Do not bother with NeedsCompilation if all values are identical
        if(length(unique(manifest$NeedsCompilation)) == 1)
            manifest$NeedsCompilation <- NULL
    }
    df <- as.data.frame(manifest, stringsAsFactors = FALSE )
    colnames(df) <- names(manifest)
    rownames(df) <- df$Package
    fullName     <- sprintf("%s_%s.tar.gz", manifest$Package, manifest$Version)

    if (!.localOffline("") && details) {
        ## Get some more information about each package
        fullPath   <- file.path(.opt('Path'), "src","contrib", fullName)
        fileInfo   <- file.info(fullPath)
        df$SizeKB  <- fileInfo$size / 1000
        df$Updated <- strftime(fileInfo$mtime, "%Y-%m-%d", usetz=FALSE)
        ## Make local cached copies of DESCRIPTION for each package,
        ## extract key/value info for each, and organize as a
        ## data.frame
        descFiles <- .extractedDESCRIPTION(fullName)
        descInfo  <- lapply(descFiles, parseDescriptionFile)
        descKeys  <- sort(unique(unlist(lapply(descInfo, names))))
        rowInds   <- seq_len(nrow(df))
        flatten   <- function (x) {
            if (is.null(x)) {
                ""
            } else if (is.list(x)) {
                ## eg "Authors@R"
                as.character(x)[1]
            } else {
                x
            }
        }
        descList  <- lapply(descKeys, function(k) vapply(rowInds,
                 function (i) flatten(descInfo[[i]][[k]]), ""))
        names(descList) <- descKeys
        ## Extract the title, description and main author
        df$Title       <- descList$Title
        df$Description <- descList$Description
        df$Maintainer  <- descList$Maintainer
        nrowDf         <- nrow(df)
        df[["Maintainer URL"]] <- character(nrowDf)
        df[["Package URL"]]    <- descList$URL
        df[["Maintainer Title"]] <- gsub("(<.+?>|\\[.+?\\])","",descList$Author)
        for (i in rowInds) {
            ## Make maintainer email actual links
            nm <- CatMisc::parenRegExp("^\\s*(.+?)\\s*<(.+?)>", df$Maintainer[i])
            if (!is.na(nm[2])) {
                ## A maintainer has been provided
                df$Maintainer[i] <- nm[1]
                myOrg <- .opt("OrgDomain")
                body  <- if (CatMisc::is.something(myOrg) &&
                    !grepl(paste0('@\\Q', myOrg, '\\E$'), nm[2])) {
                    ## An "OrgDomain" has been provided, and it seems
                    ## this address is not within it. Caution user.
                    paste0("*** CAUTION: This author has an address outside your ", myOrg," organization! ***")
                } else {
                    ## No warning needed
                    ""
                }
                df[["Maintainer URL"]][i] <- sprintf("mailto:%s?subject=R Package %s&body=%s", nm[2], df$Package[i], body)
            }
        }
        df[["Doc URL"]] <- .extractedPDF(fullName, as.url=TRUE)
        df$Doc <- ifelse(is.na(attr(df[["Doc URL"]], 'size')), NA, 'PDF')

        ## Get a list of vignettes.
        vg <- getMyVignettes(df)
        
    } else {
        df$SizeKB  <- NULL
        df$Updated <- NULL
    }
    

    df[["Version URL"]] <- sprintf("%s/src/contrib/%s", .opt('URL'), fullName)
    attr(df, 'file') <- file
    df
}

#' Get Repository Vignettes
#'
#' Get a list of all vignettes available in the repository
#'
#' @details
#'
#' The `vignette` function expects the queried package(s) to be
#' installed. I don't want to require the repo maintaining instance to
#' actually install every package - it's possible that some packages
#' will be mutually exclusive due to version requirements.
#'
#' So instead I'm going to mine the tar.gz packages and extract the
#' files from there. It appears that when the package is built (at
#' least when using devtools) the vignettes are automatically
#' processed into HTML files in \code{/doc/}.
#'
#' @param df A data.frame containing the manifest , the data frame
#' @export

getMyVignettes <- function(df=getMyManifest(cleanVersions=TRUE)) {
    "Get all vignettes from the current repository"
    tmpDir <- file.path(tempdir(), "checkVignette")
    if (!dir.exists(tmpDir)) dir.create(tmpDir)

### TODO - need to:
    ## extract archive and inspect /doc/
    ## Build data.frame of Package,VignetteName,VignetteDescription(?)
    ## Make a local copy of these files
    ## Make a dynamictable pointing to the files
    ## Integrate a summary link in the main repo summary table
    
}

.ignoredErrorList <- NULL

#' Ignored Errors
#'
#' A simple named vector of packages which have non-fatal errors
#'
#' @name dotIgnoredErrors
#'
#' @details
#'
#' Used to silence warnings for "troublesome" packages to avoid
#' alarming users that are rebuilding the BRAN repo.
#'
#' The file maintaining this list should be named ignoreErrors.txt and
#' hosted in the "resources" folder in your repository file
#' structure.
#'
#' @importFrom stats setNames
#'
#' @keywords internal
#'

.ignoredErrors <- function() {
    if (!is.null(.ignoredErrorList)) return(.ignoredErrorList)
    ## This is a file held in the "resources" folder in your
    ## repository that lists "problem" packages for which minor errors
    ## should not be mentioned.
    igFile <- paste(.opt('Path'),"resources/ignoreErrors.txt", sep='/')
    if (file.exists(igFile)) {
        pkgs   <- readLines(igFile)
        pkgs   <- pkgs[ !grepl('^(#|\\s*$)', pkgs ) ]
    } else {
        pkgs <- character()
    }
    .ignoredErrorList <- stats::setNames(rep(TRUE,length(pkgs)), pkgs)
}

#' Parse DESCRIPTION file
#'
#' Parses a packages DESCRIPTION file, returning a list of key/value pairs
#'
#' @param file Required, the path to the DESCRIPTION file
#'
#' Used to pull information out of an R package's DESCRIPTION file
#'
#' @seealso Used by \link{getMyManifest}
#' 
#' @examples
#'
#' str(parseDescriptionFile("./DESCRIPTION"))
#'
#' @importFrom utils person
#' @export

parseDescriptionFile <- function (file) {
    rv <- list(Path = file)
    if (!file.exists(file)) {
        rv$Error <- "File does not exist"
        return(rv)
    }
    dcf <- read.dcf(file)
    ## Remove newlines
    dcf <- gsub("\n", " ", dcf)
    ## Remove leading white space
    dcf <- gsub("^\\s+", "", dcf)
    for (key in colnames(dcf)) {
        val <- dcf[ , key ]
        if (grepl('@R', key)) {
            ## Parse entries that are represented as R code
            val <- eval(parse(text=val))
            ## Can never reccall precise way to do a 'real' eval:
            ## https://stackoverflow.com/a/1743796
        }
        rv[[key]] <- val
    }
    rv
}

#' rememberMyRepo
#'
#' Modifies your Rprofile to 'remember' your custom repository settings
#'
#' @details
#'
#' Will add a \code{source()} line to your .Rprofile that configures
#' your repository every time you start R.
#'
#' @param script Default is the myRepository 'Configure' option, which
#'     should be a URL to your repo's configuration script. You may
#'     optionally provide an alternative path or URL.
#'
#' @examples
#'
#' \dontrun{
#' ## 'Permanently' add configuration information to your profile
#' rememberMyRepo()
#' }
#'
#' @importFrom utils capture.output
#' @importFrom CatMisc is.something
#' 
#' @export

rememberMyRepo <- function(script=.opt("Configure")) {
    rprof <- myRprofile()
    if (!CatMisc::is.something(rprof)) {
        .wrapMsg("Failed to identify the file path to your .Rprofile",
                 "Have your R Gurus point you to the configuration script.",
                 color='red')
        return(NA)
    }
    if (!CatMisc::is.something(script)) {
        .wrapMsg("Could not determine the source of the configuration script",
                 color='red')
        return(NA)
    }
    cmt  <- sprintf("Configure the %s internal repo", .opt("Name"))
    ## We need try() in case the URL is not available, otherwise
    ## source() will wedge the rest of the .Rprofile:
    code <- sprintf("try(source('%s')) # %s", script, cmt)
    alterFile(rprof, code, cmt)
}

#' myRepoPath
#'
#' Simply returns the local file system path of the repository
#'
#' @examples
#'
#' repoPath <- myRepoPath()
#' 
#' @export

myRepoPath <- function() .opt('Path')

#' myRepoPath
#'
#' Simply returns the URL hosting your repository
#'
#' @examples
#'
#' repoURL <- myRepoURL()
#' 
#' @export

myRepoURL <- function() .opt('URL')

#' myRprofile
#'
#' Return the file path to your .Rprofile file
#'
#' @details
#'
#' Attempts to find the location of your Rprofile file. This can vary
#' depending on operating system, as well as implementation of R.
#' 
#' @param create Default \code{TRUE}, which will cause a new .Rprofile
#'     file to be created in your home directory if one does not
#'     already exist.
#'
#' @seealso \code{\link[base]{Startup}}
#'
#' @importFrom crayon blue
#'
#' @examples
#'
#' myRprofile()
#'
#' @export

myRprofile <- function (create=TRUE) {
    ## Try to find a local .Rprofile file
    ## https://stackoverflow.com/a/13736073
    homeProf   <- file.path("~", ".Rprofile") # Is ~ ok on windows??
    candidates <- normalizePath(c(file.path(getwd(), ".Rprofile"), homeProf ),
                                mustWork=FALSE)
    rv         <- Filter(file.exists, candidates)[1]
    if (is.na(rv)) {
        ## No file was found.
        if (create) {
            ## Make a blank file - append in case something messed up
            ## in the detection
            cat("## Auto-generated by myRepository\n",
                file = homeProf, append = TRUE)
            .wrapMsg("No Rprofile present, blank one created",
                     .fileCol(homeProf), color='green')
            rv <- homeProf
        }
    }
    rv
}

#' Current Package Version
#'
#' Reports the $Version from the DESCRIPTION file in the current project
#'
#' @param dir The working directory of the project, defaults to '.'
#' 
#' @details
#'
#' Will parse the DESCRIPTION file to recover the version number stored there
#'
#' @examples
#'
#' currentPackageVersion()
#'
#' @export

currentPackageVersion <- function (dir = '.') {
    ## Use currentPackageName to exhaustively explore parent directories
    pkg    <- currentPackageName(dir=dir)
    pkgDir <- pkg['SourceDir']
    if (!is.na(pkgDir)) {
        path <- file.path(packageDir(pkgDir), "DESCRIPTION")
        if (file.exists(path)) {
            info <- parseDescriptionFile(path)
            return(info$Version)
        }
    }
    as.character(NA)
}

#' Current Version Control
#'
#' Find the root directory for a version controlled project, and VC type
#'
#' @details
#'
#' Checks the requested directory to see if it is under version
#' control. If \code{recursive} is TRUE, then will crawl parent
#' directories as well.
#'
#' Detects:
#'
#' \itemize{
#'   \item git - By the presence of .git/config
#' }
#' 
#' @param dir The working directory of the project, defaults to '.'
#' @param recursive Default \code{TRUE}, which will search the
#'     requested directory and all parent directories until one under
#'     version control is found.
#'
#' @return
#'
#' A named character vector of length two, with entries:
#'
#' \itemize{
#'   \item VcDir - The root directory for the repository
#'   \item VcType - The type of version control, one of c('git')
#'   \item RepoName - The inferred name of the repository
#' }
#'
#' If no version control was detected, both values will be \code{NA}
#' 
#' @importFrom stats setNames
#' @importFrom CatMisc parenRegExp
#' 
#' @examples
#'
#' ## Should be nothing there:
#' tmp <- tempdir()
#' currentVersionControl( tmp )
#' 
#' ## Let's fake a git repo
#' vcd <- file.path(tmp, ".git")
#' dir.create(vcd)
#' file.create(file.path(vcd,"config"))
#' currentVersionControl( tmp )
#' 
#' @export

currentVersionControl <- function (dir = '.', recursive=TRUE) {
    chk <- normalizePath(dir)
    rv  <- stats::setNames(as.character(c(NA,NA,NA,NA)),
                           c("VcDir", "VcType", "RepoName","Branch"))
    cFile <- file.path(chk, '.git', 'config')
    if (file.exists(cFile)) {
        ## This level contains a git configuration directory. We will
        ## consider that to mean we have found the root level of a
        ## git-controlled repository.
        rv[1] <- chk
        rv[2] <- 'git'
        ## Try to extract the repository name
        x  <- readLines(cFile)
        rx <- which(grepl('^\\[remote', x)) # Look for [remote] sections
        if (length(rx) > 0) {
            ## Found a [remote], trim everything before it
            x  <- x[seq(rx[1], length(x))]
            sx <- which(grepl('^\\[', x)) ## Find all section markers
            ## If there are other section markers, remove them:
            if (length(sx) > 1) x <- x[-seq(sx[2], length(x))]
            ## Now finally look for a .git entry
            found <- CatMisc::parenRegExp("\\/(.+)\\.git\\s*$", x)
            found <- found[ !is.na(found) ]
            if (length(found) > 0) rv[3] <- found[1]
        }
        ## Try to find the branch
        hFile <- file.path(chk, '.git', 'HEAD')
        if (file.exists(hFile)) {
            x  <- readLines(hFile)
            found <- CatMisc::parenRegExp("^ref:.+\\/(.+?)\\s*$", x)
            if (length(found) == 1) rv[4] <- found[1]
        }
    } else {
        ## No other VC systems being checked currently. Can be added
        ## as needed.
        ## If we are recursing, check the parent directory
        par <- dirname(chk)
        if (recursive && par != chk) {
            ## Have not reached the root of the file system yet
            return( currentVersionControl( dir=par, recursive=recursive ) )
        }
    }          
    class(rv) <- "mrNamed"
    rv
}
     
#' Current Package Directory
#'
#' Detect the directory path for the current package, if any
#'
#' @details
#'
#' Given a starting file path, determine if it is within a package
#' directory structure, and if so, return the root directory of that
#' pacakge.
#'
#' @param dir The working directory of the project, defaults to '.'
#' @param name Default \code{NULL}. Can be used in the special case
#'     where \code{dir} represents a code repository with multiple R
#'     packages inside it. Providing the particular package name of
#'     interest will allow the correct path to be determined.
#' @param quiet Default \code{FALSE}. If TRUE, then will not emit some
#'     warnings.
#'
#' @return
#'
#' \code{NA} if the package directory could not be found, otherwise a
#' single character string representing the directory
#'
#' @importFrom CatMisc is.something
#' @export

currentPackageDir <- function (dir='.', name=NULL, quiet=FALSE) {
    check   <- normalizePath(dir)
    relDir  <- .opt('Releases')      # configuration for special tar.gz dir
    docDir  <- .opt('Documentation') # configuration for special pdf dir

    isPkgDir <- function(paths) {
        ## We declare success if a directory has an R/ subdirectory,
        ## plus DESCRIPTION
       vapply( paths, function(path) {
            CatMisc::is.something(path) &
                dir.exists(path) &
                dir.exists(file.path(path, "R")) &
                file.exists(file.path(path, "DESCRIPTION"))
        }, TRUE)
    }

    ## First start by checking if we are in the special case where
    ## we're starting in a parent directory of a package that happens
    ## to be a 'standard' code repository. That will be the case if
    ## both the Releases and Documentation directories are present in
    ## our starting directory:
    if (CatMisc::is.something(relDir) && dir.exists(file.path(check, relDir)) &&
        CatMisc::is.something(docDir) && dir.exists(file.path(check, docDir)) ) {
        ## Ok, it looks like we are starting in a code repository. We
        ## want to see which subdirectories look like packages
        contents <- list.files( check )
        allPkgs  <- contents[ isPkgDir( file.path(check, contents) ) ]
        if (!is.null(name)) {
            ## A specific package name has been requested
            ok <- character()
            for (pdir in allPkgs) {
                ## Because we're looking in a subdirectory that we
                ## have just confirmed to be an R package, I don't
                ## *think* that this will cause runaway recursion...
                dat <- currentPackageName( file.path(check, pdir ) )
                if (is.element(dat['PackageName'], name)) ok <- c(ok, pdir)
            }
            if (length(ok) == 1) {
                ## We succeeded in identifying one-and-only-one of the
                ## several packages as the one the user wants
                return(file.path(check, ok))
            } else if (length(ok) == 0) {
                ## None of the packages matched the request
                if (!quiet) .wrapMsg("The directory:", check, "has multiple R packages:", paste(allPkgs, collapse=",   "), sprintf("None of them match your request: %s", paste(name, collapse='|')), color="yellow")
            } else {
                ## WHOA. There are multiple directories with different
                ## names, but the same package names. Outta here.
                if (!quiet) .wrapMsg("The directory:", check, "has multiple R packages:", paste(allPkgs, collapse=",   "), sprintf("SEVERAL OF WHICH match the package name you requested: %s", paste(ok, collapse=', ')), color="yellow")
            }
        } else {
            ## No particular package specified
            if (length(allPkgs) == 1) {
                ## But that's ok, because there's only one package anyway
                return( file.path(check, allPkgs[1]) )
            } else {
                ## More than one package, and we have no criteria to choose
                if (!quiet) .wrapMsg("The directory:", check, "... appears to have multiple R packages:", paste(allPkgs, collapse=",   "), "If you wish to 'auto-detect' the relevant R package, please setwd() into the relevant one, or provide the desired name with the `name` parameter. Otherwise, please explicitly provide the directory path", color="yellow")
            }
        }
        return(as.character(NA))
    }
    
    ## We do not appear to be in a higher-level repository
    ## directory. So presume that the package is at this level, or a
    ## parent.
    
    if (isPkgDir(check)) {
        ## Yay! This looks like a wholesome R package
        ## directory. Let's declare it as such, and cease our
        ## search:
        return(check)
    }

    ## Nope, not at this level. Maybe a parent?
    par <- dirname(check)
    if (par != check) {
        ## There is still a parent to try. Recurse to it
        return( currentPackageDir(par, name=name, quiet=quiet) )
    }
    ## We have run out of file system. Admit failure with NA
    as.character(NA)
}

#' Current Package Name
#'
#' Given a directory, attempts to figure out the name of an R package
#' associated with it, as well as paths to the directory holding code
#' (that is, the parent of the 'R' subdirectory) and the directory
#' where tar.gz files should go.
#'
#' @param dir The working directory of the project, defaults to '.'
#' @param name Default \code{NULL}, optional value passed to
#'     \link{currentPackageDir} to aid resolution of projects with
#'     multiple packages
#' @param path Default \code{NULL}, will be reset to '..'. Where the
#'     .tar.gz file will be created.
#' @param quiet Default \code{FALSE}. If TRUE, then will not emit some
#'     warnings.
#' 
#' @details
#'
#' Will inspect the location of \code{dir} (default the current
#' working directory) to work out the package name
#'
#' @return
#'
#' A named character vector, with the following entries:
#'
#' \itemize{
#'   \item PackageName - The inferred name of the package
#'   \item SourceDir - The directory holding the package source
#'   \item TarGzDir - Directory that will contain built .tar.gz files
#'   \item PdfDir - Directory that will contain monolithic PDF documentation
#'   \item VcDir - Version Control directory holding the package
#'   \item VcType - Type of version control being used
#' }
#'
#' In normal cases some of these may be \code{NA}. In abberent cases
#' (you checked a directory that is not a package) most will be
#' \code{NA}.
#'
#' @examples
#'
#' currentPackageName()
#'
#' @importFrom crayon red
#' @importFrom CatMisc is.something
#' @importFrom stats setNames
#' @importFrom desc desc
#' 
#' @export
    
currentPackageName <- function (dir = '.', name=NULL, path=NULL, quiet=FALSE) {
    rv <- stats::setNames(rep(as.character(NA), 4),
                          c("PackageName", "SourceDir", "TarGzDir", "PdfDir"))
    pkg     <- rv['SourceDir'] <- currentPackageDir(dir, name, quiet)
    ## Add in version control information:
    vc      <- currentVersionControl(dir)
    for (name in names(vc)) rv[[ name ]] <- vc[[ name ]]
    
    if (is.na(pkg)) {
        ## We failed to find a package-like directory
        if (!quiet) .wrapMsg("I inspected", dir," and all parent directories, and none look like an R package folder (R/ directory and DESCRIPTION file). Most utility functions in myRepository are unlikely to work relative to that directory", color="yellow")
        
    } else {
        ## We were able to identify the R package directory. See if we
        ## can parse the DESCRIPTION file
        dFile  <- file.path(pkg, "DESCRIPTION")
        try( d <- desc::desc(dFile) )
        if (is.null(d)) {
            .wrapMsg("Something went wrong parsing the DESCRIPTION file:",
                     dFile, color="yellow")
        } else {
            rv['PackageName'] <- d$get_field("Package")
        }
        
        ## See if we can also identify any special folders
        pkgPar  <- dirname(normalizePath(pkg))
        relDir  <- .opt('Releases')      # configuration for special tar.gz dir
        docDir  <- .opt('Documentation') # configuration for special pdf dir
        if (CatMisc::is.something(relDir)) {
            ## tar.gz releases folder was specified
            pr  <- file.path(pkgPar,  relDir)
            if (dir.exists(pr)) rv['TarGzDir'] <- pr
        } else if (!is.null(path)) {
            ## User has specified a path for the tar.gz
            rv['TarGzDir'] <- normalizePath(path)
        } else {
            ## By default / tradition, tar.gz files get created "just
            ## outside" the package
        }
        if (CatMisc::is.something(docDir)) {
            ## PDF folder was specified
            pd  <- file.path(pkgPar,  docDir)
            if (dir.exists(pd)) rv['PdfDir'] <- pd
        }
    }
    class(rv) <- "mrNamed"
    rv
}

print.mrNamed <- function(x, ...) {
    "Pretty-printer for named vectors"
    nn  <- names(x)
    w   <- max(nchar(nn))
    fmt <- paste0(.actCol('%',w,'s')," : %s")
    msg <- vapply(nn, function(z) sprintf(fmt, z, x[z]), "")
    message(paste(msg, collapse="\n"))
    
}

#' Current Package Status
#'
#' Reports the name, version, and relative version status to BRAN for
#' the current package
#'
#' @details
#'
#' If the current directory does not seem to be a package (no parsable
#' DESCRIPTION) then a message will indicate that you do not appear to
#' be in a package directory. Otherwise the package name and version
#' will be reported, as well as if the version differs from that on R.
#'
#' @param dir The working directory of the project, defaults to '.'
#'
#' @return Invisibly, a named character vector reporting the current
#'     package name, current version, and version of package on your
#'     repository.
#' 
#' @examples
#'
#' currentPackageStatus()
#'
#' @importFrom crayon red green yellow cyan blue
#' @importFrom stats setNames
#' @export
    
currentPackageStatus <- function (dir = '.') {
    v <- currentPackageVersion(dir)
    rvNames   <- c("PackageName", "Version", "RepoVersion")
    if (is.na(v)) {
        .wrapMsg("You do not appear to be in a package directory", color="red")
        return(invisible(stats::setNames(c(NA, NA, NA), rvNames )))
    }
    pkgDat  <- currentPackageName(dir=dir)
    pkgName <- pkgDat["PackageName"]
    message("Working with package ", .hilite(pkgName),
            " - version ", .hilite(v))
    manifest  <- getMyManifest(details=FALSE, quiet=TRUE)
    rvNames   <- c("PackageName", "Version", "RepoVersion")
    rvVals    <- c(pkgName, v, NA)
    if (is.null(manifest)) {
        .wrapMsg("  Custom repository location not defined", color='red')
        return(invisible(stats::setNames(rvVals, rvNames )))
    }
    bv  <- manifest[pkgName, "Version"]
    msg <- "Something went wrong in myRepository..."
    bac <- paste("\n  When you are ready, run", .actCol("buildAndCheck()"))
    rN  <- .opt('Name')
    msg <- if (is.na(bv)) {
        paste(crayon::cyan("The package does not appear to be in", rN), bac)
    } else {
        cv <- compareVersion(v, bv)
        if (cv == 1) {
            paste("Your version is", crayon::green("newer than", rN), bac)
        } else if (cv == -1) {
            sprintf("Your version is %s.\n  Consider %s",
                    crayon::red("older than", rN),
                    .actCol("git fetch && git pull"))
        } else {
            sprintf("Your version is %s.\n  Consider %s",
                    crayon::yellow("the same as", rN),
                    .actCol("incrementPackageVersion()"))
        }
    }
    message("  ", msg)
    rvVals[3] <- bv
    invisible(stats::setNames(rvVals, rvNames ))
}

#' Create New Package
#'
#' Wrapper for package.skeleton that creates some additional directories
#'
#' @details
#'
#' This function runs pacakage.skeleton(), then performs additional steps:
#'
#' \itemize{
#'
#'   \item Removes the readme from package.skeleton
#'
#'   \item Modifies DESCRIPTION to include more guidance
#'
#'   \item Deletes NAMESPACE so Roxygen can handle it
#'
#'   \item Sets up the \code{testthat} testing framework
#'
#'   \item Adds an example test file that explains available tests
#'
#'   \item Adds an example code file, with Roxygen markup examples
#'
#'   \item Provides guidance on git integration
#' 
#'   \item Reminds user of next step (buildAndCheck)
#' 
#' }
#'
#' The goal here is to help guide users into using both Roxygen and
#' testthat for building their R packages. The primary audience are
#' new users, but even old users can get tripped by steps that they
#' only need to do once every few months (for example: Why won't
#' Roxygen manage my imports?!? Oh, right, I need to delete the
#' NAMESPACE file...)
#'
#' @param path Default '.', the directory to create the package in
#' @param name Default \code{NULL}, the name of the package. If not
#'     required, will use the directory basename provided in
#'     \code{path}. That is, it will presume that the parent directory
#'     has the same name as the package. This is following a
#'     convention where the package code repository does not have the
#'     package contents at the 'root' directory hierarchy. Instead,
#'     the package will 'begin' in a subdirectory, also named after
#'     the package. This allows multiple packages to be managed in one
#'     repository, and also allows for a sibling folder to hold built
#'     tar.gz files.
#' @param list Default \code{NULL}. Optional character vector of R
#'     object names that will be added to the \code{R/} subdirectory
#'     as R files. If NULL is provided, then no code files will be
#'     included (a fully blank package, aside from deletable example
#'     files)
#' @param addexamples Default TRUE, which will add a sample R file to
#'     \code{R/} and a file of test examples to
#'     \code{tests/testthat/}. This file is primarily to demonstrate
#'     the use of R
#' @param addbugs Default FALSE. If TRUE, then an example file full of
#'     broken code (both R and Roxygen) will be included. Intended to
#'     be a debugging exercise, as well as a reference that correlates
#' 
#' @examples
#'
#' \dontrun{
#' tmp     <- tempdir()
#' pkgName <- "demoPackage"
#' createNewPackage(tmp, pkgName)
#'
#' ## Or practice debugging:
#' bugName <- "bugPractice"
#' pkgPath <- createNewPackage(tmp, bugName, addbugs=TRUE)
#' buildAndCheck(pkgPath)
#' 
#' ## Clean up afterwards (removes practice folder on unix-like systems):
#' ## system2("rm", c("-rf", pkgPath))
#' }
#' 
#' @importFrom crayon blue red cyan
#' @importFrom utils package.skeleton
#' @importFrom CatMisc is.something
#'
#' @export

createNewPackage <- function (path='.', name=NULL, list=NULL,
                              addexamples=TRUE, addbugs=FALSE) {
    nPath    <- normalizePath(path, mustWork=FALSE)
    .bar(TRUE)
    if (is.null(name)) {
        ## Take the parent directory as the package name
        name <- basename(nPath)
        if (name == "") {
            .wrapMsg("Error: You did not provide 'name' and you appear to be at the root level of the file system", color='red')
            .bar()
            return(NA)
        } else {
            .wrapMsg(paste0("\nPresuming your package is named '",
                            .hilite(name),"'"))
        }
    } else {
        .wrapMsg(paste0("\nConfiguring package '", .hilite(name),"'"))
    }
    if (!dir.exists(nPath)) {
        ## Go ahead and create the 'parent' directory. Typically this
        ## will be a version control directory
        if (dir.create(nPath, recursive=TRUE)) {
            .wrapMsg("Created parent directory:", .fileCol(nPath),
                     "Consider placing your package under version control.")
        } else {
            .wrapMsg("Parent directory does not exist and I failed to make it:",
                     .fileCol(nPath), color='red')
            .bar()
            return(NA)
        }
    }
    pkgDir   <- file.path(nPath, name)
    if (dir.exists(pkgDir)) {
        ## Do not mess with existing code directories
        message("This directory already exists:",
                "\n  ", .fileCol(pkgDir),
                "\n  Pick another location or move/remove the directory to continue")
        .bar()
        return(NA)
    }

    ## Make the file structure with package.skeleton
    skelList <- list
    skelEnv  <- .GlobalEnv
    .dummyPlaceholder <- function () message("dummy function")
    if (is.null(list)) {
        ## Does not seem possible to pass an empty list, so we will
        ## construct directories with a placeholder, then clear out
        ## the generated files afterwards:
        skelList <- c(".dummyPlaceholder")
        skelEnv  <- environment()
    }

    vcFiles <- file.path(name, "DESCRIPTION")
    dFile  <- file.path(nPath, name, "DESCRIPTION")
    dThere <- file.exists(dFile) # Be safe, check if DESCRIPTION already there
    suppressMessages( package.skeleton(name=name, path=nPath, list=skelList,
                                       environment=skelEnv) )
    if (is.null(list)) {
        ## Remove the dummy placeholder file (no man for . files)
        dumPath <- file.path(pkgDir, "R", paste(name, "internal.R", sep='-'))
        if (file.exists(dumPath)) file.remove(dumPath)
        rm(".dummyPlaceholder")
    }
    ## Remove the DESCRIPTION made by skeleton:
    if (!dThere && file.exists(dFile)) file.remove(dFile)
    
    ## This function was created in part because package.skeleton is
    ## fairly confusing for new R users. It is also focused on making
    ## Rd files directly, we are trying to encourage use of
    ## Roxygen. Remove their readme and package man file
    readPath <- file.path(pkgDir, 'Read-and-delete-me')
    if (file.exists(readPath)) file.remove(readPath)
    pkgMan   <- file.path(pkgDir, 'man', paste(name, "package.Rd", sep='-'))
    if (file.exists(pkgMan)) file.remove(pkgMan)
    ## We need to nuke the NAMESPACE file if we want it maintained by
    ## Roxygen! Otherwise @import directives will not be managed.
    nsPath   <- file.path(pkgDir, 'NAMESPACE')
    if (file.exists(nsPath)) file.remove(nsPath)

    templateOpts <- list(NAME=name)

    tgzDir <- .opt('Releases')
    if (CatMisc::is.something(tgzDir)) {
        ## Create the sibling folder that will hold tar.gz files:
        fullTgz <- file.path(nPath, tgzDir)
        if (dir.exists(fullTgz)) {
            message("\nRelease folder already exists: ", .fileCol(fullTgz) )
        } else {
            dir.create(fullTgz)
            message("\nRelease folder created: ", .fileCol(fullTgz),
                    "\n  .tar.gz files will be placed there by ",
                    .actCol("buildAndCheck()"))
            .copyTemplateFile("README_Releases.md", fullTgz, templateOpts,
                              newname="README.md")
            templateOpts$RELNOTE <- sprintf('* `.tar.gz` built package files can be found in [%s/](%s)\n', tgzDir, tgzDir)
            templateOpts$RELBACK <- sprintf('* `.tar.gz` built package files can be found in [%s/](../%s)\n', tgzDir, tgzDir)
            vcFiles <- c(vcFiles, tgzDir)
            ## Create an archive folder to hold older versions
            arch <- file.path(fullTgz, "archive")
            dir.create(arch)
            .copyTemplateFile("README_archive.md", arch, templateOpts,
                              newname="README.md")
        }
    }

    pdfDir <- .opt('Documentation')
    if (CatMisc::is.something(pdfDir)) {
        ## Create the sibling folder that will hold PDF files:
        fullPdf <- file.path(nPath, pdfDir)
        if (dir.exists(fullPdf)) {
            message("\nDocumentation folder already exists: ",
                    .fileCol(fullPdf) )
        } else {
            dir.create(fullPdf)
            message("\nDocumentation folder created: ", .fileCol(fullPdf),
                    "\n  PDF files will be placed there by ",
                    .actCol("buildAndCheck()"))
            .copyTemplateFile("README_Documentation.md", fullPdf, templateOpts,
                              newname="README.md")
            templateOpts$DOCNOTE <- sprintf('* `.pdf` full documentation files can be found in [%s/](%s)\n', pdfDir, pdfDir)
            templateOpts$DOCBACK <- sprintf('* `.pdf` full documentation files can be found in [%s/](../%s)\n', pdfDir, pdfDir)
            vcFiles <- c(vcFiles, pdfDir)
            ## Create an archive folder to hold older versions
            arch <- file.path(fullPdf, "archive")
            dir.create(arch)
            .copyTemplateFile("README_archive.md", arch, templateOpts,
                              newname="README.md")
        }
    }
    
    descPath <- .copyTemplateFile("DESCRIPTION", pkgDir, templateOpts,
                                  clobber=FALSE)
    
    ## http://r-pkgs.had.co.nz/tests.html
    suppressMessages( devtools::use_testthat( pkgDir ) )
    testDir  <- file.path(pkgDir, "tests", "testthat")
    message("\nA tests directory has been generated:\n    ",
            .fileCol(testDir) )
    
    if (addexamples) {
        testFile <- "test-harmlessExamplesDeleteMe.R"
        testPath <- .copyTemplateFile(testFile, testDir, templateOpts)
        vcFiles  <- c(vcFiles, file.path(name, "tests/testthat",testFile))
        
        message("  An example test file is at:\n    ",
                .fileCol(testPath),
                "\n  You should eventually move or delete that file")
        

        examFile <- "exampleCodeDeleteMe.R"
        examPath <- .copyTemplateFile( examFile, file.path(pkgDir, "R"))
        vcFiles  <- c(vcFiles, file.path(name, "R", examFile))
        
        message("\nSample R code illustrating use of Roxygen:\n  ",
                .fileCol(examPath),
                "\n  You should eventually move or delete this file")
    }

    if (addbugs) {
        bugFile <- "errorRiddledCodeDeleteMe.R"
        bugPath <- .copyTemplateFile( bugFile, file.path(pkgDir, "R"))
        vcFiles <- c(vcFiles, file.path(name, "R", bugFile))
       
        message("\n",crayon::red("Buggy examples have been added!"),"\n  ",
                .fileCol(bugPath),
                "\n  Practice debugging the file, or remove it.")
    }

    
    message("\nYou MUST edit the file at:\n    ", .fileCol(descPath),
            "\n  The ",  crayon::cyan("RemoveMe:"), " block at the top describes what needs to be done,",
            "\n  and should be removed as a final step.")
    
    ## Running incrementPackageVersion as a lazy way to set the date
    suppressMessages( incrementPackageVersion(pkgDir) )

    readme0 <- .copyTemplateFile("README_Top.md", nPath, templateOpts,
                                 newname="README.md")
    message("  The top-level README should be edited to inform visitors to your repository:\n    ",
            .fileCol(readme0))
    readme1 <- .copyTemplateFile("README_Package.md", pkgDir, templateOpts,
                                 newname="README.md")
    message("  The package README should be edited to provide usage examples:\n    ",
            .fileCol(readme1))
    vcFiles <- c(vcFiles, readme0, readme1)
    ## We won't bother to include the top-level readme in the VC file commit

    if (length(vcFiles) != 0) {
        ## Should we suggest a commit to the user?
        message("")
        stat <- currentPackageName(nPath)
        .suggestVcCommit(vcFiles, vcdir=stat["VcDir"], vctype=stat["VcType"],
                         msg="Initial package skeleton with demo files")
    }
    
    message("\nAfter you have added some R code, you can run:",
            "\n    ", .actCol("buildAndCheck('",nPath,"')"),
            "\n  ... to build and validate your new package\n",
            .bar(), "\n")
    
    invisible(pkgDir)
}

#' incrementPackageVersion
#'
#' Tool for the lazy - updates Version in DESCRIPTION
#'
#' @details
#'
#' Will automatically increment the "Version: " line in the
#' DESCRIPTION file of an R package by "1". If a "Date:" line is
#' present, will set to the current date, or a user-supplied one.
#'
#' The code presumes that the package is following the
#' "Semantic Versioning" guidelines at \url{http://semver.org/}, such
#' that incrementing an intermediate version number
#' 
#' @param dir Default '.' (the current directory) where the
#'     DESCRIPTION file resides.
#' @param level Default \code{1}. The part of the version number to
#'     increment. 1 or 'patch' will increment the least (last) part of
#'     the number, 2 or 'minor' will increment the second-least, and 3
#'     or 'major' will increment the third-least. Other values will
#'     throw an error and return NA. When levels other than 1/patch
#'     are incremented, all the 'lesser' levels are set to zero.
#' @param date Default \code{as.character(Sys.Date())}
#'
#' @return The updated Version line, or \code{NA} if problems were
#'     encountered
#'
#' @seealso \url{http://semver.org/}
#'
#' @examples
#'
#' \dontrun{
#' ## Increase the version number by 1 for the Enhnaces Value package
#' incrementPackageVersion("~/gitwork/enhancesvalue")
#' }
#' 
#' @importFrom CatMisc parenRegExp
#' @importFrom crayon red
#' @export

incrementPackageVersion <- function (dir=".", level=1,
                                     date=as.character(Sys.Date())) {
    cpn  <- currentPackageName(dir)
    pkdir <- cpn['SourceDir']
    if (is.na(pkdir)) {
        message("Directory",dir,"does not appear to be a package directory")
        return(NA)
    }
    path <- file.path(pkdir, "DESCRIPTION")
    if (!file.exists(path)) {
        message("Failed to find file at ", path)
        return(NA)
    }
    lines <- readLines(path)
    ## Look for the Version section
    vlines <- which(grepl("^Version:", lines))
    if (length(vlines) == 0) {
        message ("No version line in ", path)
        return(NA)
    }
    if (length(vlines) > 1) {
        message ("Multiple version lines in ", path)
        return(NA)
    }

    vtxt <- lines[vlines[1]]
    vdat <- CatMisc::parenRegExp("(Version:\\s*)([0-9\\.]+)$", vtxt)
    if (is.na(vdat[1])) {
        message("Unrecognized version line in ", path,  " : ", crayon::red(vtxt))
        return(NA)
    }
    newVers <- .incVersString(vdat[2], level)
    if (is.na(newVers)) return(NA)
    
    lines[vlines[1]] <- paste("Version:", newVers)
    msg <- c("Updated ",names(newVers)," to ", newVers)
    ## Is a date defined?
    dlines <- which(grepl("^Date:", lines))
    if (length(dlines) == 1 && !is.na(date[1])) {
        ## Yes, set it to a new value
        lines[dlines[1]] <- sprintf("Date: %s", date[1])
        msg <- c(msg, " with Date: ", date[1])
    }
    ## Oooookay, clobber DESCRIPTION
    cat(paste(c(lines,""), collapse="\n"), file=path)
    message(c(msg,"\n  Next step: ", .actCol("buildAndCheck('",dir,"')")))
    newVers
}

#' Package Directory
#'
#' Identify the directory that actually contains the package
#'
#' @details
#'
#' We have structured our software management to include the package
#' directory as a subdirectory of the project. This method will check
#' for that context and return the relevant subdirectory if it seems
#' appropriate.
#'
#' @param dir The working directory, by default '.'
#' @param quiet Default \code{FALSE}. If TRUE, then do not warn user
#'     if the predicted directory lacks an \code{R/} subdirectory.
#'
#' @return The normalized file path to the R package folder
#' 
#' @examples
#'
#' rCode <- packageDir()
#'
#' @importFrom CatMisc is.something
#' @export

packageDir <- function (dir='.', quiet=FALSE) {
    rv <- normalizePath(dir)
    if (CatMisc::is.something(.opt('Releases'))) {
        ## Use presence of releases directory to suggest that code may
        ## be one level deeper
        subdir <- basename(rv)
        bpr    <- file.path(dir, .opt('Releases'))
        rsd    <- file.path(dir, subdir)
        ## If the release directory is present, and there is a
        ## subdirectory of the same name that we're in now, take that
        ## subdirectory
        if (dir.exists(bpr) && dir.exists(rsd) &&
            dir.exists(file.path(rsd, 'R'))) rv <- rsd
    }
    if (!dir.exists(file.path(rv, 'R')) && !quiet) .wrapMsg("Attempt to locate package directory failed to discover the expected 'R' subdirectory", rv, color='yellow')
    rv
}

#' buildAndCheck
#'
#' Documents, builds and then checks your R package
#'
#' @details
#'
#' This function will run several utility scripts for you, mostly from
#' the \code{devtools} package. In particular, it will:
#'
#' \itemize{
#'   \item \code{\link[devtools]{document}} - Generate \code{?} help topics (man)
#'   \item \code{\link[devtools]{build}} - Construct the tar.gz package file
#'   \item \code{\link[devtools]{check_built}} - Sanity check the
#'     tar.gz and run your tests, if any.
#'   \item \code{\link{makePDF}} - Generate a monolithic PDF file with
#'     all help topics
#' }
#'
#' The above processes tend to generate a bewildering wall of text,
#' most of which can be safely ignored. \code{buildAndCheck()}
#' attempts to help by inspecting the output of the process for you,
#' and generates a final, compact summary that will highlight both
#' successes and potential issues:
#' 
#' \figure{buildAndCheck-TestError.png}{Example run with a test failure}
#'
#' Additionally, the user is reminded of what the next steps might be
#' (uploading the package to an internal repository with
#' \code{\link{addPackageToRepo}}, or testing the package as a local
#' installation). If previously existing tar.gz or PDF files were
#' overwritten, the user will be alerted in case they need to revert
#' from version control.
#'
#' @param dir Default '.' (the current directory) that contains the R
#'     package
#' @param cran Default \code{TRUE}, which will run checks '--as-cran'
#'     (more stringent, suitable for submission to CRAN)
#' @param name Default code{NULL}, which will be reset to "." Will be
#'     passed to \link{currentPackageName}
#' @param path Default code{NULL}, will be reset to '..'. This
#'     parameter is passed to \code{devtools::build()}, and is where
#'     the .tar.gz file will be created.
#' @param max Default \code{15}. If a number is provided, then if
#'     the directories holding tar.gz and PDF files fill up with more
#'     versions than this limit, the version control commands to move
#'     older files to an archive folder will be shown. Removing or
#'     renaming the \code{archive} subfolder will suppress this
#'     functionality.
#'
#' @return A structure mostly produced by check_built(), but also has
#'     the tar.gz file path attached to (\code{$package}) and a
#'     structured summary of any test failures (\code{$failedTests}).
#'
#' @seealso \code{\link{incrementPackageVersion}}, \code{\link{addPackageToRepo}}
#' 
#' @examples
#'
#' \dontrun{
#' ## Build the package residing in the current directory (presumes
#' ## your working directory is where the DESCRIPTION file is)
#' buildDetails <- buildAndCheck()
#' }
#'
#' @importFrom crayon blue yellow red cyan
#' @importFrom CatMisc is.something relativePath
#' @importFrom devtools document build check_built
#' @importFrom utils help capture.output
#' @export
    
buildAndCheck <- function(dir='.', cran=TRUE, name=NULL, path=NULL, max=15) {
    pkgDat  <- currentPackageName(dir=dir, name=name, path=path)
    pkgName <- pkgDat["PackageName"] # Name of the package
    pkg     <- pkgDat["SourceDir"]   # 'True' package source directory
    tgzdir  <- pkgDat["TarGzDir"]    # Target directory for tar.gz file
    pdfdir  <- pkgDat["PdfDir"]      # Target dir for PDF documentation
    vcdir   <- pkgDat["VcDir"]       # Root directory of project version ctrl.
    vctype  <- pkgDat["VcType"]      # Type of version control observed

    if (is.na(pkgName)) {
        .wrapMsg("Uncertain package - not running BUILD.", color='yellow')
        return(NA)
    }

    pkSlash <- pkg
    if (!grepl('/$', pkSlash)) pkSlash <- paste0(pkSlash,'/')
    message(crayon::cyan("Documenting, building and checking:\n  ",
                         .hilite(pkgName), "in", .hilite(pkSlash)))
    
    ## Make not of already generated tar.gz packages, so we can warn
    ## user if we have overwritten one:
    tgzExists <- list.files(tgzdir, pattern='\\.tar.gz$')
    
    ## Run roxygen documentation construction - would be nice if this
    ## included a return value with warnings or errors but appears
    ## not. check_built() should capture some (most?) documentation issues.
    message(crayon::cyan("## Documenting ..."))
    ## Try to intercept documentation messages so we can summarize them
    try(docMsg <-
            utils::capture.output(devtools::document(pkg=pkg), type="message"))
    if (!exists("docMsg")) {
        print(traceback())
        stop("Documentation failed")
    }
    docI       <- character()
    ## Some messages are expected, don't bother the user with them:
    newRoxy    <- grepl('^First time using roxygen', docMsg)   # Setup
    addDel     <- grepl('^(Writing|Deleting) .+\\.Rd', docMsg) # Add/Delete
    updRoxy    <- grepl('^Updating roxygen version', docMsg)   # Update version
    benignMsgs <- c(paste("Updating", pkgName, "documentation"),
                    paste("Loading", pkgName), docMsg[addDel | updRoxy | newRoxy])
    ## Roxygen's standard warning format:
    docWarnFmt <- 'Warning: (.+) \\[.+?/(.+?)#(\\d+)\\]: (.+)'
    ## A more compact format:
    myFmt      <- "%s Line %s | %s : %s"
    for (msg in setdiff(docMsg, benignMsgs)) {
        ## Look at unexpected messages from the document() call
        err <- parenRegExp(docWarnFmt, msg)
        if (CatMisc::is.something(err)) {
            ## Looks like a formalized error string. Colorize it:
            docI <- c(docI, sprintf(myFmt, err[2], err[3],
                                    crayon::cyan(err[1]), crayon::red(err[4])))
        } else if (grepl('unloadNamespace', msg)) {
            ## Potentially concerning issues
            docI <- c(docI, crayon::yellow(msg))
        } else {
            ## All others
            docI <- c(docI, msg)
        }
    }
    
    ## Build the package:
    message(crayon::cyan("## Building ..."))
    pkgFile <- devtools::build(pkg=pkg, path=tgzdir)
    
    ## Check for issues. Having very odd issues where the R CMD call
    ## is claiming the tgz when I use a relative path:
    ## "is neither a file nor directory, skipping".
    ## If I generate an absolute file path it seems to work ok.
    message(crayon::cyan("## Checking ..."))
    output <- devtools::check_built( path=pkgFile, cran=cran)

    ## Add any issues that we noticed from documentation
    output$docissues <- docI

    
    ## Make sure the user is aware of any example files that have not
    ## yet been removed. We won't treat this as an error, because they
    ## may be using them as references (their purpose) during
    ## development.
    exF  <- file.path(pkg, c("R/exampleCodeDeleteMe.R",
                             "R/errorRiddledCodeDeleteMe.R",
                             "tests/testthat/test-harmlessExamplesDeleteMe.R"))
    exF  <- exF[ file.exists(exF) ]
    if (length(exF) != 0) output$notes <- c(output$notes,paste(c(
        "myRepository example files remain in your package:", exF),
        collapse="\n  "))

    ## Similarly, see if they've taken the time to properly customize
    ## the DESCRIPTION file. Parse it out, look for template text:
    dF    <- file.path(pkg, "DESCRIPTION")
    dDat  <- read.dcf(dF)
    dCol  <- colnames(dDat)
    dProb <- character()

    if (is.element("RemoveMe", dCol))
        dProb <- c(dProb, "You should remove the 'RemoveMe:' field")
    ## Title is needed and should be changed from boilerplate:
    if (is.element("Title", dCol)) {
        if (any(grepl('Do something useful to something else', dDat[,"Title"])))
            dProb <- c(dProb, "You should update the 'Title:' field")
    } else {
        dProb <- c(dProb, "You need a 'Title:' field")
    }
    ## Description is needed (?) and should be changed from boilerplate:
    if (is.element("Description", dCol)) {
        if (any(grepl('Be verbose here', dDat[,"Description"])))
            dProb <- c(dProb, "You should update the 'Description:' field")
    } else {
        dProb <- c(dProb, "You need a 'Description:' field")
    }
    ## One of the Authors fields should be set
    if (is.element("Authors@R", dCol)) {
        if (any(grepl('example.com', dDat[,"Authors@R"])))
            dProb <- c(dProb, "You should update the 'Authors@R:' field")
    } else if (!is.element("Author", dCol)) {
        dProb <- c(dProb, "You need an 'Author:' or 'Authors@R:' field")
    }

    if (is.element("URL", dCol)) {
        ## URL is not needed, but shouldn't be left as boilerplate
        if (any(grepl('username/reponame', dDat[,"URL"])))
            dProb <- c(dProb, "You should update or remove the 'URL:' field")
    }
    if (is.element("BugReports", dCol)) {
        ## BugReports is not needed, but shouldn't be left as boilerplate
        if (any(grepl('username/reponame', dDat[,"BugReports"])))
            dProb <- c(dProb, "You should update or remove the 'BugReports:' field")
    }
    ## I'd really like to also include a dummy/null package in the
    ## Imports: section, so I could check to see if the user has
    ## updated that. But I couldn't find any CRAN packages that I was
    ## comfortable would *never* be used (ie, the package equivalent
    ## of 'example.com')

    ## If it looks like there's work to be done on DESCRIPTION, inject
    ## it as a 'Note':
    if (length(dProb) != 0) output$notes <- c(output$notes,paste(c(
        "Your DESCRIPTION file has example text or needs additions:", dProb),
        collapse="\n  "))

    fProb <- character()
    ## Check out some other templates that would benefit from customization
    ## First the "repository level" readme:
    readme <- file.path(vcdir, "README.md")
    if (file.exists(readme)) {
        rml <- readLines(readme)
        if (any(grepl("ToDo", rml)))
            fProb <- c(fProb,"Repository (parent directory) README.md needs an updated description")
    }
    ## Now the "package level" readme:
    readme <- file.path(pkg, "README.md")
    if (file.exists(readme)) {
        rml <- readLines(readme)
        if (any(grepl("ToDo", rml)))
            fProb <- c(fProb,"Package source README.md needs an updated description")
        if (any(grepl("Note to self", rml)))
            fProb <- c(fProb,"Package source README.md needs updated examples")
    }
    
    if (length(fProb) != 0) output$notes <- c(output$notes,paste(c(
        "Some peripheral files could be edited:", fProb),
        collapse="\n  "))

    ## Did the user set a top-level package topic?
    if (exists("packHelp")) rm("packHelp")
    try(packHelp <- utils::help((pkgName), (pkgName)), silent=TRUE)
    if (!exists("packHelp")) output$notes <- c(output$notes,
        "It is strongly recommended that you add a package-level help topic; Do so with an Roxygen block associated with the string '_PACKAGE'")
   
    
    ## Tally up all warnings, errors, notes and documentation issues,
    ## and summarize them:
    nNum <- length(output$notes)
    dNum <- length(output$docissues)
    wNum <- length(output$warnings)
    eNum <- length(output$errors)
    if (nNum > 0) {
        message(.bar(), sprintf("%d Note%s:\n", nNum,
                                if(nNum==1){''}else{'s'}),
                crayon::cyan(paste(output$notes, collapse="\n")))
    }
    if (dNum > 0) {
        message(.bar(), sprintf("%d Documentation Issue%s:\n", dNum,
                                if(dNum==1){''}else{'s'}),
                paste(output$docissues, collapse="\n"))
    }
    if (wNum > 0) {
        message(.bar(), sprintf("%d Warning%s:\n", wNum,
                                if (wNum==1) {''} else {'s'}),
                crayon::yellow(output$warnings))
    }
    if (eNum > 0) {
        errs <- output$errors
        for (e in seq_len(length(errs))) {
            if (grepl('^checking tests ... ERROR', errs[e])) {
                ## This is the start of a somewhat messy message
                ## regarding a failed test. We are going to
                ## prettyprint test errors below with
                ## summarizeTests(), so tidy this entry up.
                errs[e] <- "One or more tests failed. See summary below."
            }
        }
        message(.bar(), sprintf("%d Error%s:\n", eNum,
                                if (eNum==1) {''} else {'s'}),
                crayon::red(errs))
    }

    ## Drill into testing a bit more
    output$failedTests <- summarizeTests(pkg=pkgName, errors=eNum)
    
    if (eNum == 0 && wNum == 0) {
        ## Compiled ok
        mbit <- crayon::green("Great! No errors or warnings!")
        if (nNum > 0 || dNum > 0) {
            ## Point out if there are Notes or Documenation Issues
            ## that should be looked into:
            mb <- c()
            if (nNum > 0) mb <- c(mb, paste0(nNum, " Note",
                                             ifelse(nNum == 1, '', 's')))
            if (dNum > 0) mb <- c(mb, paste0(dNum, " Doc Issue",
                                             ifelse(dNum == 1, '', 's')))
            mbit <- c(mbit, crayon::cyan(paste0(
                "  You should still check the ", paste(mb, collapse=' & '),
                " at some point.")))
        }
        message(.bar(), mbit)
    }
    .bar(TRUE)

    ## Report on generation of tar.gz package (the main reason we're here)
    baseTgz   <- basename(pkgFile)
    vcFiles   <- character()
    clobbered <- character()
    archive   <- character()
    if (!file.exists(pkgFile)) {
        .wrapMsg("[Significant Error!] Failed to find expected tar.gz build!",
                 pkgFile, color='red')
    } else {
        output$package <- pkgFile
        vfiles <- c(pkgFile, CatMisc::relativePath(vcdir,
                .makeCurrentSymlink(pkgFile, tgzdir, 'tar.gz'),normChild=FALSE))
        stat  <- fileRepoStatus(pkgFile) ## Repo status of file
        if (!is.element(baseTgz, tgzExists)) {
            ## First time file was made
            message(.hilite(baseTgz), " created in ", .fileCol(tgzdir))
        } else if (stat == 'M') {
            ## File exists, and it appears to have already been
            ## checked into the repository. It is bad practice to have
            ## multiple versions of the file with the same explicit
            ## version number on it, warn user:
            mbits <- c(.hilite(baseTgz), crayon::yellow(" replaced "),
                       "a file already present in\n  ", .fileCol(tgzdir))
            clobbered <- c(clobbered, vfiles)
            message(mbits)
        } else {
            ## File was updated, but it's not in VC
            message(.hilite(baseTgz), " updated in ", .fileCol(tgzdir))
        }
        vcFiles <- c(vcFiles, vfiles)
        archive <- c(archive,
                      .suggestVcArchive(pkgName, tgzdir, max, vcdir, vctype))
    }
    
    if (!is.na(pdfdir)) {
        ## Make PDF documentation
        pdfExists <- list.files(pdfdir, pattern='\\.pdf$')
        pdf       <- makePDF(pkgFile, dir=pdfdir, quiet=TRUE)
        basepdf   <- basename(pdf)
        if (!file.exists(pdf)) {
            .wrapMsg("Attempt to generate PDF documentation failed!",
                     color='red')
        } else {
            output$pdf <- pdf
            vfiles <- c(pdf, CatMisc::relativePath(vcdir,
                .makeCurrentSymlink(pdf, pdfdir, 'pdf'),normChild=FALSE))
            stat  <- fileRepoStatus(pdf) ## Repo status of file
            if (!is.element(basepdf, pdfExists)) {
                ## First time file was made
                message(.hilite(basepdf), " created in ", .fileCol(pdfdir))
            } else if (stat == 'M') {
                ## File exists, and it appears to have already been
                ## checked into the repository. It is bad practice to have
                ## multiple versions of the file with the same explicit
                ## version number on it, warn user:
                mbits <- c(.hilite(basepdf), crayon::yellow(" replaced "),
                           "a file already present in\n  ", .fileCol(pdfdir))
                clobbered <- c(clobbered, vfiles)
                message(mbits)
            } else {
                message(.hilite(basepdf), " updated in ", .fileCol(pdfdir))
            }
            vcFiles <- c(vcFiles, vfiles)
            archive <- c(archive,
                         .suggestVcArchive(pkgName, pdfdir, max, vcdir, vctype))
        }
    }
    ## Remind user how to restore any clobbered files:
    if (length(clobbered != 0))
        message(.suggestVcCheckout(clobbered, vcdir, vctype))

    
    np <- normalizePath(pkgFile)
    if (!.localOffline("")) {
        message(.bar(), "If satisfied with results, next step:\n  ",
                .actCol("addPackageToRepo('",pkgFile,"')"))
    } else if (CatMisc::is.something(np)) {
        message(.bar(),"If satisfied, you should now work on a server where the ",
                .opt('Name'), " file system is mounted:\n  ",
                crayon::cyan("Copy the tar.gz file to a visible location if needed, then:\n"),
                .actCol("  addPackageToRepo('",np,"')") )
    }
    if (length(archive) > 0)
        message(.brown("# Some files can be moved to archive:\n"),
                paste(archive, collapse='\n'))
    vers <- currentPackageVersion()
    vcFiles <- vcFiles[ !is.na(vcFiles) ]
    .suggestVcCommit(c(pkgName, vcFiles), vcdir, vctype, full=TRUE, tag=vers,
                     note="To record the package state in version control:",
                     msg="HELPFUL MESSAGE DESCRIBING CHANGES GOES HERE")
    
    message("To install the package locally for testing, run:\n  ",
            .actCol("install.packages('",np,"')"),"\n",
            .bar())
    invisible(output)
}

#' Summarize Test Output
#'
#' Parses the output from testthat to provide a semi-compact summary
#'
#' @details
#'
#' The default output provided to the user from failed tests is fairly
#' rudimentary. This function locates the testing folder created
#' during checks, and looks for the ".fail" file, then emits a
#' hopefully helpful message.
#'
#' If the file is missing, the user is reassured of its absence. If
#' present, it is parsed and a summary is generated from the
#' output. The summary is designed to filter out irrelevant content,
#' and presented as structured and colorized text.
#'
#' If testing did not appear to have occured, the user is coached on
#' how to set up a test framework.
#'
#' @param pkg Default \code{NULL}. The name of the package, needed to
#'     locate the output from CHECK. If NULL will default to
#'     \link{currentPackageName}.
#' @param errors Default \code{0}. Optional error count to remind user
#' that there were in-code errors even if the tests passed.
#'
#' @return Invisibly: \code{NULL} if there are problems finding the
#'     testing output folder, \code{NA} if no fail file is found, and
#'     a \code{list} object with failure details if problems were
#'     found.
#'
#' @seealso \code{\link{buildAndCheck}},
#'     \code{\link[testthat]{testthat}},
#'     \url{http://r-pkgs.had.co.nz/tests.html}
#'
#' @importFrom CatMisc parenRegExp
#' @importFrom stats setNames
#' @importFrom crayon blue green cyan red yellow
#' @export

summarizeTests <- function (pkg=NULL, errors=0) {

    .bar(TRUE)
    ## Need to know the package name to find the output directory:
    if (is.null(pkg)) pkg <- currentPackageName()['PackageName']
    if (is.na(pkg)) {
        .wrapMsg("Could not identify package name: Can't check for test output",
                 color='yellow')
        return(invisible(NULL))
    }

    ## The default temp directory should have been made for this session
    tmp <- tempdir()
    if (!dir.exists(tmp)) {
        .wrapMsg("The expected temporary directory was not found; It does not look like tests were run. Tests can't be summarized.", tmp, color='yellow')
        return(invisible(NULL))
    }
    
    ## The check process should make a particular subdirectory:
    chkDir <- sprintf("%s/%s.Rcheck", tmp, pkg)
    if (!dir.exists(chkDir)) {
        .wrapMsg("I could not find the expected Rcheck testing directory. Are you sure you've run tests (from *this* session)?", chkDir, color='yellow')
        return(invisible(NULL))
    }

    ## Ok. We can now start saying something concrete about testing.
    sdir  <- "tests/testthat"
    ttDir <- file.path(chkDir, sdir)
    if (!dir.exists(ttDir)) {
        .wrapMsg("It appears checks have been run, but no tests. If your package does not have a `tests/testthat` subfolder, run devtools::use_testthat() to set up the framework. Once present, you then should add test files to that subfolder, with file names of format 'test-SomethingUsefulHere.R'. See http://adv-r.had.co.nz/Testing.html for more guidance", color='cyan')
        return(invisible(NULL))
    }

    ## If we are here, testing appears to have been run!
    failFile <- file.path(chkDir, "tests", "testthat.Rout.fail")

    if (!file.exists(failFile)) {
        .wrapMsg("Great! Tests appear to have run, no evidence of failed tests seen!", color='green')
        if (errors > 0) .wrapMsg(paste0(
            "(But you still have ", errors, " in-code error",
            if (errors == 1) { "" } else { "s" }, ")"), color='red')
        return(invisible(NA))
    }

    struct       <- list()
    attr(struct, "details") <- failFile
        
    currentFile  <- ""
    currentBlock <- ""
    currentLine  <- ""
    crashed      <- FALSE
    lines <- readLines(failFile)
    

    .wrapMsg("Some tests appear to have failed; Details:", color='red')
    for (line in lines) {
        ## Newer versions of testthat are including UniCode in some of
        ## the output (in particular \x{2500}, light horizontal
        ## line). This is making regular expressions a bit of a
        ## hassle, and I'm concerned new unicode could crop up in the
        ## future. So strip all non-ascii characters out:
        ##   https://stackoverflow.com/a/9935242
        ascii <- iconv(line, "latin1", "ASCII", sub="")
        ## The unmodified line will still be reported to the user, and
        ## included in the returned structure.

        ## A placeholder flag used to indicate when a problem occured
        ## as a bug, rather than a failed test. Also used to suppress
        ## stack trace in the summary:
        fErr <- "Error in your code, file not reported"
        cnt <- CatMisc::parenRegExp("^OK:\\s+([0-9]+)\\s+SKIPPED:\\s+([0-9]+)\\s+FAILED:\\s+([0-9]+)", ascii)
        if (!is.na(cnt[1])) {
            ## Stop when we get to the summary
            cnt <- as.integer(cnt)
            fmt <- "%12s: %d"
            nms <- c("OK", "SKIPPED","FAILED")
            if (cnt[1] > 0) message(crayon::green(sprintf(fmt, nms[1], cnt[1])))
            if (cnt[2] > 0) message(crayon::cyan(sprintf(fmt, nms[2], cnt[2])))
            if (cnt[3] > 0) message(crayon::red(sprintf(fmt, nms[3], cnt[3])))
            if (crashed) message(crayon::red("  <Evidence of internal error>"))
            ## Attach summary counts to return value, because why not
            attr(struct, "counts") <- stats::setNames(cnt, nms)
            break
        }
        
        ## Look for failure header:
        ##   c( FailNum, FailType, Block, FileName, LineNumber )
        ##  OMG the error messages now have unicode dashes in them
        ##  To find the unicode escape, use:
        ##    sprintf("u%X", utf8ToInt(u))
        ##  ... where `u` contains the Unicode character of interest
        fhead <- CatMisc::parenRegExp("^[-\u2500 ]*([0-9]+)\\.\\s+(Failure|Error):\\s+(.+?)\\s+\\(@(.+)#([0-9]+)\\)", ascii)
        if (!is.na(fhead[1])) {
            if (fhead[4] != currentFile) {
                ## We've moved to a new file
                currentFile <- fhead[4]
                struct[[ currentFile ]] <- list()
                currentBlock <- ""
                message(crayon::red("File :", paste0(sdir, "/", currentFile)))
            }
            if (fhead[3] != currentBlock) {
                currentBlock <- fhead[3]
                struct[[ currentFile ]][[ currentBlock ]] <- list()
                message(crayon::red("  Test Block :",currentBlock))
            }
            currentLine <- as.character(fhead[5])
            struct[[ currentFile ]][[ currentBlock ]][[ currentLine ]] <-
                character()
            message(crayon::yellow(sprintf("    ==== Line %3s : %s ====",
                                           currentLine, fhead[2])))
            next
        }
        ## Another class of failure, eg:
        ##  Error in Matrix::readMM(fhDat$fh) : file is not a MatrixMarket file
        fother <- CatMisc::parenRegExp("^\\s*Error in (.+?) : (.+)$", ascii)
        if (!is.na(fother[1])) {
            ## This type of error appears to be due to fatal bugs in
            ## the code. Not sure how this gets emitted rather than
            ## 'Error:' above...
            
            ## Unfortunately, there does not appear to be clear
            ## indication of which file caused the error, let alone
            ## the line number.
            file <- fErr
            blk  <- "<Internal Error>"
            if (file != currentFile) {
                ## We've moved to a new file
                currentFile <- file
                struct[[ currentFile ]] <- list()
                currentBlock <- ""
                message(crayon::red("File :", file))
            }
            if (blk != currentBlock) {
                currentBlock <- blk
                struct[[ currentFile ]][[ currentBlock ]] <- list()
                message(crayon::red(" ",currentBlock))
            }
            ## We don't have a line number, so instead anchor with the
            ## statement that caused the error:
            currentLine <- fother[1]
            ## Nucleate the report with the error message:
            struct[[ currentFile ]][[ currentBlock ]][[ currentLine ]] <-
                c( fother[2] )
            message(crayon::yellow("    Offending statement:", currentLine))
            message(crayon::yellow("      Error:", fother[2]))
            crashed <- TRUE
            next
        }

        ## A code failure in the test file itself:
        ##     testthat/test-03-getData.R:56:15: unexpected ':'
        fother <- CatMisc::parenRegExp("^\\s+testhat/(.+?):([0-9]+):[0-9]+:\\s+(.+)$", ascii)
        if (!is.na(fother[1])) {
            file <- paste0("tests/testthat/",fother[1])
            blk  <- "<TestThat File Error>"
            if (file != currentFile) {
                ## We've moved to a new file
                currentFile <- file
                struct[[ currentFile ]] <- list()
                currentBlock <- ""
                message(crayon::red("File :", file))
            }
            if (blk != currentBlock) {
                currentBlock <- blk
                struct[[ currentFile ]][[ currentBlock ]] <- list()
                message(crayon::red(" ",currentBlock))
            }
            currentLine <- fother[2]
            ## Nucleate the report with the error message:
            struct[[ currentFile ]][[ currentBlock ]][[ currentLine ]] <-
                character()
            message(crayon::yellow(sprintf("    ==== Line %3s : %s ====",
                                           currentLine, currentLine)))
            message(crayon::yellow("      Error:", fother[3]))
        }
        
        
        
        if (currentFile == "" && currentBlock == "") next
        if (grepl("^\\s*$", ascii)) next
        if (grepl("^[ =]*testthat results[ =]*$", ascii)) next
        ## This is presumably a bit of detail on the failure. Show and
        ## add to the returned structure.
        struct[[ currentFile ]][[ currentBlock ]][[ currentLine ]] <-
            c(struct[[ currentFile ]][[ currentBlock ]][[ currentLine ]], line)
        ## This prefix shows up when our code crashes; Take out test_check:
        ascii <- gsub('^Calls: test_check \\.\\.\\.', 'Call Trace:', ascii)
        if (grepl("^[0-9]+: ", ascii) && currentFile != fErr) {
            ## This looks like a stack trace, and the problem appears
            ## to be a "real test" (was not a bug in the code). Traces
            ## are not as useful in this situation, so we won't
            ## display it (but it will be in the returned structure)
            next
        }
        message(crayon::yellow(strwrap(line, indent=6, exdent=8), collapse="\n"))
    }
    if (currentFile == "") .wrapMsg("Weird. The failure file was present, but I could not parse any 'Failure:' or 'Error:' blocks out of it. You should manually check the file:", color='red')
    .wrapMsg("Full details: ", .fileCol(failFile), color='cyan')

    return(invisible(struct))
}

#' Find BRAN Updates
#'
#' Check to see if any of your installed BRAN packages have updates available
#'
#' @param quiet Default FALSE. If TRUE, then message reporting will
#'     not occur
#' 
#' @return
#'
#' (invisibly) A data.frame with rows named for each package, $Local
#' referencing your installed version, $BRAN the repository version,
#' and $Status a token indicating either '=', '>', '<' or '?' (for
#' version number pairs that could not be mathematically compared).
#'
#' @examples
#'
#' \dontrun{
#' myBranPkgs <- findMyUpdates()
#' }
#'
#' # Note - the call to installed.packages() can be VERY slow on a
#' # system with many packages installed
#' 
#' @importFrom utils installed.packages compareVersion
#' @importFrom crayon red blue green
#' @export

findMyUpdates <- function( quiet=FALSE ) {
    ## https://stackoverflow.com/a/9341735
    whatIhave <- installed.packages()
    manifest  <- getMyManifest(details=FALSE, quiet=quiet)
    if (is.null(manifest)) {
        .wrapMsg("Custom repository location not defined ", color='red')
        return(invisible(NA))
    }
    myBran    <- intersect(rownames(whatIhave), rownames(manifest))
    nmb       <- length(myBran) 
    if (nmb == 0) {
        if (!quiet) message("You do not have any",.opt('Name'),"packages installed")
        rv <- data.frame(Local  = character(),
                         Status = character())
        rv[[ .opt('Name') ]]  <- character()
        attr(rv, 'stale') <- character()
        return(invisible(rv))
    }
    branPkg <- data.frame(Local     = whatIhave[ myBran, "Version"],
                          Status    = character(nmb),
                          row.names = myBran,
                          stringsAsFactors=FALSE)
    branPkg[[ .opt('Name') ]] <- manifest[myBran,"Version"]
    compToken <- c("<", "=", ">")
    branPkg$Status <- vapply(seq_len(nmb), function(i) {
        compToken[ 2+compareVersion(branPkg[i, "Local"], branPkg[i, .opt('Name') ]) ]}, "")
    neq      <- branPkg$Status != '='
    notEqual <- branPkg[ neq, ]
    if (nrow(notEqual) > 0) {
        if (!quiet) {
            message(crayon::red("Some of your packages differ from", .opt('Name')))
            print(notEqual[ order(notEqual$Status, rownames(notEqual)),])
        }
        outDated <- rownames(notEqual[ notEqual$Status == '<', ])
        if (length(outDated) > 0 && !quiet) message(
             crayon::blue("You can update all old packages with:"),
             "\n  install.packages(c('",paste(outDated,collapse="','"),
             "'))")
        attr(branPkg, 'stale') <- outDated
    } else {
        if (!quiet) message(crayon::green("All your packages are the same version as", .opt('Name')))
        attr(branPkg, 'stale') <- character()
    }
    invisible(branPkg)
}

#' Repository Statistics
#'
#' Get download statistics from repository web logs
#'
#' @param collate Default \code{month}, which will report monthly
#'     totals for downloads of each package
#'
#' @importFrom CatMisc is.something
#' @export

myRepoStatistics <- function(collate="month") {
    ## Parse the access_log file to determine how often packages are installed
    lf <- .opt("LogFile")
    if (!CatMisc::is.something(lf)) return(NA)
    message("TODO - still need to code up myRepoStatistics()")
    
}
