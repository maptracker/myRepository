## These settings are designed to configure your R session to use the
## <NAME> custom package repository. The values set are interpreted by
## the myRepository package. To use your internal repository, you
## should do ONE of the following:
##
##     * Copy-paste the contents of this file into an active R session
##     * source() this file, either by URL or by file path
##       (presuming your repository's file system is locally mounted)
##
## To silence status messages when this file is loaded, add to your .Rprofile:
##    options( myRepoQuiet=TRUE )

### --- Configuration settings for the 'myRepository' package: ---

## Defining your internal and external repositories. Requests to
## install packages will reference the first repository found in this
## vector that contains the request. This option is the most important
## one to allow install.packages to utilize your custom repository!
options( repos=stats::setNames(c(<REPOURLS>), c(<REPONAMES>)) )

## This option is a list structure that allows myRepository functions
## to interact with your repository. It is not neccesary if you only
## wish to install packages, but will be needed if you wish to
## contribute packages back to the repository:

options( myRepoOptions = list(
             
    ## Define the name of your local repository - this is mostly aesthetic
    Name="<NAME>",
    
    ## A description of your repositroy (entirely aesthetic):
    Description="<DESCRIPTION>",
    
    ## The local filesystem path of the repo, needed for modifying the
    ## repository. IMPORTANT if you wish to CONTRIBUTE packages!
    Path="<PATH>",
    
    ## The URL to the repository, needed to load information over HTTP/HTTPS
    ## IMPORTANT if you wish to USE packages!
    URL="<URL>",

    ## The URL (or path, if URL is blank) to this configuration script:
    Configure="<CONFSCRIPT>",
    
    ## The CRAN mirror you wish to use. If @CRAN@, will let (make) you
    ## choose each session you interact with CRAN, eg install.packages
    ## IMPORTANT in almost all cases!
    CRAN=c(<CRAN>),
    
    ## Optional *nix group, applied to user-created files and directories
    ## Usually important if you are contributing packages as a team effort
    Group="<GROUP>",

    ## Optional domain name for your organization, used to alert users
    ## that an email template might be to people outside your team
    OrgDomain="<ORGDOMAIN>",
    
    ## Optional folder for holding built tar.gz archives
    Releases="<RELEASES>",

    ## Optional folder for holding PDF documentation
    Documentation="<DOCUMENTATION>"
))

## Provide some feedback
.myRepoVB   <- is.null(getOption("myRepoQuiet")) || !getOption("myRepoQuiet")[1]
## Does it look like we're in install.packages? If so, suppress messaging
.nowInstall <- grepl('^/tmp/.+R\\.INSTALL', getwd())

if (.myRepoVB && !.nowInstall) {
    .myRepoName <- getOption("myRepoOptions")$Name
    .myRepoVers <- utils::installed.packages()
    .myRepoVers <- .myRepoVers[ which(rownames(.myRepoVers) == "myRepository"),
                               "Version"]
    ## Remind our users that BRAN is now 'available', and myRepository
    ## can be used to aid in package development:
    message("Internal ", .myRepoName,
            " repository will be consulted by install.packages()")

    if (length(.myRepoVers) == 0) {
        ## Not currently installed
        message("Additional development tools are available via:\n",
                "  install.packages('myRepository')")
        ## Initially I also auto-attached (library()'ed) myRepository
        ## if it was available. THIS IS A BAD IDEA. It causes
        ## significant problems when trying to install.package() newer
        ## versions of myRepository *or* any of the dependent
        ## pacakges.
    } else {
        message(sprintf("myRepository %s available via %s\n  More help: %s",
                        .myRepoVers[1],
                        crayon::magenta("library('myRepository')"),
                        crayon::magenta("?myRepository")))
    }
    rm(".myRepoName", ".myRepoVers") # Clean out temp variables
}
rm(".myRepoVB") # Clean out temp variables
