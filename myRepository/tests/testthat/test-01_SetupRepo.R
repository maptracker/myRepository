library("myRepository")

realRepos <- getOption("repos")

tmpd    <- tempdir()
rSubDir <- 'buildTestRepo'
rNm     <- 'ToyBuildRepo'
repoD   <- file.path(tmpd, rSubDir)
desc    <- "Toy R repository for build tests"


verifySessionSettings <- function(context) {
    ## Settings can be defined several ways, this function abstracts
    ## testing after alterations.
    mro <- getOption("myRepoOptions")
    expect_identical(mro$Name, rNm,
                     info=paste0(context, ": Repository name properly set"))
    expect_identical(mro$Description, desc,
                     info=paste0(context, ": Repository desc properly set"))
    expect_identical(mro$Path, repoD,
                     info=paste0(context, ": Repository directory properly set"))
    expect_identical(mro$URL, repoD,
                     info=paste0(context, ": Repository url is a fallback to path"))
    expect_identical(mro$Releases, "packageReleases",
                     info=paste0(context, ": Default tar.gz folder properly set"))
    expect_identical(mro$Documentation, "packageDocumentation",
                     info=paste0(context, ": Default PDF folder properly set"))
}

test_that("Initialize my very own R package repository", {
    expect_true(dir.create(repoD), info="Setting up toy R repository directory")

    ## Initialize the toy repository. This will also configure some settings:
    myRepoInitialize( path=repoD, name=rNm, description=desc )
    ## This action should have auto-sourced the settings file, check:
    verifySessionSettings("Initialization")
   

    ## I'm lazy and like strsplit as a way to manage some vectors
    ## Check that basic file structure is there:
    expDir <- file.path(repoD, strsplit('resources src/contrib', ' ')[[1]])
    ## The extracted_* directories are now tucked away under resources/ :
    expDir <- c(expDir, file.path(repoD, "resources", strsplit('extracted_DESCRIPTION extracted_PDF', ' ')[[1]]))
    expect_true(all(dir.exists(expDir)),
                info="Expected directory structure has been created")

    ## Check that the main directories were created:
    nowRepos <- getOption("repos")
    expect_identical(names(nowRepos), c(rNm, "CRAN"),
                     info="options('repos') names set")
    expect_equivalent(nowRepos[1], repoD,
                      info="Custom repo directory set in options")

    conf <- file.path(repoD, "resources", "configureRepo.R")
    expect_true(file.exists(conf), info="Configuration script exists")
    expect_identical(conf, getOption("myRepoOptions")$Configure,
                     info="Repo configuration script set in options")
})

test_that("Confirm sourcing of settings file", {
    conf <- getOption("myRepoOptions")$Configure
    ## Clear the options:
    options(myRepoOptions=NULL)
    ## Source them into environment:
    source(conf)
    ## Double-check
    verifySessionSettings("Via Source")
})
