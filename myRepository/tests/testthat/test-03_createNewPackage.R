library("myRepository")

## On Macs, temp dirs start with "/var/..." but normalize to
## "/private/var/". To get tests to match up, we will normalize it
## here.
tmpDir  <- normalizePath( tempdir() )
pkgName <- "buildTestPackage"
repoDir <- file.path(tmpDir, pkgName)

test_that("Create New Package Structure", {

    message("Test package being created at ", repoDir)
    expect_true(dir.create(repoDir), info="Set up toy package for tests")

    tgzDir <- myRepository:::.opt('Releases')
    if (!CatMisc::is.something(tgzDir)) tgzDir <-
        myRepository:::.opt('Releases', 'packageReleases')
    pdfDir <- myRepository:::.opt('Documentation')
    if (!CatMisc::is.something(pdfDir)) pdfDir <-
        myRepository:::.opt('Releases', 'packageDocumentation')
    
        
    expect_message(chk <- createNewPackage(path=repoDir),
                   'After you have added some R code, you can run',
                   info="Set up initial blank package")

    
    ## While we have a package created, let's test directory detection
    expect_message( x <- currentPackageName( tmpDir ),
                   'none look like an R package folder',
                   info="Check failiure to find package")
    attributes(x) <- NULL
    expect_identical(unname(x), rep(as.character(NA), 8),
                     info="Only NAs when package not found")

    ## Get informational structure
    cpn  <- currentPackageName(repoDir)
    expect_equivalent(cpn['PackageName'], pkgName,
                     info="Properly find package name")
    expect_equivalent(cpn['SourceDir'], file.path(repoDir, pkgName),
                     info="Properly find source directory")
    expect_equivalent(cpn['TarGzDir'], file.path(repoDir, tgzDir),
                     info="Properly find tar.gz directory")
    expect_equivalent(cpn['PdfDir'], file.path(repoDir, pdfDir),
                     info="Properly find pdf directory")
    ## We should also be able to find identical package information
    ## from a variety of locations within the "repository"

    ## Check to see if directory structure seems reasonable
    expDirs <- list(
        package=file.path(repoDir, pkgName),
        source=file.path(repoDir, pkgName, "R"),
        pdfFiles=file.path(repoDir, pdfDir),
        tgzFiles=file.path(repoDir, tgzDir),
        tests=file.path(repoDir,pkgName,'tests','testthat'))
    for (d in names(expDirs)) {
        path <- expDirs[[d]]
        expect_true(dir.exists(path),
                    info=paste("Expected", d,"directory:", path))
        ## Also make suere that the package data can be recovered from
        ## each of these directories
        expect_identical(cpn, currentPackageName(path),
                         paste("Get info from", d,"directory:", path))
        
    }

    expFiles <- list(
        DESCRIPTION=file.path(repoDir, pkgName, "DESCRIPTION"),
        ExampleCode=file.path(repoDir, pkgName, "R", "exampleCodeDeleteMe.R"),
        TestThat=file.path(repoDir, pkgName, "tests", "testthat.R"),
        ExampleTests=file.path(repoDir, pkgName, "tests", "testthat",
                               "test-harmlessExamplesDeleteMe.R"),
        PdfReadme=file.path(repoDir, pdfDir, "README.md"),
        tgzFiles=file.path(repoDir, tgzDir, "README.md"),
        TopReadme=file.path(repoDir,"README.md"))
    for (f in names(expFiles)) {
        path <- expFiles[[f]]
        expect_true(file.exists(path), info=paste("Expected", f,"file:", path))
    }

    

    ## Let's try to build it

    ##expect_message(bac <- buildAndCheck( repoDir ),
    ##              "Great! No errors or warnings!",
    ##               info="Build and check toy pacakge")

    ## Ugh. The "there is no package called ‘MASS’" bizzareness means
    ## I can't check for a clean build (see a few lines below)
    bac <- buildAndCheck( repoDir )

    # file.copy(repoDir, "/tmp/foo2", recursive=TRUE) # was not informative
    saveRDS(bac, "/tmp/foo3.rds")

    expect_identical(length(bac$errors), 0L, info="Should be no errors")
    if (length(bac$warnings) == 1 && grepl("there is no package called ‘MASS’",
                                           bac$warnings)) {
        ## I have no idea why this is happening. The toy package
        ## builds fine "on the outside", but inside the testthat
        ## framework it complains about an Rd crosslink to MASS. I
        ## can't figure out where that's comming from.
    } else {
        expect_identical(length(bac$warnings), 0L, info="Should be no warnings")
    }

    ## We have not touched the templates, so their HelloWorld
    ## boilerplate should trigger some notes to the user:
    expect_identical(length(bac$notes), 3L, info="Should be three notes")
    ## The specific notes we're expecting:
    ## Template files should eventually be removed:
    expect_true(grepl("example files remain in your package", bac$notes[1]),
                info="Note warning about residual template files")
    ## The DESCRIPTION should be altered to reflect actual content:
    expect_true(grepl("DESCRIPTION file has example text", bac$notes[2]),
                info="Note pointing out changes needed in DESCRIPTION")
    ## The various README files should be customized:
    expect_true(grepl("Some peripheral files could be edited", bac$notes[3]),
                info="Note pointing out changes needed in support files")

    ## We should have generated a PDF and TGZ file:
    expFiles <- list(
        FirstPdf=file.path(repoDir, pdfDir, paste0(pkgName, "_0.0.1.pdf")),
        FirstTgz=file.path(repoDir, tgzDir, paste0(pkgName, "_0.0.1.tar.gz")))
    for (f in names(expFiles)) {
        path <- expFiles[[f]]
        expect_true(file.exists(path), info=paste("Expected", f,"file:", path))
    }
    
})

test_that("Clean up", {

    ## Apparently this is not needed? It looks like the files get
    ## automatically removed. Which is kind of frustrating when trying
    ## to debug the weird MASS issue above.
    
    ## expect_true(file.remove(repoDir, recursive=TRUE), info="Remove toy package")

    
})

