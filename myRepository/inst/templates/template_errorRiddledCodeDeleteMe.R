## This file contains examples of bad code, either bad R or bad
## Roxygen. It's designed to be used as a way to learn how to
## interpret errors generated during build and check, and to highlight
## pitfalls you might stumble into with Roxygen.

## In particular, not all errors are readily interpretable. It's hoped
## this file will serve as a searchable reference of error messages
## that can be correlated with potential causes.

### You can integrate this file into a new project with:
##    createNewPackage(addbugs=TRUE)

# Each Roxygen block will be preceeded with one or more example error
## messages (flagged with ERROR:) that will be generated some stage of
## buildAndCheck().

### - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

## ERROR: (none generated, but output is not as desired)

#' Unescaped Percent Signs
#'
#' @name error_LatexPercents
#'
#' @details
#'
#' The percent sign is used in LaTeX as a comment character. If it is not
#' escaped, it will not be rendered and will also mask the rest of the
#' line. Instead, use a backslash to escape it, like this: 100\%
#'
#' Unescaped percent signs will likely just cause missing chunks in
#' your documentation, but in some cases may remove enough
#' "functional" bits to cause a weird (and likely hard to track)
#' error.
#'
#' @seealso \url{https://cran.r-project.org/doc/manuals/R-exts.html#Insertions}
#'
NULL

## ERROR: (? I think this can yield errors, but have not found an example yet)

#' Unescaped Curly Braces Signs
#'
#' @name error_LatexCurlyBraces
#'
#' @details
#'
#' Both left and right curly braces should be escaped with a backslash, eg:
#'
#' JSON: \{ 'color': 'winterfrost' \}
#'
#' I seem to recall having an error once with this, but have not been
#' able to reproduce an example.
#'
#' @seealso \url{https://cran.r-project.org/doc/manuals/R-exts.html#Insertions}
#'
NULL


## ERROR: unknown macro '\whatever_it_was_that_had_a_slash'

#' Unescaped Latex Backslashes
#'
#' @name error_LatexStrayBackslashes
#'
#' @details
#'
#' If you need to use literal backslashes you can't leave them 'naked':
#'
#' A stray \ all by itself apparently will not throw an error? But I
#' suspect it will not render, either.
#'
#' Some text about "\foo" will fail
#'
#' Instead, make sure you escape the backslash (with two backslashes):
#'
#' Some text about "\\bar" is ok
#'
NULL


## ERROR: unknown macro '\item'

#' LaTex Space After Backslash
#'
#' @name error_LatexSpaces1
#'
#' @details
#'
#' You can not leave a space between a latex command and the preceding
#' backslash:
#'
#' \ itemize{
#'
#'     \item The space after 'itemize' will upset Rd checks
#' }
#'
#' In this case, the error manifests as confusion over the item tag,
#' which was expected inside an 'itemize' that didn't get registered
#' because of the stray space.
#'
NULL


## ERROR: unexpected TEXT ' ', expecting '{'

#' LaTex Space Before Curly Bracket
#'
#' @name error_LatexSpaces2
#'
#' @details
#'
#' You can not leave a space between a latex command and the curly bracket:
#'
#' \itemize {
#'
#'     \item The space after 'itemize' will upset Rd checks
#' }
#'
NULL

## ERROR: Skipping invalid path: .someFunctionWithALeadingDot

#' No documentation for functions starting with dots
#'
#' @details
#'
#' Not technically an 'error', but perhaps undesired behavior...
#'
#' Objects in R that have a name starting with a dot are treated
#' specially. The do not show up in \code{ls()} unless you also
#' specify \code{all.names=TRUE}. In general, they are intended to
#' serve as internal entities that are not intended for user access.
#'
#' Sometimes you many wish to provide documentation for these objects,
#' however. The documentation will NOT be written, unless you specify
#' an alternative name using the Roxygen 'name' field. The convention
#' seems to be to replace the '.' with 'dot', eg:
#'
#' \code{#' @name dotHiddenDocumentation}
#' 
#' This issue will be announced with an early warning during building
#' along the lines of "Skipping invalid path:"

.hiddenDocumentation <- function () {
    message("This function does not do much. It begins with a dot")
}

## ERROR: cp: cannot stat ‘myRepo/R/.#main.R’: No such file or directory

#' Stray files in pacakge directory during BUILD
#'
#' BUILD operations are offended by unexpected files
#'
#' @name error_unkownFiles
#'
#' @details
#'
#' I find I frequently encounter this error when running
#' \link{buildAndCheck}:
#'
#' ERROR: cp: cannot stat ‘myRepo/R/.#main.R’: No such file or directory
#'
#' This message does little to explain what is happening. It indicates
#' that R has encounted a "disallowed" file during the build process,
#' and is refusing to continue (the build will halt immediately).
#'
#' In my case, 99% of these errors are because I have not saved a
#' 'valid' file that I was editting, and my editor (emacs) is leaving
#' a temporary file in the directory. The above example would be that
#' the \code{R/main.R} file was "under construction" and emacs has
#' placed a \code{R/.#main.R} temp file in the folder until I save the
#' "original" file.
#'
#' Whatever the reason, R is telling you the path to the file, and it
#' needs to be removed (or in some cases renamed to a valid name)
#' before you can build the pacakge.
#'
#' @seealso For full details, see \url{https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Package-subdirectories} for allowed folder/file structure.
#' 
NULL

## ERROR: Package required and available but unsuitable version: ‘FooBar’

#' Version failure on build
#' 
#' One or more packages needed for a build are the wrong version
#'
#' @name error_buildVersionMismatch
#'
#' If you are building a package (for example, with
#' \link{buildAndCheck}) and it fails with the error:
#'
#' \code{Package required and available but unsuitable version: ‘FooBar’}
#'
#' ... that indicates that package "FooBar" is required by your
#' package (or one of its dependencies) with additional version
#' requirements. In almost all cases this will mean that your package
#' is too old. These requirements are specified in the DESCRIPTION
#' file of each pacakge, for example you may have FooBar 1.4.1
#' installed, but your pacakge (or a dependency) is specifying the
#' requirement as \code{FooBar (>= 1.6.3)}.
#'
#' An almost-always-bad solution is to find that line, and remove the
#' version requirement, or decrease it to a version you already
#' have. This is bad because such requirements are put in for a
#' reason, generally because a function is needed from that package
#' that is not present in older versions. The only time it might be
#' reasonable to take this approach is if *you* are the one who put
#' the requirement in, and you know it to be in error.
#'
#' The almost-always-good solution is to update the out-of-date
#' package, by running \link[utils]{install.packages} (here it would be
#' \code{install.packages("FooBar")}. There might (?) be rare cases
#' when this could cause problems? In particular, it is possible to
#' specify that a package needs an *older* version of a dependency (eg
#' \code{FooBar (< 0.4.1)}). I'm not sure if an R installation can
#' maintain multiple versions of a package; If not, then in theory
#' (and very rarely) upgrading a package may cause another package
#' that depends on older versions of that code to break.
#'
#' That said, in >99% of all cases your path forward is to upgrade the
#' offending package.
#' 
NULL


## ERROR: Error in unloadNamespace(pkg_name) : 
## ERROR:   namespace ‘X’ is imported by ‘Y’ so cannot be unloaded
##   will also emit:
## ERROR: installation of package ‘X’ had non-zero exit status
##   ... but there are many other paths to both of the above messages.

#' Clean environment not possible
#'
#' R is failing to unload a package
#'
#' @name error_canNotCleanEnvironment
#'
#' I suspect that this error can occur in several situations. Where I
#' have encountered it is when I am running
#' \link[utils]{install.packages} to try to install package 'X' but my
#' \code{.Rprofile} file had code in in it that loaded package
#' 'Y'. That's fine, until I need to update either 'X' or 'Y'.
#'
#' The reason seems to be that install.packages desires a "fresh" R
#' environment to perform the installation; In particular, it needs
#' the package being installed (and all of its dependencies) to be
#' absent. However, it also sources your .Rprofile file. In my case, I
#' then "polluted" that environment by running my code.
#'
#' It is impossible to update the package until you "fix" your
#' .Rprofile. If you don't wish to remove the code entirely, you can
#' comment it out during the installation. Offending lines are ones
#' that call \link[base]{library} or \link[base]{require}, *OR* any
#' calls that directly access a package element with \code{::} or
#' \code{:::}, eg \code{somePackage::somefunction()}.
#'
#' After installation has occured, you can uncomment the lines
#' again. If you have multiple lines that cause issues, you can wrap
#' them in a block that's controlled by a single line. This is what's
#' in my .Rprofile:
#'
#' \code{
#'    # .installSafety <- FALSE # Uncomment to install dependent packages
#'    if (!exists('.installSafety')) {
#'        ## Attaching packages - can interfere with install.packages of
#'        ## these or dependent packages:
#'        library("myRepository")
#'        myRepository::currentPackageStatus()
#'    } else {
#'        rm(".installSafety") # Just tidy up a bit
#'    }
#' }
#'
#' When I need to update "myRepository", I uncomment the
#' .installSafety line, run the installation, then comment the line
#' back again.
#' 
NULL

## ERROR: 'tolerance' should be numeric

#' Bad argument to all.equal method
#'
#' Can be emitted from testthat::expect_equivalent
#'
#' @name error_testthatInfoMisplaced
#'
#' This was a confusing error I encountered while using testthat. I
#' was running a test along the lines of:
#'
#' \code{expect_equivalent(x, 1:5, "Check column extraction")}
#'
#' The problem is that the deffinition of expect_equivalent is:
#'
#' \code{
#' expect_equivalent(object, expected, ..., info = NULL, label = NULL,
#'       expected.label = NULL)
#' }
#'
#' ... where the issue is the "...". Without parameterizing the info
#' parameter, it got "sucked into" the dots and assigned to a
#' "downstream" call to \link[base]{all.equal} as the `tolerance`
#' parameter. The solution is to explicitly parameterize the info
#' field with \code{info=}:
#'
#' \code{expect_equivalent(x, 1:5, info="Check column extraction")}
#'
#' This is of course a problem for any function with parameters after
#' \dots, but fortunately the solution is simple.
#'
#' Note: This was not an issue in my older version (< 2.0.0) of
#' testthat
#'
#' @examples
#'
#' testthat::expect_equivalent(1:5, 1:5, "check something") # Error
#' testthat::expect_equivalent(1:5, 1:5, info="check something")
NULL
