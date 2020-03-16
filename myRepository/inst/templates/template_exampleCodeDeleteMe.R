## This file is primarily intended as an instructional example in the
## usage of Roxygen documentation. It begins below with an exaustive
## example that attempts to use as many Roxygen features as
## possible. Following that, much more detailed examples break out and
## discuss each Roxygen component.

#' Retrieve Everything or Nothing
#'
#' An example chock full of Roxygen features
#'
#' @aliases wholeshebang allstuff
#' 
#' @details
#'
#' \code{everything} returns \emph{everything}, or negative
#' everything. Everything has some very useful properties:
#'
#' \itemize{
#'
#'   \item It is the inverse of nothing
#'
#'   \item It is likely larger than 7, though this has not been proven
#'         and is the subject of some disagreement.
#'
#'   \item It is not recognized as legal tender in most localities.
#' 
#' }
#'
#' The \code{nothing} function provides an important counterpoint to
#' \code{everything}. It can be used in situations where it is
#' important to have nothing.
#'
#' @param negative Default \code{FALSE}. If \code{TRUE}, will return
#'     negative everything instead
#' @param pointless Default \code{3.1414}. Parameter of unknown, and
#'     apparently unused, function. The default value is suspiciously
#'     close to - \strong{but not identical to} - \code{pi}.
#'
#' @return For \code{everything}, A large (or negatively large)
#'     numeric. For \code{nothing}, \code{NULL}.
#'
#' @seealso \code{\link{is.infinite}{base}},
#'     \url{https://en.wikipedia.org/wiki/Douglas_Adams},
#'     \href{http://kbroman.org/pkg_primer/pages/docs.html}{Nice
#'     intro}. Also consider the use of \code{0} instead of \code{nothing}.
#'
#' @examples
#'
#' everything()
#' everything( TRUE ) # Negative Everything
#' 
#' \dontrun{
#' ## Please do not pass *answers* to everything as arguments
#' everything( 42L )
#' }
#' 
#' @import crayon
#' @importFrom utils alarm
#' 
#' @export

everything <- function( negative=FALSE, pointless=3.1414 ) {
    rv <- if (negative) { -Inf } else { Inf }
    alarm() # Get the user's attention!
    message(crayon::blue("Preparing ... "), crayon::magenta("EVERYTHING"))
    rv
}

## Note that in order for everything() and nothing() to share the same
## documentation, we need to use @rdname to associate 'nothing' with
## 'everything'. We also need to @export, and can optionally add
## function-specific examples:

#' @rdname everything
#' @aliases nada zilch zippo
#'
#' @examples
#'
#' ## Get nothing
#' nothing()
#' 
#' @export

nothing <- function () {
    ## TODO: JSON, XML and ASN1 variants
    NULL
}


### - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #

## This file is designed to illustrate documentation with Roxygen.  It
## should be moved or deleted before you publish your pacakge, but it
## shouldn't cause harm (just clutter) if you forget.

## The functions below attempt to break Roxygen into bite-sized chunks
## to illustrate how the markup works. For each function, Roxygen
## instructional text will be in the @details block.

## All Roxygen lines start with "#'" (numbersign + singlequote)
## Generally, you also have a space as a third character to make it
## 'tidy'. You can also include empty lines (just the #') for
## legibility. Empty lines are sometimes needed to separate implied
## sections, such as between the name and title.

## The word 'object' in nearly all cases will be refering to an R
## function. However, it can also be a data set, an exported R
## variable, or an abstract topic; Any of these can be documented by
## an Roxygen block.

### - # - # - # - # - # - # - # - # - # - # - # - # - # - # - # - #


#' Basic Function
#'
#' This is a one-line description of a function that just returns 42
#'
#' @details
#'
#' This function is essentially useless, but illustrates the most
#' basic aspects of Roxygen.
#'
#' The section you're reading now is 'details', which in almost all
#' cases should be included to provide a more verbose description of
#' what the function does. You can provide as much commentary here as
#' needed to inform the user of the uses of the object.
#'
#' Immediately above the details block is the 'title' (here
#' "This is a one-line description of a function that just returns 42"),
#' which *can* occupy multiple lines, but should in most places be a
#' single sentance. It's a *brief* description of what the object
#' does.
#'
#' Above that, at the top of the Roxygen block is the 'name'. It can
#' be the literal name of the object (eg 'basicFunction' here), or a
#' more "human readable" name (eg as shown here, "Basic Function")
#'
#' Finally, below this block is the 'export' field. It instructs
#' Roxygen to add the object to the NAMESPACE file, which will in turn
#' "expose" the object to code that require()'s or library()'s the
#' package. You do not need to export your objects, but if you don't
#' they will only be "available" inside the package itself, or if you
#' use triple-colon notation (packageName:::objectName) to access
#' them.
#'
#' Also important: In order for Roxygen to properly associate this
#' special comment block with the object, the object should
#' *immediately* follow the Roxygen block (with the exception of blank
#' lines). There are 'name' and 'alias' fields (discussed in other
#' examples) but in almost all cases you will want to have this block
#' in immediate proximity to your code anyway.
#'
#' @export

basicFunction <- function () {
    ## For all those times you need to get answers
    42L
}

#' Object-less topics
#'
#' Sometimes you have something to say, but nothing to do
#'
#' @details
#'
#' You can make help topics on abstract subjects, not just
#' code-implemented objects. This can be very useful when you want to
#' provide documentation for theories, ideas or use cases that don't
#' neatly fit under a particular object that you've designed.
#'
#' @name abstractTopics
NULL

#' Special Characters
#'
#' A handful of special characters need to be escaped in Roxygen
#'
#' @details
#'
#' There are a few characters that need to be escaped with a backslash
#' if you want them to be literally printed. They are the percent
#' sign, both curly braces, and the backslash itself. In other words:
#'
#' \%, \{, \}, \\
#'
#' Additionally, since Roxygen uses the 'at sign' (@), you should be
#' careful not to start lines with it. It appears that older versions
#' of Roxygen require double at signs (@@) but single signs are ok in
#' newer releases.
NULL

#' Cross Links
#'
#' Topics can be linked to each other, or to websites
#'
#' @details
#'
#' It is possible to make links between topics. This requires the use
#' of "LaTeX-like" commands, which have the format of
#' '\\command\{\}'. There are several types of links available:
#'
#' \\link\{function\} - for functions in the same package, eg
#' \link{basicFunction}
#'
#' \\link\{function\}\{package\} - for a function in a different
#' package, eg \link{runif}{stats}
#'
#' \\url\{website\} - a link to a URL, eg \url{https://cran.r-project.org/}
#'
#' Links can be used anywhere, but their major utility is in the
#' 'seealso' Roxygen block. That is the customary location for placing
#' cross links to related topics, and can contain one or more links,
#' optionally including additional descriptive text. The convention
#' for linking to topics is to also include 'code' markup (see below),
#' as in \code{\link{basicFunction}}
#'
#' @seealso
#'
#' A few more examples of cross links are at
#' \url{https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html},
#' additionally this is an extraneous internal link to
#' \code{\link{basicFunction}}
#'
#' @examples
#'
#' ## This function just shows a URL for more formatting information
#' formatHelp()
#'
#' @export

formatHelp <- function() {
    url <- "https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html"
    message("Visit ",url," for more formatting help")
}

#' Pretty text
#'
#' Plain ASCII is boring, formatting commands are available
#'
#' @details
#'
#' This function points out that you can use LaTex syntax to add more
#' advanced formating to your documentation.
#'
#' The Roxygen code that you use does not get directly packaged into
#' your documentation. Instead, it is converted into the Rd file
#' format, which is more-or-less an ancient and powerful neck-beard
#' language called LaTex. So why use Roxygen anyway? Because:
#'
#' \itemize{
#'
#'     \item Roxygen is simpler and easier to use
#'
#'     \item Roxygen can be included "in line" with your code
#'
#'     \item Roxygen includes additional features like import (see below)
#' 
#' }
#'
#' However, it does not completely replace LaTex. For example, the
#' list above is using the "itemize" block, plus "item" bullet
#' points. It's worth remembering in case you feel bulleted lists
#' might be useful in your documentation. Here are some other commands
#' that are handy
#'
#' \\code\{\} can be used to designate blocks of text representing
#' functional R code. It is often used for reserved words such as
#' \code{TRUE}, \code{FALSE}, \code{NA} and \code{NULL}. It can also
#' be used for any code snippet, for example \code{1:10},
#' \code{runif(10,0,100)}.
#'
#' Italics can be specified with \\emph\{\} (eg \emph{A Tale of Two
#' Cities}) and bold text with \\strong\{\} (eg \strong{USE WITH
#' CAUTION})
#'
#' It is worth noting that formatting will vary depending on the
#' medium the documentaiton is being viewed in. 'Plain text' - as
#' shown in on the command line / terminal / shell - is generally
#' fairly restrictive. PDF and HTML documentation will display much
#' more extensive formatting.
#'
#' @export

fancyText <- function() {
    message("ASCII is dull. Text formatting is nice! Use \\{} blocks!")
}

#' Add Things
#'
#' Add two or three numeric values and return the sum
#'
#' @details
#'
#' Adds together two or (optionally) three values. This function is
#' designed to fill a gap in organizations where usage of the plus
#' symbol is taboo.
#'
#' This function illustrates the use of the 'param', 'return' and
#' 'example' fields.
#' 
#' If the function has parameters, you should describe them with
#' 'param' fields. If you are submitting to CRAN, they MUST be
#' described; Failure to do so will generate a warning when checking a
#' compiled package. It is nice to indicate default values in addition
#' to the purpose of the parameter, as well as any constraints (either
#' theoretical or enforced by the function)
#'
#' The 'return' block should describe the nature of the returned
#' value; At minimum it should specify the type ("character vector",
#' "numeric matrix", "data frame", etc) but ideally will also provide
#' a bit more information ("character vector of matching gene names",
#' "numeric matrix representing weights for each sample-model pair",
#' "data frame of metadata values for requested tags")
#' 
#' In most cases, it will be VERY useful to have an 'examples'
#' block. The code there must be executable R, as the examples will be
#' run when the compiled package is checked (though you can flag
#' examples to not be run, see destroyEverything below)
#'
#' @param x Default \code{5}, the value of X.
#' @param y Required. The value of Y. Y is an important value, as
#'     indicated by this extra text I added to demonstrate parameter
#'     deffinition on multiple lines.
#' @param z Default \code{NULL}, an optional third value to include in
#'     the sum. You can specify code components (numbers, TRUE/FALSE,
#'     NULL, function names, etc) using the optional \\code{} markup.
#'
#' @return A numeric value, the sum of the provided arguments
#'
#' @examples
#'
#' addThings(1,2) # Add two values
#' addThings(1,2,3) # Add three values
#' 
#' @export

addThings <- function (x=5, y, z=NULL) {
    rv <- x + y
    if (!is.null(z)) rv <- rv + z
    rv
}

#' Multiplied Uniform Distribution
#'
#' Product against the uniform distribution to highlight more special characters
#'
#' @details
#'
#' Multiples random numbers from the uniform distribution by a
#' user-supplied value.
#'
#' Illustrates special LaTeX-ish symbols
#' \itemize{
#'
#'     \item \\dots - Special code to get an elipsis, shows up as \dots
#'
#' }
#'
#' @param x Default 100, the number to multiply the random values by
#' @param n Default 5. Passed to \code{runif()} as the number of
#'     random values to pull from the uniform distribution
#' @param \dots Passed on to \code{runif()}
#'
#' @examples
#'
#' dotUniform()
#' ## Other parameters will be applied to runif:
#' dotUniform(min=-10, max=10)
#'
#' @importFrom stats runif
#' @export

dotUniform <- function(x=100, n=5, ...) {
    x * runif(n=n, ...)
}

#' Roxygen Field Order
#'
#' @export
#'
#' @examples
#'
#' scramble(1:10)
#'
#' @param x Required, a vector that is going to be scrambled
#'
#' @description In most cases, Roxygen field order does not matter!
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Literate_programming}
#'
#' @details
#'
#' This example is showing that the Roxygen fields can be provided in
#' any order. There are a few special case exceptions, but those are
#' really just short cuts that fill in for implicit fields:
#' 'description' and 'title'. To avoid making this example too
#' confusing, I've left the 'title' field ("Roxygen Field Order") as
#' the implicit first line, but I've moved the description to another
#' location. In doing so I now need to explicitly mark it with a
#' 'description' field.
#'
#' Having noted this 'feature', it is still STRONGLY suggested that
#' you use a consistent field order to help make your Roxygen more
#' readable. This is because Roxygen is not only a handy way to
#' generate the .Rd documentation files that will go into the
#' \code{man/} subfolder, it is also a form of Literate Programming;
#' The documenation is inline with and initimately associated with the
#' code that it documents. As such, keeping a predictable structure
#' will make it easier for another programmer to explore and
#' \strong{understand} your code.
#'
#' Once you become used to Roxygen, and begin reading other
#' Roxygen-documented code, you'll find this little example jarring
#' and disconcerting.  I will be using the general order of:
#'
#' @export

scramble <- function(x) {
    ## Randomly organize a provided vector
    sample(x, length(x), replace=FALSE)
}

#' Names and Aliases
#'
#' Topic names can be assigned automatically, or manually
#'
#' @details
#'
#' Normally the "name" of a topic is discovered automatically by
#' Roxygen by looking at the code/object immediately following the Roxygen
#' block. For example, the definition of the basicFunction function
#' above will allow the Roxygen topic to be found under
#' "?basicFunction"
#'
#' Sometimes you might have a topic that is not directly associated
#' with an object, or you are concerned that users may misremember the
#' 'actual' name and want them to locate the help through likely
#' misremembered terms (eg allow ?columnweights to find the 'real'
#' topic ?sampleweights). Alternatively, you might have multiple
#' distinct functions that should logically share the same help topic.
#'
#' The 'name' block addresses the first problem. This topic is not
#' directly associated with an object, so we \strong{need} to put
#' \code{NULL} immediately following the block. We also then add a
#' 'name' block that will allow the topic to be associated with
#' 'namesandaliases'
#'
#' We then want two of our functions to be associated with this topic;
#' They're listed after the NULL. In order to fully "tie" them to the
#' topic, we need to provide little Roxygen blocks for each function
#' that include the 'rdname' field linking them to the documentation
#' for "namesandaliases".
#'
#' If we wanted, we could provide only the 'rdname' and 'export'
#' fields in those blocks. That would be sufficient to: Tie them to
#' the 'namesandaliases' topic; Automatically add the function names
#' as aliases; Automatically generate appropriate "Usage" lines; and
#' finally to make the functions available (export). However, if
#' desired you can add additional Roxygen fields specific to each
#' function; Here we've chosen here to add examples for each.
#'
#' If you want to provide other names for the user to locate the help
#' topic, you can do so with the 'aliases' field. Here we've included
#' "speakName" and "speakAlias" as additional paths to find this help
#' (eg ?speakName works in addition to ?sayName). Including such
#' aliases may be useful in some cases, but it can be reasonably
#' argued that it might make the help system more cluttered or
#' confusing.
#'
#' Note that instead of making this topic block associated with
#' \code{NULL}, we could have instead associated it with one of the
#' functions (eg sayName), and then would only need to add the
#' 'rdname' field to the other(s). Having the main topic be abstractly
#' associated with NULL may make the documentation clearer.
#'
#' @name namesandaliases
#' @aliases speakName speakAlias
#' 
NULL

#' @rdname namesandaliases
#'
#' @examples
#'
#' sayName()
#'
#' @export

sayName <- function() {
    message("My name is Sam")
}

#' @rdname namesandaliases
#' 
#' @examples
#'
#' sayAlias()
#'
#' @export

sayAlias <- function() {
    message("People also know me as Sammy")
}

#' Function With Import
#'
#' A function that uses functions from other packages
#'
#' @details
#'
#' Lets you know, colorfully, who is happy
#'
#' This function demonstrates the \code{@import} and
#' \code{@importFrom} sections in Roxygen, buy defining a function
#' that utilizes two "external" packages.
#'
#' "imports" are external functions, data structures or whole packages
#' that are "imported" into your package. Imports are managed in two
#' places: The NAMESPACE and DESCRIPTION files. Roxygen will manage
#' the NAMESPACE file for you, but it will be up to you to keep
#' DESCRIPTION up to date. The vast majority of imported packages
#' should be listed under "Imports:" in that file.
#'
#' Roxygen has two import blocks; \code{@import} and
#' \code{@importFrom}. \code{@import} allows you to import an entire
#' package. \code{@importFrom} is more surgical, and allows one or
#' more specific objects to be imported. \code{@importFrom} is likely
#' more informative in terms of helping to document your code.
#'
#' Also note: 'base' is not the only package that is loaded by default
#' when you start R. Other notable packages are 'stats', 'methods',
#' 'utils', and 'graphics'. You may not recognize that you use
#' functions from these packages. If you see warnings that you haven't
#' specified all imports properly, check if the function is from one
#' of the "default" packages.
#'
#' "no visible global function definition for <func>" is the error
#' generated during build that indicates a lack of import. If you
#' encounter this, find where you're calling the function and add the
#' relevent import/importFrom block to the Roxygen.
#'
#' I have noticed that if I make typos in my Roxygen, or decide to
#' remove a package from imports, that I sometimes have to manually
#' edit NAMESPACE to remove extraneous lines.
#'
#' @param who Default \code{"Everyone"}, the person who is happy
#' @param happy Default \code{TRUE}. Are they really happy?
#'
#' @importFrom stats runif
#' @import crayon
#'
#' @examples
#'
#' importHappiness("SpongeBob")
#' importHappiness("The Grinch", FALSE)
#' 
#' @export

importHappiness <- function ( who="Everyone", happy=TRUE ) {
    ## runif "lives" in the "stats" package, we have "surgically"
    ## imported just that function
    level <- runif(1, 1, 100)
    if (happy) {
        ## We have imported *ALL* of the crayon package, so we can
        ## directly use the "green" method from that package, which
        ## will add ANSI color codes to make the text green in
        ## terminal output:
        message(green(sprintf("%s is %.1f%% happy!", who, level)))
    } else {
        ## HOWEVER - it does not hurt to explicitly specify
        ## PKG::FUNC. This is proof positive against unforseen
        ## namespace collisions; That is, it prevents ambiguity if
        ## your search space has imported *another* function named
        ## 'red'. As much as possible, it is advised to explicitly
        ## specify the package when using imported functions:
        message(crayon::red(sprintf("%s is %.1f%% unhappy!", who, level)))
        ## Realistically ... you can avoid doing this in most cases,
        ## particularly if the relevant functions have atypical or
        ## distinct names. But *it does not hurt*.
    }
}

#' Function related to another function
#'
#' Tells you who is sad
#'
#' @details
#'
#' This function is just a vehicle to illustrate internal cross-links
#' between functions with the 'seealso' field.
#'
#' @param who Default \code{"Panda"}, the person who is sad
#'
#' @seealso \code{\link{importHappiness}}
#'
#' @examples
#'
#' sadness("Philbert")
#'
#' @export

sadness <- function (who="Panda") {
    message(sprintf("%s is sad.", who))
}

#' Dangerous and Conditional Functions
#'
#' This function destroys everything
#'
#' @details
#'
#' Demolish. Every. Single. Thing. In a reproducible manner!
#'
#' Sometimes you have a function that:
#'
#' \itemize{
#' 
#'    \item Can be damaging when run
#'
#'    \item Can not run unless other conditions have been met
#'
#'    \item Takes a long time to run
#' }
#'
#' In these cases, you likely do not want to run (or simply can't run)
#' the function during package compilation. You can use the
#' \\dontrun\{\} block to provide an example that won't be run during
#' build or check operations.
#'
#' @examples
#'
#' \dontrun{
#' destroyEverything()
#' }
#' 
#' @export

destroyEverything <- function() {
    message("BWHAHAHA!!!")

### TODO: Implement global destruction when time permits. Outsource?
    
}

#' Package-level documentation
#'
#' Provide a "top-level" help topic for the entire package
#'
#' @details
#'
#' It's highly recommended that you include a help topic associated
#' with the package as a whole. This is done by simply making an
#' Roxygen block that is associated with the "naked" string
#' \code{"_PACKAGE"}. The help files will then include a topic for
#' YourPackageName-package.
#'
#' You can then access the topic using
#' \code{?YourPackageName-package}, and additionally with
#' \code{?YourPackageName}, unless the package includes a function of
#' that name as well. If this is the case, it's a good idea to put
#' \code{@seealso} cross-links between ?YourPackageName-package and
#' ?YourPackageName.
#'
#' Additionally, this topic will be sorted to the top of all topics
#' when the monolithic PDF file is generated. This is often far more
#' useful than starting with the first alphabetically sorted function.
#'
#' The topic should describe what the package as a whole is meant to
#' do; You may wish to synchronize the summary you put here with the
#' summary in the \code{Description:} section of your DESCRIPTION
#' file. It's also a nice idea to list out the primary functions
#' you're going to use; I like using an itemized list of linked
#' functions, maybe in a dedicated section, as shown
#' below. Alternatively / additionally, you could use the
#' \code{@seealso} section to iterate major functions.
#'
#' If you have formal examples and/or vignettes, the package-level
#' topic is a great place to advertise their presence. If not, or in
#' addition, you can include an \code{@examples} section to highlight
#' enlightening code snippets.
#'
#' You should NOT use a \code{@name} block - Roxygen will
#' automatically set up the names for you, and determine if
#' \code{?YourPackageName} should point to the package or a same-named
#' function.
#'
#' @section Primary Functions:
#' 
#' \itemize{
#'   \item \link{everything} - Get everything
#'   \item \link{nothing} - Get nothing
#'   \item \link{formatHelp} - Show helpful URL on Roxygen formatting
#'   \item \link{fancyText} - Exhorts you to use formatting
#'   \item etc etc
#' }
#'
#' @examples
#'
#' ## Typical use case for this package. Begin your workflow:
#' boxes <- list()
#'
#' ## Start packing
#' boxes$Kitchen  <- everything()
#' boxes$Basement <- everything()
#' boxes$Office   <- nothing()
#'
#' ## Oops, left out some things from the kitchen
#' boxes$Kitchen  <- addThings(boxes$Kitchen, 3)
#' 

"_PACKAGE"
