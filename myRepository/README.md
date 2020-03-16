## myRepository R Package

This is the `myRepository` R package source code. To install and read
the initial documentation, run:

```R
install.packages("myRepository")
library("myRepository")
?myRepository
```

This package can be used by the [R programming language][R] to perform
two tasks:

### 1. Help you develop your own R pacakage

Several functions are available to make pacakge development easier. At
their foundation, these functions are using existing methods
(generally in `utils` or `devtools`), but include additional feedback
and "more modern" approaches to R development, particularly
documentation using [Roxygen][Roxygen].

```R
#### Create a new, empty R package:
createNewPackage("myOwnPackage")
```

`createNewPackage` is superficially using `package.skeleton`, but
includes a variety of additional steps and alterations. For the above
call, it will create the following file structure:

```
myOwnPackage/           # The 'version control' level
  README.md             # Template, to encourage documentation

  packageReleases/      # Target folder for tar.gz built packages
    README.md           # Reminder of why the folder is there, what it holds
    
  packageDocumentation/ # Target folder for PDF files
    README.md           # Reminder of why the folder is there, what it holds
    
  myOwnPackage/   # The actual R package level
    README.md     # Template, to encourage documentation
    DESCRIPTION   # Includes built-in instructions on how to edit itself
    R/
      exampleCodeDeleteMe.R              # An Roxygen tutorial / reference
    tests/
      testthat/
        test-harmlessExamplesDeleteMe.R  # A testing tutorial / reference
```

Calling `createNewPackage()` will also remind the user:

* Which files should then be altered / customized
* Which files should eventually be deleted (the examples)
* To run relevant version contol commands to check in the initial state
* To run `buildAndCheck` once they have some R code available.

#### Guided buidling of the tar.gz package

The `buildAndCheck()` process wraps up several `devtools` command, in
particular:

1. `devtools::document()`
2. `devtools::build()`
3. `devtools::check()`

Additionally, it checks to verify that template files have had
boilerplate updated; This includes `DESCRIPTION` and both the
repository and package `README.md` files. It will also note any
example files that are still present.

buildAndCheck() will also compactly summarize errors, warnings and
notes, and will also attempt to make a structured report for failed
tests.

It will finally report the commands needed to add the package to your
internal repository (see below), as well as version control commands
needed to commit the current state of the package.

### 2. Allow your organization to easily set up "your own CRAN"

The package includes several functions to setup and populate your own
personal repository, where you and your team can internally publish
your own R packages. Initial setup would follow something like:

```R
#### This gets run *once* when setting up:
myRepoInitialize(
    ## Your in-house name for your new repository:
    name="AcmeRAN",
    
    ## Longer description (cosmetic):
    description="Acme Corporation R Archive Network",

    ## The file system directory it will reside in:
    path="/data/AcmeRAN",

    ## The URL you are hosting it on:
    url="http://acme.example.com/",

    ## The path to CRAN you prefer:
    cran="https://cran.cnr.berkeley.edu",

    ## The Linux group name for people using the repo
    group="statuser",

    ## Subfolder users will put tar.gz files into
    releases='packageReleases',
    
    ## Subfolder PDF files go into:
    documentation='packageDocumentation' 
)

## It will create a configuration script that you can share with your
## team. The script will configure the "repos" option to put your
## in-house repository at "the front" of the list, and will allow
## transparent install.packages() calls to the repo. In the above
## example, that script will be:
source('http://acme.example.com/resources/configureRepo.R')

## Users can run that script "manually", or can easily add it to their
## .Rprofile file with:
rememberMyRepo()
```

__Note:__ You will need to manage the setup of the file system, domain
name and web server yourself, and to assure that those components meet
your organization's security requirements.

Once established, users can add and register their packages in the
repository using:

```R
addPackageToRepo("~/coolPackage/coolPackage_1.2.1.tar.gz")
updateMyRepo()
```

#### Repository Resources

The [parent directory](../) contains information not associated with
the formal R source.

* `.tar.gz` built package files can be found in
  [packageReleases/](../packageReleases)
* `.pdf` full documentation files can be found in
  [packageDocumentation/](../packageDocumentation)

[R]:
[Roxygen]: 
