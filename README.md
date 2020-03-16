## myRepository R Package

The `myRepository` package contains tools to allow an organization to
initialize, maintain and populate their own, internal R package
archive. In other words, you can use it to set up your own in-house
CRAN.

It also contains utilities to aid users in the development of
well-documented R packages. These tools are targetted towards new
programmers, or programmers who are new to the R language. They
encapsulate the process of:

* Creating a "blank" package (the `createNewPackage()` function)
* Building and checking the package (`buildAndCheck()`)
* Learning how to use Roxygen to easily document the package (the
  `exampleCodeDeleteMe.R` example file)
* Learning to use tests (`test-harmlessExamplesDeleteMe.R`)

#### Files

* Source code is in the [myRepository/](myRepository) subdirectory.
    * See the source's [README](myRepository/README.md) for usage examples.
* Package releases are in the [packageReleases/](packageReleases) directory
* Full documentation as PDFs can be found in
  [packageDocumentation/](packageDocumentation)

