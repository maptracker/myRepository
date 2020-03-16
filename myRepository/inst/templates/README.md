These files are used by `myRepository`, generally either when running
`myRepoInitialize()` to establish a new R archive, or when calling
`createNewPackage()` to initialize a blank package structure.

The files are read by `copyTemplateFile()`, which then searches for
`<TAG>` tokens and substitutes values provided by the `replace=`
parameter.

