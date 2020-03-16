#!/bin/bash

# This is just a shell script to grep through R sources and find
# functions that do not explicitly reference the package they are
# exported from. That is, the script attempts to find calls to bar(),
# rather than foo::bar(). The mechanism is imperfect, but is intended
# to help track down functions that might not be implicitly listed in
# @importFrom sections of Roxygen blocks.

srcDir="$1" # The source directory to scan - this should be your R/ folder


## Just a big, chained grep. Components:
##
##   Find things that look like function calls
##
##   Filter out those with ::
##
##   Filter out base:: functions. You can argue that even these should
##      be prefixed with ::, but here we're excluding them from the output



## Will fail to find issues in some circumstances:
##
##   Functions that colide with base. For example the Matrix::t function
##      conflicts with base::t, but is filtered out above.
##
##   foo::bar( bim() )  # Finds bim, then filters line out due to ::

## Will report non-existant issues in others:
##
##   Random text strings and comments - "Be sure to run process() to complete"



egrep -io '[-:0-9_a-z.]+\(' "$srcDir"/*.R | \
    grep -v '::' | \
    egrep -v ':(if|for|function|while)\(' | \
    egrep -v ':(abbreviate|abs|acos|acosh|addNA|addTaskCallback|agrep|agrepl|alist|all|all.equal|all.names|all.vars|any|anyDuplicated|anyNA|aperm|append|apply|Arg|args|array|arrayInd|as.array|as.call|as.character|as.complex|as.data.frame|as.Date|as.difftime|as.double|as.environment|as.expression|as.factor|as.function|as.hexmode|as.integer|as.list|as.logical|as.matrix|as.name|as.null|as.numeric|as.numeric_version|as.octmode|as.ordered|as.package_version|as.pairlist|as.POSIXct|as.POSIXlt|as.qr|as.raw|as.single|as.symbol|as.table|as.vector|asin|asinh|asNamespace|asS3|asS4|assign|atan|atan2|atanh|attach|attachNamespace|attr|attr.all.equal|attributes|autoload|autoloader|backsolve|baseenv|basename|besselI|besselJ|besselK|besselY|beta|bindingIsActive|bindingIsLocked|bindtextdomain|bitwAnd|bitwNot|bitwOr|bitwShiftL|bitwShiftR|bitwXor|body|bquote|browser|browserCondition|browserSetDebug|browserText|builtins|by|bzfile|c|call|callCC|capabilities|casefold|cat|cbind|ceiling|char.expand|character|charmatch|charToRaw|chartr|check_tzones|chkDots|chol|chol2inv|choose|class|clearPushBack|close|closeAllConnections|col|colMeans|colnames|colSums|commandArgs|comment|complex|computeRestarts|conditionCall|conditionMessage|conflicts|Conj|contributors|cos|cosh|cospi|crossprod|Cstack_info|cummax|cummin|cumprod|cumsum|curlGetHeaders|cut|data.class|data.frame|data.matrix|date|debug|debuggingState|debugonce|default.stringsAsFactors|delayedAssign|deparse|det|detach|determinant|dget|diag|diff|difftime|digamma|dim|dimnames|dir|dir.create|dir.exists|dirname|do.call|dontCheck|double|dput|dQuote|drop|droplevels|dump|duplicated|dyn.load|dyn.unload|dynGet|eapply|eigen|emptyenv|enc2native|enc2utf8|encodeString|Encoding|endsWith|enquote|env.profile|environment|environmentIsLocked|environmentName|eval|eval.parent|evalq|exists|exp|expand.grid|expm1|expression|extSoftVersion|factor|factorial|fifo|file|file.access|file.append|file.choose|file.copy|file.create|file.exists|file.info|file.link|file.mode|file.mtime|file.path|file.remove|file.rename|file.show|file.size|file.symlink|Filter|Find|find.package|findInterval|findPackageEnv|findRestart|floor|flush|flush.connection|force|forceAndCall|formals|format|formatC|formatDL|forwardsolve|gamma|gc|gcinfo|gctorture|gctorture2|get|get0|getAllConnections|getCallingDLL|getCallingDLLe|getConnection|getDLLRegisteredRoutines|getElement|geterrmessage|getExportedValue|getHook|getLoadedDLLs|getNamespace|getNamespaceExports|getNamespaceImports|getNamespaceInfo|getNamespaceName|getNamespaceUsers|getNamespaceVersion|getNativeSymbolInfo|getOption|getRversion|getSrcLines|getTaskCallbackNames|gettext|gettextf|getwd|gl|globalenv|gregexpr|grep|grepl|grepRaw|grouping|gsub|gzcon|gzfile|I|iconv|iconvlist|icuGetCollate|icuSetCollate|identical|identity|ifelse|Im|importIntoEnv|inherits|integer|interaction|interactive|intersect|intToBits|intToUtf8|inverse.rle|invisible|invokeRestart|invokeRestartInteractively|is.array|is.atomic|is.call|is.character|is.complex|is.data.frame|is.double|is.element|is.environment|is.expression|is.factor|is.finite|is.function|is.infinite|is.integer|is.language|is.list|is.loaded|is.logical|is.matrix|is.na|is.name|is.nan|is.null|is.numeric|is.numeric_version|is.object|is.ordered|is.package_version|is.pairlist|is.primitive|is.qr|is.R|is.raw|is.recursive|is.single|is.symbol|is.table|is.unsorted|is.vector|isatty|isBaseNamespace|isdebugged|isIncomplete|isNamespace|isNamespaceLoaded|ISOdate|ISOdatetime|isOpen|isRestart|isS4|isSeekable|isSymmetric|isSymmetric.matrix|isTRUE|jitter|julian|kappa|kappa.lm|kappa.qr|kronecker|l10n_info|La_library|La_version|La.svd|labels|lapply|lazyLoad|lazyLoadDBexec|lazyLoadDBfetch|lbeta|lchoose|length|lengths|levels|lfactorial|lgamma|libcurlVersion|library|licence|license|list|list.dirs|list.files|list2env|load|loadedNamespaces|loadingNamespaceInfo|loadNamespace|local|lockBinding|lockEnvironment|log|log10|log1p|log2|logb|logical|lower.tri|ls|make.names|make.unique|makeActiveBinding|Map|mapply|margin.table|mat.or.vec|match|match.arg|match.call|match.fun|Math.data.frame|Math.Date|Math.difftime|Math.factor|Math.POSIXt|matrix|max|max.col|mean|mem.limits|memCompress|memDecompress|memory.profile|merge|message|mget|min|missing|Mod|mode|months|names|namespaceExport|namespaceImport|namespaceImportClasses|namespaceImportFrom|namespaceImportMethods|nargs|nchar|ncol|NCOL|Negate|new.env|NextMethod|ngettext|nlevels|noquote|norm|normalizePath|nrow|NROW|numeric|numeric_version|nzchar|objects|oldClass|OlsonNames|on.exit|open|Ops.data.frame|Ops.Date|Ops.difftime|Ops.factor|Ops.numeric_version|Ops.ordered|Ops.POSIXt|options|order|ordered|outer|package_version|packageEvent|packageHasNamespace|packageStartupMessage|packBits|pairlist|parent.env|parent.frame|parse|parseNamespaceFile|paste|paste0|path.expand|path.package|pcre_config|pipe|pmatch|pmax|pmin|polyroot|pos.to.env|Position|pretty|prettyNum|print|prmatrix|proc.time|prod|prop.table|provideDimnames|psigamma|pushBack|pushBackLength|q|qr|quarters|quit|quote|R_system_version|R.home|R.Version|range|rank|rapply|raw|rawConnection|rawConnectionValue|rawShift|rawToBits|rawToChar|rbind|rcond|Re|read.dcf|readBin|readChar|readline|readLines|readRDS|readRenviron|Recall|Reduce|reg.finalizer|regexec|regexpr|registerS3method|registerS3methods|regmatches|remove|removeTaskCallback|rep|rep_len|replace|replicate|require|requireNamespace|restartDescription|restartFormals|retracemem|returnValue|rev|rle|rm|RNGkind|RNGversion|round|row|row.names|rowMeans|rownames|rowsum|rowSums|sample|sapply|save|save.image|saveRDS|scale|scan|search|searchpaths|seek|seek.connection|seq|seq_along|seq_len|sequence|serialize|set.seed|setdiff|setequal|setHook|setNamespaceInfo|setSessionTimeLimit|setTimeLimit|setwd|showConnections|shQuote|sign|signalCondition|signif|simpleCondition|simpleError|simpleMessage|simpleWarning|simplify2array|sin|single|sinh|sink|sinpi|slice.index|socketConnection|socketSelect|solve|sort|source|split|sprintf|sqrt|sQuote|srcfile|srcfilealias|srcfilecopy|srcref|standardGeneric|startsWith|stderr|stdin|stdout|stop|stopifnot|storage.mode|strftime|strptime|strrep|strsplit|strtoi|strtrim|structure|strwrap|sub|subset|substitute|substr|substring|sum|summary|suppressMessages|suppressPackageStartupMessages|suppressWarnings|svd|sweep|switch|sys.call|sys.calls|Sys.chmod|Sys.Date|sys.frame|sys.frames|sys.function|Sys.getenv|Sys.getlocale|Sys.getpid|Sys.glob|Sys.info|sys.load.image|Sys.localeconv|sys.nframe|sys.on.exit|sys.parent|sys.parents|Sys.readlink|sys.save.image|Sys.setenv|Sys.setFileTime|Sys.setlocale|Sys.sleep|sys.source|sys.status|Sys.time|Sys.timezone|Sys.umask|Sys.unsetenv|Sys.which|system|system.file|system.time|system2|t|table|tabulate|tan|tanh|tanpi|tapply|taskCallbackManager|tcrossprod|tempdir|tempfile|testPlatformEquivalence|textConnection|textConnectionValue|tolower|topenv|toString|toupper|trace|traceback|tracemem|tracingState|transform|trigamma|trimws|trunc|truncate|truncate.connection|try|tryCatch|typeof|unclass|undebug|union|unique|units|unix.time|unlink|unlist|unloadNamespace|unlockBinding|unname|unserialize|unsplit|untrace|untracemem|unz|upper.tri|url|UseMethod|utf8ToInt|validEnc|validUTF8|vapply|vector|Vectorize|warning|warnings|weekdays|which|which.max|which.min|with|withAutoprint|withCallingHandlers|within|withRestarts|withVisible|write|write.dcf|writeBin|writeChar|writeLines|xor|xor.hexmode|xor.octmode|xpdrows.data.frame|xtfrm|xzfile|zapsmall)\(' | \
    sort | uniq 


## List all objects in a package: https://stackoverflow.com/a/20535309
##   lsf.str("package:base")
##     ... plus a bunch of manual edits to take out .class redundancy  
