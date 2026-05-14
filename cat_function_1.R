
# ══════════════════════════════════════════════════════════════
# SECTION 0: IMPORT DIAGNOSTICS HELPERS
# ══════════════════════════════════════════════════════════════

.getMlxtranDataPath <- function(mlxtran) {
  .file <- tryCatch(mlxtran$DATAFILE$FILEINFO$FILEINFO$file,
                    error = function(e) NULL)
  if (is.null(.file) || length(.file) == 0) return(NULL)
  .file <- gsub("^'|'$", "", .file)
  .pwd <- tryCatch(monolix2rx:::.monolixGetPwd(mlxtran),
                   error = function(e) ".")
  file.path(.pwd, .file)
}

.readRawCsvHeader <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(
    names(read.csv(path, nrows = 0, check.names = FALSE, stringsAsFactors = FALSE)),
    error = function(e) NULL
  )
}

.loadRawCsv <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  tryCatch(
    read.csv(path, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) NULL
  )
}

.getMlxtranDeclaredHeader <- function(mlxtran) {
  .hdr <- tryCatch(mlxtran$DATAFILE$FILEINFO$FILEINFO$header,
                   error = function(e) NULL)
  if (!is.null(.hdr)) return(as.character(.hdr))
  
  .txt <- tryCatch(as.character(mlxtran), error = function(e) NULL)
  if (is.null(.txt) || length(.txt) == 0) return(NULL)
  
  .m <- regmatches(.txt, regexec("header\\s*=\\s*\\{([^}]*)\\}", .txt))[[1]]
  if (length(.m) < 2) return(NULL)
  trimws(strsplit(.m[2], ",", fixed = TRUE)[[1]])
}

.diagnoseHeaderMismatch <- function(mlxtran) {
  .path <- .getMlxtranDataPath(mlxtran)
  .actual <- .readRawCsvHeader(.path)
  .expected <- .getMlxtranDeclaredHeader(mlxtran)
  
  if (is.null(.actual) || is.null(.expected)) return(NULL)
  
  .same <- identical(.actual, .expected)
  .onlyActual <- setdiff(.actual, .expected)
  .onlyExpected <- setdiff(.expected, .actual)
  .orderDiff <- setequal(.actual, .expected) && !identical(.actual, .expected)
  .lengthDiff <- length(.actual) - length(.expected)
  
  list(
    path = .path,
    actual = .actual,
    expected = .expected,
    same = .same,
    orderDiff = .orderDiff,
    onlyActual = .onlyActual,
    onlyExpected = .onlyExpected,
    lengthDiff = .lengthDiff
  )
}

.reconcileRawDataHeader <- function(raw, expectedHeader) {
  if (is.null(raw) || is.null(expectedHeader)) return(NULL)
  
  .actualHeader <- names(raw)
  
  # Case 1: extra unnamed first column in file from row.names
  if (length(.actualHeader) == length(expectedHeader) + 1 &&
      identical(.actualHeader[1], "")) {
    raw <- raw[, -1, drop = FALSE]
    .actualHeader <- names(raw)
  }
  
  # Case 2: mlxtran expects leading EMPTY column that is absent in file
  if (length(expectedHeader) == length(.actualHeader) + 1 &&
      identical(expectedHeader[1], "EMPTY")) {
    raw <- cbind(EMPTY = seq_len(nrow(raw)), raw, stringsAsFactors = FALSE)
    .actualHeader <- names(raw)
  }
  
  # Final reconciliation
  if (length(.actualHeader) == length(expectedHeader)) {
    names(raw) <- expectedHeader
    return(raw)
  }
  
  NULL
}

.diagnoseRawNonNumericColumns <- function(mlxtran, nExample = 3) {
  .path <- .getMlxtranDataPath(mlxtran)
  if (is.null(.path) || !file.exists(.path)) return(NULL)
  
  .raw <- .loadRawCsv(.path)
  if (is.null(.raw)) return(NULL)
  
  .res <- lapply(names(.raw), function(.nm) {
    .x <- .raw[[.nm]]
    if (!is.character(.x)) return(NULL)
    .ux <- unique(.x[!is.na(.x) & nzchar(.x)])
    if (length(.ux) == 0) return(NULL)
    
    .num <- suppressWarnings(as.numeric(.x))
    if (!any(is.na(.num) & !is.na(.x) & nzchar(.x))) return(NULL)
    
    data.frame(
      column = .nm,
      examples = paste(head(.ux, nExample), collapse = ", "),
      stringsAsFactors = FALSE
    )
  })
  
  .res <- .res[!vapply(.res, is.null, logical(1))]
  if (length(.res) == 0) return(NULL)
  do.call(rbind, .res)
}

.warnDetailedHeaderMismatch <- function(diag) {
  if (is.null(diag) || isTRUE(diag$same)) return(invisible(NULL))
  
  .msg <- c(
    "Data header mismatch detected during Monolix import.",
    paste0("File: ", diag$path),
    paste0("Expected header from mlxtran: ", paste(diag$expected, collapse = ", ")),
    paste0("Actual header in file: ", paste(diag$actual, collapse = ", "))
  )
  
  if (length(diag$onlyActual) > 0) {
    .msg <- c(.msg, paste0("Columns present only in file: ", paste(diag$onlyActual, collapse = ", ")))
  }
  if (length(diag$onlyExpected) > 0) {
    .msg <- c(.msg, paste0("Columns present only in mlxtran: ", paste(diag$onlyExpected, collapse = ", ")))
  }
  if (isTRUE(diag$orderDiff)) {
    .msg <- c(.msg, "The same column names were found but in a different order.")
  }
  if (length(diag$actual) > 0 && identical(diag$actual[1], "")) {
    .msg <- c(.msg, "The file contains an unnamed first column, often caused by writing CSV row names.")
  }
  
  warning(paste(.msg, collapse = "\n"), call. = FALSE)
}

.warnDetailedCoercion <- function(coercionDiag, nWarnings = 0) {
  if (is.null(coercionDiag) || nrow(coercionDiag) == 0 || nWarnings == 0) {
    return(invisible(NULL))
  }
  
  .lines <- apply(coercionDiag, 1, function(z) {
    paste0("- ", z[["column"]], ": example values = ", z[["examples"]])
  })
  
  .msg <- c(
    paste0("Non-numeric text values were encountered during mlxtran-based data remapping (", nWarnings, " coercion warning", if (nWarnings > 1) "s" else "", ")."),
    "The following raw data columns contain non-numeric text values:",
    .lines,
    "Please verify that columns expected to be numeric in the Monolix import are coded numerically."
  )
  
  warning(paste(.msg, collapse = "\n"), call. = FALSE)
}

.importMonolixDataSafe <- function(ui) {
  .mlxtran <- monolix2rx:::.monolixGetMlxtran(ui)
  .headerDiag <- .diagnoseHeaderMismatch(.mlxtran)
  .coercionDiag <- .diagnoseRawNonNumericColumns(.mlxtran)
  
  .warn <- character(0)
  .data <- withCallingHandlers(
    try(monolix2rx:::monolixDataImport(ui), silent = TRUE),
    warning = function(w) {
      .warn <<- c(.warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if (inherits(.data, "try-error")) {
    .msg <- as.character(.data)
    
    if (grepl("length of the headers between the mlxtran specified model and data are different",
              .msg, fixed = TRUE)) {
      .path <- .getMlxtranDataPath(.mlxtran)
      .raw <- .loadRawCsv(.path)
      .expected <- .getMlxtranDeclaredHeader(.mlxtran)
      .rawFixed <- .reconcileRawDataHeader(.raw, .expected)
      
      if (!is.null(.headerDiag)) {
        .warnDetailedHeaderMismatch(.headerDiag)
      }
      
      if (is.null(.rawFixed)) {
        stop(
          paste(
            "Monolix data import failed because the data-file header length differs from the mlxtran-declared header and could not be reconciled automatically.",
            "Please check the file header against the mlxtran header declaration."
          ),
          call. = FALSE
        )
      }
      
      .warn <- character(0)
      .data <- withCallingHandlers(
        try(monolix2rx:::monolixDataImport(ui, data = .rawFixed), silent = TRUE),
        warning = function(w) {
          .warn <<- c(.warn, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
      
      if (inherits(.data, "try-error")) {
        stop(as.character(.data), call. = FALSE)
      }
    } else {
      stop(.msg, call. = FALSE)
    }
  }
  
  .warn <- unique(.warn)
  .headerWarn <- any(grepl("header does not match what was specified", .warn, fixed = TRUE))
  .coercionN <- sum(grepl("NAs introduced by coercion", .warn, fixed = TRUE))
  
  if (.headerWarn && !is.null(.headerDiag)) .warnDetailedHeaderMismatch(.headerDiag)
  if (.coercionN > 0) .warnDetailedCoercion(.coercionDiag, .coercionN)
  
  .data
}


# ══════════════════════════════════════════════════════════════
# SECTION 1: ENDPOINT HANDLER
# ══════════════════════════════════════════════════════════════

.handleCategoricalEndpoint <- function(endpoint) {
  .cats <- endpoint$err$categories
  .code <- endpoint$err$code
  .parsed <- regmatches(.code, regexec(
    "^(\\w+)\\(P\\(\\s*\\w+\\s*=\\s*([^)]+)\\)\\)\\s*=\\s*(.+)$",
    .code
  ))[[1]]
  if (length(.parsed) != 4L) {
    stop("cannot parse categorical endpoint definition: '", .code, "'",
         call. = FALSE)
  }
  .link <- trimws(.parsed[2])
  .catVal <- trimws(.parsed[3])
  .linPred <- trimws(.parsed[4])
  if (length(.cats) == 2L) {
    .pName <- paste0("p", .catVal)
    if (.link == "logit") {
      .invLink <- paste0("1/(1 + exp(-(", .linPred, ")))")
    } else if (.link == "probit") {
      .invLink <- paste0("phi(", .linPred, ")")
    } else if (.link == "log") {
      .invLink <- paste0("exp(", .linPred, ")")
    } else {
      stop("unsupported link '", .link, "'", call. = FALSE)
    }
    return(paste(c(
      paste0(.pName, " <- ", .invLink),
      paste0("var ~ binom(n, ", .pName, ")")
    ), collapse = "\n"))
  }
  stop("categorical endpoints with >2 categories not yet supported",
       call. = FALSE)
}

# Shared patch environment: stores original monolix2rx functions once
.monolix2rx_patch_env <- if (
  exists(".monolix2rx_patch_env", envir = .GlobalEnv, inherits = FALSE)
) {
  get(".monolix2rx_patch_env", envir = .GlobalEnv)
} else {
  e <- new.env(parent = emptyenv())
  assign(".monolix2rx_patch_env", e, envir = .GlobalEnv)
  e
}


# ══════════════════════════════════════════════════════════════
# SECTION 2: IDEMPOTENT PATCHED .handleSingleEndpoint
# ══════════════════════════════════════════════════════════════

.install_handleSingleEndpoint_patch <- function(force = FALSE) {
  current <- monolix2rx:::.handleSingleEndpoint
  
  if (
    !force &&
    isTRUE(attr(current, "categorical_endpoint_patch"))
  ) {
    message("monolix2rx::.handleSingleEndpoint patch already installed.")
    return(invisible(TRUE))
  }
  
  if (!exists("handleSingleEndpoint_original", envir = .monolix2rx_patch_env, inherits = FALSE)) {
    assign(
      "handleSingleEndpoint_original",
      current,
      envir = .monolix2rx_patch_env
    )
  }
  
  .handleSingleEndpoint_patched <- function(endpoint) {
    if (endpoint$dist == "event") {
      stop("'event' endpoint not supported in translation yet", call. = FALSE)
    } else if (endpoint$dist == "categorical") {
      return(.handleCategoricalEndpoint(endpoint))
    } else if (endpoint$dist == "count") {
      stop("'count' endpoint not supported in translation yet", call. = FALSE)
    } else if (endpoint$dist == "lognormal") {
      .add <- "lnorm"
    } else if (endpoint$dist == "normal") {
      .add <- "add"
    } else if (endpoint$dist == "logitnormal") {
      .add <- "logitNorm"
    } else if (endpoint$dist == "probitnormal") {
      .add <- "probitNorm"
    }
    
    .prd <- ""
    
    if (endpoint$var != endpoint$pred) {
      .prd <- paste0(endpoint$var, " <- ", endpoint$pred, "\n")
    }
    
    if (endpoint$err$errName == "constant") {
      return(paste0(
        .prd,
        endpoint$var,
        " ~ ",
        .add,
        "(",
        endpoint$err$typical[1],
        ifelse(
          endpoint$dist == "logitnormal",
          paste0(", ", endpoint$min, ", ", endpoint$max),
          ""
        ),
        ")"
      ))
    } else if (endpoint$err$errName == "proportional") {
      return(paste0(
        .prd,
        endpoint$var,
        " ~ ",
        ifelse(.add == "add", "", paste0(.add, "(NA) + ")),
        "prop(",
        endpoint$err$typical[1],
        ")"
      ))
    }
    
    if (endpoint$err$errName %in% c("combined1", "combined1c")) {
      .combined <- " + combined1()"
    } else if (endpoint$err$errName %in% c("combined2", "combined2c")) {
      .combined <- " + combined2()"
    }
    
    if (endpoint$err$errName %in% c("combined1", "combined2")) {
      .prop <- paste0(" + prop(", endpoint$err$typical[2], ")")
    } else if (endpoint$err$errName %in% c("combined1c", "combined2c")) {
      .prop <- paste0(
        " + pow(",
        endpoint$err$typical[2],
        ", ",
        endpoint$err$typical[3],
        ")"
      )
    }
    
    paste0(
      .prd,
      endpoint$var,
      " ~ ",
      .add,
      "(",
      endpoint$err$typical[1],
      ")",
      .prop,
      .combined
    )
  }
  
  environment(.handleSingleEndpoint_patched) <- environment()
  
  attr(.handleSingleEndpoint_patched, "categorical_endpoint_patch") <- TRUE
  
  assignInNamespace(
    ".handleSingleEndpoint",
    .handleSingleEndpoint_patched,
    ns = "monolix2rx"
  )
  
  message("Installed monolix2rx::.handleSingleEndpoint categorical patch.")
  
  invisible(TRUE)
}

.uninstall_handleSingleEndpoint_patch <- function() {
  if (exists("handleSingleEndpoint_original", envir = .monolix2rx_patch_env, inherits = FALSE)) {
    assignInNamespace(
      ".handleSingleEndpoint",
      get("handleSingleEndpoint_original", envir = .monolix2rx_patch_env),
      ns = "monolix2rx"
    )
    
    message("Restored original monolix2rx::.handleSingleEndpoint.")
  }
  
  invisible(TRUE)
}



# ══════════════════════════════════════════════════════════════
# SECTION 3: IDEMPOTENT PATCHED .def2ini — Inject n <- fix(1) once
# ══════════════════════════════════════════════════════════════



.def2ini_has_n_fix <- function(x) {
  any(grepl("\\bn\\s*<-\\s*fix\\s*\\(", deparse(x), perl = TRUE))
}

.install_def2ini_patch <- function(force = FALSE) {
  current <- monolix2rx:::.def2ini
  
  if (
    !force &&
    isTRUE(attr(current, "categorical_n_patch"))
  ) {
    message("monolix2rx::.def2ini patch already installed.")
    return(invisible(TRUE))
  }
  
  if (!exists("def2ini_original", envir = .monolix2rx_patch_env, inherits = FALSE)) {
    assign(
      "def2ini_original",
      current,
      envir = .monolix2rx_patch_env
    )
  }
  
  .def2ini_patched <- function(def, pars, longDef) {
    .origFn <- get(
      "def2ini_original",
      envir = .monolix2rx_patch_env
    )
    
    .ini <- .origFn(def, pars, longDef)
    
    .hasCategorical <- any(vapply(seq_along(longDef$endpoint), function(i) {
      identical(longDef$endpoint[[i]]$dist, "categorical")
    }, logical(1)))
    
    if (!.hasCategorical) {
      return(.ini)
    }
    
    # Do not inject twice.
    if (.def2ini_has_n_fix(.ini)) {
      return(.ini)
    }
    
    .innerBlock <- .ini[[2]]
    .nExpr <- quote(n <- fix(1))
    .newBlock <- as.call(c(as.list(.innerBlock), list(.nExpr)))
    .ini[[2]] <- .newBlock
    
    .ini
  }
  
  environment(.def2ini_patched) <- environment()
  
  attr(.def2ini_patched, "categorical_n_patch") <- TRUE
  
  assignInNamespace(
    ".def2ini",
    .def2ini_patched,
    ns = "monolix2rx"
  )
  
  message("Installed monolix2rx::.def2ini categorical n patch.")
  
  invisible(TRUE)
}

.uninstall_def2ini_patch <- function() {
  if (exists("def2ini_original", envir = .monolix2rx_patch_env, inherits = FALSE)) {
    assignInNamespace(
      ".def2ini",
      get("def2ini_original", envir = .monolix2rx_patch_env),
      ns = "monolix2rx"
    )
    
    message("Restored original monolix2rx::.def2ini.")
  }
  
  invisible(TRUE)
}

# ══════════════════════════════════════════════════════════════
# SECTION 4: COVARIATE CODE FIXER
# ══════════════════════════════════════════════════════════════

.fixCategoricalCovariateCode <- function(mlxtran) {
  def <- mlxtran$MODEL$INDIVIDUAL$DEFINITION
  if (is.null(def)) return(mlxtran)
  .rx <- def$rx
  .var <- def$var
  for (vName in names(.var)) {
    .v <- .var[[vName]]
    if (is.null(.v$cov) || is.null(.v$coef)) next
    for (i in seq_along(.v$cov)) {
      .covName <- .v$cov[i]
      .coefVec <- .v$coef[[i]]
      .nonRefCoefs <- .coefVec[!grepl("^rxCov_", .coefVec)]
      if (length(.nonRefCoefs) > 1) {
        for (.coef in .nonRefCoefs) {
          .prefix <- paste0("^.*", .covName, "_")
          .catVal <- sub(.prefix, "", .coef)
          if (nchar(.catVal) > 0) {
            .oldPattern <- paste0(
              "(", gsub("\\.", "\\\\.", .coef), ")\\s*\\*\\s*",
              .covName, "(?![_\\w])"
            )
            .newReplace <- paste0("\\1 * (", .covName, " == ", .catVal, ")")
            .rx <- gsub(.oldPattern, .newReplace, .rx, perl = TRUE)
          }
        }
      }
      if (length(.nonRefCoefs) == 1) {
        .pattern <- paste0(
          "\\(\\s*", .covName, "\\s*==\\s*['\"][^'\"]+['\"]\\s*\\)"
        )
        .rx <- gsub(.pattern, .covName, .rx, perl = TRUE)
      }
    }
  }
  mlxtran$MODEL$INDIVIDUAL$DEFINITION$rx <- .rx
  mlxtran
}


# ══════════════════════════════════════════════════════════════
# SECTION 5: DATA FIXER
# ══════════════════════════════════════════════════════════════

.fixCategoricalCovariateData <- function(data, mlxtran) {
  if (is.null(data)) return(data)
  def <- mlxtran$MODEL$INDIVIDUAL$DEFINITION
  if (is.null(def)) return(data)
  .var <- def$var
  .catDefs <- mlxtran$MODEL$INDIVIDUAL$INDIVIDUAL$cat
  .rawData <- NULL
  .fileInfo <- mlxtran$DATAFILE$FILEINFO$FILEINFO
  if (!is.null(.fileInfo$file)) {
    .dataPath <- gsub("^'|'$", "", .fileInfo$file)
    .pwd <- tryCatch(monolix2rx:::.monolixGetPwd(mlxtran),
                     error = function(e) ".")
    .fullPath <- file.path(.pwd, .dataPath)
    if (file.exists(.fullPath)) {
      .rawData <- read.csv(.fullPath, stringsAsFactors = FALSE)
    }
  }
  for (vName in names(.var)) {
    .v <- .var[[vName]]
    if (is.null(.v$cov) || is.null(.v$coef)) next
    for (i in seq_along(.v$cov)) {
      .covName <- .v$cov[i]
      .coefVec <- .v$coef[[i]]
      .nonRefCoefs <- .coefVec[!grepl("^rxCov_", .coefVec)]
      if (!(.covName %in% names(data))) next
      .catDef <- .catDefs[[.covName]]
      if (is.null(.catDef)) next
      .col <- data[[.covName]]
      .isStringCat <- any(.catDef$quote) && !is.character(.col)
      .allNA <- all(is.na(.col))
      if (length(.nonRefCoefs) == 1 && (.isStringCat || .allNA)) {
        .prefix <- paste0("^.*", .covName, "_")
        .nonRefLabel <- sub(.prefix, "", .nonRefCoefs[1])
        .nonRefValue <- gsub("_", " ", .nonRefLabel)
        if (!is.null(.rawData) && .covName %in% names(.rawData)) {
          data[[.covName]] <- as.numeric(.rawData[[.covName]] == .nonRefValue)
          data[[.covName]][is.na(data[[.covName]])] <- 0
          message("  \u2139 recoded '", .covName, "': 1 = '",
                  .nonRefValue, "', 0 = reference")
        }
      }
    }
  }
  data
}

# ══════════════════════════════════════════════════════════════
# MODEL TXT PATH PATCHER
# Rewrites missing Monolix model .txt references inside mlxtran
# ══════════════════════════════════════════════════════════════

.isAbsolutePath <- function(x) {
  grepl("^([A-Za-z]:)?[\\/]", x)
}

.absFrom <- function(baseDir, path) {
  if (.isAbsolutePath(path)) {
    path
  } else {
    file.path(baseDir, path)
  }
}

.stripQuotes <- function(x) {
  gsub("^['\"]|['\"]$", "", x)
}

.patchMlxtranModelTxtPath <- function(mlxtranFile,
                                      modelTxtDir = NULL,
                                      modelTxtFile = NULL,
                                      preferLocal = TRUE,
                                      patchedSuffix = "_patched_modelpath") {
  mlxtranFile <- normalizePath(
    mlxtranFile,
    winslash = "/",
    mustWork = TRUE
  )
  
  mlxtranDir <- dirname(mlxtranFile)
  
  if (!is.null(modelTxtDir)) {
    modelTxtDir <- normalizePath(
      modelTxtDir,
      winslash = "/",
      mustWork = TRUE
    )
  }
  
  if (!is.null(modelTxtFile)) {
    modelTxtFile <- normalizePath(
      modelTxtFile,
      winslash = "/",
      mustWork = TRUE
    )
  }
  
  txt <- readLines(mlxtranFile, warn = FALSE)
  
  # Find .txt references, quoted or unquoted.
  # Examples:
  #   '../../some/path/model.txt'
  #   "../some/path/model.txt"
  #   model.txt
  .matches <- gregexpr(
    "['\"][^'\"]+\\.txt['\"]|[^[:space:],;=]+\\.txt",
    txt,
    perl = TRUE
  )
  
  tokens <- unique(unlist(regmatches(txt, .matches)))
  tokens <- tokens[nzchar(tokens)]
  
  if (length(tokens) == 0) {
    message("No .txt references found in mlxtran file; no patching needed.")
    return(mlxtranFile)
  }
  
  resolveReplacement <- function(oldPath, nTokens) {
    oldAbs <- .absFrom(mlxtranDir, oldPath)
    
    # If the original path still exists, no replacement is needed.
    if (file.exists(oldAbs)) {
      return(NULL)
    }
    
    oldBase <- basename(oldPath)
    
    # Explicit file has highest priority.
    if (!is.null(modelTxtFile)) {
      if (basename(modelTxtFile) == oldBase || nTokens == 1L) {
        return(modelTxtFile)
      }
    }
    
    # Search in user-supplied directory.
    if (!is.null(modelTxtDir)) {
      cand <- file.path(modelTxtDir, oldBase)
      if (file.exists(cand)) {
        return(normalizePath(cand, winslash = "/", mustWork = TRUE))
      }
    }
    
    # Optionally search beside the mlxtran and in current working directory.
    if (isTRUE(preferLocal)) {
      localCandidates <- c(
        file.path(mlxtranDir, oldBase),
        file.path(getwd(), oldBase)
      )
      
      localCandidates <- localCandidates[file.exists(localCandidates)]
      
      if (length(localCandidates) > 0) {
        return(normalizePath(localCandidates[1], winslash = "/", mustWork = TRUE))
      }
    }
    
    NULL
  }
  
  changed <- FALSE
  unresolved <- character(0)
  
  for (tok in tokens) {
    oldPath <- .stripQuotes(tok)
    newPath <- resolveReplacement(oldPath, length(tokens))
    
    if (is.null(newPath)) {
      oldAbs <- .absFrom(mlxtranDir, oldPath)
      
      if (!file.exists(oldAbs)) {
        unresolved <- c(unresolved, oldPath)
      }
      
      next
    }
    
    quoteChar <- substr(tok, 1, 1)
    isQuoted <- quoteChar %in% c("'", "\"")
    
    if (isQuoted) {
      replacement <- paste0(quoteChar, newPath, quoteChar)
    } else {
      replacement <- paste0("'", newPath, "'")
    }
    
    txt <- gsub(tok, replacement, txt, fixed = TRUE)
    changed <- TRUE
    
    message("Patched model .txt path:")
    message("  old: ", oldPath)
    message("  new: ", newPath)
  }
  
  if (!changed) {
    if (length(unresolved) > 0) {
      stop(
        paste0(
          "Could not resolve missing model .txt reference(s):\n",
          paste0("  - ", unresolved, collapse = "\n"),
          "\n\nProvide modelTxtDir or modelTxtFile."
        ),
        call. = FALSE
      )
    }
    
    return(mlxtranFile)
  }
  
  patchedFile <- file.path(
    mlxtranDir,
    paste0(
      tools::file_path_sans_ext(basename(mlxtranFile)),
      patchedSuffix,
      ".mlxtran"
    )
  )
  
  writeLines(txt, patchedFile, useBytes = TRUE)
  
  # If a matching .mlxproperties file exists, copy it beside the patched mlxtran.
  oldProp <- sub("\\.mlxtran$", ".mlxproperties", mlxtranFile, ignore.case = TRUE)
  newProp <- sub("\\.mlxtran$", ".mlxproperties", patchedFile, ignore.case = TRUE)
  
  if (file.exists(oldProp)) {
    file.copy(oldProp, newProp, overwrite = TRUE)
  }
  
  normalizePath(patchedFile, winslash = "/", mustWork = TRUE)
}



attach_monolix_data <- function(rx,
                                mlxtranFile,
                                dataDir = NULL,
                                searchDirs = NULL) {
  mlxtranFile <- normalizePath(
    mlxtranFile,
    winslash = "/",
    mustWork = TRUE
  )
  
  mlxtranDir <- dirname(mlxtranFile)
  
  txt <- readLines(mlxtranFile, warn = FALSE)
  
  data_line <- grep(
    "^\\s*(file|datafile)\\s*=",
    txt,
    ignore.case = TRUE,
    value = TRUE
  )
  
  if (length(data_line) == 0) {
    data_line <- grep(
      "file\\s*=",
      txt,
      ignore.case = TRUE,
      value = TRUE
    )
  }
  
  if (length(data_line) == 0) {
    stop("Could not find data file entry in mlxtran file.")
  }
  
  old_data_path <- sub(
    ".*file\\s*=\\s*",
    "",
    data_line[1],
    ignore.case = TRUE
  )
  
  old_data_path <- sub(",.*$", "", old_data_path)
  old_data_path <- gsub("[\"']", "", old_data_path)
  old_data_path <- trimws(old_data_path)
  
  old_data_file <- basename(old_data_path)
  
  is_absolute <- grepl("^[A-Za-z]:|^/", old_data_path)
  
  candidate_paths <- character()
  
  if (is_absolute) {
    candidate_paths <- c(candidate_paths, old_data_path)
  } else {
    candidate_paths <- c(
      candidate_paths,
      file.path(mlxtranDir, old_data_path),
      file.path(getwd(), old_data_path),
      file.path(mlxtranDir, old_data_file),
      file.path(getwd(), old_data_file)
    )
  }
  
  if (!is.null(dataDir)) {
    candidate_paths <- c(
      file.path(dataDir, old_data_path),
      file.path(dataDir, old_data_file),
      candidate_paths
    )
  }
  
  if (!is.null(searchDirs)) {
    candidate_paths <- c(
      file.path(searchDirs, old_data_path),
      file.path(searchDirs, old_data_file),
      candidate_paths
    )
  }
  
  candidate_paths <- unique(normalizePath(
    candidate_paths,
    winslash = "/",
    mustWork = FALSE
  ))
  
  existing_paths <- candidate_paths[file.exists(candidate_paths)]
  
  message("Monolix data path detected:")
  message("  old: ", old_data_path)
  message("  old file: ", old_data_file)
  
  if (length(existing_paths) == 0) {
    message("  resolved: not found")
    message("")
    message("Tried these candidate paths:")
    for (p in candidate_paths) {
      message("  - ", p)
    }
    
    stop(
      "Could not find Monolix data file '",
      old_data_file,
      "'. Provide its folder with dataDir = 'path/to/data/folder'."
    )
  }
  
  new_data_path <- normalizePath(
    existing_paths[1],
    winslash = "/",
    mustWork = TRUE
  )
  
  new_data_file <- basename(new_data_path)
  
  message("Patched Monolix data path:")
  message("  old: ", old_data_path)
  message("  old file: ", old_data_file)
  message("  new: ", new_data_path)
  message("  new file: ", new_data_file)
  
  monolixData <- tryCatch(
    read.csv(
      new_data_path,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e1) {
      tryCatch(
        read.delim(
          new_data_path,
          stringsAsFactors = FALSE,
          check.names = FALSE
        ),
        error = function(e2) {
          read.table(
            new_data_path,
            header = TRUE,
            sep = "",
            stringsAsFactors = FALSE,
            check.names = FALSE
          )
        }
      )
    }
  )
  
  attr(rx, "monolixData") <- monolixData
  attr(rx, "monolixDataPath") <- new_data_path
  attr(rx, "monolixDataPathOld") <- old_data_path
  attr(rx, "monolixDataFile") <- new_data_file
  attr(rx, "monolixDataFileOld") <- old_data_file
  
  rx
}

# ══════════════════════════════════════════════════════════════
# SECTION 6: MAIN WRAPPER — monolix2rx_categorical()
# ══════════════════════════════════════════════════════════════

.hasCategoricalEndpointMlxtran <- function(mlxtran) {
  .def <- tryCatch(mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint,
                   error = function(e) NULL)
  if (is.null(.def) || length(.def) == 0) return(FALSE)
  any(vapply(seq_along(.def), function(i) {
    identical(.def[[i]]$dist, "categorical")
  }, logical(1)))
}

monolix2rx_categorical <- function(mlxtranFile,
                                   modelTxtDir = NULL,
                                   modelTxtFile = NULL,
                                   update = TRUE,
                                   thetaMatType = c("sa", "lin"),
                                   ...) {
  if (!is.null(modelTxtDir) || !is.null(modelTxtFile)) {
    mlxtranFile <- .patchMlxtranModelTxtPath(
      mlxtranFile = mlxtranFile,
      modelTxtDir = modelTxtDir,
      modelTxtFile = modelTxtFile
    )
  }
  
  monolix2rx_fn <- monolix2rx::monolix2rx
  .b <- as.list(body(monolix2rx_fn))
  
  .b <- c(
    .b[1],
    list(substitute(.fixCategoricalCovariateCode <- FUNC,
                    list(FUNC = .fixCategoricalCovariateCode))),
    list(substitute(.fixCategoricalCovariateData <- FUNC,
                    list(FUNC = .fixCategoricalCovariateData))),
    list(substitute(.hasCategoricalEndpointMlxtran <- FUNC,
                    list(FUNC = .hasCategoricalEndpointMlxtran))),
    list(substitute(.getMlxtranDataPath <- FUNC,
                    list(FUNC = .getMlxtranDataPath))),
    list(substitute(.readRawCsvHeader <- FUNC,
                    list(FUNC = .readRawCsvHeader))),
    list(substitute(.loadRawCsv <- FUNC,
                    list(FUNC = .loadRawCsv))),
    list(substitute(.getMlxtranDeclaredHeader <- FUNC,
                    list(FUNC = .getMlxtranDeclaredHeader))),
    list(substitute(.diagnoseHeaderMismatch <- FUNC,
                    list(FUNC = .diagnoseHeaderMismatch))),
    list(substitute(.reconcileRawDataHeader <- FUNC,
                    list(FUNC = .reconcileRawDataHeader))),
    list(substitute(.diagnoseRawNonNumericColumns <- FUNC,
                    list(FUNC = .diagnoseRawNonNumericColumns))),
    list(substitute(.warnDetailedHeaderMismatch <- FUNC,
                    list(FUNC = .warnDetailedHeaderMismatch))),
    list(substitute(.warnDetailedCoercion <- FUNC,
                    list(FUNC = .warnDetailedCoercion))),
    list(substitute(.importMonolixDataSafe <- FUNC,
                    list(FUNC = .importMonolixDataSafe))),
    .b[2:length(.b)]
  )
  
  .modelLineIdx <- which(vapply(seq_along(.b), function(i) {
    any(grepl("\\.model <- c\\(", deparse(.b[[i]])))
  }, logical(1)))
  if (length(.modelLineIdx) == 1L) {
    .b <- c(
      .b[1:(.modelLineIdx - 1)],
      list(quote(.mlxtran <- .fixCategoricalCovariateCode(.mlxtran))),
      .b[.modelLineIdx:length(.b)]
    )
  }
  
  .dataImportIdx <- which(vapply(seq_along(.b), function(i) {
    any(grepl("\\.monolixData <- try\\(monolixDataImport\\(", deparse(.b[[i]])))
  }, logical(1)))
  if (length(.dataImportIdx) == 1L) {
    .b[[.dataImportIdx]] <- quote(.monolixData <- .importMonolixDataSafe(.ui))
  }
  
  .dataLineIdx <- which(vapply(seq_along(.b), function(i) {
    .dep <- paste(deparse(.b[[i]]), collapse = "")
    grepl("\\.ui\\$monolixData <- \\.monolixData", .dep)
  }, logical(1)))
  if (length(.dataLineIdx) == 1L) {
    .b <- c(
      .b[1:.dataLineIdx],
      list(quote(.ui$monolixData <- .fixCategoricalCovariateData(
        .ui$monolixData, .mlxtran))),
      .b[(.dataLineIdx + 1):length(.b)]
    )
  }
  
  .validateIdx <- which(vapply(seq_along(.b), function(i) {
    any(grepl("\\.validateModel\\(", deparse(.b[[i]])))
  }, logical(1)))
  if (length(.validateIdx) == 1L) {
    .b[[.validateIdx]] <- quote(
      if (.hasCategoricalEndpointMlxtran(.mlxtran)) {
        .minfo("categorical endpoint detected; skipping default model validation")
      } else {
        try(.validateModel(.ui, ci = ci, sigdig = sigdig), silent = TRUE)
      }
    )
  }
  
  body(monolix2rx_fn) <- as.call(.b)
  environment(monolix2rx_fn) <- asNamespace("monolix2rx")
  
  monolix2rx_fn(
    mlxtranFile,
    update = update,
    thetaMatType = thetaMatType,
    ...
  )
}


# ══════════════════════════════════════════════════════════════
# SECTION 7: STRING COMPARISON TO INDICATOR HELPERS
# ══════════════════════════════════════════════════════════════

.normCategoryLabel <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x <- tolower(x)
  x <- gsub("[^[:alnum:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

.extractStringComparisons <- function(exprs) {
  .txt <- vapply(exprs, deparse1, character(1))
  .pat <- "([[:alnum:]_]+)\\s*==\\s*['\"]([^'\"]+)['\"]"
  
  .res <- list()
  .k <- 1L
  
  for (i in seq_along(.txt)) {
    .m <- gregexpr(.pat, .txt[i], perl = TRUE)
    .hits <- regmatches(.txt[i], .m)[[1]]
    if (length(.hits) == 0 || identical(.hits, character(0))) next
    
    for (.hit in .hits) {
      .parts <- regmatches(.hit, regexec(.pat, .hit, perl = TRUE))[[1]]
      if (length(.parts) != 3) next
      
      .var <- .parts[2]
      .lab <- .parts[3]
      .norm <- .normCategoryLabel(.lab)
      .ind <- paste0(.var, "_", .norm)
      
      .res[[.k]] <- data.frame(
        variable = .var,
        label = .lab,
        normalized = .norm,
        indicator = .ind,
        stringsAsFactors = FALSE
      )
      .k <- .k + 1L
    }
  }
  
  if (length(.res) == 0) {
    return(data.frame(
      variable = character(0),
      label = character(0),
      normalized = character(0),
      indicator = character(0),
      stringsAsFactors = FALSE
    ))
  }
  
  .df <- do.call(rbind, .res)
  .df[!duplicated(.df), , drop = FALSE]
}

.rewriteStringComparisonsToIndicators <- function(exprs, map) {
  .txt <- vapply(exprs, deparse1, character(1))
  
  if (nrow(map) > 0) {
    for (i in seq_along(.txt)) {
      for (j in seq_len(nrow(map))) {
        .labEsc <- gsub("([][{}()+*^$.|\\\\?])", "\\\\\\1", map$label[j])
        
        .pat1 <- paste0(
          "\\(\\s*", map$variable[j], "\\s*==\\s*['\"]",
          .labEsc, "['\"]\\s*\\)"
        )
        .pat2 <- paste0(
          map$variable[j], "\\s*==\\s*['\"]",
          .labEsc, "['\"]"
        )
        
        .txt[i] <- gsub(.pat1, map$indicator[j], .txt[i], perl = TRUE)
        .txt[i] <- gsub(.pat2, map$indicator[j], .txt[i], perl = TRUE)
      }
    }
  }
  
  .newLines <- vapply(seq_along(.txt), function(i) {
    .d <- .txt[i]
    if (grepl("~\\s*binom\\(", .d)) {
      .m <- regmatches(.d, regexec("binom\\([^,]+,\\s*([^)]+)\\)", .d))[[1]]
      if (length(.m) >= 2) {
        return(paste0("rx_pred_ <- ", trimws(.m[2])))
      }
    }
    .d
  }, character(1))
  
  rxode2::rxode2(paste(c(.newLines, "rx_r_ <- 1"), collapse = "\n"))
}

.categoricalPredModel <- function(ui) {
  .map <- .extractStringComparisons(ui$lstExpr)
  .rewriteStringComparisonsToIndicators(ui$lstExpr, .map)
}


.getAttachedMonolixData <- function(ui) {
  .data <- attr(ui, "monolixData", exact = TRUE)
  if (!is.null(.data)) return(.data)
  
  .data <- tryCatch(ui$monolixData, error = function(e) NULL)
  if (!is.null(.data)) return(.data)
  
  NULL
}

.restoreMonolixDataAttrs <- function(to, from) {
  .attrs <- c(
    "monolixData",
    "monolixDataPath",
    "monolixDataPathOld",
    "monolixDataFile",
    "monolixDataFileOld"
  )
  
  for (.nm in .attrs) {
    .val <- attr(from, .nm, exact = TRUE)
    if (!is.null(.val)) {
      attr(to, .nm) <- .val
    }
  }
  
  to
}





.categoricalPrepData <- function(ui, predModel = NULL) {
  .data <- .getAttachedMonolixData(ui)
  
  if (is.null(.data)) {
    stop(
      paste(
        "monolixData is missing from the imported model object.",
        "Expected it either as attr(ui, 'monolixData') or ui$monolixData."
      ),
      call. = FALSE
    )
  }
  
  .data2 <- .data
  
  .idCol <- intersect(c("id", "ID", "Id"), names(.data2))
  if (length(.idCol) == 0) {
    stop("Could not identify ID column in monolixData", call. = FALSE)
  }
  .idCol <- .idCol[1]
  
  .timeCol <- intersect(c("time", "TIME", "Time"), names(.data2))
  if (length(.timeCol) == 0) {
    stop("Could not identify TIME column in monolixData", call. = FALSE)
  }
  .timeCol <- .timeCol[1]
  
  .dvCol <- intersect(c("dv", "DV", "Y", "y", "LIDV", "lidv"), names(.data2))
  if (length(.dvCol) == 0) {
    stop("Could not identify categorical endpoint column in monolixData", call. = FALSE)
  }
  .usedCol <- .dvCol[1]
  
  .obs <- rep(TRUE, nrow(.data2))
  
  if ("EVID" %in% names(.data2)) {
    .obs <- .obs & (is.na(.data2$EVID) | .data2$EVID == 0)
  } else if ("evid" %in% names(.data2)) {
    .obs <- .obs & (is.na(.data2$evid) | .data2$evid == 0)
  }
  
  if ("MDV" %in% names(.data2)) {
    .obs <- .obs & (is.na(.data2$MDV) | .data2$MDV == 0)
  } else if ("mdv" %in% names(.data2)) {
    .obs <- .obs & (is.na(.data2$mdv) | .data2$mdv == 0)
  }
  
  .obs <- .obs & !is.na(.data2[[.usedCol]])
  
  .data2 <- .data2[.obs, , drop = FALSE]
  
  .origId <- .data2[[.idCol]]
  
  .data2$id <- as.integer(as.factor(.origId))
  .data2$time <- .data2[[.timeCol]]
  .data2$DV <- .data2[[.usedCol]]
  
  .data2$DV <- suppressWarnings(as.numeric(.data2$DV))
  
  if (any(is.na(.data2$DV))) {
    stop(
      "Categorical endpoint column contains values that could not be converted to numeric: ",
      .usedCol,
      call. = FALSE
    )
  }
  
  .map <- .extractStringComparisons(ui$lstExpr)
  
  if (nrow(.map) > 0) {
    for (j in seq_len(nrow(.map))) {
      .var <- .map$variable[j]
      .ind <- .map$indicator[j]
      .lab <- .map$normalized[j]
      
      if (!(.var %in% names(.data2))) {
        stop("Required string-valued model variable missing from monolixData: ", .var, call. = FALSE)
      }
      
      .data2[[.ind]] <- as.numeric(.normCategoryLabel(.data2[[.var]]) == .lab)
      .data2[[.ind]][is.na(.data2[[.ind]])] <- 0
    }
  }
  
  if (is.null(predModel)) {
    predModel <- .categoricalPredModel(ui)
  }
  
  .neededFromData <- predModel$params[predModel$params %in% names(.data2)]
  .keep <- unique(c("id", "time", "DV", .neededFromData))
  .solveData <- .data2[, .keep, drop = FALSE]
  
  message("  \u2139 using ID column: ", .idCol)
  message("  \u2139 using time column: ", .timeCol)
  message("  \u2139 using observed endpoint column: ", .usedCol)
  message("  \u2139 retained observation rows: ", nrow(.solveData), " of ", nrow(.data))
  
  list(
    data = .solveData,
    origId = .origId,
    usedCol = .usedCol,
    stringComparisonMap = .map
  )
}


# ══════════════════════════════════════════════════════════════
# SECTION 8: as.nlmixr2 SUPPORT
# ══════════════════════════════════════════════════════════════

.hasCategoricalEndpoint <- function(ui) {
  if (inherits(ui, "rxUi")) ui <- rxode2::rxUiDecompress(ui)
  .mlxtran <- tryCatch({
    if (exists("mlxtran", envir = ui)) get("mlxtran", envir = ui) else NULL
  }, error = function(e) tryCatch(ui$mlxtran, error = function(e2) NULL))
  if (is.null(.mlxtran)) return(FALSE)
  .endpoints <- .mlxtran$MODEL$LONGITUDINAL$DEFINITION$endpoint
  any(vapply(seq_along(.endpoints), function(i) {
    .endpoints[[i]]$dist == "categorical"
  }, logical(1)))
}

.categoricalBuildParams <- function(ui, pop = FALSE) {
  .etaData <- ui$etaData
  .iniDf <- ui$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .thetaVals <- setNames(.theta$est, .theta$name)
  .params <- .etaData
  .params$id <- as.integer(as.factor(.params$id))
  for (.n in names(.thetaVals)) .params[[.n]] <- .thetaVals[.n]
  if (pop) {
    .eta <- .iniDf[is.na(.iniDf$ntheta), ]
    .eta <- .eta[.eta$neta1 == .eta$neta2, ]
    for (.n in .eta$name) .params[[.n]] <- 0
  }
  .params
}

.categoricalEtaObf <- function(ui) {
  .etaData <- ui$etaData
  .iniDf <- ui$iniDf
  .eta <- .iniDf[is.na(.iniDf$ntheta), ]
  .eta <- .eta[.eta$neta1 == .eta$neta2, ]
  .etaObf <- data.frame(ID = as.integer(as.factor(.etaData$id)))
  for (.n in .eta$name) {
    .etaObf[[.n]] <- if (.n %in% names(.etaData)) .etaData[[.n]] else 0
  }
  .indLL <- tryCatch(ui$monolixIndividualLL, error = function(e) NULL)
  if (!is.null(.indLL) && nrow(.indLL) > 0) {
    .indLL$ID <- as.integer(as.factor(.indLL$id))
    .indLL$OBJI <- .indLL[[2]]
    .etaObf <- merge(.etaObf, .indLL[, c("ID", "OBJI")], by = "ID")
  } else {
    .etaObf$OBJI <- NA_real_
  }
  .etaObf
}

.categoricalFullTheta <- function(ui) {
  .iniDf <- ui$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  .popParams <- tryCatch(ui$monolixPopulationParameters, error = function(e) NULL)
  .fullTheta <- setNames(.theta$est, .theta$name)
  if (!is.null(.popParams)) {
    for (i in seq_along(.theta$name)) {
      .n <- .theta$name[i]
      .w <- which(.popParams$parameter == .n)
      if (length(.w) == 1) .fullTheta[.n] <- .popParams$value[.w]
      .w <- which(.popParams$parameter == paste0(.n, "_pop"))
      if (length(.w) == 1) .fullTheta[.n] <- .popParams$value[.w]
    }
  }
  .fullTheta
}

.categoricalOmega <- function(ui) {
  .iniDf <- ui$iniDf
  .eta <- .iniDf[is.na(.iniDf$ntheta), ]
  .eta <- .eta[.eta$neta1 == .eta$neta2, ]
  .popParams <- tryCatch(ui$monolixPopulationParameters, error = function(e) NULL)
  .n <- length(.eta$name)
  .omega <- matrix(0, .n, .n, dimnames = list(.eta$name, .eta$name))
  if (!is.null(.popParams)) {
    for (i in seq_along(.eta$name)) {
      .w <- which(.popParams$parameter == .eta$name[i])
      if (length(.w) == 1) .omega[i, i] <- .popParams$value[.w]^2
      else .omega[i, i] <- .eta$est[i]
    }
  } else {
    diag(.omega) <- .eta$est
  }
  .omega
}

.categoricalTheta <- function(ui) {
  .fullTheta <- .categoricalFullTheta(ui)
  .iniDf <- ui$iniDf
  .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
  data.frame(
    lower = .theta$lower,
    theta = unname(.fullTheta),
    fixed = .theta$fix,
    upper = .theta$upper,
    row.names = .theta$name
  )
}

as.nlmixr2.categorical <- function(x, ..., ci = 0.95) {
  .xOriginal <- x
  x <- rxode2::rxUiDecompress(x)
  x <- .restoreMonolixDataAttrs(x, .xOriginal)
  if (!.hasCategoricalEndpoint(x)) {
    return(babelmixr2:::as.nlmixr2.monolix2rx(x, ...))
  }
  message("\u2139 Categorical endpoint detected - using categorical import path")
  message("\u2139 Building solvable prediction model...")
  .predModel <- .categoricalPredModel(x)
  message("  \u2713 Prediction model compiled")
  message("\u2139 Preparing data...")
  .prep <- .categoricalPrepData(x, predModel = .predModel)
  .dataNum <- .prep$data
  message("  \u2713 Data prepared: ", nrow(.dataNum), " rows")
  message("  \u2713 Observed endpoint column used: ", .prep$usedCol)
  .indParams <- .categoricalBuildParams(x, pop = FALSE)
  .popParams <- .categoricalBuildParams(x, pop = TRUE)
  message("  \u2713 Parameters built: ", nrow(.indParams), " subjects")
  message("\u2139 Computing ipred...")
  .ipredSolve <- rxode2::rxSolve(
    .predModel, .indParams, .dataNum,
    returnType = "data.frame",
    covsInterpolation = "locf",
    omega = NULL,
    addDosing = FALSE
  )
  message("  \u2713 ipred: ", nrow(.ipredSolve), " rows")
  message("\u2139 Computing pred...")
  .predSolve <- rxode2::rxSolve(
    .predModel, .popParams, .dataNum,
    returnType = "data.frame",
    covsInterpolation = "locf",
    omega = NULL,
    addDosing = FALSE
  )
  message("  \u2713 pred: ", nrow(.predSolve), " rows")
  
  .result <- data.frame(
    ID = .ipredSolve$id,
    TIME = .ipredSolve$time,
    DV = .dataNum$DV,
    IPRED = .ipredSolve$rx_pred_,
    PRED = .predSolve$rx_pred_
  )
  
  .result$IRES <- .result$DV - .result$IPRED
  .result$IWRES <- .result$IRES / sqrt(pmax(.result$IPRED * (1 - .result$IPRED), 1e-12))
  attr(x, "predIpredData") <- .result
  
  message("\n\u2139 Model import summary:")
  message("  Observations: ", nrow(.result))
  message("  Subjects: ", length(unique(.result$ID)))
  message("  OFV: ", tryCatch(x$monolixObjf, error = function(e) "N A"))
  message("  Mean P(Y=1|DV=0): ",
          round(mean(.result$IPRED[.result$DV == 0], na.rm = TRUE), 4))
  message("  Mean P(Y=1|DV=1): ",
          round(mean(.result$IPRED[.result$DV == 1], na.rm = TRUE), 4))
  message("\n\u2713 Categorical model imported successfully")
  
  list(
    ui = x,
    predIpredData = .result,
    etaObf = .categoricalEtaObf(x),
    omega = .categoricalOmega(x),
    theta = .categoricalTheta(x),
    fullTheta = .categoricalFullTheta(x),
    objf = tryCatch(x$monolixObjf, error = function(e) NA_real_),
    method = "monolix2rx (categorical)",
    observedColumn = .prep$usedCol
  )
}


# ══════════════════════════════════════════════════════════════
# SECTION 9: CATEGORICAL VPC PLOT
# ══════════════════════════════════════════════════════════════

plot_categorical_vpc <- function(result,
                                 nBins = 10,
                                 nSim = 500,
                                 ci = 0.90,
                                 seed = 12345,
                                 xlab = "Time",
                                 ylab = "P(Y = 1)",
                                 title = NULL,
                                 xScale = 1,
                                 xOffset = 0,
                                 nXTicks = 8,
                                 xBreaks = NULL,
                                 xTickBy = NULL,
                                 xTickStart = 0,
                                 xLimits = NULL,
                                 xLabelDigits = 0,
                                 rotateXLabels = FALSE,
                                 showEmpirical = TRUE,
                                 showPI = TRUE,
                                 showMedian = TRUE,
                                 strata = NULL,
                                 timeCol = NULL,
                                 simCol = "sim_id",
                                 yCol = NULL,
                                 probCol = NULL,
                                 quiet = TRUE) {
  
  
  .toPlotTime <- function(x) {
    (as.numeric(x) - xOffset) / xScale
  }
  
  .findCol <- function(dat, x = NULL, choices = NULL) {
    if (!is.null(x)) {
      hit <- names(dat)[tolower(names(dat)) == tolower(x)]
      if (length(hit) > 0) return(hit[1])
      return(NA_character_)
    }
    
    hit <- names(dat)[tolower(names(dat)) %in% tolower(choices)]
    if (length(hit) > 0) return(hit[1])
    
    NA_character_
  }
  
  .makeStratum <- function(dat, strata) {
    if (is.null(strata)) {
      return(rep("Overall", nrow(dat)))
    }
    
    strataCols <- vapply(
      strata,
      function(z) .findCol(dat, z),
      character(1)
    )
    
    if (any(is.na(strataCols))) {
      stop(
        "Missing strata columns: ",
        paste(strata[is.na(strataCols)], collapse = ", "),
        "\nAvailable columns are: ",
        paste(names(dat), collapse = ", ")
      )
    }
    
    do.call(
      paste,
      c(dat[strataCols], sep = " | ")
    )
  }
  
  .piLabel <- paste0(ci * 100, "% prediction interval")
  .alpha <- (1 - ci) / 2
  
  # ===========================================================================
  # MODE 1: post rxSolve or post simulation data.frame
  # ===========================================================================
  
  if (is.data.frame(result)) {
    
    dat <- as.data.frame(result)
    
    timeCol <- .findCol(
      dat,
      timeCol,
      c("profday", "PROFDAY", "timeMid", "time", "TIME")
    )
    
    if (is.na(timeCol)) {
      stop("No time column found. Provide timeCol.")
    }
    
    simCol0 <- .findCol(dat, simCol)
    
    if (is.na(simCol0)) {
      simCol0 <- .findCol(
        dat,
        NULL,
        c("sim_id", "sim.id", "study", "stud", "nStud", "replicate")
      )
    }
    
    if (is.na(simCol0)) {
      dat$.sim_id <- 1
      simCol0 <- ".sim_id"
      if (!quiet) {
        message("No simulation replicate column found; using one replicate only.")
      }
    }
    
    if (!is.null(yCol)) {
      yCol <- .findCol(dat, yCol)
      probCol <- NA_character_
    } else if (!is.null(probCol)) {
      probCol <- .findCol(dat, probCol)
      yCol <- NA_character_
    } else {
      yCol <- .findCol(
        dat,
        NULL,
        c("y_sim", "simDV", "sim", "DV")
      )
      
      probCol <- .findCol(
        dat,
        NULL,
        c("prob", "p1", "P1", "p", "P", "PRED", "IPRED")
      )
    }
    
    if (is.na(yCol) && is.na(probCol)) {
      stop("No probability or simulated outcome column found. Provide probCol or yCol.")
    }
    
    dat$timeMid <- .toPlotTime(dat[[timeCol]])
    dat$stratum <- .makeStratum(dat, strata)
    
    if (!is.na(yCol)) {
      vpcSim <- dat |>
        group_by(
          sim_id = .data[[simCol0]],
          stratum,
          timeMid
        ) |>
        summarise(
          prob = mean(.data[[yCol]], na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      vpcSim <- dat |>
        group_by(
          sim_id = .data[[simCol0]],
          stratum,
          timeMid
        ) |>
        summarise(
          prob = mean(.data[[probCol]], na.rm = TRUE),
          .groups = "drop"
        )
    }
    
    plotDat <- vpcSim |>
      group_by(stratum, timeMid) |>
      summarise(
        piLow = quantile(prob, .alpha, na.rm = TRUE),
        piMed = quantile(prob, 0.5, na.rm = TRUE),
        piHigh = quantile(prob, 1 - .alpha, na.rm = TRUE),
        .groups = "drop"
      )
    
    empDat <- NULL
  }
  
  # ===========================================================================
  # MODE 2: original categorical result object from as.nlmixr2.categorical()
  # ===========================================================================
  
  if (!is.data.frame(result)) {
    
    .ui <- result$ui
    .omega <- result$omega
    .etaNames <- colnames(.omega)
    
    .predModel <- .categoricalPredModel(.ui)
    
    .prep <- if (quiet) {
      suppressMessages(.categoricalPrepData(.ui))
    } else {
      .categoricalPrepData(.ui)
    }
    
    .dataNum <- .prep$data
    .dataNum$timePlot <- .toPlotTime(.dataNum$time)
    .dataNum$stratum <- .makeStratum(.dataNum, strata)
    
    .iniDf <- .ui$iniDf
    .theta <- .iniDf[!is.na(.iniDf$ntheta), ]
    .thetaVals <- setNames(.theta$est, .theta$name)
    
    .nSubj <- length(unique(.dataNum$id))
    .ids <- sort(unique(.dataNum$id))
    
    .tRange <- range(.dataNum$timePlot, na.rm = TRUE)
    
    if (!is.null(xLimits)) {
      .binRange <- xLimits
    } else {
      .binRange <- .tRange
    }
    
    .breaks <- seq(
      .binRange[1],
      .binRange[2],
      length.out = nBins + 1
    )
    
    .dataNum$timeBin <- cut(
      .dataNum$timePlot,
      breaks = .breaks,
      include.lowest = TRUE
    )
    
    .dataNum$timeMid <- ave(
      .dataNum$timePlot,
      interaction(.dataNum$stratum, .dataNum$timeBin),
      FUN = median
    )
    
    if (showEmpirical) {
      empDat <- .dataNum |>
        filter(!is.na(timeBin)) |>
        group_by(stratum, timeMid, timeBin) |>
        summarise(
          empirical = mean(DV, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      empDat <- NULL
    }
    
    set.seed(seed)
    
    simList <- lapply(seq_len(nSim), function(s) {
      
      .newEtas <- MASS::mvrnorm(
        n = .nSubj,
        mu = rep(0, length(.etaNames)),
        Sigma = .omega
      )
      
      if (is.null(dim(.newEtas))) {
        .newEtas <- matrix(.newEtas, ncol = length(.etaNames))
      }
      
      colnames(.newEtas) <- .etaNames
      
      .simParams <- data.frame(id = .ids)
      
      for (.n in names(.thetaVals)) {
        .simParams[[.n]] <- .thetaVals[.n]
      }
      
      for (j in seq_along(.etaNames)) {
        .simParams[[.etaNames[j]]] <- .newEtas[, j]
      }
      
      .simSolve <- tryCatch({
        if (quiet) {
          suppressWarnings(
            suppressMessages(
              rxode2::rxSolve(
                .predModel,
                .simParams,
                .dataNum,
                returnType = "data.frame",
                covsInterpolation = "locf",
                omega = NULL,
                addDosing = FALSE
              )
            )
          )
        } else {
          rxode2::rxSolve(
            .predModel,
            .simParams,
            .dataNum,
            returnType = "data.frame",
            covsInterpolation = "locf",
            omega = NULL,
            addDosing = FALSE
          )
        }
      }, error = function(e) NULL)
      
      if (is.null(.simSolve)) {
        return(NULL)
      }
      
      .simProb <- pmin(
        pmax(.simSolve$rx_pred_, 1e-10),
        1 - 1e-10
      )
      
      data.frame(
        sim_id = s,
        stratum = .dataNum$stratum,
        timeMid = .dataNum$timeMid,
        timeBin = .dataNum$timeBin,
        simDV = rbinom(
          n = nrow(.simSolve),
          size = 1,
          prob = .simProb
        )
      ) |>
        filter(!is.na(timeBin)) |>
        group_by(sim_id, stratum, timeMid, timeBin) |>
        summarise(
          prob = mean(simDV),
          .groups = "drop"
        )
    })
    
    vpcSim <- bind_rows(simList)
    
    plotDat <- vpcSim |>
      group_by(stratum, timeMid) |>
      summarise(
        piLow = quantile(prob, .alpha, na.rm = TRUE),
        piMed = quantile(prob, 0.5, na.rm = TRUE),
        piHigh = quantile(prob, 1 - .alpha, na.rm = TRUE),
        .groups = "drop"
      )
  }
  
  # ===========================================================================
  # Axis breaks
  # ===========================================================================
  
  if (!is.null(xBreaks)) {
    .xBreaks <- xBreaks
  } else if (!is.null(xTickBy)) {
    .xRange <- if (is.null(xLimits)) {
      range(plotDat$timeMid, na.rm = TRUE)
    } else {
      xLimits
    }
    
    .xBreaks <- seq(
      from = xTickStart,
      to = ceiling(max(.xRange, na.rm = TRUE) / xTickBy) * xTickBy,
      by = xTickBy
    )
  } else {
    .xBreaks <- pretty(
      range(plotDat$timeMid, na.rm = TRUE),
      n = nXTicks
    )
  }
  
  .xText <- if (rotateXLabels) {
    element_text(angle = 45, hjust = 1, vjust = 1)
  } else {
    element_text()
  }
  
  # ===========================================================================
  # Plot
  # ===========================================================================
  
  plotDat$piElement <- .piLabel
  plotDat$medianElement <- "Predicted median"
  
  p <- ggplot()
  
  if (showPI) {
    p <- p +
      geom_ribbon(
        data = plotDat,
        aes(
          x = timeMid,
          ymin = piLow,
          ymax = piHigh,
          fill = piElement
        ),
        alpha = 0.35
      )
  }
  
  if (showMedian) {
    p <- p +
      geom_line(
        data = plotDat,
        aes(
          x = timeMid,
          y = piMed,
          color = medianElement,
          linetype = medianElement
        ),
        linewidth = 0.9
      )
  }
  
  if (showEmpirical && !is.null(empDat)) {
    empDat$empElement <- "Empirical probability"
    
    p <- p +
      geom_line(
        data = empDat,
        aes(
          x = timeMid,
          y = empirical,
          color = empElement,
          linetype = empElement
        ),
        linewidth = 1
      ) +
      geom_point(
        data = empDat,
        aes(
          x = timeMid,
          y = empirical,
          color = empElement
        ),
        size = 1.8
      )
  }
  
  p <- p +
    scale_fill_manual(
      name = NULL,
      values = setNames("#6BAED6", .piLabel)
    ) +
    scale_color_manual(
      name = NULL,
      values = c(
        "Empirical probability" = "#08519C",
        "Predicted median" = "black"
      )
    ) +
    scale_linetype_manual(
      name = NULL,
      values = c(
        "Empirical probability" = "solid",
        "Predicted median" = "dashed"
      )
    ) +
    scale_x_continuous(
      limits = xLimits,
      breaks = .xBreaks,
      labels = function(x) {
        format(
          round(x, xLabelDigits),
          trim = TRUE,
          scientific = FALSE
        )
      },
      expand = expansion(mult = c(0.01, 0.03))
    ) +
    scale_y_continuous(
      limits = c(0, NA),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      x = xlab,
      y = ylab,
      title = title
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(fill = "white"),
      legend.key.width = unit(1.5, "cm"),
      legend.spacing.x = unit(0.5, "cm"),
      legend.text = element_text(size = 11),
      axis.text.x = .xText,
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  if (length(unique(plotDat$stratum)) > 1) {
    p <- p + facet_wrap(~ stratum)
  }
  
  p
}


.install_handleSingleEndpoint_patch()
.install_def2ini_patch()
