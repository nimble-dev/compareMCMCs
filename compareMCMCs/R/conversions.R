## Tools for converting among parameterizations

## conversions entries will be processed in order.
## an entry of NULL of "" will remove a column.
## an entry can be text that will be parsed or a call or a function.
##
## new_column_name can be an existing column_name

#' Apply a set of parameter conversions to MCMC output
#' 
#' Create transformed parameters from original parameters in MCMC output
#' 
#' @param samples One of: an \code{\link{MCMCresult}} object; a named
#'     list of `MCMCresult` objects (such as returned by
#'     \code{\link{compareMCMCs}}); a matrix of MCMC samples (such as
#'     the `samples` element of an `MCMCresult` object); or a named
#'     list of such matrices.  In the first two cases, conversions will 
#'     be done in place (as a "side effect" modifying the arguments) 
#'     because \code{\link{MCMCresult}} objects are R6 objects and are thus
#'     passed by reference.
#'  
#' @param conversions One of: a list of conversion specifications (see
#'     below); a named list of conversion specifications, with names
#'     matching those of a list provided for `samples`.
#'  
#' @details A conversion specification is a named list. For each
#'     element:
#' 
#' - its name will be the name of a new column appended to a `samples` matrix.
#'
#' - its value should be a character string that can be parsed as code
#'  to calculate elements of the new column.  It can use existing
#'  column names in `samples`.  Calculations will be done row-wise.
#'  Column names are often something like "beta\[2\]".  To have this
#'  used as a name, enclose it in backticks, e.g. "`` `beta[2]` ``".
#'  For example, an entry could be ``log_beta2 = "log(`beta\[2\]`)"``.
#'  A list value of `NULL` will remove the named column.
#'
#'  The conversion specification list will be processed in order.
#'  This allows creating new columns and removing old ones in a
#'  sensible order.
#'  
#'  If both `conversions` and `samples` are named lists, they will be
#'  matched: the `conversions` element (itself a list of conversion
#'  specifications) used on a `samples` element will have the same
#'  name.  If there is no `conversions` element for a given `samples`
#'  element, that `samples` element will be included in the returned
#'  list without any conversions.
#'  
#' @return An object of the same type as `samples` after application
#'     of conversions.
#'  
#' @export
applyConversions <- function(samples,
                             conversions) {
  if(is.list(samples)) {
    convertedSamples <- list()
    sampleNames <- names(samples)
    for(sN in sampleNames) {
      if(!is.null(conversions[[sN]]))
        convertedSamples[[sN]] <- applyConversions(samples[[sN]], conversions[[sN]])
      else
        convertedSamples[[sN]] <- samples[[sN]]
    }
    return(convertedSamples)
  }
  if(inherits(samples, "MCMCresult")) {
    samples$samples <- applyConversions(samples$samples, conversions)
    return(samples)
  }
  if(!length(conversions)) return(invisible(NULL)) #lacks test coverage
  if(!is.list(conversions))
    stop("conversions must be a list.") #lacks test coverage
  conversionNames <- names(conversions)
  if(is.null(conversionNames))
    stop("conversions list must have names") #lacks test coverage
  if(!is.data.frame(samples))
    workSamples <- as.data.frame(samples)
  else
    workSamples <- samples #lacks test coverage

  for(i in seq_along(conversions)) {
    new_column_name <- conversionNames[i]
    if(is.null(conversions[[i]])) {
      ## This removes the column, if it existed
      workSamples[[new_column_name]] <- NULL 
      next
    }
    if(is.character(conversions[[i]]) | is.call(conversions[[i]])) {
      if(is.character(conversions[[i]])) {
        if(conversions[[i]][1] == "") {
          workSamples[[new_column_name]] <- NULL
          next  
        } else {
          code <- parse(text = conversions[[i]][1],
                        keep.source = FALSE)[[1]]
        }
      } else {
        code <- conversions[[i]]
      }
      new_column <- try(eval(code, envir = workSamples))
    } else {
      if(is.function(conversions[[i]])) {
        new_column <- try(conversions[[i]](samples))
      } else {
        warning(paste0("conversion entry for ", #lacks test coverage
                       new_column_name,
                       " is not valid.\n"))
      }
    }
    if(inherits(new_column, 'try-error')) {
      warning(paste0("There was a problem creating conversion ", #lacks test coverage
                     new_column_name, ".\n"))
    } else {
      ok <- try(workSamples[[new_column_name]] <- new_column)
      if(inherits(ok, 'try-error')) {
        warning(paste0("Conversion ", new_column_name,
                       "created something invalid.\n"))
        workSamples[[new_column_name]] <- NULL
      }
    }
  }
  as.matrix(workSamples)
}
