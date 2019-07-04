## Tools for converting among parameterizations

## conversions entries will be processed in order.
## an entry of NULL of "" will remove a column.
## an entry can be text that will be parsed or a call or a function.
##
## new_column_name can be an existing column_name

#' @export
applyConversions <- function(samples,
                             conversions) {
  if(is.list(samples)) {
    sampleNames <- names(conversions)
    for(sN in sampleNames) {
      if(!is.null(conversions[[sN]]))
        applyConversions(samples[[sN]], conversions[[sN]])
    }
    return(invisible(NULL))
  }
  if(inherits(samples, "MCMCresult")) {
    samples$samples <- applyConversions(samples$samples, conversions)
    return(invisible(NULL))
  }
  if(!length(conversions)) return(invisible(NULL))
  if(!is.list(conversions))
    stop("conversions must be a list.")
  conversionNames <- names(conversions)
  if(is.null(conversionNames))
    stop("conversions list must have names")
  if(!is.data.frame(samples))
    workSamples <- as.data.frame(samples)
  else
    workSamples <- samples

  for(i in seq_along(conversions)) {
    new_column_name <- conversionNames[i]
    if(is.null(conversions[[i]])) {
      workSamples[[new_column_name]] <- NULL ## This removes the column, if it existed
      next
    }
    if(is.character(conversions[[i]]) | is.call(conversions[[i]])) {
      if(is.character(conversions[[i]])) {
        if(conversions[[i]][1] == "") {
          workSamples[[new_column_name]] <- NULL ## Another way to remove a column
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
        warning(paste0("conversion entry for ",
                       new_column_name,
                       " is not valid.\n"))
      }
    }
    if(inherits(new_column, 'try-error')) {
      warning(paste0("There was a problem creating conversion ",
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
