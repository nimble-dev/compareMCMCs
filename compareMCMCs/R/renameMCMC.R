# Tool for renaming results, which is particularly useful 
# before combining results.

## If only name1 is provided, that is the new name
## If name1 and name2 are provided, name1 is old, name2 is new.

#' @export
renameMCMC <- function(MCMCresult, newName, oldName) {
  if(inherits(MCMCresult, "MCMCresult")) {
    MCMCresult$rename(newName, oldName)
    return(invisible(NULL))
  }
  if(!is.list(MCMCresult))
    stop("MCMCresult must be an MCMCresult object or list of MCMCresult objects, such as returned by compareMCMCs.")
  if(missing(oldName))
    stop("MCMCresult is a list, so oldName argument must be provided")
  MCMCresult <- lapply(MCMCresult,
                       function(oneRes) {
                         if(inherits(oneRes, "MCMCresult")) {
                           oneRes$rename(newName, oldName)
                           oneRes
                         } else {
                           warning("Skipping an element of MCMCresult that is not an MCMCresult object.")
                         }
                       })
  names(MCMCresult)[names(MCMCresult) == oldName] <- newName
  MCMCresult
}