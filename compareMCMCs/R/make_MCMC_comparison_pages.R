compareMCMCs_registered_pageComponents <- new.env()

#' Register, unregister and access page components used by
#' \code{make_MCMC_comparison_pages}
#'
#' @name pageComponents
#' @aliases registerPageComponents unregisterPageComponents getPageComponents
#'
#' @param pageComponents A named list of new page components to register
#' @param name Character name of a page component to unregister
#'
#' @details A page component is an element that can be included in an
#'   MCMC comparison page by naming it in the \code{pageComponents}
#'   argument to \code{make_MCMC_comparison_pages}. See package
#'   vignette for explanation page components.
#'
#' @importFrom stats sd
#' @seealso \link{make_MCMC_comparison_pages}
NULL

#' @rdname pageComponents
#' @export
registerPageComponents <- function(pageComponents) {
  list2env(pageComponents, compareMCMCs_registered_pageComponents)
}

#' @rdname pageComponents
#' @export
unregisterPageComponents <- function(name) {
  rm(list = name, envir = compareMCMCs_registered_pageComponents)
}

#' @rdname pageComponents
#' @export
getPageComponents <- function() {
  compareMCMCs_registered_pageComponents
}

#' Create html output with comparisons of MCMC results
#' 
#' @param comparisonResults An list of `MCMCresult` objects 
#' such as returned by \code{\link{compareMCMCs}}.
#' 
#' @param dir A directory in which to place the html file and any
#'   figure files used in it.  This defaults to tempdir() (which
#'   will be erased when the R session is closed).
#'   Use \code{dir = getwd()} to use current working directory.
#' 
#' @param pageComponents A list whose names are registered page
#'   components and values are `TRUE` (to include a component) or
#'   `FALSE` (to omit a component).  Components can also be omitted by
#'   leaving them out of the list.
#' 
#' @param modelName A name to be used for the model in generated
#'   output.
#' 
#' @param control A named list of control parameters.
#' 
#' @param plot `TRUE` to generate results, `FALSE` not to do so.  Use
#'   of `FALSE` is useful if one wants to use the returned object
#'   (including plottable components) in one's own way.
#' 
#' @details
#' 
#' See package vignette for information about page components,
#' including about default page components and how to write and
#' register new page components.
#' 
#' To see built-in page components and their options, use
#' `as.list(getPageComponents())`.
#' 
#' @return 
#' 
#' A list of objects returned from each page component plugin.  For figures,
#' these contain a `plottable` object such as a `ggplot` object.  For text,
#' these contain information for text output such as an `xtable` object.
#' 
#' @export
make_MCMC_comparison_pages <- function(comparisonResults,
                                       dir = tempdir(),
                                       pageComponents,
                                       modelName = "model",
                                       control,
                                       plot = TRUE) {
  ## pageComponents can have standard names with TRUE or FALSE or it
  ## can a list with elements control options include makeTopPage and
  ## mainPageName

  ## Establish directory and work there.
  curDir <- getwd()
  outputDir <- dir
  if(!file.exists(outputDir))
    dir.create(outputDir, recursive = TRUE)
  setwd(outputDir)
  on.exit(setwd(curDir))

  ## Set control list by combining defaults with any user input.
  controlDefaults <- list(##makeTopPage = "if_needed",
                          mainPageName = 'main')
  if(missing(control))
    control <- controlDefaults
  else
    for(i in names(controlDefaults)) {
      if(is.null(control[[i]]))
        control[[i]] <- controlDefaults[[i]]
    }

  ## Set up default page components
  if(missing(pageComponents)) {
    pageComponents <- list(timing = TRUE,
                           efficiencySummary = FALSE,
                           efficiencySummaryAllParams = TRUE,
                           paceSummaryAllParams = TRUE,
                           efficiencyDetails = TRUE,
                           posteriorSummary = TRUE)
  }

  ## Check validity of pageComponents (relevant if it was provided by user).
  if(!is.list(pageComponents))
    stop('pageComponents must be a list')
  if(is.null(names(pageComponents)))
    stop('pageComponents elements must be named list')
  if(any(names(pageComponents)==""))
    stop('pageComponents elements must be named list')

  ## populate page components from page components library
  for(j in names(pageComponents)) {
    if(is.logical(pageComponents[[j]])) {
      if(pageComponents[[j]]) {
        if(j %in% names(compareMCMCs_registered_pageComponents)) {
          pageComponents[[j]] <- compareMCMCs_registered_pageComponents[[j]]
        } else {
          stop(paste0('no registered pageComponent definition for ', j))
        }
      } else {
        pageComponents[[j]] <- NULL
      }
    }
  }

  combinedComparisonResults <- combineMetrics(comparisonResults, 
                                              include_times = TRUE)

    madePageComponents <- list()
    for(j in names(pageComponents)) {
      madePageComponents[[j]] <- eval(call(pageComponents[[j]][['make']],
                                           combinedComparisonResults,
                                           pageComponents[[j]][['control']]))
    }

    for(j in names(pageComponents)) {
      if(!is.null(madePageComponents[[j]][['plottable']])) {
        filename <- paste0(modelName,
                           pageComponents[[j]]$fileSuffix,
                           '.jpg')
        grDevices::jpeg(filename = filename,
                        height = madePageComponents[[j]]$height,
                        width = madePageComponents[[j]]$width,
                        units = 'in',
                        res = 300)
        eval(call(
          if(is.null(pageComponents[[j]][['plot']]))
            'plot'
          else
            pageComponents[[j]][['plot']],
          madePageComponents[[j]]$plottable
        ))
        grDevices::dev.off()
      }
    }

    if(plot)
      make_example_html(modelName,
                        pageComponents,
                        madePageComponents)

  invisible(madePageComponents)
}

make_example_html <- function(modelName,
                              pageComponents,
                              madePageComponents,
                              control) {
  numComponents <- length(pageComponents)
  if(numComponents == 0) return()
  tags <- paste0('p', 1:numComponents)
  linkTexts <- unlist(lapply(pageComponents, `[[`, 'linkText'))
  ## NULLs are dropped by unlist
  headerLinkEntries <- paste(paste0("<a href='#",
                                    tags,
                                    "'>",
                                    linkTexts,
                                    "</a><br>"),sep="\n")

  pageComponentEntries <- character()
  for(i in 1:numComponents) {
    if(is.null(pageComponents[[i]])) next ## NULLs are dropped
    fileSuffix <- pageComponents[[i]]$fileSuffix
    jpgName <- if(is.null(fileSuffix))
      NULL
    else
      paste0(modelName, fileSuffix, '.jpg')
    if(is.null(jpgName)) {
      ## text or table
      if(inherits(madePageComponents[[i]][['printable']], 'xtable'))
        element <- paste0("<h2 id='",
                          tags[i],
                          "'>",
                          linkTexts[[i]],
                          "</h2>\n",
                          print(madePageComponents[[i]][['printable']],
                                type = 'html',
                                print.results = FALSE,
                                include.rownames = FALSE)
                          )
      else
        element <- paste0("<h2 id='",
                          tags[i],
                          "'>",
                          linkTexts[[i]],
                          "</h2>\n",
                          madePageComponents[[i]][['printable']])
    } else {
      element <- paste0("<br><h2 id='",
                        pageComponents[[i]][['fileSuffix']],
                        "'>",
                        linkTexts[[i]],
                        "</h2>
                        <img id='",
                        tags[i],
                        "' src=\"",
                        jpgName,
                        "\"",
                        madePageComponents[[i]][['html_img_args']],
                        "</img>")
    }
    pageComponentEntries <- append(pageComponentEntries, element)
  }

  html <- paste0("<!DOCTYPE html PUBLIC>
              <html>
              <head>
              <link rel='stylesheet' type='text/css' href='style.css'/>
              </head>
              <body>

              <h1>", modelName,"</h1>\n",
              paste(headerLinkEntries, collapse="\n"),
              paste(pageComponentEntries, collapse="\n"),
              "
              </body>
              </html>")
  cat(html,file=paste(modelName,".html",sep=""))
}

timeComparisonComponent <- function(comparisonResults,
                                    control) {
  if(!requireNamespace("xtable"))
    stop("package xtable is required to include time in comparison pages.")
  times <- comparisonResults$times
  times <- cbind(data.frame(MCMC = row.names(times)), times)
  row.names(times) <- NULL
  list(printable = xtable::xtable(times))
}

minMeanComparisonComponent <- function(comparisonResults,
                                       control) {
  if(!requireNamespace('ggplot2', quietly = TRUE))
    stop('Package ggplot2 is required but is not installed.')
  if(!requireNamespace('reshape2', quietly = TRUE))
    stop('Package ggplot2 is required but is not installed.')

  if(missing(control)) control <- list()
  ## If invert is TRUE, do Pace = seconds/ESS, else do Efficiency = ESS/second
  defaults <- list(invert = FALSE,                   
                   min_efficiency_name = "min_efficiency",
                   mean_efficiency_name = "mean_efficiency",
                   suffix = "")
  control <- updateDefaults(defaults, control)
  invert <- control$invert
  min_efficiency_name <- paste0(control$min_efficiency_name, control$suffix) 
  mean_efficiency_name <- paste0(control$mean_efficiency_name, control$suffix) 
  
  byMCMC <- comparisonResults$byMCMC
  columnsToUse <- c('MCMC',
                    min_efficiency_name,
                    mean_efficiency_name)
  if(!all(columnsToUse %in% colnames(byMCMC))) {
    missingCols <- columnsToUse[!(columnsToUse %in% colnames(byMCMC))]
    warnings(paste0("MCMC column names ", paste(missingCols, collapse = ','),
                    " expected but not found for minMeanComparisonComponent."))
    return(NULL)
  }
  efficiencyResults <- byMCMC[, columnsToUse]
  colnames(efficiencyResults) <- c('MCMC','minimum','mean')
  Efficiency <- reshape2::melt(efficiencyResults,
                               id = 'MCMC',
                               variable.name = 'type',
                               value.name = 'Efficiency')

##  Efficiency <- comparisonResults$Efficiency
  if(invert) Efficiency$Efficiency <- 1/Efficiency$Efficiency
  if(invert)
    levels(Efficiency$type)[ levels(Efficiency$type) == 'minimum' ] <- 'maximum'
  ylabel <- if(!invert)
    'Effective sample size\n per second'
  else
    'Seconds per\n effective sample'
  title <- if(!invert)
             paste0("MCMC efficiency summary\n",
                    "(Minimum and mean effective sample size per second)")
  else
    "MCMC pace summary\n (Maximum and mean seconds per effective sample)"
  if(length(unique(Efficiency$MCMC)) * length(unique(Efficiency$type)) ==
       length(Efficiency$MCMC)) {
    p <- ggplot2::ggplot(Efficiency,
                         ggplot2::aes_string(x = "MCMC",
                                             y= "Efficiency",
                                             fill = "MCMC")) +
      ggplot2::geom_bar(position=ggplot2::position_dodge(),stat='identity') +
      ggplot2::ggtitle(title)+
      ggplot2::facet_wrap(~ type,ncol=2,scales='free') +
      ggplot2::ylab(ylabel) +
      ggplot2::theme(legend.position = "top")
  } else {
    ## there are multiple runs
    title <- paste0(title, "\n \"-\" shows mean.")
    p <- ggplot2::ggplot(Efficiency,
                         ggplot2::aes_string(x = "MCMC",
                                             y = "Efficiency",
                                             fill = "MCMC",
                                             color = "MCMC")) +
      ggplot2::geom_point(stat='identity')+
      ggplot2::stat_summary(fun = 'mean',
                            fun.min = function(x) mean(x) - sd(x),
                            fun.max = function(x) mean(x) + sd(x),
                            shape = '-',
                            size = 1) +
      ggplot2::ggtitle(title)+
      ggplot2::facet_wrap(~ type,ncol=2,scales='free') +
      ggplot2::ylab(ylabel) +
      ggplot2::theme(legend.position = "top")
  }
  list(plottable = p,
       height = 6,
       width = 14,
       html_img_args = "height = \"600\" width = \"1000\"")
}

minMeanAllComparisonComponent <- function(comparisonResults,
                                          control) {
  part1 <- minMeanComparisonComponent(comparisonResults,
                                      control)
  part2 <- allParamEfficiencyComparisonComponent(comparisonResults,
                                                 control)
  list(plottable = list(minMean = part1$plottable,
                        allParams = part2$plottable),
       height = 6,
       width = 15,
       html_img_args = "height = \"600\" width = \"1500\"")
}

allParamEfficiencyComparisonComponent <- function(comparisonResults,
                                                  control) {
  if(!requireNamespace('ggplot2', quietly = TRUE))
      stop('ggplot2 is required but not installed.')

  vars <- comparisonResults$byParameter
  if(missing(control)) control <- list()
  defaults <- list(invert = FALSE,
                   efficiency_name = "efficiency",
                   suffix = "")
  control <- updateDefaults(defaults, control)
  invert <- control$invert
  efficiency_name <- paste0(control$efficiency_name, control$suffix)

  if(invert) vars[[efficiency_name]] <- 1/vars[[efficiency_name]]

  ylabel <- if(!invert) 'Effective sample size\n per second'
  else 'Seconds per\n effective sample'

  title <- if(!invert) "MCMC efficiency for\n each parameter"
  else "MCMC pace for\n each parameter"

  replicatedRuns <-
    !(length(unique(vars$MCMC)) * length(unique(vars$Parameter)) ==
        length(vars[[efficiency_name]]))

  if(replicatedRuns) {
      vars <- stats::aggregate(vars[[efficiency_name]],
                               list(vars$MCMC, vars$Parameter),
                               mean)
    colnames(vars) <- c('MCMC', 'Parameter', efficiency_name)
    title <- paste("Mean", title)
  }

  p <- ggplot2::ggplot(vars,
                       ggplot2::aes_string(x = "MCMC",
                                           y = efficiency_name,
                                           color = "Parameter",
                                           group = "Parameter")) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::ylab(ylabel) +
      ggplot2::guides(colour = ggplot2::guide_legend(title = "Parameter")) +
      ggplot2::ggtitle(title)

  list(plottable = p,
       height = 6,
       width = 5,
       html_img_args = "height = \"600\" width = \"500\"")
}

efficiencyDetailsComparisonComponent <- function(comparisonResults,
                                                 control = list()) {
  if(!requireNamespace('ggplot2', quietly = TRUE))
    stop('ggplot2 is required but not installed')
  df <- comparisonResults$byParameter
  defaults <- list(ncol = 4,
                   efficiency_name = 'efficiency',
                   suffix = '')
  control <- updateDefaults(defaults, control)
  efficiency_name <- paste0(control$efficiency_name, control$suffix)
  
  ncol <- control$ncol
  if(length(unique(df$Parameter)) * length(unique(df$MCMC)) == nrow(df)) {
    p <- ggplot2::ggplot(df,
                         ggplot2::aes_string(x = "MCMC",
                                             y= efficiency_name,
                                             fill = "MCMC"))+ 
      ggplot2::geom_bar(position=ggplot2::position_dodge(),stat='identity')+
      ggplot2::ggtitle(
        paste0("MCMC efficiency details\n",
               "(Effective sample size per second for each parameter)")) +
      ggplot2::ylab('Effective sample size per second') +
      ggplot2::facet_wrap(~ Parameter,ncol=ncol,scales='free') +
      ggplot2::theme(legend.position = "top")
  } else {
    ## multiple points for each method
    p <- ggplot2::ggplot(df,
                         ggplot2::aes_string(x = "MCMC" ,
                                             y = efficiency_name,
                                             fill = "MCMC", color = "MCMC")) +
      ggplot2::geom_point(stat='identity') +
      ggplot2::stat_summary(fun = 'mean',
                            fun.min = function(x) mean(x) - sd(x),
                            fun.max = function(x) mean(x) + sd(x),
                            shape = '-',
                            size = 1) +
      ggplot2::ggtitle(
        paste0("MCMC efficiency details\n",
               "(Effective sample size per second for each parameter)\n",
               "\"-\" shows mean."))+
      ggplot2::ylab('Effective sample size per second') +
      ggplot2::facet_wrap(~ Parameter,ncol=ncol,scales='free') +
      ggplot2::theme(legend.position = "top")
  }
  numVars <- length(unique(df[,'Parameter']))
  height <- max(floor(numVars*4.5/3),
                3)
  list(plottable = p,
       height = height,
       width = 12,
       html_img_args = paste0("height=\"",height*100,"\" width=\"1200\""))
}

plotMinMeanAll <- function(plottable) {
  requireNamespace('grid', quietly = TRUE)
  print(plottable[['minMean']],
        vp = grid::viewport(x = .25, y = 0.5, width = 0.5, height = 1.0))
  print(plottable[['allParams']],
        vp = grid::viewport(x = .75, y = 0.5, width = 0.5, height = 1.0))
}


posteriorSummaryComparisonComponent <- function(comparisonResults,
                                                ##modelName,
                                                control = list(ncol = 4)) {
  df <- comparisonResults$byParameter
  if(!requireNamespace('ggplot2', quietly = TRUE))
    stop('Package ggplot2 is required but not installed.')
##  df <- comparisonResults$varSummaries
  ncol <- control$ncol
  p<-ggplot2::ggplot(df,
                     ggplot2::aes_string(x = "MCMC", y = "mean")) +
    ggplot2::geom_point(ggplot2::aes_string(color= "MCMC" ,size=1)) +
    ggplot2::ggtitle("Posterior mean, median, and 95% CIs") +
    ggplot2::guides(size="none",
                    colour="none") +
    ggplot2::geom_point(ggplot2::aes_string(x = "MCMC", y = "median", size=1),
                        shape=4) +
    ggplot2::facet_wrap(~ Parameter,
                        ncol=ncol,
                        scales='free') +
    ggplot2::geom_errorbar(ggplot2::aes_string(ymax = "CI95_upp",
                                               ymin = "CI95_low"),
                           width=.25) +
    ggplot2::labs(y = 'Posterior values')

  numVars <- length(unique(df[,'Parameter']))
  height <- max(floor(numVars*4.5/3),
                2)
  list(plottable = p,
       height = height,
       width = 12,
       html_img_args = paste0("height=\"",
                              height*100,
                              "\" width=\"1200\""))
}
