# compareMCMCs
[![CRAN](https://www.r-pkg.org/badges/version/compareMCMCs)](https://cran.r-project.org/package=compareMCMCs)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03844/status.svg)](https://doi.org/10.21105/joss.03844)

compareMCMCs is a package for running, managing, and comparing results from different MCMC packages.  It was refactored from the nimble package as part of the release of nimble version 0.8.0.

The User Manual (aka package vignette) giving an overview of compareMCMCs can be viewed [here](https://cran.r-project.org/web/packages/compareMCMCs/vignettes/compareMCMCs.html).

Right now, compareMCMCs works with NIMBLE, JAGS and Stan.  It has a plug-in system to make it easy to include other MCMC engines in comparisons.

compareMCMCs allows you to configure a set of MCMCs to run and then automatically time them and process their results to generate html output with comparisons of efficiency and posterior distributions. This system started life as part of the `nimble` package.  It can make use of nimble's MCMC configuration system, or it can be used independently of nimble.

Use of other MCMCs is supported by a plugin system.  Plugins are provided for JAGS and Stan.  Since nimble and JAGS (as well as WinBUGS and OpenBUGS) use different dialects of the same model language, it is sometimes possible to compare them using the same model code.

It is also possible to provide new comparison metrics and/or new figure components for html comparison pages.

In summary, compareMCMCs provides:

- the `compareMCMCs` function to run one or more MCMCs and manage the results;
- the `MCMCresult` class to manage results by storing samples, timing information, metrics or summaries of performance, and other details;
- a plugin systems to include new MCMC engines;
- a plugin system for new metrics for comparison among MCMCs;
- a system for generating html pages with figures from comparison metrics, including a plugin system to provide new page components;
- partial backward compatibility to nimble's original `MCMCsuite` and `compareMCMCs` functions.

## Installation

`compareMCMCs` is on CRAN and can be installed from R via

```r
install.packages("compareMCMCs")
```

To install compareMCMCs from Github 

```r
library(devtools)
install_github("nimble-dev/compareMCMCs", subdir = "compareMCMCs")
```
Or, of course, you can download and build the package, which is in the directory `compareMCMCs`.

### Dependencies to run MCMC plug-ins 

To use the `compareMCMCs` plugin for JAGS you first need to install JAGS. See [JAGS [homepage](http://mcmc-jags.sourceforge.net/) for platform-based instructions. You will also need the `rjags` package:

```r
install.packages("rjags")
```

To use the `compareMCMCs` plugin for stan, you first need to install the `rstan` package:

```r
install.packages("rstan")
```

## Contributing and requesting support

We welcome contributions to `compareMCMCs`.   Of particular interest are contributions for the various plug-in features, including:

- plug-ins for different MCMC engines,
- plug-ins for different comparison metrics, and
- plug-ins for different comparison page components.

We also welcome support requests, bug fixes and documentation suggestions.

To request support or report a bug, please be signed in to a GitHub account and submit an issue [here](https://github.com/nimble-dev/compareMCMCs/issues).

To contribute code, please make a GitHub pull request.  If it is very short or you want to start discussion about the idea before implementing it, you can do so by submitting an issue.
