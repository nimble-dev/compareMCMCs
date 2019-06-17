# compareMCMCs

compareMCMCs is a package for running, managing, and comparing results from different MCMC packages.   It was refactored from the nimble package as part of the release of nimble version 0.8.0.

Right now, compareMCMCs is incomplete and not yet released on CRAN.

compareMCMCs allows you to configure a set of MCMCs to run and then let compareMCMCs time them and generate html output with comparisons of efficiency and posterior distributions. This system started life as part of the nimble package. Although it is now a separate package, it remains somewhat nimble-centric.

Use of other MCMCs is supported by a plugin system.  Plugins are provided for JAGS, WinBUGS, OpenBUGS and Stan.  Since nimble, JAGS, WinBUGS and OpenBUGS use different dialects of the same model language, it is sometimes possible to compare them using the same model code. It is possible to write new plugins for new MCMC packages fairly easily. 

It is also possible to provide new comparison metrics and/or new figure components for html comparison pages.

In summary, compareMCMCs provides:

- the runMCMCs function to run one or more MCMCs and manage the results;
- the MCMCresult class to manage results by storing samples, timing information, metrics or summaries of performance, and other details;
- a plugin systems to include new MCMC engines;
- a plugin system for new metrics for comparison among MCMCs;
- a system for generating html pages with figures from comparison metrics, including a plugin system to provide new page components;
- partial backward compatibility to nimble's original MCMCsuite and compareMCMCs functions.
