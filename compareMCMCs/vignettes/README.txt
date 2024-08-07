The vignette for compareMCMCs is managed using the trick described on this
rOpenSci blog post by Jeroen Ooms:
https://ropensci.org/blog/2019/12/08/precompute-vignettes/, which we found from
Chapter 17 of Wickham and Bryan's "R packages" 2nd edition: https://r-pkgs.org/.

The problems for our vignette are that it takes a few minutes to run (and so is
not convenient to run repeatedly during package building), and refers in its
text to results it has run (which could differ on different machines so are
handy to use from a known machine), and relies on other packages and a compiler
toolchain that are not otherwise package needs. Hence we prefer to run the
vignette code on a known machine.

The trick presented in the blog post is to put the Rmd source code in
"compareMCMCs.Rmd.orig" and run knitr::knit("compareMCMCs.Rmd.orig", output =
"compareMCMCs.Rmd"). This step runs the R code in the "orig" file and puts the R
output into the code chunks of Rmd output file. Then when the vignette is built
in the package build process (e.g. R CMD build), the source is compareMCMCs.Rmd,
which contains the already-run output. This builds quickly.

In addition, the compareMCMCs vignette links to results of
make_MCMC_comparison_pages for example1 and example2 (i.e. html and jpg files
generated by compareMCMCs). Those files are marked for copying with the vignette
by inclusion in vignettes/.install_extras. We are pretty sure that "Writing R
Extensions" (https://cran.r-project.org/doc/manuals/R-exts.html) was updated to
clarify how to do that since we first tried to solve our vignette needs. Thanks
to the folks who maintain that document.

We previously had tried a different workflow for building the vignette. This
included a step of embedding jpg figures in html. That is no longer necessary,
but since we worked it out we are retaining the code in "embed_jpg_in_html.R" in
our package source in case we need it in the future.

Users wanting to run the vignette from source should note the "work_dir"
variable created in the "setup" code chunk (and illustrated in a later code
chunk). We show as a default how to set work_dir to "tempdir()" (R's
session-specific temporary directory for working files) to comply with CRAN's
policy that vignettes should not write into a user's workspace. However, you
will then need to find the generated files (from make_MCMC_comparison_pages) to
open them and see the results. If you don't mind having files written locally,
uncomment the code "work_dir = getwd()", which will result in the files being
placed in your curreng working directory.
