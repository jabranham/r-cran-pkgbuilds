mk_deps_suggests <- function(x, optdeps = FALSE) {
  x <- unlist(strsplit(x, ",[[:space:]]*"))
  x <- gsub("R[[:space:]]*\\(*", NA, x)
  x <- gsub("[[:space:]]*NA", NA, x)
  x <- gsub("\\n", "", x)
  x <- x[!is.na(x)]
  x <- sub("[ (].*", "", x)
  x <- x[!x %in% c("base", "boot", "class", "cluster", "codetools",
                  "compiler", "datasets", "foreign", "graphics",
                  "grDevices", "grid", "KernSmooth", "lattice",
                  "MASS", "Matrix", "methods", "mgcv", "nlme",
                  "nnet", "parallel", "rpart", "spatial", "splines",
                  "stats", "stats4", "survival", "tcltk", "tools",
                  "translations", "utils")]
  x <- tolower(x)
  ## packages named r-cran-*:
  x <- ifelse(x %in% c("animation", "bh", "bindr", "bindrcpp", "bit",
                      "bitops", "broom", "catools", "chron", "cli",
                      "coda", "corpus", "curl", "data.table",
                      "depmix", "dicekriging", "distr", "dplyr",
                      "expm", "extrafont", "extrafontdb", "forcats",
                      "foreign", "gdal", "gdtools", "ggplot2", "glue",
                      "gnumeric", "gridextra", "hms", "ipsur",
                      "juniperkernel", "lattice", "lazyeval",
                      "linkcomm", "lubridate", "mnormt", "msbvar",
                      "msm", "mvtnorm", "nlme", "pillar", "pkgconfig",
                      "plogr", "plyr", "psych", "purrr", "random",
                      "rcpp", "readr", "repr", "reshape2", "rgl",
                      "rinside", "rlang", "rttf2pt1", "scales",
                      "scatterplot3d", "sfsmisc", "sp", "startupmsg",
                      "sweavelistingutils", "tibble", "tidyr",
                      "tidyselect", "tnet", "utf8", "viridislite",
                      "wikibooks", "xml", "xtable"),
             paste0("'r-cran-", x, "'"),
             paste0("'r-", x, "'"))
  rpkgs <- paste0(x, collapse = " ")
  if (rpkgs == "'r-'") rpkgs <- NULL
  if (optdeps) {
    x <- paste0("optdepends=(", rpkgs, ")")
  } else {
    x <- paste0("depends=('r' ", rpkgs, ")")
  }
  x
}

sub_license <- function(x){
  GPL3 <- c("GPL (>= 3)",
           "GPL-3",
           "GNU General Public License version 3",
           "GNU General Public License",
           "GPL (>= 2.15.1)",
           "GPL (>= 3.0)")
  GPL <- c("GPL (>= 2)",
          "GPL-2 | GPL-3",
          "GPL (>= 2.0)",
          "GPL-2 | GPL-3")
  BSD <- c("Free BSD",
          "FreeBSD",
          "BSD_3_clause + file LICENSE")
  LGPL3 <- c("LGPL (>= 3)",
            "LGPL-3",
            "LGPL (>= 3.0)")
  LGPL <- c("LGPL (>= 2)",
           "LGPL-2 | LGPL-3",
           "LGPL (>= 2.0)",
           "LGPL-2 | LGPL-3")
  apache <- c("Apache License 2.0",
             "Apache License",
             "Apache License (== 2.0)")
  artistic <- c("Artistic-2.0")
  mozilla <- c("Mozilla Public License")
  x <- ifelse(x %in% artistic, "Artistic2.0", x)
  x <- ifelse(x %in% GPL3, "GPL3", x)
  x <- ifelse(x %in% mozilla, "MPL2", x)
  x <- ifelse(x %in% GPL, "GPL", x)
  x <- ifelse(x %in% apache, "Apache", x)
  x <- ifelse(x == "GPL-2", "GPL2", x)
  x <- ifelse(x %in% BSD, "BSD", x)
  x <- ifelse(x == "MIT + file LICENSE", "MIT", x)
  x <- ifelse(x == "file LICENSE", "custom", x)
  return(x)
}

clean_pkgdesc <- function(desc, name){
  if (name == "rstan"){
    desc <- "User-facing R functions for Stan models"
  } else if (name == "stanheaders"){
    desc <- "C++ header files of the Stan project"
  } else if (name=="zoo") {
    desc <- "Methods for totally ordered indexed observations"
  } else if (name == "digest"){
    desc <- "Create compact hash digests of R objects"
  } else if (name == "inline"){
    desc <- "Dynamically define R functions & S4 methods with inlined C, C++ or Fortran code"
  } else if (name == "knitr"){
    desc <- "A general-purpose tool for dynamic report generation in R"
  } else if (name == "rcppeigen"){
    "R and 'Eigen' integration using Rcpp"
  } else if (name == "stringr"){
    desc <- "Consistent, simple, easy to use set of wrappers around the stringi package"
  } else if (name == "tidyverse"){
    desc <- "A set of packages that work in harmony"
  } else if (name == "timedate"){
    desc <- "Rmetrics - Chronological and Calendar Objects"
  } else{
    ## Stupidly remove all quotes and cut the desc at 80 chars
    desc <- gsub("'", "", desc)
    desc <- gsub('"', "", desc)
    if (nchar(desc) > 80) x <- substr(desc, 1, 80)
  }
  desc
}

make_pkgbuild <- function(pkg) {
  PKGBUILD_TEMPLATE <-
    "# Maintainer: Alex Branham <branham@utexas.edu>
_cranname=CRANNAME
_cranver=CRANVERSION
_pkgtar=${_cranname}_${_cranver}.tar.gz
pkgname=r-PKGNAME
pkgver=${_cranver//[:-]/.}
pkgrel=1
pkgdesc=\"PKGDESC\"
arch=('any')
url=\"https://cran.r-project.org/web/packages/${_cranname}/index.html\"
LICENSE
DEPENDS
OPTDEPENDS
source=(\"https://cran.r-project.org/src/contrib/${_pkgtar}\")
md5sums=(MD5SUM)

build(){
    R CMD INSTALL ${_pkgtar} -l $srcdir
}
package() {
    install -d \"$pkgdir/usr/lib/R/library\"
    cp -r \"$srcdir/$_cranname\" \"$pkgdir/usr/lib/R/library\"
}
"
  cran_pkg <- pkg[["Package"]]
  cran_version <- pkg[["Version"]]
  depends <- paste0(pkg[["Depends"]], ", ", pkg[["Imports"]])
  depends <- mk_deps_suggests(depends)
  optdepends <- mk_deps_suggests(pkg[["Suggests"]], TRUE)
  license <- sub_license(pkg[["License"]])
  pkg_name <- tolower(cran_pkg)
  md5sum <- paste0("'", pkg[65], "'")
  desc <- clean_pkgdesc(pkg[["Description"]], pkg_name)
  PKGBUILD <- gsub("CRANNAME", cran_pkg, PKGBUILD_TEMPLATE)
  PKGBUILD <- gsub("CRANVERSION", cran_version, PKGBUILD)
  PKGBUILD <- gsub("PKGNAME", pkg_name, PKGBUILD)
  PKGBUILD <- gsub("LICENSE", paste0("license=('", license, "')"), PKGBUILD)
  PKGBUILD <- gsub("OPTDEPENDS", paste0(optdepends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("DEPENDS", paste0(depends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("MD5SUM", md5sum, PKGBUILD)
  PKGBUILD <- gsub("PKGDESC", desc, PKGBUILD)
}

write_pkgbuild <- function(pkg){
  whitelist <- c("r-rstan", "r-stanheaders", "r-zoo", "r-digest", "r-inline",
                "r-knitr", "r-rcppeigen", "r-stringr", "r-tidyverse",
                "r-timedate")
  if(pkg[1] %in% whitelist){
    dir <- paste0("PKGBUILDS/", pkg[1])
    PKGBUILD <- pkg[2]
    writeLines(PKGBUILD, paste0(dir, "/PKGBUILD"))
    system(paste0("cd ", dir, " & makepkg --printsrcinfo > .SRCINFO"))
  } else message("Skipping", pkg[1])
}

write_all_pkgbuilds <- function(){
  av <- tools::CRAN_package_db()
  p <- apply(av, 1, make_pkgbuild)
  n <- paste0("r-", tolower(av$Package))
  pkgs <- vector("list", length = length(p))
  for(i in seq_along(p)){
    pkgs[[i]] <- c(dir = n[i], PKGBUILD = p[i])
  }
  lapply(pkgs, write_pkgbuild)
}
