mk_deps_suggests <- function(x, name, optdeps = FALSE) {
  x <- unlist(strsplit(x, ",[[:space:]]*"))
  x <- gsub("R[[:space:]]*\\(*", NA, x)
  x <- gsub("[[:space:]]*NA", NA, x)
  x <- gsub("\\n", "", x)
  x <- x[!is.na(x)]
  x <- sub("[ (].*", "", x)
  ## Base packages:
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
    if (length(x) >= 1 & nchar(x[1]) > 0) x <- paste0("optdepends=(", rpkgs, ")")
  } else if (name %in% c("inline")){
    x <- paste0("depends=('r' 'gcc-fortran' ", rpkgs, ")")
  } else x <- paste0("depends=('r' ", rpkgs, ")")
  x
}

sub_license <- function(x){
  ## Make x a character vector where each element is a license:
  x <- unlist(strsplit(x, " \\| "))
  ## (L)GPL get treated specially:
  if (sum(x == c("GPL-2", "GPL-3")) == 2){
    return("license=('GPL')")
  } else if (sum(x == c("LGPL-2", "LGPL-3")) == 2){
    return("license=('LGPL')")
  }

  x <- ifelse(x %in% c("GPL (>= 3)",
                      "GPL-3",
                      "GNU General Public License version 3",
                      "GNU General Public License",
                      "GPL (>= 2.15.1)",
                      "GPL (>= 3.0)"),
             "GPL3",
      ifelse(x %in% c("GPL (>= 2)",
                      "GPL (>= 2.0)"),
             "GPL",
      ifelse(x %in% c("GPL-2"),
             "GPL2",
      ifelse(x %in% c("Free BSD",
                      "FreeBSD",
                      "BSD_3_clause + file LICENSE"),
             "BSD",
      ifelse(x %in% c("LGPL (>= 3)",
                      "LGPL-3",
                      "LGPL (>= 3.0)"),
             "LGPL3",
      ifelse(x %in% c("LGPL (>= 2)",
                      "LGPL (>= 2.0)"),
             "LGPL",
      ifelse(x %in% c("Apache License 2.0",
                      "Apache License",
                      "Apache License (== 2.0)"),
             "Apache",
      ifelse(x %in% c("MIT", "MIT + file LICENSE"),
             "MIT",
      ifelse(x %in% c("Artistic-2.0"),
             "Artistic2.0",
      ifelse(x %in% c("Mozilla Public License"),
             "MPL2",
      ifelse(x %in% c("file LICENSE"),
             "custom", x)))))))))))
  paste0("license=(",
         paste0("'", x, "'", collapse = " "),
         ")")
}

clean_pkgdesc <- function(desc, name){
  ## Stupidly remove all quotes and cut the desc at 80 chars
  desc <- gsub("'", "", desc)
  desc <- gsub('"', "", desc)
  desc <- gsub('\n', " ", desc)
  if (nchar(desc) > 80) x <- substr(desc, 1, 80)
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
  pkg_name <- tolower(cran_pkg)
  cran_version <- pkg[["Version"]]
  depends <- paste0(pkg[["Depends"]], ", ", pkg[["Imports"]], ", ", pkg[["LinkingTo"]])
  depends <- mk_deps_suggests(depends, pkg_name)
  optdepends <- mk_deps_suggests(pkg[["Suggests"]], pkg_name, TRUE)
  license <- sub_license(pkg[["License"]])
  md5sum <- paste0("'", pkg[[65]], "'")
  desc <- clean_pkgdesc(pkg[["Title"]], pkg_name)
  PKGBUILD <- gsub("CRANNAME", cran_pkg, PKGBUILD_TEMPLATE)
  PKGBUILD <- gsub("CRANVERSION", cran_version, PKGBUILD)
  PKGBUILD <- gsub("PKGNAME", pkg_name, PKGBUILD)
  PKGBUILD <- gsub("LICENSE", license, PKGBUILD)
  PKGBUILD <- gsub("OPTDEPENDS", paste0(optdepends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("DEPENDS", paste0(depends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("MD5SUM", md5sum, PKGBUILD)
  PKGBUILD <- gsub("PKGDESC", desc, PKGBUILD)
}

write_pkgbuild <- function(pkg){
  name <- pkg["Package"]
  whitelist <- c(
    "assertthat",
    "base64enc",
    "callr",
    "cellranger",
    "dbi",
    "dbplyr",
    "dichromat",
    "digest",
    "gtable",
    "haven",
    "htmltools",
    "inline",
    "knitr",
    "labeling",
    "modelr",
    "munsell",
    "processx",
    "rcolorbrewer",
    "rcppeigen",
    "readxl",
    "rematch",
    "reprex",
    "rlang",
    "rmarkdown",
    "rprojroot",
    "rstan",
    "rvest",
    "selectr",
    "stanheaders",
    "stringr",
    "tidyverse",
    "timedate",
    "xml2",
    "zoo"
  )
  if(tolower(name) %in% whitelist){
    dir <- paste0("PKGBUILDS/r-", tolower(name))
    dir.create(dir, showWarnings = FALSE)
    PKGBUILD <- make_pkgbuild(pkg)
    writeLines(PKGBUILD, paste0(dir, "/PKGBUILD"))
  } else message("Skipping ", pkg[1])
}

write_all_pkgbuilds <- function(){
  av <- tools::CRAN_package_db()
  apply(av, 1, write_pkgbuild)
  system("git submodule foreach 'makepkg --printsrcinfo > .SRCINFO'")
  message("Done!")
}
