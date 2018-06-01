get_pkg_var <- function(pkg, var){
  suppressWarnings(
    ## Non-zero exit code if the variable doesn't exist, but we don't care
    system2("git",
            paste0("config --file .gitmodules --get submodule.PKGBUILDS/r-",
                   pkg, ".", var),
            stdout = TRUE, stderr = NULL))
}


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
  x <- ifelse(x %in% c("animation", "cli", "corpus", "dicekriging",
                      "extrafont", "extrafontdb", "gdal", "gdtools",
                      "gridextra", "juniperkernel", "lubridate",
                      "msbvar", "pillar", "repr", "rinside",
                      "rttf2pt1", "scales", "sp", "utf8",
                      "viridislite"),
             paste0("'r-cran-", x, "'"),
             paste0("'r-", x, "'"))
  rpkgs <- paste0(x, collapse = " ")
  ## Remove if we didn't find anything
  if (rpkgs == "'r-'") rpkgs <- NULL
  var <- if (optdeps) {
    "optdepends"
  } else "depends"
  other_pkgs <- get_pkg_var(name, var)
  ## if (nchar(other_pkgs) > 0)
  x <- paste0(other_pkgs, " ", rpkgs)
  x <- trimws(x)
  ## Only write depends if we actually have something
  if (optdeps & nchar(x) > 0){
    x <- paste0("optdepends=(", x, ")")
  } else if (!optdeps){
    x <- paste0("depends=('r' ", x, ")")
  }
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

determine_arch <- function(pkg){
  if (tolower(pkg) == "yes"){
    return("arch=('x86_64')")
  } else return("arch=('any')")
}

gen_replaces <- function(pkg){
  ifelse(pkg %in% c(
    "bit",
    "bitops",
    "catools",
    "coda",
    "depmix",
    "distr",
    "expm",
    "gnumeric",
    "linkcomm",
    "msm",
    "mvtnorm",
    "random",
    "rgl",
    "rlang",
    "scatterplot3d",
    "sfsmisc",
    "startupmsg",
    "sweavelistingutils",
    "tnet",
    "wikibooks",
    "xtable",
    "xml"
  ), paste0("\nreplaces=('", "r-cran-", pkg, "')"),
  "")
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
ARCH
url=\"https://cran.r-project.org/package=${_cranname}\"
LICENSE
DEPENDS
OPTDEPENDS
source=(\"https://cran.r-project.org/src/contrib/${_pkgtar}\")
md5sums=(MD5SUM)REPLACES

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
  arch <- determine_arch(pkg[["NeedsCompilation"]])
  license <- sub_license(pkg[["License"]])
  md5sum <- paste0("'", pkg[[65]], "'")
  desc <- clean_pkgdesc(pkg[["Title"]], pkg_name)
  replaces <- gen_replaces(pkg_name)
  PKGBUILD <- gsub("CRANNAME", cran_pkg, PKGBUILD_TEMPLATE)
  PKGBUILD <- gsub("CRANVERSION", cran_version, PKGBUILD)
  PKGBUILD <- gsub("PKGNAME", pkg_name, PKGBUILD)
  PKGBUILD <- gsub("ARCH", arch, PKGBUILD)
  PKGBUILD <- gsub("LICENSE", license, PKGBUILD)
  PKGBUILD <- gsub("OPTDEPENDS", paste0(optdepends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("DEPENDS", paste0(depends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("MD5SUM", md5sum, PKGBUILD)
  PKGBUILD <- gsub("PKGDESC", desc, PKGBUILD)
  PKGBUILD <- gsub("REPLACES", replaces, PKGBUILD)
}

write_pkgbuild <- function(pkg){
  name <- pkg["Package"]
  dir <- paste0("PKGBUILDS/r-", tolower(name))
  dir.create(dir, showWarnings = FALSE)
  PKGBUILD <- make_pkgbuild(pkg)
  writeLines(PKGBUILD, paste0(dir, "/PKGBUILD"))
}

write_all_pkgbuilds <- function(){
  whitelist <- system2("git",
                      c("config --file .gitmodules  --name-only --get-regexp path"),
                      stdout = TRUE, stderr = NULL)
  ## Remove submodule.PKGBUILDS/r-<pkgname>.path
  whitelist <- substr(whitelist, nchar("submodule.PKGBUILDS/r-") + 1, nchar(whitelist) - 5)
  av <- tools::CRAN_package_db()
  av <- av[tolower(av$Package) %in% whitelist, ]
  apply(av, 1, write_pkgbuild)
  system("git submodule foreach 'makepkg --printsrcinfo > .SRCINFO'")
  message("Done!")
}

add_new_package <- function(name){
  name <- tolower(name)
  system2("git",
          paste0("submodule add --name PKGBUILDS/r-",
                 name,
                 " ssh://aur@aur.archlinux.org/r-",
                 name, ".git PKGBUILDS/r-", name))
  warning("Don't forget to check SystemRequirements")
}
