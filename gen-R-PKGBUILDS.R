mk_deps_suggests <- function (x, optdeps = FALSE) {
  x <- unlist(strsplit(x, ",[[:space:]]*"))
  x <- gsub("R[[:space:]]*\\(*", NA, x)
  x <- gsub("[[:space:]]*NA", NA, x)
  x <- gsub("\\n", "", x)
  x <- x[!is.na(x)]
  x <- sub("[ (].*", "", x)
  x <- paste0("depends=(", paste0("'r-cran-", tolower(x), "'", collapse = " "), ")")
  if (optdeps) {
    x <- paste0("opt", x)
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

make_pkgbuild <- function (pkg) {
  PKGBUILD_TEMPLATE <-
    "# Maintainer: Alex Branham <branham@utexas.edu>
_cranname=CRANNAME
_cranver=CRANVERSION
pkgname=r-cran-PKGNAME
pkgver=PKGVERSION
pkgrel=1
pkgdesc='Methods for totally ordered indexed observations'
url=\"https://cran.r-project.org/web/packages/${_cranname}/index.html\"
arch=('x86_64')
LICENSE
DEPENDS
OPTDEPENDS
source=(\"https://cran.r-project.org/src/contrib/${_cranname}_${_cranver}.tar.gz\")
md5sums=()

package() {
    mkdir -p ${pkgdir}/usr/lib/R/library
    cd ${srcdir}
    R CMD INSTALL ${_cranname} -l ${pkgdir}/usr/lib/R/library
}
"
  cran_pkg <- pkg[["Package"]]
  cran_version <- pkg[["Version"]]
  depends <- paste0(pkg[["Depends"]], ", ", pkg[["Imports"]])
  depends <- mk_deps_suggests(depends)
  optdepends <- mk_deps_suggests(pkg[["Suggests"]], TRUE)
  license <- sub_license(pkg[["License"]])
  pkg_name <- cran_pkg
  pkg_version <- gsub("[:-]", ".", cran_version)
  PKGBUILD <- gsub("CRANNAME", cran_pkg, PKGBUILD_TEMPLATE)
  PKGBUILD <- gsub("CRANVERSION", cran_version, PKGBUILD)
  PKGBUILD <- gsub("PKGNAME", tolower(pkg_name), PKGBUILD)
  PKGBUILD <- gsub("PKGVERSION", pkg_version, PKGBUILD)
  PKGBUILD <- gsub("LICENSE", paste0("license=('", license, "')"), PKGBUILD)
  PKGBUILD <- gsub("OPTDEPENDS", paste0(optdepends, "\n"), PKGBUILD)
  PKGBUILD <- gsub("DEPENDS", paste0(depends, "\n"), PKGBUILD)
}

write_pkgbuild <- function(pkg){
  dir <- pkg[1]
  PKGBUILD <- pkg[2]
  dir.create(dir)
  writeLines(PKGBUILD, paste0("./", dir, "/PKGBUILD"))
  ## system("makepkg --printsrcinfo > .SRCINFO")
  ## system("updpkgsums")
}

write_all_pkgbuilds <- function(){
  av <- data.frame(available.packages(), stringsAsFactors = FALSE)
  p <- apply(av, 1, make_pkgbuild)
  n <- paste0("r-cran-", tolower(names(p)))
  pkgs <- vector("list", length = length(p))
  for(i in seq_along(p)){
    pkgs[[i]] <- c(dir = n[i], PKGBUILD = p[i])
  }
  lapply(pkgs, write_pkgbuild)
}
