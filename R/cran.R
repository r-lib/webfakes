
random_title <- function(n = 1, names = NULL) {
  tools::toTitleCase(paste(
    if (!is.null(names)) names else "This",
    "is a",
    nice(n = n),
    "fake R package"
  ))
}

random_version <- function(n = 1) {
  sample(names(version_hist), n, replace = TRUE, prob = version_hist)
}

random_authors_at_r <- function(n = 1) {
  rep('c(
    person(
      given = "Gábor",
      family = "Csárdi",
      role = c("aut", "cre"),
      email = "csardi.gabor@gmail.com"
    ),
    person(
      family = "RStudio, Pbc.",
      role = "cph"
    )
  )', n)
}

random_description <- function(n = 1) {
  rep("Description. Really does not matter what is here...", n)
}

random_license <- function(n = 1) {
  sample(names(license_hist), n, replace = TRUE, prob = license_hist)
}

#' Fake app for CRAN
#'
#' @return webfakes app that fakes the web servers of one or more
#'   CRAN-like repositories.
#'
#' @export

# TODO:
# * R versions, platforms and binaries
# * serve the actual package files

cran_app <- function() {

  app <- new_app()

  # ----------------------------------------------------------------------
  # Internals

  repos <- structure(list(), names = character())

  rx_pkg_file <- "(?<pkg>.*)_(?<version>.*)\\.tar\\.gz"

  config <- list(
    Package = function(n) webfakes::ids("dogs", n = n),
    Title = random_title,
    Version = random_version,
    `Authors@R` = random_authors_at_r,
    Description = random_description,
    License = random_license,
    fields = c(
      "Package", "Version", "Depends", "Imports", "Suggests", "Enhances",
      "License", "MD5sum", "NeedsCompilation"),
    packages_files = c("PACKAGES", "PACKAGES.gz", "PACKAGES.rds")
  )

  temp <- NULL

  cache <- list()

  make_temp <- function() {
    mkdirp(temp <<- tempfile())
    temp
  }

  get_meta_file <- function(repo, file) {
    temp <- temp %||% make_temp()
    path <- file.path(temp, repo, file)
    types <- config$packages_files

    if (! basename(file) %in% types) return(NA_character_)
    if (file.exists(path)) return(path)

    mkdirp(dirname(path))
    fields <- intersect(config$fields, colnames(repos[[repo]]$packages))
    db <- repos[[repo]]$packages[, fields]
    if (basename(file) == "PACKAGES") {
      write.dcf(db, file = path)
    } else if (basename(file) == "PACKAGES.gz") {
      con <- gzfile(path, "wt")
      on.exit(close(con), add = TRUE)
      write.dcf(db, con)
    } else if (basename(file) == "PACKAGES.rds") {
      db <- as.matrix(db)
      rownames(db) <- db[, "Package"]
      saveRDS(db, path, compress = "xz")
    }

    path
  }


  # ----------------------------------------------------------------------
  # Public API

  app$list_repos <- function() repos

  app$get_config <- function(key) config[[key]]

  app$set_config <- function(key, value) config[[key]] <<- value

  app$add_repo <- function(
      name = "CRAN",
      platforms = c("source", "windows", "macos"),
      num_pkgs = 10) {
    if (name %in% names(repos)) stop("Duplicate repo name")
    config <- list(platforms = platforms)
    repos[[name]] <- list(config = config, packages = NULL)
    app$add_packages(name, num_pkgs = num_pkgs)
  }

  app$add_packages <- function(
      repo = "CRAN",
      ...,
      platforms = c("source", "windows", "macos"),
      num_pkgs = 1) {
    fds <- list(...)
    fds$Package      <- fds$Package      %||% config$Package(num_pkgs)
    fds$Title        <- fds$Title        %||% config$Title(num_pkgs, fds$Package)
    fds$Version      <- fds$Version      %||% config$Version(num_pkgs)
    fds$`Authors@R` <- fds$`Authors@R` %||% config$`Authors@R`(num_pkgs)
    fds$Description  <- fds$Description  %||% config$Description(num_pkgs)
    fds$License      <- fds$License      %||% config$License(num_pkgs)
    meta <- data.frame(stringsAsFactors = FALSE, fds)
    repos[[repo]]$packages <<- rbind(repos[[repo]]$packages, meta)
  }

  # ----------------------------------------------------------------------
  # Routes

  app$get(
    paste0("/:repo/src/contrib/PACKAGES", c("", ".gz", ".rds")),
    function(req, res) {
      path <- get_meta_file(req$params$repo, basename(req$url))
      if (is.na(path)) {
        res$send_status(404)
      } else {
        res$send_file(path, root = "/")
      }
  })

  app$get(
    new_regexp(paste0(
      "^/(?<repo>[^/]*)",
      "/src/contrib/",
      rx_pkg_file, "$"
    )),
    function(req, res) {
      TODO
  })

  app
}

depends_hist <- c(
  `0` = 25384L, `1` = 5686L, `2` = 3232L, `3` = 2078L,
  `4` = 1190L, `5` = 731L, `6` = 290L, `7` = 155L, `8` = 245L,
  `9` = 33L, `10` = 12L, `11` = 10L, `12` = 4L, `13` = 1L, `14` = 5L,
  `15` = 1L, `16` = 2L, `17` = 2L, `19` = 1L, `23` = 4L, `27` = 2L
)

imports_hist <- c(
  `0` = 10903L, `1` = 4486L, `2` = 3949L, `3` = 3151L,
  `4` = 2882L, `5` = 2415L, `6` = 1864L, `7` = 1662L, `8` = 1298L,
  `9` = 1136L, `10` = 889L, `11` = 745L, `12` = 603L, `13` = 524L,
  `14` = 466L, `15` = 346L, `16` = 280L, `17` = 257L, `18` = 187L,
  `19` = 173L, `20` = 143L, `21` = 116L, `22` = 100L, `23` = 82L,
  `24` = 71L, `25` = 61L, `26` = 42L, `27` = 46L, `28` = 32L, `29` = 22L,
  `30` = 23L, `31` = 25L, `32` = 22L, `33` = 14L, `34` = 10L, `35` = 8L,
  `36` = 5L, `37` = 6L, `38` = 2L, `39` = 4L, `40` = 2L, `41` = 2L,
  `42` = 2L, `45` = 6L, `51` = 2L, `56` = 2L, `72` = 2L
)

suggests_hist <- c(
  `0` = 15383L, `1` = 4912L, `2` = 4671L, `3` = 3802L,
  `4` = 2767L, `5` = 1955L, `6` = 1435L, `7` = 986L, `8` = 693L,
  `9` = 527L, `10` = 391L, `11` = 312L, `12` = 251L, `13` = 180L,
  `14` = 138L, `15` = 117L, `16` = 69L, `17` = 68L, `18` = 71L,
  `19` = 63L, `20` = 54L, `21` = 23L, `22` = 28L, `23` = 13L, `24` = 21L,
  `25` = 15L, `26` = 12L, `27` = 12L, `28` = 15L, `29` = 8L, `30` = 9L,
  `31` = 5L, `32` = 8L, `33` = 7L, `34` = 8L, `36` = 6L, `38` = 2L,
  `41` = 6L, `42` = 2L, `44` = 2L, `46` = 2L, `47` = 2L, `48` = 2L,
  `50` = 1L, `51` = 2L, `66` = 2L, `67` = 2L, `70` = 2L, `76` = 2L,
  `86` = 2L, `108` = 2L
)

# dput(table(pkgcache::meta_cache_list()$license))

license_hist <- c(
  ACM = 6L, `ACM | file LICENSE` = 6L, AGPL = 19L,
  `AGPL (>= 3)` = 14L, `AGPL + file LICENSE` = 6L, `AGPL | file LICENSE` = 2L,
  `AGPL-3` = 239L, `AGPL-3 | file LICENSE` = 33L, `Apache License` = 16L,
  `Apache License (== 2.0)` = 114L, `Apache License (== 2.0) + file LICENSE` = 2L,
  `Apache License (== 2.0) | file LICENSE` = 26L, `Apache License (== 2)` = 2L,
  `Apache License (== 3.0) | file LICENSE` = 2L, `Apache License (>= 2.0)` = 86L,
  `Apache License (>= 2.0) | file LICENSE` = 1L, `Apache License (>= 2)` = 4L,
  `Apache License | file LICENSE` = 8L, `Apache License 2.0` = 211L,
  `Apache License 2.0 | file LICENSE` = 83L, `Apache License Version 2.0` = 2L,
  `Artistic License 2.0` = 20L, `Artistic-1.0` = 4L, `Artistic-2.0` = 1934L,
  `Artistic-2.0 + file LICENSE` = 7L, `Artistic-2.0 | file LICENSE` = 12L,
  `Artistic-2.0 | GPL-2 + file LICENSE` = 2L, BSD = 4L, `BSD 2-clause License + file LICENSE` = 23L,
  `BSD 3-clause License` = 2L, `BSD 3-clause License + file LICENSE` = 8L,
  BSD_2_clause = 2L, `BSD_2_clause + file LICENCE` = 6L, `BSD_2_clause + file LICENSE` = 221L,
  BSD_3_clause = 2L, `BSD_3_clause + file LICENCE` = 12L, `BSD_3_clause + file LICENSE` = 270L,
  `BSD_3_clause + file LICENSE | GPL (>= 2)` = 4L, `BSD_3_clause + file LICENSE | GPL-2` = 2L,
  BSL = 4L, `BSL-1.0` = 10L, `CC BY 4.0` = 49L, `CC BY-NC 4.0` = 11L,
  `CC BY-NC-ND 4.0` = 6L, `CC BY-NC-ND 4.0 + file LICENSE` = 4L,
  `CC BY-NC-SA 4.0` = 34L, `CC BY-SA 4.0` = 41L, `CC BY-SA 4.0 + file LICENSE` = 4L,
  `CC BY-SA 4.0 | GPL (>= 2)` = 2L, `CC BY-SA 4.0 | GPL-3 | file LICENSE` = 2L,
  CC0 = 373L, `CC0 | file LICENSE` = 2L, CeCILL = 21L, `CeCILL-2` = 28L,
  `CeCILL-2 | file LICENSE` = 4L, `CeCILL-2 | GPL-2` = 2L, `Common Public License Version 1.0` = 4L,
  CPL = 2L, `CPL (>= 2)` = 2L, `CPL-1.0` = 2L, `Creative Commons Attribution 4.0 International License` = 4L,
  EPL = 6L, `EPL (>= 1.0)` = 4L, EUPL = 34L, `EUPL (== 1.1)` = 2L,
  `EUPL (>= 1.2)` = 2L, `EUPL | file LICENSE` = 2L, `EUPL-1.1` = 4L,
  `file LICENCE` = 2L, `file LICENSE` = 139L, FreeBSD = 18L, `FreeBSD | file LICENSE` = 4L,
  `FreeBSD | GPL-2 | file LICENSE` = 2L, `GNU Affero General Public License` = 4L,
  `GNU General Public License` = 65L, `GNU General Public License (>= 2)` = 4L,
  `GNU General Public License (>= 3)` = 10L, `GNU General Public License version 2` = 12L,
  `GNU General Public License version 3` = 5L, `GNU Lesser General Public License` = 3L,
  GPL = 1132L, `GPL (<= 2.0)` = 2L, `GPL (<= 2)` = 2L, `GPL (== 2)` = 6L,
  `GPL (> 2)` = 35L, `GPL (> 3)` = 4L, `GPL (>= 1.0)` = 2L, `GPL (>= 2.0.0)` = 1L,
  `GPL (>= 2.0)` = 232L, `GPL (>= 2.0) | file LICENCE` = 2L, `GPL (>= 2.0) | file LICENSE` = 6L,
  `GPL (>= 2.1)` = 8L, `GPL (>= 2.10)` = 2L, `GPL (>= 2.15.1)` = 4L,
  `GPL (>= 2.15)` = 2L, `GPL (>= 2.2)` = 2L, `GPL (>= 2)` = 9245L,
  `GPL (>= 2) + file LICENSE` = 8L, `GPL (>= 2) | BSD_2_clause + file LICENSE` = 4L,
  `GPL (>= 2) | BSD_3_clause + file LICENSE` = 4L, `GPL (>= 2) | file LICENCE` = 22L,
  `GPL (>= 2) | file LICENSE` = 102L, `GPL (>= 2) | FreeBSD` = 2L,
  `GPL (>= 2) | LGPL (>= 2)` = 4L, `GPL (>= 2) | MIT + file LICENSE` = 2L,
  `GPL (>= 3.0.0)` = 2L, `GPL (>= 3.0)` = 81L, `GPL (>= 3.2)` = 2L,
  `GPL (>= 3.3.2)` = 4L, `GPL (>= 3.5.0)` = 2L, `GPL (>= 3)` = 1739L,
  `GPL (>= 3) + file LICENSE` = 8L, `GPL (>= 3) | CC BY 4.0` = 2L,
  `GPL (>= 3) | file LICENCE` = 10L, `GPL (>= 3) | file LICENSE` = 61L,
  `GPL | file LICENSE` = 6L, `GPL-2` = 5682L, `GPL-2 + file LICENSE` = 6L,
  `GPL-2 | Artistic-2.0` = 2L, `GPL-2 | file LICENCE` = 6L, `GPL-2 | file LICENSE` = 140L,
  `GPL-2 | GPL (>= 2) | GPL-3` = 2L, `GPL-2 | GPL-3` = 673L, `GPL-2 | GPL-3 | BSD_3_clause + file LICENSE` = 2L,
  `GPL-2 | GPL-3 | file LICENSE` = 4L, `GPL-2 | GPL-3 | MIT + file LICENSE` = 4L,
  `GPL-2 | LGPL-2.1 | MPL-1.1` = 2L, `GPL-2 | MIT + file LICENCE` = 2L,
  `GPL-2 | MIT + file LICENSE` = 2L, `GPL-3` = 8148L, `GPL-3 + file LICENCE` = 2L,
  `GPL-3 + file LICENSE` = 95L, `GPL-3 | file LICENCE` = 2L, `GPL-3 | file LICENSE` = 365L,
  `GPL-3 | GPL-2` = 2L, `GPL-3 | LGPL-2.1` = 2L, LGPL = 435L, `LGPL (> 2.0)` = 2L,
  `LGPL (>= 2.0, < 3)` = 4L, `LGPL (>= 2.0, < 3) | file LICENSE` = 2L,
  `LGPL (>= 2.0, < 3) | Mozilla Public License` = 8L, `LGPL (>= 2.0)` = 39L,
  `LGPL (>= 2.1)` = 103L, `LGPL (>= 2.1) | file LICENSE` = 2L,
  `LGPL (>= 2)` = 105L, `LGPL (>= 2) | file LICENSE` = 2L, `LGPL (>= 3.0)` = 4L,
  `LGPL (>= 3)` = 99L, `LGPL (>= 3) | file LICENSE` = 6L, `LGPL-2` = 50L,
  `LGPL-2 | Apache License 2.0` = 2L, `LGPL-2 | BSD_3_clause + file LICENSE` = 4L,
  `LGPL-2 | LGPL-3 | GPL-2 | GPL-3` = 3L, `LGPL-2.1` = 52L, `LGPL-2.1 | file LICENSE` = 12L,
  `LGPL-3` = 360L, `LGPL-3 + file LICENSE` = 7L, `LGPL-3 | Apache License 2.0` = 8L,
  `LGPL-3 | file LICENSE` = 12L, `Lucent Public License` = 2L,
  MIT = 28L, `MIT + file LICENCE` = 39L, `MIT + file LICENSE` = 5324L,
  `MIT + file LICENSE | Apache License 2.0` = 2L, `MIT + file LICENSE | Unlimited` = 2L,
  `MIT +file LICENSE` = 2L, `MIT | file LICENSE` = 4L, `MIT License` = 2L,
  `MIT License + file LICENSE` = 10L, `MIT+file LICENSE` = 8L,
  `Mozilla Public License 1.1` = 2L, `Mozilla Public License 2.0` = 18L,
  `Mozilla Public License Version 2.0` = 2L, MPL = 4L, `MPL (== 2.0)` = 4L,
  `MPL (>= 2.0)` = 4L, `MPL (>= 2)` = 2L, `MPL (>= 2) | file LICENSE` = 2L,
  `MPL (>= 2) | GPL (>= 2) | file LICENSE` = 2L, `MPL-1.1` = 2L,
  `MPL-2.0` = 18L, `MPL-2.0 | file LICENSE` = 4L, Unlimited = 115L,
  `Unlimited | file LICENSE` = 5L
)

# dput(sort(table(pkgcache::meta_cache_list()$version), decreasing=TRUE)[1:200])

version_hist <- c(
  `0.1.0` = 2135L, `1.0` = 1685L, `1.0.0` = 1365L,
  `0.1.1` = 1042L, `1.0.1` = 866L, `1.1` = 823L, `0.2.0` = 758L,
  `0.1.2` = 579L, `1.1.0` = 530L, `1.0.2` = 520L, `1.2` = 507L,
  `0.1` = 504L, `1.2.0` = 481L, `0.1.3` = 432L, `0.2.1` = 402L,
  `1.1.1` = 384L, `0.3.0` = 382L, `1.0.3` = 325L, `1.3` = 312L,
  `1.4.0` = 302L, `0.0.1` = 293L, `0.2.2` = 265L, `0.1.4` = 261L,
  `0.4.0` = 259L, `0.3.1` = 255L, `0.2` = 248L, `2.0` = 234L, `1.2.1` = 230L,
  `2.18.0` = 226L, `1.0.4` = 219L, `1.6.0` = 218L, `1.4` = 210L,
  `1.18.0` = 200L, `0.1.5` = 197L, `1.10.0` = 197L, `3.12.0` = 196L,
  `2.0.0` = 195L, `1.16.0` = 185L, `1.8.0` = 185L, `0.5.0` = 178L,
  `1.1.2` = 178L, `1.12.0` = 174L, `1.26.0` = 174L, `0.3` = 161L,
  `0.3.2` = 160L, `1.5` = 159L, `0.0.2` = 158L, `0.1.6` = 156L,
  `0.2.3` = 153L, `1.28.0` = 152L, `2.1` = 151L, `1.3.1` = 148L,
  `1.22.0` = 142L, `1.24.0` = 142L, `2.0.1` = 140L, `1.0.5` = 139L,
  `1.14.0` = 138L, `0.4` = 130L, `1.2.2` = 129L, `1.20.0` = 128L,
  `0.0.3` = 124L, `1.3.0` = 124L, `2.0.2` = 123L, `1.1.3` = 119L,
  `1.6` = 116L, `0.4.1` = 114L, `0.5.1` = 114L, `1.0-1` = 112L,
  `0.0.4` = 110L, `3.0` = 108L, `0.2.4` = 107L, `0.6.0` = 106L,
  `2.1.0` = 106L, `3.2.3` = 104L, `1.0-0` = 102L, `1.30.0` = 102L,
  `1.4.1` = 102L, `0.9.0` = 101L, `0.1-1` = 98L, `0.3.3` = 97L,
  `1.2.4` = 97L, `0.4.2` = 93L, `0.5` = 93L, `0.6.1` = 89L, `1.1.4` = 89L,
  `0.5.2` = 87L, `1.0.6` = 84L, `2.2` = 83L, `1.3.2` = 81L, `1.4.2` = 81L,
  `0.1-2` = 80L, `0.1.7` = 80L, `0.7.0` = 79L, `0.2.5` = 78L, `1.15.0` = 78L,
  `1.32.0` = 78L, `1.36.0` = 78L, `2.1.1` = 78L, `0.6` = 76L, `1.0.7` = 76L,
  `1.1-1` = 74L, `0.1.9` = 73L, `1.0-2` = 73L, `0.0.6` = 71L, `0.8.0` = 70L,
  `1.0-3` = 70L, `1.2.3` = 70L, `0.2-1` = 67L, `1.5.1` = 67L, `1.7` = 67L,
  `1.0.8` = 66L, `0.1.8` = 65L, `1.38.0` = 65L, `0.3.5` = 64L,
  `2.3` = 64L, `0.5.3` = 63L, `0.3.4` = 62L, `0.9` = 62L, `0.2.6` = 60L,
  `1.34.0` = 60L, `0.9.2` = 58L, `1.3.3` = 58L, `1.62.0` = 58L,
  `1.1.5` = 55L, `0.0.5` = 54L, `0.3.6` = 54L, `0.7` = 54L, `2.4` = 54L,
  `3.0.0` = 54L, `0.4.3` = 53L, `1.50.0` = 53L, `2.2.1` = 53L,
  `0.7.1` = 51L, `1.0-4` = 51L, `2.0.3` = 51L, `1.3.5` = 50L, `1.5.0` = 49L,
  `1.8` = 49L, `0.1-3` = 48L, `0.2.7` = 48L, `0.9.1` = 48L, `1.0-5` = 48L,
  `0.2-2` = 47L, `1.4.3` = 47L, `1.42.0` = 47L, `2.2.0` = 47L,
  `0.3-0` = 46L, `0.3.7` = 46L, `0.4.4` = 46L, `0.8.2` = 45L, `1.2.5` = 45L,
  `0.1-5` = 44L, `1.6.1` = 44L, `8.7.0` = 44L, `0.1-0` = 43L, `0.9.3` = 43L,
  `1.0-6` = 43L, `1.48.0` = 43L, `0.8` = 42L, `1.0.9` = 42L, `1.3.4` = 42L,
  `0.1-6` = 40L, `0.9.6` = 40L, `1.7.0` = 40L, `2.0.4` = 40L, `2.3.0` = 40L,
  `0.1-4` = 39L, `1.5.2` = 39L, `0.6.2` = 38L, `2.8.0` = 38L, `1.1-3` = 37L,
  `2.1.2` = 37L, `3.0.1` = 37L, `0.3-1` = 36L, `1.0-8` = 36L, `1.1-0` = 36L,
  `1.2.6` = 36L, `3.1` = 36L, `3.4.0` = 36L, `0.0-2` = 35L, `0.5.4` = 34L,
  `0.8.1` = 34L, `0.9.4` = 34L, `1.2-2` = 34L, `1.40.0` = 34L,
  `1.44.0` = 34L, `1.9` = 34L, `2.1.5` = 34L, `0.4-0` = 33L, `0.0.7` = 32L,
  `0.4.5` = 32L, `0.7.3` = 32L, `1.1-2` = 32L, `1.6.2` = 32L, `1.64.0` = 32L,
  `2.2.2` = 32L, `1.8.2` = 31L, `2.1.3` = 31L, `0.2-0` = 30L, `0.3-3` = 30L
)
