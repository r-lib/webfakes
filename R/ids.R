
#' Generate random ids.
#' 
#' @param data_set Data set to use. See currently included data sets
#'   below.
#' @param n Number of ids to generate.
#' @param unique Whether to make sure that ids are unique by appending
#'   numbers to them.
#' @param join How to join the words. Only applies if data set has
#'   multiple words.
#' @param case Whether to convert words to lower or upper case, or keep
#'   them as they are.
#' @param pkgname Whether to make sure that the generated ids are valid
#'   package names, by dropping forbidden characters, and potentially
#'   prefixing with `pkg`.
#'
#' @details
#' ## Data sets
#'
#' * `cran`: names of CRAN packages.
#' * `cats`: cat names, from https://github.com/sindresorhus/cat-names
#' * `dogs`: dog names, from https://github.com/sindresorhus/dog-names
#' * `pokemon`: Pokemon names, from https://github.com/sindresorhus/pokemon
#' * `superheroes`: Superhero names, from
#'   https://github.com/sindresorhus/superheroes
#' * `supervillains`: Supervillain names, from
#'   https://github.com/sindresorhus/supervillains
#' * `dinosaurs`: dinosaur names, from https://github.com/dariusk/corpora
#' * `animals`: adjective-animals, from
#'   https://github.com/a-type/adjective-adjective-animal
#' 
#' @export

ids <- function(data_set = c("cran", "cats", "dogs", "pokemon",
                             "superherores", "supervillains", "dinosaurs",
                             "animals"),
                n = 1, unique = TRUE,
                join = c("empty", "snake", "dot"),
                case = c("lower", "upper", "title", "keep"),
                pkgname = TRUE) {

  data_set <- match.arg(data_set)
  data <- get_data(data_set)

  join <- match.arg(join)
  case <- match.arg(case)
  
  # sample
  smp <- lapply(data, sampleex, n = n)
  smp <- lapply(smp, function(x) unlist(lapply(x, rawToChar)))
  
  # case
  smp <- lapply(smp, do_case, case = case)

  # join
  sep <- c(empty = "", snake = "_", dot = ".")[join]
  smp <- do.call("paste", c(smp, list(sep = sep)))

  # pkgname
  if (pkgname) {
    smp <- gsub("[^a-zA-Z0-9.]", "", smp)
    smp <- sub("[^a-zA-Z0-9]*$", "", smp)
    smp <- ifelse(grepl("^[a-zA-Z]", smp), smp, paste0("pkg", smp))
  }

  # unique
  if (unique) smp <- make.unique(smp)

  smp
}

get_data <- function(data_set) {
  env <- new.env(parent = emptyenv())
  data(list = data_set, package = .packageName, envir = env)
  data <- env[[data_set]]
  if (!is.list(data[[1]])) data <- list(data)
  data
}

sampleex <- function(x, n) {
  if (length(x) == 1) {
    rep(x, n)
  } else if (length(x) < n) {
    sample(x, n, replace = TRUE)
  } else {
    sample(x, n)
  }
} 

do_case <- function(x, case) {
  if (case == "lower") {
    x <- tolower(x)
  } else if (case == "upper") {
    x <- toupper(x)
  } else if (case == "title") {
    x <- tolower(x)
    substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  }
  x
}

# ----------------------------------------------------------------------

function() {
  pkgs <- asNamespace("pkgcache")$meta_cache_list()
  cran <- unique(pkgs$package[pkgs$type == "cran"])
  cran <- lapply(cran, charToRaw)
  save(
    cran,
    file = "data/cran.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}

# ----------------------------------------------------------------------
# https://github.com/sindresorhus/cat-names

function() {
  catsjs <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/cat-names/master/cat-names.json"
  )
  cats <- lapply(catsjs, charToRaw)
  save(
    cats,
    file = "data/cats.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}

# ----------------------------------------------------------------------
# https://github.com/sindresorhus/dog-names

function() {
  dogsjs1 <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/dog-names/master/female-dog-names.json"
  )
  dogsjs2 <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/dog-names/master/male-dog-names.json"
  )
  dogsjs <- c(dogsjs1, dogsjs2)
  dogs <- lapply(dogsjs, charToRaw)
  save(
    dogs,
    file = "data/dogs.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}

# ----------------------------------------------------------------------
# https://github.com/sindresorhus/pokemon

function() {
  pokemonjs <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/pokemon/master/data/en.json"
  )                       
  pokemon <- lapply(pokemonjs, charToRaw)
  save(
    pokemon,
    file = "data/pokemon.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}
  
# ----------------------------------------------------------------------
# https://github.com/sindresorhus/superheroes

function() {
  superheroesjs <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/superheroes/master/superheroes.json"
  )                       
  superheroes <- lapply(superheroesjs, charToRaw)
  save(
    superheroes,
    file = "data/superheroes.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}

# ----------------------------------------------------------------------
# https://github.com/sindresorhus/supervillains

function() {
  supervillainsjs <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/supervillains/master/supervillains.json"
  )                       
  supervillains <- lapply(supervillainsjs, charToRaw)
  save(
    supervillains,
    file = "data/supervillains.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}
  
# ----------------------------------------------------------------------
# https://github.com/sindresorhus/supervillains

function() {
  supervillainsjs <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/sindresorhus/supervillains/master/supervillains.json"
  )                       
  supervillains <- lapply(supervillainsjs, charToRaw)
  save(
    supervillains,
    file = "data/supervillains.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}
  
# ----------------------------------------------------------------------
# https://github.com/dariusk/corpora

function() {
  dinojs <- jsonlite::fromJSON(
    "https://raw.githubusercontent.com/dariusk/corpora/master/data/animals/dinosaurs.json"
  )
  dinosaurs <- lapply(dinojs$dinosaurs, charToRaw)
  save(
    dinosaurs,
    file = "data/dinosaurs.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}

# ----------------------------------------------------------------------
# https://github.com/a-type/adjective-adjective-animal

function() {
  adj <- readLines(
    "https://raw.githubusercontent.com/a-type/adjective-adjective-animal/master/lib/lists/adjectives.js"
  )
  adj <- gsub('[",]', "", adj[-length(adj)][-1])
  ani <- readLines(
    "https://raw.githubusercontent.com/a-type/adjective-adjective-animal/master/lib/lists/animals.js",
    warn = FALSE
  )
  ani <- gsub('["\t,]', "", ani[-length(ani)][-1])
  animals <- list(
    lapply(adj, charToRaw),
    lapply(ani, charToRaw)
  )
  save(
    animals,
    file = "data/animals.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}

# ----------------------------------------------------------------------

nice <- function(n = 1) {
  out <- adjnice[sample(seq_along(adjnice), n, replace = TRUE)]
  map_chr(out, rawToChar)
}

function() {
  adj <- source("https://raw.githubusercontent.com/cran/praise/master/R/adjective.R")
  adjnice <- lapply(adj$value, charToRaw)
  save(
    adjnice,
    file = "data/adjnice.rda",
    version = 2,
    compress = "xz",
    compression_level = 9L
  )
}
