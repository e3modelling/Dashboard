# Base-R regression tests; no app startup, cache writes, or model data required.
# Run from the repository root: Rscript tests/test_shiny_paths.R
repo <- normalizePath(".", winslash = "/", mustWork = TRUE)
app_file <- file.path(repo, "app.R")
expressions <- parse(app_file)
resolver <- Filter(function(expr) {
  is.call(expr) && identical(expr[[1]], as.name("<-")) &&
    identical(expr[[2]], as.name("resolve_app_dir"))
}, as.list(expressions))
stopifnot(length(resolver) == 1L)
eval(resolver[[1]])

# A test file is active/sourced while Shiny evaluates app.R from its actual path.
stopifnot(identical(resolve_app_dir(
  frames = list(list(ofile = file.path(repo, "tests", "test_shiny_paths.R")),
                list(file_norm = app_file)),
  args = character(), working_dir = file.path(repo, "tests")), repo))

# base::source with a path, irrespective of the current working directory.
stopifnot(identical(resolve_app_dir(
  frames = list(list(ofile = app_file)), args = character(),
  working_dir = tempdir()), repo))

# Rscript app.R and the normal Shiny app-directory working directory.
stopifnot(identical(resolve_app_dir(
  frames = list(), args = paste0("--file=", app_file), working_dir = tempdir()), repo))
stopifnot(identical(resolve_app_dir(
  frames = list(), args = character(), working_dir = repo), repo))

# An unrelated source file must not turn tests/ into the application directory.
stopifnot(inherits(try(resolve_app_dir(
  frames = list(list(ofile = file.path(repo, "tests", "test_shiny_paths.R"))),
  args = character(), working_dir = file.path(repo, "tests")), silent = TRUE), "try-error"))

# Exercise Shiny's actual sourcing implementation using a minimal temporary app.
if (requireNamespace("shiny", quietly = TRUE)) {
  local({
    fixture_dir <- tempfile("shiny-path-test-")
    dir.create(fixture_dir)
    fixture_dir <- normalizePath(fixture_dir, winslash = "/", mustWork = TRUE)
    stopifnot(startsWith(fixture_dir, paste0(normalizePath(tempdir(), winslash = "/"), "/")))
    on.exit(unlink(fixture_dir, recursive = TRUE))
    fixture_app <- file.path(fixture_dir, "app.R")
    writeLines(c(deparse(resolver[[1]]), "resolved <- resolve_app_dir()"), fixture_app)
    startup <- new.env(parent = asNamespace("shiny"))
    get("sourceUTF8", asNamespace("shiny"))(fixture_app, envir = startup)
    stopifnot(identical(startup$resolved, fixture_dir))
    startup <- new.env(parent = asNamespace("shiny"))
    source(fixture_app, local = startup)
    stopifnot(identical(startup$resolved, fixture_dir))
  })
}
cat("Shiny application-directory checks passed.\n")
