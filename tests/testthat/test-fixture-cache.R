test_that("run_build_raad_cache writes a listing per root and loads it", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  dbs <- raad_filedb_path(fx$roots)
  expect_true(all(file.exists(dbs)))
  db <- get_raad_filenames(all = TRUE)
  expect_s3_class(db, "tbl_df")
  expect_named(db, c("root", "file"))
  expect_equal(nrow(db), sum(fx$n))
  ## the listing does not list itself
  expect_false(any(grepl("^\\.raad_admin", db$file)))
  expect_setequal(unique(db$root), fx$roots)
  ## file column is root-relative, no leading slash
  expect_false(any(grepl("^/", db$file)))
  expect_true(all(file.exists(file.path(db$root, db$file))))
  expect_s3_class(attr(getOption("raadfiles.env")$raadfiles.filename.database, "raad_time_stamp"), "POSIXct")
})

test_that("all = FALSE drops deprecated roots by unique root, not per row", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  db_all <- get_raad_filenames(all = TRUE)
  db <- suppressMessages(get_raad_filenames())
  expect_equal(nrow(db), nrow(db_all) - fx$n[3])
  expect_false(any(grepl("data_deprecated", db$root)))
})

test_that("database status is a size+mtime signature and short-circuits reload", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  st <- getOption("raadfiles.database.status")
  expect_named(st, c("db", "md5", "file_ok"))
  expect_equal(st$db, raad_filedb_path(fx$roots))
  expect_true(all(grepl("^[0-9]+_[0-9]+\\.[0-9]+$", st$md5)))
  expect_message(set_raad_filenames(), "up to date")
})

test_that("a rebuilt listing is detected and reloaded", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  n0 <- nrow(get_raad_filenames(all = TRUE))
  ## add a file to one root and rebuild; sleep so mtime moves on coarse filesystems
  dir.create(file.path(fx$roots[1], "example.org/new"), recursive = TRUE)
  file.create(file.path(fx$roots[1], "example.org/new/added.nc"))
  Sys.sleep(1.1)
  suppressMessages(capture.output(run_build_raad_cache()))
  expect_equal(nrow(get_raad_filenames(all = TRUE)), n0 + 1)
})

test_that("local copies are kept under R_user_dir and refreshed on change", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  cd <- local_cache_dir()
  tabs <- list.files(cd, "\\.tab$", full.names = TRUE)
  sigs <- list.files(cd, "\\.sig$", full.names = TRUE)
  expect_length(tabs, length(fx$roots))
  expect_length(sigs, length(fx$roots))
  ## copies are byte-identical to the originals
  dbs <- raad_filedb_path(fx$roots)
  copies <- raadfiles:::local_filedb_copies(dbs, raadfiles:::db_signature(dbs))
  expect_true(all(copies != dbs))
  expect_equal(unname(tools::md5sum(copies)), unname(tools::md5sum(dbs)))
  ## sig sidecars hold the current signature
  expect_setequal(vapply(sigs, readLines, "", n = 1L), raadfiles:::db_signature(dbs))
  ## change one listing only: its copy is replaced, the others are untouched
  m0 <- file.mtime(copies)
  Sys.sleep(1.1)
  cat("root\tfile\n", file = dbs[2], append = FALSE)
  cat(sprintf("%s\texample.org/new/added2.nc\n", fx$roots[2]), file = dbs[2], append = TRUE)
  suppressMessages(set_raad_filenames())
  copies2 <- raadfiles:::local_filedb_copies(dbs, raadfiles:::db_signature(dbs))
  expect_equal(copies2, copies)
  expect_gt(file.mtime(copies[2]), m0[2])
  expect_equal(file.mtime(copies[c(1, 3)]), m0[c(1, 3)])
  db <- get_raad_filenames(all = TRUE)
  expect_equal(sum(db$root == fx$roots[2]), 1)
})

test_that("raadfiles.local.cache = FALSE reads the originals", {
  fx <- make_fixture(); cleanup <- use_fixture(fx, local_cache = FALSE); on.exit(cleanup())
  expect_length(list.files(local_cache_dir(), "\\.tab$"), 0)
  dbs <- raad_filedb_path(fx$roots)
  expect_equal(raadfiles:::local_filedb_copies(dbs, raadfiles:::db_signature(dbs)), dbs)
  expect_equal(nrow(get_raad_filenames(all = TRUE)), sum(fx$n))
})

test_that("an unwritable cache dir falls back to the originals silently", {
  fx <- make_fixture(); cleanup <- use_fixture(fx); on.exit(cleanup())
  Sys.setenv(R_USER_CACHE_DIR = file.path(tempdir(), "no", "such", "\001"))
  dbs <- raad_filedb_path(fx$roots)
  out <- tryCatch(raadfiles:::local_filedb_copies(dbs, raadfiles:::db_signature(dbs)),
                  warning = function(w) stop("warned: ", conditionMessage(w)))
  expect_true(all(out == dbs | file.exists(out)))
  expect_no_warning(expect_message(set_raad_filenames(clobber = TRUE), "Uploading"))
  expect_equal(nrow(get_raad_filenames(all = TRUE)), sum(fx$n))
})

test_that("no roots gives an empty database and a warning, not an error", {
  old <- options(raadfiles.data.roots = "", raadfiles.database.status = NULL)
  on.exit(options(old))
  expect_warning(set_raad_filenames(clobber = TRUE), "no file cache found")
})
