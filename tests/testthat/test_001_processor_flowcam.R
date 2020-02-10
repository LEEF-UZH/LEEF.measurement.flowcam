
# Setup -------------------------------------------------------------------

hash <- function(dir) {
  new_files <- list.files(dir, full.names = FALSE, recursive = TRUE)
  x <- lapply(
    new_files,
    function(fn) {
      fnc <- file.path( dir, fn )
      f <- file( fnc, open = "rb" )
      hash <- as.character( openssl::sha256( f ) )
      close(f)
      rm(f)
      hash <- paste(hash, fn, sep = "  ")
    }
  )
  unlist(x)
}

raw <- system.file( "0.raw.data", package = "LEEF.measurement.flowcam")
pre_processed <- tempfile(pattern = "test_processed_")
extracted <- tempfile(pattern = "test_extracted_")

# Skip if data not there --------------------------------------------------

if (!file.exists(raw)) skip("Test data not available!")

# Test pre_processor ------------------------------------------------------

context("Test pre_processor_flowcam()")

test_that(
  "pre_processor_flowcam() runs without error",
  expect_error(
    object = suppressMessages( pre_processor_flowcam( input = raw, output = pre_processed ) ),
    regexp = NA
  )
)

test_that(
  "pre_processor_flowcam() returns correct result",
  expect_known_hash(
    hash( file.path(pre_processed, "flowcam") ),
    hash = "22ff98e199"
  )
)


# Test extractor ----------------------------------------------------------


context("Test extractor_flowcam()")

test_that(
  "extractor_flowcam() runs without error",
  expect_error(
    object = suppressMessages( extractor_flowcam( input = pre_processed, output = extracted) ),
    regexp = NA
  )
)

test_that(
  "extractor_flowcam() returns correct result",
  expect_known_hash(
    hash( file.path(extracted, "flowcam") ),
    hash = "ee3423236a"
  )
)


# Teardown ----------------------------------------------------------------

unlink(pre_processed, recursive = TRUE, force = TRUE)
unlink(extracted, recursive = TRUE, force = TRUE)

