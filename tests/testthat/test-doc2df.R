library(testthat)

# helper to load Doc2Df without sourcing full script
load_doc2df <- function() {
  lines <- readLines(file.path("..", "..", "codes_new", "CBR_web.R"))
  start <- grep("^Doc2Df <-", lines)[1]
  count_pattern <- function(x, pattern) {
    res <- gregexpr(pattern, x, fixed = TRUE)[[1]]
    sum(res != -1)
  }
  braces <- 0
  end <- start
  repeat {
    braces <- braces + count_pattern(lines[end], "{") - count_pattern(lines[end], "}")
    if (braces == 0) break
    end <- end + 1
  }
  code <- paste(lines[start:end], collapse = "\n")
  eval(parse(text = code), envir = .GlobalEnv)
}

load_doc2df()


test_that("Doc2Df converts xml to data frame", {
  xml_text <- "<root><row><a>1</a><b>2</b></row><row><a>3</a><b>4</b></row></root>"
  library(XML)
  doc <- xmlInternalTreeParse(xml_text)
  df <- Doc2Df(doc, "row")
  expect_equal(names(df), c("a", "b"))
  expect_equal(nrow(df), 2)
})
