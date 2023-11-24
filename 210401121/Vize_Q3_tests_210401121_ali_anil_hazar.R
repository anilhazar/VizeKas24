library(testthat)

current_dir <- getwd()
print(current_dir)
relative_path <- file.path(current_dir, "Vize_Q3_210401121_ali_anil_hazar.R")

source(relative_path)

# Test 3.1
test_that("Global Workspace’de my_artists adlı bir değişken olmalı.", {
  expect_true(exists("my_artists"))
})

# Test 3.2
test_that("my_artists adlı değişkenin class’ı “data.frame” olmalı", {
  expect_is(my_artists, "data.frame")
})

# Test 3.3
test_that("my_artists adlı değişken 2 sütun bulundurmalı", {
  my_artists <- data.frame(artist = character(), id = character(), stringsAsFactors = FALSE)
  expect_equal(ncol(my_artists), 2)
})

# Test 3.4
test_that("my_artists adlı değişken sütun isimleri c('artist', 'id') olmalı", {
  my_artists <- data.frame(artist = character(), id = character(), stringsAsFactors = FALSE)
  expect_identical(names(my_artists), c("artist", "id"))
})