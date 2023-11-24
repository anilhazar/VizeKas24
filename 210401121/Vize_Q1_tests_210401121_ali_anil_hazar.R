if (!require(testthat)) {
  install.packages("testthat")
  library(testthat)
}

rm(list = ls())

current_dir <- getwd()
print(current_dir)
relative_path <- file.path(current_dir, "Vize_Q1_210401121_ali_anil_hazar.R")

source(relative_path)

test_that("Global Workspace’de spotify_token adlı bir değişken olmalı", {
  expect_true(exists("spotify_token"))
})


test_that("spotify_token adlı bir değişken var", {
  expect_true(exists("spotify_token"))
})


test_that("spotify_token adlı değişkenin tipi 'function'", {
  expect_true(is.function(spotify_token))
})



test_that("spotify_token returns a list", {
  result <- spotify_token()
  expect_is(result, "list")
})



test_that("spotify_token returns a list with two elements", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(length(result), 2)
})



test_that("spotify_token returns a list with 'status_code' as the first element", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[1], "status_code")
})



test_that("spotify_token returns a list with 'status_code' as the first element and it's numeric", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[1], "status_code")
  expect_is(result$status_code, "integer")
})



test_that("spotify_token returns a list with 'status_code' as the first element and it's equal to 200", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[1], "status_code")
  expect_equal(result$status_code, 200)
})



test_that("spotify_token returns a list with 'token' as the second element", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[2], "token")
})



test_that("spotify_token returns a list with 'token' as the second element and it's a character", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[2], "token")
  expect_is(result$token, "character")
})



test_that("spotify_token returns a list with 'token' as the second element and it starts with 'Bearer '", {
  result <- spotify_token()
  expect_is(result, "list")
  expect_equal(names(result)[2], "token")
  expect_true(startsWith(result$token, "Bearer "))
})



testthat::test_that("spotify_token() çağrıldığında döndürdüğü listenin ikinci elementi character değişkeninin içinde 122 adet harf bulunmalı", {
  result <- spotify_token()
  expect_length(strsplit(result$token, "")[[1]], 122)
})