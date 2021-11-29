context("applyConversions works")

test_that("conversion specifications work", {
#  library(compareMCMCs)
  
  samples <- matrix(1:50, ncol = 10)
  colnames(samples) <- paste0("a[", 1:10, "]")
  
  check1 <- as.data.frame(samples)
  check1$z <- sqrt(check1$`a[2]`)
  check1 <- as.matrix(check1)
  
  check2 <- as.data.frame(check1)
  check2$`a[2]` <- NULL
  check2 <- as.matrix(check2)
  
  check3 <- as.data.frame(check2)
  check3$z2 <- check3$z^2
  check3$z <- NULL
  check3 <- as.matrix(check3)
  
  conversions <- list(z = "sqrt(`a[2]`)")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check1)
  
  conversions <- list(z = "sqrt(`a[2]`)", 'a[2]' = NULL)
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  conversions <- list(z = "sqrt(`a[2]`)", `a[2]` = NULL)
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  conversions <- list(z = "sqrt(`a[2]`)", `a[2]` = "")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  conversions <- list(z = quote(sqrt(`a[2]`)), `a[2]` = "")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  myConv <- function(samples) {
    sqrt(samples[,'a[2]'])
  }
  
  conversions <- list(z = myConv, `a[2]` = "")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  conversions <- list(z = quote(sqrt(`a[2]`)),
                      `a[2]` = "", z2 = "z^2", z = NULL)
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check3)
}
)
