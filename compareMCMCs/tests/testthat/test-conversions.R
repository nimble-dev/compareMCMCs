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
  
  # One conversion
  conversions <- list(z = "sqrt(`a[2]`)")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check1)
  
  # Use matching named lists
  newSamples <- applyConversions(list(someMCMC = samples, anotherMCMC = samples), list(someMCMC = conversions))
  expect_identical(newSamples$someMCMC, check1)
  expect_identical(newSamples$anotherMCMC, samples)
  
  # Use MCMCresult object
  origSamples <- MCMCresult$new(samples = samples)
  newSamples <- applyConversions(origSamples, conversions)
  expect_true(inherits(newSamples, 'MCMCresult'))
  expect_identical(origSamples$samples, check1) # conversion was done in place

  # Use MCMCresult object in named list
  newSamples <- applyConversions(list(someMCMC = MCMCresult$new(samples = samples)), list(someMCMC = conversions))
  expect_true(inherits(newSamples$someMCMC, 'MCMCresult'))
  expect_identical(newSamples$someMCMC$samples, check1)
  
  # Conversion then removal
  conversions <- list(z = "sqrt(`a[2]`)", 'a[2]' = NULL)
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)

  # Different use of quotes  
  conversions <- list(z = "sqrt(`a[2]`)", `a[2]` = NULL)
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  # Different model of removal ("" instead of NULL)
  conversions <- list(z = "sqrt(`a[2]`)", `a[2]` = "")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  # quote() instead of ""
  conversions <- list(z = quote(sqrt(`a[2]`)), `a[2]` = "")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  myConv <- function(samples) {
    sqrt(samples[,'a[2]'])
  }
  
  # Provided as a function for a whole column
  conversions <- list(z = myConv, `a[2]` = "")
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check2)
  
  # Additional steps and removals
  conversions <- list(z = quote(sqrt(`a[2]`)),
                      `a[2]` = "", z2 = "z^2", z = NULL)
  newSamples <- applyConversions(samples, conversions)
  expect_identical(newSamples, check3)
  
  # invalid calculation:
  myInvalidConv <- function(samples) {
    rep(1, nrow(samples)-1) # will give a size error
  }
  expect_warning(newSamples <- applyConversions(samples, list(z = myInvalidConv)))
  
  
}
)
