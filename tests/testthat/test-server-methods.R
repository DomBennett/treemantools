# Test server-methods

# LIBS
library(treemantools)
library(testthat)

# RUNNING
context ('Testing \'server-methods\'')
test_that ('.safeFromJSON([basic]) works', {
  # simply show that an error is thrown and handled
  expect_that (treemantools:::.safeFromJSON(url='dummyaddress',
                                            max_trys=0), throws_error())
})

test_that ('taxaResolve([basic]) works', {
  res <- taxaResolve('Homo sipins')
  expect_that(res$taxid, equals('9606'))
})
