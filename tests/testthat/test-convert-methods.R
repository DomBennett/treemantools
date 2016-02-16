# Test convert-methods

# LIBS
library(treemantools)
library(testthat)

# RUNNING
context ('Testing \'convert-methods\'')
test_that ('setAs(from=TreeMan, to=phylo) works', {
  tree <- randTree(10)
  tree <- as(tree, 'phylo')
  expect_true(is(tree, 'phylo'))
})

