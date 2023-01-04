library(testthat)        # load testthat package
library(shiny)

#Basic test method
test_that("Basic Test Works.", {
  expect_equal(1,1)
})

test_that("Increase headers work", {
  # Basic Server Test Method
  print(getwd())
  shiny::testServer(server, expr= {
    session$setInputs()
    r$nHeaders <- 5
    expect_equal(r$nHeaders,5)
  })
})
test_that("File Upload Works", {
  # Tests that files are being uploaded correctly
  shiny::testServer(server, expr = {
    session$setInputs(file1=list(datapath="diffDOBSSN_0_0.csv"), file2=list(datapath="diffDOBSSN_1_1.csv"))
    expect_equal(read.csv("diffDOBSSN_0_0.csv"), mydata1())
    expect_equal(read.csv("diffDOBSSN_1_1.csv"), mydata2())
  })
})
test_that("Header Linkage and Subset works.", {
  # Test that
  shiny::testServer(server, expr = {
    session$setInputs(file1=list(datapath="diffDOBSSN_0.csv"), file2=list(datapath="diffDOBSSN_1.csv"))
    r$nHeaders <- 3
    r$headersLinked <- TRUE
    r$leftSelect <- append(r$leftSelect, list("Full_Name"))
    r$leftSelect <- append(r$leftSelect, list("DOB"))
    r$leftSelect <- append(r$leftSelect, list("SSN"))
    r$rightSelect <- append(r$rightSelect, list("Full_Name"))
    r$rightSelect <- append(r$rightSelect, list("DOB"))
    r$rightSelect <- append(r$rightSelect, list("SSN"))
    session$setInputs(link="click", threshold=.1, show_vars=c("Full_Name","DOB","SSN"))
    Sys.sleep(3)
    expect_equal(names(dataframeValues$testDataA),names(dataframeValues$testDataB));
  })
})

