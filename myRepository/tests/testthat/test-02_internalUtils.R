library("myRepository")

test_that("Internal utility functions", {

    ## Internal options management:
    oKey <- "testKey"
    oVal <- "testValue"
    ## Shouldn't be set initially:
    expect_null(myRepository:::.opt(oKey))
    ## Setting a value should still return the value
    expect_identical(myRepository:::.opt(oKey, oVal), oVal)
    ## Recover the value with just the key
    expect_identical(myRepository:::.opt(oKey), oVal)
    
})
