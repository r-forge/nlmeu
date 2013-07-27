library(traceR)

library(testthat)
traceRedit(ns = "testthat")
detach(package:testthat)

library(testthat)
traceR.on()
expect_that
expect_that(5 * 2, equals(10))
head(.traceRmap)
# traceR.optim()
dim(.traceRmap)
traceR.off()
Remove(list = Objects())

unloadNamespace("testthat")
detach(package:traceR)
detach(package:SOAR)
sessionInfo()
