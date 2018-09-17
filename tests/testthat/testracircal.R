library(testthat)
library(racir)
context("Calibrating racir files")

unlink("df", recursive = FALSE)
unlink("dfcal", recursive = FALSE)
unlink("df.csv", recursive = FALSE)

set.seed(5)
n <- 50
df <- data.frame(obs   = c(rep(0, 52), "obs", 0, runif(n, 1, 200)),
                 E     = c(rep(0, 52), "E", 0, runif(n, 10 / 10000, 20 / 1000)),
                 A     = c(rep(0, 52), "A", 0, runif(n, -1, 30)),
                 Ci    = c(rep(0, 52), "Ci", 0, runif(n, 50, 700)),
                 gtc   = c(rep(0, 52), "gtc", 0, runif(n, 0.02, 0.3)),
                 Ca    = c(rep(0, 52), "Ca", 0, runif(n, 300, 900)),
                 CO2_r = c(rep(0, 52), "CO2_r", 0, runif(n, 300, 900)),
                 CO2_s = c(rep(0, 52), "CO2_s", 0, runif(n, 300, 900)),
                 H2O_r = c(rep(0, 52), "H2O_r", 0, runif(n, 10, 20)),
                 H2O_s = c(rep(0, 52), "H2O_s", 0, runif(n, 10, 20)),
                 Tleaf = c(rep(0, 52), "Tleaf", 0, rep(25, n)),
                 Qin   = c(rep(0, 52), "Qin", 0, rep(1500, n))
)
write.table(df, "df", sep = "\t", row.names = FALSE)

set.seed(6)
n <- 50
df <- data.frame(obs   = c(rep(0, 52), "obs", 0, runif(n, 1, 200)),
                 E     = c(rep(0, 52), "E", 0, runif(n, 10 / 10000, 20 / 1000)),
                 A     = c(rep(0, 52), "A", 0, runif(n, -1, 30)),
                 Ci    = c(rep(0, 52), "Ci", 0, runif(n, 50, 700)),
                 gtc   = c(rep(0, 52), "gtc", 0, runif(n, 0.02, 0.3)),
                 Ca    = c(rep(0, 52), "Ca", 0, runif(n, 300, 900)),
                 CO2_r = c(rep(0, 52), "CO2_r", 0, runif(n, 300, 900)),
                 CO2_s = c(rep(0, 52), "CO2_s", 0, runif(n, 300, 900)),
                 H2O_r = c(rep(0, 52), "H2O_r", 0, runif(n, 10, 20)),
                 H2O_s = c(rep(0, 52), "H2O_s", 0, runif(n, 10, 20)),
                 Tleaf = c(rep(0, 52), "Tleaf", 0, rep(25, n)),
                 Qin   = c(rep(0, 52), "Qin", 0, rep(1500, n))
)
write.table(df, "dfcal", sep = "\t", row.names = FALSE)

files_list_1 <- list.files()

expected_list_length <- length(files_list_1) + 1

racircal(calfile = "dfcal", datafile = "df")

files_list_2 <- list.files()

dfcol <- ncol(df)

dfcorrected <- read.csv("df.csv")

test_that("Columns", {
  expect_is(object = files_list_1, class = "character")
  expect_is(object = files_list_2, class = "character")
  expect_length(object = files_list_2, expected_list_length)
  expect_equal(object = length(dfcorrected), dfcol + 4)
})

unlink("df", recursive = FALSE)
unlink("dfcal", recursive = FALSE)
unlink("df.csv", recursive = FALSE)
