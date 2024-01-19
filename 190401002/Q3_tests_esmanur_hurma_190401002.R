library(testthat)
library(readr)

# Veri setini okuma testi
test_that("Veri setini doğru şekilde oku", {
  myDF <- "cyberthreat.csv"
  
  expect_is(myDF, "data.frame")
  expect_true(!is.null(myDF), "Veri seti boş.")
})

# Satır ve sütun sayısı testi
test_that("Satır ve sütun sayısı doğru mu?", {
  dosya_yolu <- "C:/Users/Casper/final/cyberthreat.csv"
  myDF <- read_csv(dosya_yolu)
  
  expect_equal(nrow(myDF), 500, "Satır sayısı beklenen değere eşit değil.")
  expect_equal(ncol(myDF), 10, "Sütun sayısı beklenen değere eşit değil.")
})

# Değişken tipi testi
test_that("Değişken tipi doğru mu?", {
  dosya_yolu <- "C:/Users/Casper/final/cyberthreat.csv"
  myDF <- read_csv(dosya_yolu)
  
  expect_equal(class(myDF)[[1]], "data.frame", "Değişken tipi beklenen değere eşit değil.")
})

# Eksik değer kontrolü testi
test_that("Eksik değer yok mu?", {
  dosya_yolu <- "C:/Users/Casper/final/cyberthreat.csv"
  myDF <- read_csv(dosya_yolu)
  
  expect_equal(sum(is.na(myDF)), 0, "Eksik değer bulunmaktadır.")
})
