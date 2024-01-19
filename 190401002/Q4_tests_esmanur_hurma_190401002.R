library(testthat)
library(VIM)
library(dplyr)

# Eksik Veri Kontrolü Testi
test_that("Eksik Veri Kontrolü", {
  myDF <- "cyberthreat.csv"
  
  # Her sütundaki eksik değer sayıları kontrolü
  eksik_degerler <- colSums(is.na(myDF))
  expect_equal(sum(eksik_degerler), 0, "Eksik değer bulunmaktadır.")
  
  # Eksik değer görselleştirme kontrolü
  aggr_plot <- aggr(myDF, col=c('navajowhite1', 'navajowhite3'), 
                    numbers=TRUE, sortVars=TRUE, labels=names(myDF), 
                    cex.axis=.7, gap=3, 
                    ylab=c("Histogram of missing data","Pattern"))
  expect_true(!is.null(aggr_plot), "Eksik değer görselleştirme başarısız.")
})

# Veri Dağılımını Görselleştirme Testi
test_that("Veri Dağılımını Görselleştirme", {
  dosya_yolu <- "C:/Users/Casper/final/cyberthreat.csv"
  myDF <- read.csv(dosya_yolu)
  
  # Numeric sütunları kontrol et
  numeric_columns <- sapply(myDF, is.numeric)
  numeric_columns_names <- names(numeric_columns[numeric_columns])
  
  # Histogram kontrolü
  for (column in numeric_columns_names) {
    hist_plot <- hist(myDF[[column]], main = paste(column, "Distribution"), 
                      xlab = "Values", col = "lightblue")
    expect_true(!is.null(hist_plot), paste(column, "Histogram görselleştirme başarısız."))
  }
})

# Belirli Sütunları Kaldırma Testi
test_that("Belirli Sütunları Kaldırma", {
  dosya_yolu <- "C:/Users/Casper/final/cyberthreat.csv"
  myDF <- read.csv(dosya_yolu)
  
  # Belirli sütunları kaldır
  myDF_kaldirilmis <- myDF %>%
    select(-`Flag`, -`Source IP Address`, -`Destination IP Address`)
  
  # Kaldırılmış sütunları kontrol et
  colnames_kaldirilmis <- colnames(myDF_kaldirilmis)
  expect_true("Flag" %in% colnames_kaldirilmis == FALSE, "Flag sütunu kaldırılamadı.")
  expect_true("Source IP Address" %in% colnames_kaldirilmis == FALSE, "Source IP Address sütunu kaldırılamadı.")
  expect_true("Destination IP Address" %in% colnames_kaldirilmis == FALSE, "Destination IP Address sütunu kaldırılamadı.")
})
