library(testthat)
library(randomForest)

test_that("Model Eğitme ve Değerlendirme", {
  # Gerekli kütüphaneleri yükleme
  install.packages("randomForest")
  library(randomForest)
  
  # Model için veriyi ayırma
  set.seed(123)  
  sample_index <- sample(1:nrow(myDF), 0.8 * nrow(myDF))  # %80 train, %20 test
  train_data <- myDF[sample_index, ]
  test_data <- myDF[-sample_index, ]
  
  # Hedef değişkeni faktöryel hale getirme
  train_data$`Target Variable` <- as.factor(train_data$`Target Variable`)
  
  # Modeli eğitme
  model <- randomForest(`Target Variable` ~ Protocol + Packet, data = train_data, ntree = 100)
  
  # Test verisi üzerinde tahmin yapma
  predictions <- predict(model, test_data)
  
  # Confusion matrix ve accuracy hesaplama
  conf_matrix <- table(predictions, test_data$`Target Variable`)
  accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
  
  # Accuracy kontrolü
  expect_true(accuracy >= 0 & accuracy <= 1, "Accuracy değeri geçerli aralıkta değil.")
  
  # Precision, Recall ve F1-score hesaplama
  precision <- diag(conf_matrix) / rowSums(conf_matrix)
  recall <- diag(conf_matrix) / colSums(conf_matrix)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  # Metrik değerleri kontrolü
  expect_true(all(precision >= 0 & precision <= 1), "Precision değerleri geçerli aralıkta değil.")
  expect_true(all(recall >= 0 & recall <= 1), "Recall değerleri geçerli aralıkta değil.")
  expect_true(all(f1_score >= 0 & f1_score <= 1), "F1-Score değerleri geçerli aralıkta değil.")
})
