# Load the necessary packages, download if not installed yet
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(ggforce)) install.packages("ggforce", 
                                          repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", 
                                       repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", 
                                        repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                     repos = "http://cran.us.r-project.org")
if(!require(kernlab)) install.packages("kernlab", 
                                            repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", 
                                       repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", 
                                    repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", 
                                          repos = "http://cran.us.r-project.org")
if(!require(patchwork)) install.packages("patchwork", 
                                         repos = "http://cran.us.r-project.org")

# Download and read the data (used because it was mentioned in the kaggle data sets)
all_data = fread("https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv")

# Add the column names (as per https://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset))
names(all_data) = c("Age", "Gender", "Total_bilirubin", "Direct_bilirubin", "Alkaline_phosphatase", "Alanine_aminotransferase", "Aspartate_aminotransferase", "Total_protein", "Albumin", "Albumin_globulin_ratio", "Affection_status")

# Check if any data is missing
sum(is.na(all_data)) # Yes four missing values, where? # Albumin_globulin_ratio; also missing in original data set

# Move rows with missing values to separate data frame, once the model is finished, check if prediction is possible when filling in the missing values
missing_data = all_data %>% filter(is.na(Albumin_globulin_ratio))

# Filter out rows with missing values
all_data = all_data %>% filter(!is.na(Albumin_globulin_ratio))

# Check the dimensions of the data set
dim(all_data) # 579 observations # 11 variables

# Recode affection status
all_data = all_data %>% mutate(Affection_status = ifelse(Affection_status == 1, "Affected", "Unaffected"))

# Distribution of affection status
all_data %>% dplyr::select(Affection_status) %>% table() %>% prop.table() # approx. 70:30 split

# Split data into model and validation data set (9:1)
# set.seed(1, sample.kind="Rounding")
set.seed(1)
test_index = createDataPartition(y = all_data$Affection_status, times = 1, p = 0.1, list = FALSE)
model_data = all_data[-test_index,]
validation_data = all_data[test_index,]

# Check if distribution of affection status is still similar
model_data %>% dplyr::select(Affection_status) %>% table() %>% prop.table() # approx. 70:30 split
validation_data %>% dplyr::select(Affection_status) %>% table() %>% prop.table() # approx. 70:30 split

# Disregard the validation data for now and only use the model data for exploratory data analysis (EDA)

# Age distribution
model_data %>% ggplot(aes(x=Age)) + geom_histogram(bins=20) + ggtitle("Age distribution") + ylab("Count")

# Gender distribution
model_data %>% ggplot(aes(x=Gender)) + geom_bar() + ggtitle("Gender distribution") + ylab("Count")

# Age distribution stratified by gender
model_data %>% ggplot(aes(x=Age, fill=Gender)) + geom_histogram(bins=20, alpha=0.5, position="identity") + ggtitle("Age distribution stratified by gender") + ylab("Count")

# Affection status
model_data %>% ggplot(aes(x=Affection_status)) + geom_bar() + ggtitle("Affection status distribution") + ylab("Count")

# Affection status stratified by age and gender (with an additional Tufte boxplot of age stratified by age only)
model_data %>% ggplot(aes(y=Age, x=Affection_status, color=Gender)) + geom_sina() + geom_tufteboxplot(colour="black") + ggtitle("Affection status stratified by age and gender")

# Recode outcome to factor for later model training
model_data$Affection_status = as.factor(model_data$Affection_status)

# Recode gender to numeric for later model training (1: Male; 2: Female)
model_data = model_data %>% mutate(Gender=ifelse(Gender=="Male", 1, 2))

# Total bilirubin stratified by affection status
model_data %>% ggplot(aes(y=Total_bilirubin, x=Affection_status)) + geom_boxplot() + 
  scale_y_sqrt() + ggtitle("Total bilirubin stratified by affection status") + xlab("Affection status") + 
  ylab("Total bilirubin (μmol/L)")

# Direct bilirubin stratified by affection status
model_data %>% ggplot(aes(y=Direct_bilirubin, x=Affection_status)) + geom_boxplot() + 
  scale_y_sqrt() + ggtitle("Direct bilirubin stratified by affection status") + xlab("Affection status") + 
  ylab("Direct bilirubin (μmol/L)")

# Alkaline phosphatase stratified by affection status
model_data %>% ggplot(aes(y=Alkaline_phosphatase, x=Affection_status)) + geom_boxplot() + 
  scale_y_sqrt() + ggtitle("Alkaline phosphatase stratified by affection status") + xlab("Affection status") + 
  ylab("Alkaline phosphatase activity (U/L)")

# Alanine aminotransferase stratified by affection status
model_data %>% ggplot(aes(y=Alanine_aminotransferase, x=Affection_status)) + geom_boxplot() + 
  scale_y_sqrt() + ggtitle("Alanine aminotransferase stratified by affection status") + xlab("Affection status") + 
  ylab("Alanine aminotransferase activity (U/L)")

# Aspartate aminotransferase stratified by affection status
model_data %>% ggplot(aes(y=Aspartate_aminotransferase, x=Affection_status)) + geom_boxplot() + 
  scale_y_sqrt() + ggtitle("Aspartate aminotransferase stratified by affection status") + xlab("Affection status") + 
  ylab("Aspartate aminotransferase activity (U/L)")

# Total protein stratified by affection status
model_data %>% ggplot(aes(y=Total_protein, x=Affection_status)) + geom_boxplot() + 
  ggtitle("Total protein stratified by affection status") + xlab("Affection status") + ylab("Total protein (g/dl)")

# Albumin stratified by affection status
model_data %>% ggplot(aes(y=Albumin, x=Affection_status)) + geom_boxplot() + 
  ggtitle("Albumin stratified by affection status") + xlab("Affection status") + ylab("Albumin (U/L)")

# Albumin globulin ratio stratified by affection status
model_data %>% ggplot(aes(y=Albumin_globulin_ratio, x=Affection_status)) + geom_boxplot() + 
  ggtitle("Albumin globulin ratio stratified by affection status") + xlab("Affection status") + 
  ylab("Albumin globulin ratio")

# Further split the model data into training and test data set (9:1)
# set.seed(1, sample.kind="Rounding")
set.seed(1)
test_index = createDataPartition(y = model_data$Affection_status, times = 1, p = 0.1, list = FALSE)
train_data = model_data[-test_index,]
test_data = model_data[test_index,]

# Again, check if distribution of affection status is still similar
train_data %>% dplyr::select(Affection_status) %>% table() %>% prop.table() # approx. 70:30 split
test_data %>% dplyr::select(Affection_status) %>% table() %>% prop.table() # approx. 70:30 split

# Split data into predictors and outcome
train_data_x = train_data %>% dplyr::select(-Affection_status, -Identifier)
train_data_y = train_data %>% pull(Affection_status)
test_data_x = test_data %>% dplyr::select(-Affection_status, -Identifier)
test_data_y = test_data %>% pull(Affection_status)

# Do a PCA
pca = prcomp(train_data_x)
summary(pca) # First two PCs account for > 90 % of variability; first three for > 99 % # For the first few PCs, the enzymes appear to be most important, in PCs five and six, total and direct bilirubin play an important role
data.frame(pca$x[,1:2], Affection_status=train_data_y) %>% 
  ggplot(aes(PC1,PC2, fill = Affection_status))+
  geom_point(cex=3, pch=21) + ggtitle("First two PCs")

# Create baseline algorithm (i.e. toin coss, expecting 50% accuracy)
guess = function(data){
  n = nrow(data)
  prediction = sample(c("Affected", "Unaffected"), n, replace=TRUE)
  prediction = factor(prediction, levels=c("Affected", "Unaffected"))
  return(prediction)
}
# Performance:
# set.seed(2, sample.kind="Rounding")
set.seed(2)
results = data.frame(Model = "Guessing", Accuracy = mean(guess(test_data_x) == test_data_y))

# Create second baseline algorithm (i.e. predict "affected" for everyone, because the data set is slightly imbalanced)
affected = function(data){
  n = nrow(data)
  prediction = rep("Affected", n)
  prediction = factor(prediction, levels=c("Affected", "Unaffected"))
  return(prediction)
}
# Performance:
results = bind_rows(results, data.frame(Model = "All affected", Accuracy = mean(affected(test_data_x) == test_data_y)))

# Literature --> reference values for liver function tests
ref_values = data.frame(Parameter = c("Total bilirubin", "Direct bilirubin", "Alkaline phosphatase", "Alanine aminotransferase", "Aspartate aminotransferase"),
                        Unit = c("µmol/L", "µmol/L", "U/L", "U/L", "U/L"),
                        Reference_lower = c(2, 0, 41, 7, 0),
                        Reference_upper = c(21, 8, 133, 56, 35))

# Create third algorithm: based on clinical factors (at least one/two/three values outside the reference range)
clinical = function(data=test_data_x, ref=ref_values, threshold=1) {
  data %>% mutate(ToBi = ifelse(Total_bilirubin < ref_values[1,3] | Total_bilirubin > ref_values[1,4], 1, 0)) %>%
    mutate(DiBi = ifelse(Direct_bilirubin < ref_values[2,3] | Direct_bilirubin > ref_values[2,4], 1, 0)) %>%
    mutate(AlPh = ifelse(Alkaline_phosphatase < ref_values[3,3] | Alkaline_phosphatase > ref_values[3,4], 1, 0)) %>%
    mutate(AlAm = ifelse(Alanine_aminotransferase < ref_values[4,3] | Alanine_aminotransferase > ref_values[4,4], 1, 0)) %>%
    mutate(AsAm = ifelse(Aspartate_aminotransferase < ref_values[5,3] | Aspartate_aminotransferase > ref_values[5,4], 1, 0)) %>%
    mutate(All = ToBi + DiBi + AlPh + AlAm + AsAm) %>%
    mutate(prediction = ifelse(All >= threshold, "Affected", "Unaffected")) %>%
    pull(prediction)
}
# Performance:
results = bind_rows(results, data.frame(Model = c("Clinical parameters (1)", "Clinical parameters (2)", "Clinical parameters (3)", "Clinical parameters (4)", "Clinical parameters (5)"), 
                                        Accuracy = c(mean(clinical(data=test_data_x, threshold=1) == test_data_y), mean(clinical(data=test_data_x, threshold=2) == test_data_y), mean(clinical(data=test_data_x, threshold=3) == test_data_y), mean(clinical(data=test_data_x, threshold=4) == test_data_y), mean(clinical(data=test_data_x, threshold=5) == test_data_y))))

# 'True' algorithms
# Set control paramter for cross-validation # Relatively small data set so many CVs do not take too much time
control = trainControl(method="cv", number=50, p=0.9)

# Train k-nearest neighbor model # tune k (number of neighbors) --> set k around sqrt(nrow(train_data_x))
# set.seed(3, sample.kind="Rounding")
set.seed(3)
fit_knn = train(train_data_x, train_data_y, method="knn", tuneGrid=data.frame(k=seq(11, 31, 2)), trControl=control)
predict(fit_knn, test_data_x) %>% table() %>% prop.table()

# Train random forest model # tune mtry (number of variables available at tree split)
# set.seed(5, sample.kind="Rounding")
set.seed(5)
fit_rf = train(train_data_x, train_data_y, method="rf", tuneGrid=data.frame(mtry=seq(1, 10, 1)), trControl=control)
predict(fit_rf, test_data_x) %>% table() %>% prop.table()

# Train generalized linear model # no tuning parameters, hence no cross-validation
# set.seed(8, sample.kind="Rounding")
set.seed(8)
fit_glm = train(train_data_x, train_data_y, method="glm")
predict(fit_glm, test_data_x) %>% table() %>% prop.table()

# Train support vector machine model # tune cost (c; penalty factor) # tune C (penalty factor) and sigma (distance of training spheres used)
# set.seed(13, sample.kind="Rounding")
set.seed(13)
fit_svm = train(train_data_x, train_data_y, method="svmRadialSigma", tuneGrid=data.frame(expand.grid(C=c(2**seq(-5, 1, 2)), sigma=c(2**seq(-5, 1, 2)))), trControl=control)
predict(fit_svm, test_data_x) %>% table() %>% prop.table() # only affected predicted

# Train regularized discriminant analysis (RDA) model
# set.seed(21, sample.kind="Rounding")
set.seed(21)
fit_rda = train(train_data_x, train_data_y, method="rda")
predict(fit_rda, test_data_x) %>% table() %>% prop.table() # only affected predicted

# Performance
results = bind_rows(results, data.frame(Model= c("k-nearest neighbors", "Random forest", "Generalized linear model", "Support vector machine", "Regularized discriminant analysis"),
                    Accuracy = c(mean(predict(fit_knn, test_data_x) == test_data_y), mean(predict(fit_rf, test_data_x) == test_data_y), mean(predict(fit_glm, test_data_x) == test_data_y), mean(predict(fit_svm, test_data_x) == test_data_y), mean(predict(fit_rda, test_data_x) == test_data_y))))

# Dimension reduction
dimensions = 3
x_train_dim = pca$x[, 1:k]
y_train_dim = train_data_y

x_test_dim = as.matrix(sweep(test_data_x, 2, colMeans(test_data_x))) %*% pca$rotation
x_test_dim = x_test_dim[, 1:k]

# set.seed(34, sample.kind="Rounding")
set.seed(34)
fit_knn_dim = train(x_train_dim, y_train_dim, method="knn", tuneGrid=data.frame(k=seq(11, 31, 2)), trControl=control)
predict(fit_knn_dim, x_test_dim) %>% table() %>% prop.table()

# set.seed(55, sample.kind="Rounding")
set.seed(55)
fit_rf_dim = train(x_train_dim, y_train_dim, method="rf", tuneGrid=data.frame(mtry=seq(1, 3, 1)), trControl=control)
predict(fit_rf_dim, x_test_dim) %>% table() %>% prop.table()

# set.seed(89, sample.kind="Rounding")
set.seed(89)
fit_glm_dim = train(x_train_dim, y_train_dim, method="glm") # warning
predict(fit_glm_dim, x_test_dim) %>% table() %>% prop.table() # only affected predicted

# set.seed(144, sample.kind="Rounding")
set.seed(144)
fit_svm_dim = train(x_train_dim, y_train_dim, method="svmRadialSigma", tuneGrid=data.frame(expand.grid(C=c(2**seq(-5, 1, 2)), sigma=c(2**seq(-5, 1, 2)))), trControl=control)
predict(fit_svm_dim, x_test_dim) %>% table() %>% prop.table() # only affected predicted

# set.seed(233, sample.kind="Rounding")
set.seed(233)
fit_rda_dim = train(x_train_dim, y_train_dim, method="rda")
predict(fit_rda_dim, x_test_dim) %>% table() %>% prop.table() # only affected predicted

# Performance
results = bind_rows(results, data.frame(Model = c("k-nearest neighbors (dim. red.)", "Random forest (dim. red.)", "Generalized linear model (dim. red.)", "Support vector machine (dim. red.)", "Regularized discriminant analysis (dim. red.)"),
                                        Accuracy = c(mean(predict(fit_knn_dim, x_test_dim) == test_data_y), mean(predict(fit_rf_dim, x_test_dim) == test_data_y), mean(predict(fit_glm_dim, x_test_dim) == test_data_y), mean(predict(fit_svm_dim, x_test_dim) == test_data_y), mean(predict(fit_rda_dim, x_test_dim) == test_data_y))))


# Ensemble before dimension reduction
ensemble = function(data=test_data_x, threshold=1){
  output = data.frame(knn = rep(NA, nrow(data)))
  output$knn = predict(fit_knn, data)
  output$rf = predict(fit_rf, data)
  output$glm = predict(fit_glm, data)
  output$svm = predict(fit_svm, data)
  output$rda = predict(fit_rda, data)
  output$count = rowCounts(as.matrix(output), value="Unaffected")
  output = output %>% mutate(prediction = ifelse(count>=threshold, "Unaffected", "Affected"))
  output$prediction = factor(output$prediction, levels=c("Affected", "Unaffected"))
  return(output)
}
# Accuracy
confusionMatrix(ensemble()$prediction, test_data_y)

# Performance
results = bind_rows(results, data.frame(Model = c("Ensemble (1)", "Ensemble (2)", "Ensemble (3)", "Ensemble (4)", "Ensemble (5)"), 
                                        Accuracy = c(mean(ensemble(data=test_data_x, threshold=1)$prediction == test_data_y), mean(ensemble(data=test_data_x, threshold=2)$prediction == test_data_y), mean(ensemble(data=test_data_x, threshold=3)$prediction == test_data_y), mean(ensemble(data=test_data_x, threshold=4)$prediction == test_data_y), mean(ensemble(data=test_data_x, threshold=5)$prediction == test_data_y))))

# Ensemble after dimension reduction
ensemble_dim = function(data=x_test_dim, threshold=1){
  output = data.frame(knn = rep(NA, nrow(data)))
  output$knn = predict(fit_knn_dim, data)
  output$rf = predict(fit_rf_dim, data)
  output$glm = predict(fit_glm_dim, data)
  output$svm = predict(fit_svm_dim, data)
  output$rda = predict(fit_rda_dim, data)
  output$count = rowCounts(as.matrix(output), value="Unaffected")
  output = output %>% mutate(prediction = ifelse(count>=threshold, "Unaffected", "Affected"))
  output$prediction = factor(output$prediction, levels=c("Affected", "Unaffected"))
  return(output)
}
# Accuracy
confusionMatrix(ensemble_dim()$prediction, test_data_y)

# Performance
results = bind_rows(results, data.frame(Model = c("Ensemble (dim. red.) (1)", "Ensemble (dim. red.) (2)", "Ensemble (dim. red.) (3)", "Ensemble (dim. red.) (4)", "Ensemble (dim. red.) (5)"), 
                                        Accuracy = c(mean(ensemble_dim(data=x_test_dim, threshold=1)$prediction == test_data_y), mean(ensemble_dim(data=x_test_dim, threshold=2)$prediction == test_data_y), mean(ensemble_dim(data=x_test_dim, threshold=3)$prediction == test_data_y), mean(ensemble_dim(data=x_test_dim, threshold=4)$prediction == test_data_y), mean(ensemble_dim(data=x_test_dim, threshold=5)$prediction == test_data_y))))


# Create training set for complete model data
model_data_x = model_data %>% dplyr::select(-Affection_status, -Identifier)
model_data_y = model_data %>% pull(Affection_status)

# Train final model on the original model data
# set.seed(377, sample.kind="Rounding")
set.seed(377)
fit_glm_final = train(model_data_x, model_data_y, method="glm")

# Obtain performance parameters for the missing data
# Fill in missing values with mean of model data
missing_data$Albumin_globulin_ratio = mean(model_data$Albumin_globulin_ratio)
# Prepare missing data (recoding)
# missing_data = missing_data %>% mutate(Affection_status = ifelse(Affection_status == 1, "Affected", "Unaffected"))
missing_data$Affection_status = as.factor(missing_data$Affection_status)
missing_data = missing_data %>% mutate(Gender=ifelse(Gender=="Male", 1, 2))
missing_data_x = missing_data %>% dplyr::select(-Affection_status)
missing_data_y = missing_data %>% pull(Affection_status)

# Performance:
results = bind_rows(results, data.frame(Model = "GLM on missing data", Accuracy = mean(predict(fit_glm_final, missing_data_x) == missing_data_y)))

# Obtain final performance parameters on the validation data
# Prepare validation data (recoding)
validation_data$Affection_status = as.factor(validation_data$Affection_status)
validation_data = validation_data %>% mutate(Gender=ifelse(Gender=="Male", 1, 2))
validation_data_x = validation_data %>% dplyr::select(-Affection_status, -Identifier)
validation_data_y = validation_data %>% pull(Affection_status)

# Performance
results = bind_rows(results, data.frame(Model = "GLM on validation data", Accuracy = mean(predict(fit_glm_final, validation_data_x) == validation_data_y)))

# Round accuracies
results$Accuracy = round(results$Accuracy, 3)

# Final results A
View(results) # All obtained accuracies

# Final results B
glm_performance = confusionMatrix(predict(fit_glm_final, validation_data_x), validation_data_y)$byClass
glm_performance = as.data.frame(glm_performance)
names(glm_performance) = c("Value")
View(glm_performance) # Further performance parameters of the final model on the validation data
