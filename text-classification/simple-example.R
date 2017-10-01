library(RTextTools)
dataDirectory <- "~/data-playground/text-classification/data/"
data <- read.csv(paste(dataDirectory, "sunnyData.csv",sep = ""), header=TRUE)

dtMatrix <- create_matrix(data["Text"])

container <- create_container(dtMatrix, data$IsSunny, trainSize=1:11, virgin=FALSE)

model <- train_model(container, "SVM", kerner="linear", cost=1)

predictionData <- list("sunny sunny sunny rainy rainy", "rainy sunny rainy rainy", "hello", "", "this is another rainy world")

# create a prediction document term matrix
#trace("create_matrix", edit=T)
predMatrix <- create_matrix(predictionData, originalMatrix=dtMatrix)

# create the corresponding container
predSize = length(predictionData);
predictionContainer <- create_container(predMatrix, labels=rep(0,predSize), testSize=1:predSize, virgin=FALSE)

results <- classify_model(predictionContainer, model)
results