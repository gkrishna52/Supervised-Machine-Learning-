# Supervised Machine Learning Using R.

#Storing the data

x <- data.frame(read.csv("http://bit.ly/w-data"))
x

# using head check the top data

head(x)

# Graphical Analysis

#Fetching two columns from datasets

data <- x[,c('Hours','Scores')]
data

# Giving a name to the chart file. 

png(file = "scatter.png")

# Plotting the chart for student scores with Hours between 1.1 to 9.2 and scores between 21 to 95

plot(x = data$Hours,y = data$Scores, xlab = "Hours Studied", ylab = "Percentage Scores", xlim = c(1.1,9.2), ylim = c(20,95), main = "Hours v/s Scores") 

# Saving the file.  
dev.off()  

#correlation

cor(x$Hours, x$Scores)

#Build Linear Model

linearMod <- lm(Hours ~ Scores, data=x)
linearMod

#linear Diagnostics

summary(linearMod)

#predict linear Model

set.seed(100)
trainingRowindex <- sample(1:nrow(x), 0.8*nrow(x))
trainingData <- x[trainingRowindex, ]  # model training data
testData  <- x[-trainingRowindex, ] 

# Develop the model on the training data and use it to predict the score on test data

lmMod <- lm(Hours ~ Scores, data=trainingData)  # build the model
scorePred <- predict(lmMod, testData)
scorePred

# Review diagnostic measures.
summary (lmMod)

#Calculate prediction accuracy and error rates

actuals_preds <- data.frame(cbind(actuals=testData$Scores, predicteds=scorePred))

correlation_accuracy <- cor(actuals_preds)


head(actuals_preds)

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
min_max_accuracy*100

mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
mape*100


