## Predicting titanic survival using logit regression

traindf <- read.csv("train.csv")
testdf <- read.csv("test.csv")

## removes names, ticket number, NA values, cabin
traindf <- traindf[complete.cases(traindf),-c(4,9,11)]

traindf$Sex <- as.factor(traindf$Sex)
traindf <- traindf[-which(traindf$Embarked == ""),]
traindf$Embarked <- as.factor(traindf$Embarked)
summary(traindf)

## Logit regression
glm_fit <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
               traindf, family = binomial)
summary(glm_fit)

## Parch, Fare Embarked not significant

glm_fit2 <- glm(Survived ~ Pclass + Sex + Age + SibSp,
               traindf, family = binomial)
summary(glm_fit2)

## AIC == 646.18

## consider interaction terms

glm_fit3 <- glm(Survived ~ Pclass + Sex + SibSp + Sex*Age,
                traindf, family = binomial)
summary(glm_fit3)

## prediction

testdf$Sex <- as.factor(testdf$Sex)


sapply(testdf, anyNA)
meanAge <- mean(testdf[!is.na(testdf$Age), 5])
testdf[is.na(testdf$Age), 5] <- meanAge

glm_prob <- predict.glm(glm_fit3, testdf, type = 'response')
glm_class <- rep(0, nrow(testdf))
glm_class[ glm_prob>0.5] <- 1

submission <- data.frame(testdf$PassengerId, glm_class)
names(submission) <- c("PassengerID","Survived")
write.csv(submission, 'submission.csv', row.names = FALSE)
