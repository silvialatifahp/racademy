library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

hr <- read.csv("~/silvialatifahp/racademy-master/racademy-master/Classification/HR-Employee-Attrition.csv")
dim(hr)
head(hr,10)
sum(is.na(hr))

index_train <- sample(1:nrow(hr), 0.8*nrow(hr))
train <- hr[index_train, ]
test <- hr[-index_train, ]

#decision tree
tree <- rpart(Attrition ~., train, method = "class") # fungsi titik setelah ~ berarti yang mana ingin dijadikan prediktor. jika ia titik maka semua variabel pada file csv menjadi predikto
prp(tree)
fancyRpartPlot(tree)
prediction <- predict(tree, test, type = "class")

conf <- table(test$Attrition, prediction)
conf

TP <- conf [1, 1]
FN <- conf [1, 2]
FP <- conf [2, 1]
TN <- conf [2, 2]

# akurasi #
acc <- (TP+TN)/(TP+FN+FP+TN)
akurasi <- acc
akurasi

# presisisi #
prec <- TP/(TP+FP)
prec

# recall #
rec <- TP/(TP+FN)
rec

#### naive bayes #########
library(naivebayes)

nb <- naive_bayes(Attrition ~ BusinessTravel+DailyRate+Department+DistanceFromHome, data = train)
nb

par(mfrow=c(2,4)) #jumlah row dan kolom untuk visualisasi
plot(nb)
pred_nb <- predict(nb, as.data.frame(test))

confnb <- table(test$Attrition, pred_nb)
confnb

TP1 <- confnb[1, 1]
FN1 <- confnb[1, 2]
FP1 <- confnb[2, 1]
TN1 <- confnb[2, 2]

# akurasi #
accnb <- (TP1+TN1)/(TP1+FN1+FP1+TN1)
akurasinb <- accnb
akurasinb

# presisi #
precnb <- TP1 / (TP1 + FP1)
precnb

#recall
recnb <= TP1 / (TP1 + FN1)
recnb


########## KNN #####
library(class)
library(tidyverse)

hr1 <- hr  %>%
  mutate_if(is.factor, as.numeric)

normalize <- function(x) {
  temp <- (x-min(x))/(max(x)-min(x))
  return(temp)
}

hr2 <- as.data.frame(lappy(hr1[,c(1:7)], normalize))

index_train <- sample(1:nrow(hr2), 0.8*nrow(hr2))
hr2_train <- hr2[index_train, ]
hr2_test <- hr2[-index_train, ]

hr2_train_target <- hr1[index_train, 8]
hr2_test_target <- hr1[-index_train, 8]

hr2model <- knn(train=hr2_train, test = hr2_test, c1=hr2_train_target, k=2)

confhr2 <- table(hr2_test_target, hr2model)
confhr2

TP2 <- confhr2[1, 1]
FN2 <- confhr2[1, 2]
FP2 <- confhr2[2, 1]
TN2 <- confhr2[2, 2]

acchr2 <- (TP2+TN2)/(TP2+FN2+FP2+TN2)
akurasiknn <- acchr2
akurasiknn

prechr2 <- TP2/(TP2/FP2)
prechr2

reck <- TP2/(TP2 + FN2)
reck



akurasi
akurasinb
akurasiknn
