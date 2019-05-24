#========================================
#Classification Menggunakan Naive Bayes
#========================================
## Naive Bayes

#Import Library
library(class)
library(tidyverse)

### Import Dataset

#Import Data
employee <- read.csv("HR-Employee-Attrition.csv")
employee

### Data Exploration
#Melihat Kondisi Data
dim(employee)
head(employee,10)

#Melihat Data Kosong
sum(is.na(employee))

### Data Preprocessing
#Mengubah Data Ke Tipe Numerik
employee1 <- employee %>% mutate_if(is.factor, as.numeric)
employee1

#Membuat fungsi Normalisasi
#K-NN sensitif terhadap perbedaan skala, sehingga harus dinormalisasi ke nilai 0-1. Nol (0) untuk tiap kolom dengan nilai minimum, dan kebalikannya.
normalize<-function(x){
  temp<-(x-min(x))/(max(x)-min(x))
  return(temp)
}

#Melakukan Normalisasi
#yang dinormalisasi adalah kolom atribut saja bukan kolom class.

#kemployee_n<-as.data.frame(lapply(employee1[,c(1,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)],normalize))

list <- c(2,22,9,27)
kemployee_n <- data.frame(lapply(employee1[,-list], normalize))
kemployee_n

set.seed(123)

#Membagi ke Data Train dan Data Testing
index_train <- sample(1:nrow(kemployee_n), 0.7 * nrow(kemployee_n))
kemployee_train <- kemployee_n[index_train, ]
kemployee_test <- kemployee_n[-index_train, ]

#Mengambil Label
kemployee_train_target<-employee1[index_train,2]
kemployee_test_target<-employee1[-index_train,2]

### Model Building
#Membuat KNN-Model dengan Nilai K=2
knnmodel <-knn(train=kemployee_train,test=kemployee_test,cl=kemployee_train_target,k=2)

### Validation
#Validasi Menggunakan Confussion Matrix
confknn <- table(kemployee_test_target, knnmodel)
confknn

TPk <- confknn[1, 1] 
FNk <- confknn[1, 2] 
FPk <- confknn[2, 1] 
TNk <- confknn[2, 2]

#Melihat Nilai Akurasi K-NN
acck <- (TPk + TNk)/(TPk + FNk + FPk + TNk)
acck
#0.7777778

#Melihat Nilai Precision K-NN
preck <- TPk / (TPk + FPk)
preck
#0.867374

#Melihat Nilai Recall K-NN
reck <- TPk / (TPk + FNk)
reck
#0.872
