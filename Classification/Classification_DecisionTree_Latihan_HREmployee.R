#========================================
#Classification Menggunakan Decision Tree
#========================================
## Decision Tree

### Import Library

# Import Library
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)


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
#Membangi Data Ke Training dan Testing (70:30)
#tidak ada ketentuan pembagian data training maupun data testing

index_train <- sample(1:nrow(employee), 0.7 * nrow(employee))
train <- employee[index_train, ]
test <- employee[-index_train, ]

### Model Building
tree <- rpart(Attrition ~., train, method = "class")
#Attrition adalah nama kolom, Attrition akan dijadikan target Class

#Memvisualisasikan Model Decision Tree
prp(tree)

#Memvisualisasikan Decison Tree dengan lebih informatif
fancyRpartPlot(tree)

#Menggunakan Untuk Melakukan Prediksi Pada Data Testing
prediction <- predict(tree, test, type = "class")

### Validation

#Validasi Menggunakan "Confussion Matrix"
conf <- table(test$Attrition, prediction)
conf

TP <- conf[1, 1] #TP True Positif Ambil dari Validasi di atas, dari baris 1, kolom 1. Karena ini data asuransi, akan True jika No Claim
FN <- conf[1, 2] #TN True Negative
FP <- conf[2, 1] #FP False Positive
TN <- conf[2, 2] 

#Menghitung Nilai Akurasi
acc <- (TP + TN)/(TP + FN + FP + TN)
accdt <- acc
acc
#0.9427861

#Menghitung Nilai Precision
prec <- TP / (TP + FP)
prec
#0.9444444

#Menghitung Nilai Recall
rec <- TP / (TP + FN)
rec
#0.9161677