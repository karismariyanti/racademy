#========================================
#Classification Menggunakan Naive Bayes
#========================================
## Naive Bayes

#Import Library
library(naivebayes)

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
#Membuat model prediksi Naive Bayes
nb <- naive_bayes(Attrition ~ ., data = train)

#Melihat model yang telah dibuat 
nb

#Visualisasi Model
par(mfrow=c(2,4))
plot(nb)

#Melakukan prediksi dengan data testing
pred_nb <- predict(nb, as.data.frame(test))

### Validation
#Membuat Confussion Matrix Naive Bayes
confnb <- table(test$Claim, pred_nb)
confnb

TPn <- confnb[1, 1] 
FNn <- confnb[1, 2] 
FPn <- confnb[2, 1] 
TNn <- confnb[2, 2] 

#Menghitung Nilai Akurasi
accnb <- (TPn + TNn)/(TPn + FNn + FPn + TNn)
accnb
#0.7736318

#Menghitung Nilai Precision
precnb <- TPn / (TPn + FPn)
precnb
#0.69

#Menghitung Nilai Recall
recnb <- TPn / (TPn + FNn)
recnb
#0.8263473