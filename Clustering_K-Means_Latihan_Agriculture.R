#1. Import Library
#2. Import Data
#3. View Data/Value (untuk memahami isi data)
#4. Normalisasi (jika perlu kalo value nya punya range yang jauh antar kolom)
#5. Clustering
#6. Jumlah cluster optimal
#7. Informasi yang rendah


#==============================
#Clustering Menggunakan K-Means
#==============================

# Panggil library yang diperlukan
library(tidyverse)
library(cluster)
library(factoextra)

#Import data
data(agriculture)
df <- agriculture

#Melihat Dimensi Data dim(), berapa kolom dan berapa variable
dim(df)

#Melihat 10 Baris Pertama mnggunakan fungsi head
head(df)

#Melihat isi data
df

#Mengecek apakah ada Data Kosong (N/A) dalam data. 
sum(is.na(df))

#Lakukan Standarisasi dan assign ke variable baru dengan nama "dfnorm". 
#tidak perlu karena skalanya tidak terlalu jauh
#dfnorm <- scale(df)

#Melihat isi data
dfnorm

## Clustering Distance Measures
#Menhitung Ditance Matrix menggunakan fungsi get_dist()
distance <- get_dist(dfnorm, method = "euclidean" )

#Memvisualisasikan Distance Matrix menggunakan fungsi fviz_dist()
fviz_dist(distance)

#Computing k-means clustering in R
#Membuat Untuk kluster dengan masing-masing K =2.3.4.5
k3 <- kmeans(dfnorm, centers = 2, nstart = 25)
k3 <- kmeans(dfnorm, centers = 3, nstart = 25)
k4 <- kmeans(dfnorm, centers = 4, nstart = 25)
k5 <- kmeans(dfnorm, centers = 5, nstart = 25)

# Membuat Komparasi Plot
p1 <- fviz_cluster(k2, geom = "point", data = dfnorm) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point", data = dfnorm) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point", data = dfnorm) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point", data = dfnorm) + ggtitle("k = 5")

#install packages gridExtra
library(gridExtra) 
grid.arrange(p1, p2, p3, p4, nrow = 2)

##Determining Optimal Clusters
#Visualisasi Elbow Method menggunakan fungsi fviz_nbclust()
fviz_nbclust(dfnorm, kmeans, method = "wss")
#>>> cluster elbow 2.3.4.5, maka titik tengah

#Visualisasi Average Silhoutte menggunakan fugsi fviz_nbclust()
fviz_nbclust(dfnorm, kmeans, method = "silhouette")
#>>> cluster paling tinggi adalah 2

#Extracting Results
set.seed(123)
final <- kmeans(dfnorm, 2, nstart = 25)
print(final)

# Visualisasi Final Cluster
fviz_cluster(final, data = dfnorm)

#statistik deskriptif (rata-rata) di tingkat cluster
agriculture %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#Kesimpulan
#y=masyarakat yang kerja di bid agriculture
#x=GNP
#pada tahun tsb terbagi dua. 
#GNP tinggi tapi agriculture rendah
#GNP rendah tapi agriculture tinggi
