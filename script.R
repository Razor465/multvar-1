setwd('C:\\Users\\Razor\\Desktop\\дистанційне навчання\\статистичний аналіз багатовимірних даних\\lab1\\part2')

library(readxl) # підключимо бібліотеки
library(factoextra)
library(CCA)
library(rgl)
library(cluster)
library(fossil)

# зчитаємо і підготуємо дані
data <- read_excel('data.xlsx')

data <- as.data.frame(data)

countries <- as.vector(t(data[,1]))
row.names(data) <- countries
data <- data[,-1]

# центрування і нормування

values <- data.frame(row.names = c('mean','var'))
data_mod <- matrix(nrow = 143, ncol = 7)

for(i in 1:7){
  values[1,i] <- mean(as.numeric(unlist(data[,i])))
  values[2,i] <- sqrt(var(as.numeric(unlist(data[,i])))*18/17)
}

for(i in 1:7){
  for(j in 1:143){
    data_mod[j,i] <- as.numeric((as.numeric(unlist(data[j,i])) - values[1,i])/(values[2,i]))
  }
}

row.names(data_mod) <- row.names(data)
colnames(data_mod) <- colnames(data)
data_mod <- as.data.frame(data_mod)

data <- data_mod

# задамо палітру кольорів
col = c('black', 'red', 'green', 'blue', 'orange', 
        'purple', 'yellow', 'brown', 'burlywood', 
        'deepskyblue', 'darkseagreen', 'deeppink', 
        'salmon', 'turquoise1', 'darkblue', 'darkred',
        'aquamarine', 'grey', 'chocolate', 'magenta')

# побудуємо діаграму внутрішньогрупової суми квадратів
fviz_nbclust(data, kmeans, method = "wss",k.max = 20) 

# діаграма середніх силуетів
fviz_nbclust(data, kmeans, method = "silhouette",k.max = 20)

#
# k = 2 
#

km.res2 <- kmeans(data, 2, nstart = 25)
km.res2$betweenss/km.res2$tot.withinss # відношення міжкластерної суми квадратів до внутрішньокластерної

plot(data[,1], data[,2], col = col[km.res2$cluster]) # діаграма розсіювання 1 і 2 змінної з кластеризацією

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[km.res2$cluster],cex=0.2)

plot3d(princomp(data)$scores[,1], princomp(data)$scores[,2], princomp(data)$scores[,3], col = col[km.res2$cluster])

require(CCA)
cl2 <- km.res2$cluster
k <- length(levels(as.factor(cl2)))
n <- nrow(data)
C <- matrix(data = as.numeric(rep(cl2, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res2 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(cc_res2$scores$xscores[,1:2], col = col[cl2], cex=0.5)

#
# k = 4
#

km.res4 <- kmeans(data, 4, nstart = 25)
km.res4$betweenss/km.res4$tot.withinss # відношення міжкластерної суми квадратів до внутрішньокластерної

plot(data[,1], data[,2], col = col[km.res4$cluster]) # діаграма розсіювання 1 і 2 змінної з кластеризацією

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[km.res3$cluster],cex=0.5)

cl3 <- km.res3$cluster
k <- length(levels(as.factor(cl3)))
C <- matrix(data = as.numeric(rep(cl3, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res3 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(cc_res3$scores$xscores[,1:2], col = col[cl3], cex=0.5)

#
# k = 7
#

km.res7 <- kmeans(data, 7, nstart = 25)
km.res7$betweenss/km.res7$tot.withinss # відношення міжкластерної суми квадратів до внутрішньокластерної

plot(data[,1], data[,2], col = col[km.res7$cluster]) # діаграма розсіювання 1 і 2 змінної з кластеризацією

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[km.res7$cluster],cex=1)

plot3d(princomp(data)$scores[,1], princomp(data)$scores[,2], princomp(data)$scores[,3], col = col[km.res7$cluster])

cl7 <- km.res7$cluster
k <- length(levels(as.factor(cl7)))
C <- matrix(data = as.numeric(rep(cl7, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res7 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(cc_res7$scores$xscores[,1:2], col = col[cl7], cex=1)

#
# k = 9
#

km.res9 <- kmeans(data, 9, nstart = 25)
km.res9$betweenss/km.res9$tot.withinss # відношення міжкластерної суми квадратів до внутрішньокластерної

plot(data[,1], data[,2], col = col[km.res9$cluster]) # діаграма розсіювання 1 і 2 змінної з кластеризацією

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[km.res9$cluster],cex=1)

plot3d(princomp(data)$scores[,1], princomp(data)$scores[,2], princomp(data)$scores[,3], col = col[km.res9$cluster])

cl9 <- km.res9$cluster
k <- length(levels(as.factor(cl9)))
C <- matrix(data = as.numeric(rep(cl9, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res9 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(cc_res9$scores$xscores[,1:2], col = col[cl9], cex=1)

# метод медоїдів

# k = 2

pam.res2 <- pam(data, 2)

plot(data[,1], data[,2], col = col[pam.res2$cluster]) # діаграма розсіювання перших двох змінних з кластеризацією

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[pam.res2$cluster],cex=1)

rand.index(pam.res2$clustering,km.res2$cluster)

pcl2 <- pam.res2$cluster
k <- length(levels(as.factor(pcl2)))
C <- matrix(data = as.numeric(rep(pcl2, k) == rep(1:k, each = n)), ncol = k, nrow = n)
pcc_res2 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(pcc_res2$scores$xscores[,1:2], col = col[pcl2], cex=1)

# k = 4

pam.res4 <- pam(data, 4)

plot(data[,1], data[,2], col = col[pam.res4$cluster]) # діаграма розсіювання перших двох змінних з кластеризацією

rand.index(pam.res4$clustering,km.res4$cluster)

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[pam.res4$cluster],cex=1)

rand.index(pam.res2$clustering,km.res2$cluster)

pcl4 <- pam.res4$cluster
k <- length(levels(as.factor(pcl4)))
C <- matrix(data = as.numeric(rep(pcl4, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res4 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(cc_res4$scores$xscores[,1:2], col = col[pcl4], cex=1)

# k = 9

pam.res9 <- pam(data, 9)

plot(data[,1], data[,2], col = col[pam.res9$cluster]) # діаграма розсіювання перших двох змінних з кластеризацією

# діаграма розсіювання даних у просторі перших двох головних компонент
plot(princomp(data)$scores[,1:2],col=col[pam.res9$cluster],cex=1)

pcl9 <- pam.res9$cluster
k <- length(levels(as.factor(pcl9)))
C <- matrix(data = as.numeric(rep(pcl9, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res9 <- rcc(data,C,0.1,0.1)
# діаграма розсіювання перших двох канонічних компонент
plot(cc_res9$scores$xscores[,1:2], col = col[pcl9], cex=1)


table(pam.res4$clustering)
which(pam.res4$clustering == 1)
which(pam.res4$clustering == 2)
which(pam.res4$clustering == 3)
which(pam.res4$clustering == 4)
