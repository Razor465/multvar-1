library(factoextra)
library(CCA)
library(rgl)
library(cluster)
library(fossil) # ��������� ��������

# ������������ ������ ���������
data <- read.table('C:\\Users\\Razor\\Desktop\\����������� ��������\\������������ ����� ������������� �����\\lab1\\mult6.txt')

# ������ ������ �������
col = c('black', 'red', 'green', 'blue', 'orange', 
        'purple', 'yellow', 'brown', 'burlywood', 
        'deepskyblue', 'darkseagreen', 'deeppink', 
        'salmon', 'turquoise1', 'darkblue', 'darkred',
        'aquamarine', 'grey', 'chocolate', 'magenta')

# ����� ��������
km.res <- kmeans(data, 7, nstart = 25)
km.res$betweenss/km.res$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res$cluster]) # ������� ���������� ������ ���� ������ � ��������������

# ��������������� ���� ��������
fviz_nbclust(data, kmeans, method = "wss",k.max = 20) 

# ������� �������� �������
fviz_nbclust(data, kmeans, method = "silhouette",k.max = 20)

#
# k = 3
#
km.res3 <- kmeans(data, 3, nstart = 25)
km.res3$betweenss/km.res3$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res3$cluster]) # ������� ���������� ������ ���� ������ � ��������������

# ������� ���������� ����� � ������� ������ ���� �������� ���������
plot(princomp(data)$scores[,1:2],col=col[km.res3$cluster],cex=0.2)

plot3d(princomp(data)$scores[,1], princomp(data)$scores[,2], princomp(data)$scores[,3], col = col[km.res3$cluster])

require(CCA)
cl3 <- km.res3$cluster
k <- length(levels(as.factor(cl3)))
n <- nrow(data)
C <- matrix(data = as.numeric(rep(cl3, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res3 <- rcc(data,C,0.1,0.1)
# ������� ���������� ������ ���� ���������� ���������
plot(cc_res3$scores$xscores[,1:2], col = col[cl3], cex=0.2)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(cc_res3$scores$xscores[,1], cc_res3$scores$xscores[,2], cc_res3$scores$xscores[,3], col = col[cl3])

#
# k = 5
#
km.res5 <- kmeans(data, 5, nstart = 25)
km.res5$betweenss/km.res5$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res5$cluster]) # ������� ���������� ������ ���� ������ � ��������������

# ������� ���������� ����� � ������� ������ ���� �������� ���������
plot(princomp(data)$scores[,1:2],col=col[km.res5$cluster],cex=0.2)

cl5 <- km.res5$cluster
k <- length(levels(as.factor(cl5)))
n <- nrow(data)
C <- matrix(data = as.numeric(rep(cl5, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res5 <- rcc(data,C,0.1,0.1)
# ������� ���������� ������ ���� ���������� ���������
plot(cc_res5$scores$xscores[,1:2], col = col[cl5], cex=0.2)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(cc_res5$scores$xscores[,1], cc_res5$scores$xscores[,2], cc_res5$scores$xscores[,3], col = col[cl5])


#
# k = 6
#
km.res6 <- kmeans(data, 6, nstart = 25)
km.res6$betweenss/km.res6$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res6$cluster]) # ������� ���������� ������ ���� ������ � ��������������

# ������� ���������� ����� � ������� ������ ���� �������� ���������
plot(princomp(data)$scores[,1:2],col=col[km.res6$cluster],cex=0.2)

cl6 <- km.res6$cluster
k <- length(levels(as.factor(cl6)))
n <- nrow(data)
C <- matrix(data = as.numeric(rep(cl6, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res6 <- rcc(data,C,0.1,0.1)
# ������� ���������� ������ ���� ���������� ���������
plot(cc_res6$scores$xscores[,1:2], col = col[cl6], cex=0.2)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(cc_res6$scores$xscores[,1], cc_res6$scores$xscores[,2], cc_res6$scores$xscores[,3], col = col[cl6])

#
# k = 7
#
km.res7 <- kmeans(data, 7, nstart = 25)
km.res7$betweenss/km.res7$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res7$cluster]) # ������� ���������� ������ ���� ������ � ��������������

# ������� ���������� ����� � ������� ������ ���� �������� ���������
plot(princomp(data)$scores[,1:2],col=col[km.res7$cluster],cex=0.2)

cl7 <- km.res7$cluster
k <- length(levels(as.factor(cl7)))
n <- nrow(data)
C <- matrix(data = as.numeric(rep(cl7, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res7 <- rcc(data,C,0.1,0.1)
# ������� ���������� ������ ���� ���������� ���������
plot(cc_res7$scores$xscores[,1:2], col = col[cl7], cex=0.2)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(cc_res6$scores$xscores[,1], cc_res6$scores$xscores[,2], cc_res6$scores$xscores[,3], col = col[cl6])


#
# k = 14
#
km.res14 <- kmeans(data, 14, nstart = 25)
km.res14$betweenss/km.res14$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res14$cluster]) # ������� ���������� ������ ���� ������ � ��������������

cl14 <- km.res14$cluster
k <- length(levels(as.factor(cl14)))
C <- matrix(data = as.numeric(rep(cl14, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res14 <- rcc(data,C,0.1,0.1)
# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(cc_res14$scores$xscores[,1], cc_res14$scores$xscores[,2], cc_res14$scores$xscores[,3], col = col[cl14])


#
# k = 18
#
km.res18 <- kmeans(data, 18, nstart = 25)
km.res18$betweenss/km.res18$tot.withinss # ��������� ����������� ���� �������� �� ������������������

plot(data[,1], data[,2], col = col[km.res18$cluster]) # ������� ���������� ������ ���� ������ � ��������������

cl18 <- km.res18$cluster
k <- length(levels(as.factor(cl18)))
C <- matrix(data = as.numeric(rep(cl18, k) == rep(1:k, each = n)), ncol = k, nrow = n)
cc_res18 <- rcc(data,C,0.1,0.1)
# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(cc_res18$scores$xscores[,1], cc_res18$scores$xscores[,2], cc_res18$scores$xscores[,3], col = col[cl18])

#
# ����� ������
#

# k = 3

pam.res3 <- pam(data, 3)

plot(data[,1], data[,2], col = col[pam.res3$cluster]) # ������� ���������� ������ ���� ������ � ��������������

# ������� ���������� ����� � ������� ������ ���� �������� ���������
plot(princomp(data)$scores[,1:2],col=col[pam.res3$cluster],cex=0.2)

pcl3 <- pam.res3$cluster
k <- length(levels(as.factor(pcl3)))
n <- nrow(data)
C <- matrix(data = as.numeric(rep(pcl3, k) == rep(1:k, each = n)), ncol = k, nrow = n)
pcc_res3 <- rcc(data,C,0.1,0.1)
# ������� ���������� ������ ���� ���������� ���������
plot(cc_res5$scores$xscores[,1:2], col = col[cl5], cex=0.2)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(pcc_res3$scores$xscores[,1], pcc_res3$scores$xscores[,2], pcc_res3$scores$xscores[,3], col = col[pcl3])

# k = 5

pam.res5 <- pam(data, 5)

plot(data[,1], data[,2], col = col[pam.res5$cluster]) # ������� ���������� ������ ���� ������ � ��������������

pcl5 <- pam.res5$cluster
k <- length(levels(as.factor(pcl5)))
C <- matrix(data = as.numeric(rep(pcl5, k) == rep(1:k, each = n)), ncol = k, nrow = n)
pcc_res5 <- rcc(data,C,0.1,0.1)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(pcc_res5$scores$xscores[,1], pcc_res5$scores$xscores[,2], pcc_res5$scores$xscores[,3], col = col[pcl5])

# k = 7

pam.res7 <- pam(data, 7)

plot(data[,1], data[,2], col = col[pam.res7$cluster]) # ������� ���������� ������ ���� ������ � ��������������

pcl7 <- pam.res7$cluster
k <- length(levels(as.factor(pcl7)))
C <- matrix(data = as.numeric(rep(pcl7, k) == rep(1:k, each = n)), ncol = k, nrow = n)
pcc_res7 <- rcc(data,C,0.1,0.1)

# ��������� ������� ���������� ������ ����� ���������� ���������
plot3d(pcc_res7$scores$xscores[,1], pcc_res7$scores$xscores[,2], pcc_res7$scores$xscores[,3], col = col[pcl7])

rand.index(km.res3$cluster, pam.res3$clustering)
rand.index(km.res5$cluster, pam.res5$clustering)

library(MASS)
table(pam.res5$clustering,km.res5$cluster)
