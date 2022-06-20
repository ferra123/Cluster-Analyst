library(readxl)
data <- read_excel("D:/kuliah/PKL/LAPORAN PKL/data.xlsx", 
                   sheet = "Sheet3")
View(data)
summary(data)

library(pastecs)
stat.desc(data)


library(car)
multikol <- cor(data[,2:17])
multikol

jarak <- dist(data[,2:17])
jarak

#metode average
hierarkiave <- hclust(dist(scale(data[,2:17])), method = "ave")
hierarkiave
(dendro <- as.dendrogram(hierarkiave))
print(dendro)
plot(hierarkiave, labels = data$KECAMATAN)
rect.hclust(hierarkiave, 3)
anggotaave <- data.frame(id = data$KECAMATAN, cutree(hierarkiave, k =3))
#hasil kelompok data
anggotaave
cophenetic(hierarkiave) #jarak cophenetic average
#korelasi cophenetic
d1 <- dist(data[, 2:17])
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
corave = cor(d1, d2)
corave

#metode complete
hierarkicomp<-hclust(dist(scale(data[,2:17])), method="complete")
hierarkicomp
plot(hierarkicomp, labels = data$KECAMATAN) #dendogram
rect.hclust(hierarkicomp,3)           #plot mengelompokkan data
anggotacomp<-data.frame(id=data$KECAMATAN, cutree(hierarkicomp,k=3)) #hasil kelompok data
anggotacomp
cophenetic(hierarkicomp) #jarak cophenetic complete
#korelasi cophenetic
d1 <- dist(data[,2:17])
hc <- hclust(d1, "complete")
d2 <- cophenetic(hc)
corcomp=cor(d1, d2)
corcomp

#metode single
hierarkising<-hclust(dist(scale(data[,2:17])), method="single")
hierarkising
plot(hierarkising, labels = data$KECAMATAN) #dendogram
rect.hclust(hierarkising,3)           #plot mengelompokkan data
anggotasing<-data.frame(id=data$KECAMATAN, cutree(hierarkising,k=3)) #hasil kelompok data
anggotasing
cophenetic(hierarkising) #jarak cophenetic single
#korelasi cophenetic
d1 <- dist(data[,2:17])
hc <- hclust(d1, "single")
d2 <- cophenetic(hc)
corsing=cor(d1, d2)
corsing

#metode ward
hierarkiward<-hclust(dist(scale(data[,2:17])), method="ward.D")
hierarkiward
plot(hierarkiward, labels = data$KECAMATAN) #dendogram
rect.hclust(hierarkiward,3) #plot mengelompokkan data
anggotaward<-data.frame(id=data$KECAMATAN, cutree(hierarkiward,k=3)) #hasil kelompok data
anggotaward
cophenetic(hierarkiward) #jarak cophenetic ward
#korelasi cophenetic
d1 <- dist(data[,2:17])
hc <- hclust(d1, "ward.D")
d2 <- cophenetic(hc)
corward=cor(d1, d2)
corward

#metode centroid
hierarkicent<-hclust(dist(scale(data[,2:17])), method="centroid")
hierarkicent
plot(hierarkicent, labels = data$KECAMATAN) #dendogram
rect.hclust(hierarkicent,3) #plot mengelompokkan data
anggotacent<-data.frame(id=data$KECAMATAN,cutree(hierarkicent,k=3)) #hasil kelompok data
anggotacent
cophenetic(hierarkicent) #jarak cophenetic centroid
#korelasi cophenetic
d1 <- dist(data[,2:17])
hc <- hclust(d1, "centroid")
d2 <- cophenetic(hc)
corcent=cor(d1, d2)
corcent

#metode terbaik
metode.terbaik<-data.frame(corave, corcomp, corsing, corward, corcent)

metode.terbaik

