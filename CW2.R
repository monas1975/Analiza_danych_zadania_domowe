#####################################################################################
##                    ZADANIE 1                                                 #####
#####################################################################################

#1. Uøywjπc zbioru painters (subiektywne oceny malarzy) z pakietu MASS wykonaj analizÍ
#sk≥adowych g≥ównych. Zbadaj ≥adunki trzech pierwszych sk≥adowych g≥ównych. Narysuj
#diagram rozrzutu dla dwóch pierwszych sk≥adowych g≥ównych uøywajπc róønych
#kolorów lub symboli do rozróønienia szkó≥ malarstwa.

library(MASS)
?painters
painters
group<-factor(painters$School)
#usuwam z analizy kolumne dotyczaca szkol
painters<-painters[,-5]
boxplot(painters); var(painters)

#buduje model
# nie ma zbyt duzej roznicy w skali
# dodatkowo w dokumentacj dla painters widze skladowe sa  w skali 0-20 ( z wyjatkiem szkoly, ale ta wczesniej zostala usunieta)
model.pca <- prcomp(painters)

summary(model.pca)
# pierwsze 2 skladowe PC1,PC2 wyjasniaja zmiennosc w 84,5%
# zas pierwsze PC1,PC2,PC3 w 93,6%

#wykres osypiska
screeplot(model.pca, type = 'l', pch = 20)
# z wykresu widac, ze 2 pierwsze skladowe maja najwiiekszy wpływ na zmiennosci 

#Badam zaladunki 3 pierwszych 

model.pca$rotation[,1:3]


plot(model.pca$rotation[,1], model.pca$rotation[,2],model.pca$rotation[,3],
     xlim=c(-1,1), ylim=c(-1,1), pch=20,
     main='Loadings for PC1 vs. PC2')

