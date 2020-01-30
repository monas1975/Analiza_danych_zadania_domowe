
mtcars
#1. Dla zbioru danych mtcars przeprowadü hierarchicznπ analizÍ skupieÒ (metoda úredniego
#wiπzania) i nie-hierarchicznπ analizÍ skupieÒ (k-úrednich). Narysuj dendorgram.
#Ile skupieÒ wydaje siÍ sensowne? Wyznacz indeks CH oraz wspó≥czynnik zarysu. Ile
#skupieÒ moøesz zaproponowaÊ na ich podstawie?

?mtcars
dim(mtcars) #11 zmiennych, 32 obserwacje

# a) hierrchiczna analiza skupien

#skaluje dane
mtcars2<-scale(mtcars)
#mtcars2 # wyswietlam dane
dist.mtcars2 <- dist(mtcars2, method = "euclidean") # odleglos euklidesowa miesy parami obiektow
dist.mtcars2 
as.matrix(dist.mtcars2) # wyswielam matryce odleglosci \ podobienstw

#majac obliczone odleglosci, laczone sa te obiekty znajdujace sie najblizej siebie.
#funkcja laczenia pobiera informacje o odleglosci, zwroconej przez funkcje dist(),laczy podobne obiekty w klastrya te nastepnie w jeszcze wieksze klastry
#Proces jest powtarzany, do momenty az wszystkie obiekty w zestawie zastana polaczone w drzewo
?hclust
cluster.mtcars2 <- hclust(dist.mtcars2, method = "average") # tworzenie drzewa
plot(cluster.mtcars2) # rysuje dendogram

# Na podstawie wykreslonego dendogramu, wydaje sie ze 3 skupienia wydaja sie sensowne
grp<-cutree(cluster.mtcars2, k = 3) # przypisywanie poszczegolych obiektow do gruop
table(grp)#liczebnosc grup
rect.hclust(cluster.mtcars2, k = 3, border = 2) #kreslenie granicy podzialow


#wydaje sie ze otrzymany podzial jest calkiem sensowy.
#W grupie 1 mamy pojazdy bardziej "stndardowe".
# W grupie 2 mamy pojazdy "z gornej polki"
# do trzeciej grupy zakwalifikowane zostalt 2 pojazdy cechujace sie duza moca



#b) nie hierarchiczna analiza skupien, metoda k-srednich


model.kmeans<-kmeans(mtcars,centers=3,nstart=100)
model.kmeans$cluster # Clustering vector; przypisanie poszczegolnych pojazdow do jeden z 3 grup
model.kmeans$centers # Centers of clusters
model.kmeans$size # liczebnosc poszczegolnych grup

#w stworzonym modelu, do 3szej grupy trafily pojazy z mniejsza iloscia cylindrow i z wieksza moca, ale z wiekszym wspolczynnikiem mpg (mile/galon)
#do grupy 1szej trafily pojazdy z 8 cylidrami, wyzsza moca i z gorszym wspolczynnikiem mpg
# do grupy 2giej trafily pojazy z 6 cylindrami, lub z 8, ale z mniejsza moca nie pojazy z frupy 1


# wizualizacja
library(cluster)
clusplot(mtcars, model.kmeans$cluster, color = TRUE, shade = TRUE, labels = 2, cex = 0.7)



?cascadeKM
library(vegan) # For cascadeKM() command
model.cascade <- cascadeKM(mtcars,2,10) #default value "calinski", 
model.cascade$results # CH indexe
#2 groups    3 groups    4 groups    5 groups    6 groups    7 groups   8 groups   9 groups 10 groups
#SSE      152564.4479 91343.40940 63319.83966 42849.39379 34380.93007 26743.14367 19398.5044 12288.8951 9260.0228
#calinski     92.5818    84.45753    82.55387    91.45128    89.08526    92.95908   106.7515   142.9672  162.1163
calinski.best <- as.numeric(which.max(model.cascade$results[2,]))
#[1] 9 # proponowany podzia - 9 clastrow, co jest chyba raczej troche duzo.
# rownie wysokie wyniki mama 3 grupy

plot(model.cascade)

#wspolczynnik zarysu
library(cluster) # For silhouette() command
sil.index <- silhouette(model.kmeans$cluster, dist = dist(mtcars, method = 'euclidean'))
summary(sil.index) # mean s(i) for each cluster
plot(sil.index) # brak waskich grup ] sylwetek na wykresie - dobre grupowanie
sil.index[,3] # s(i)
mean(sil.index[,3]) # mean s(i)
#[1] 0.5408295
#Na podstawie wspolczynnika zarys nalezaloby podzielic na 3 grupy

#Odpowiedz :  zaproponowalbym podzial na 3 grupy

