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
#wykres pudelkowy  dla poszczegolnych zmiennych
boxplot(painters); var(painters)
text(painters, labels = c(1:100), cex = 0.7) # Scatterplot with data id

#budowanie modelu
# nie ma zbyt duzej roznicy w skali
# dodatkowo w dokumentacj dla painters widze skladowe sa  w skali 0-20 ( z wyjatkiem szkoly, ale ta wczesniej zostala usunieta)
#dlatego nie decyduje sie na skalowanie
model.pca <- prcomp(painters) 

#Wyznaczanie ilosci skladowych glownych potrzebnych do analizy

# wykres ten przedstawia ile wariancji jest wyjasnionych przez kolejne zmienne
plot(model.pca)
#te same wiadomosci jak na wykresie powyzej, ale w postaci tabelki
summary(model.pca)

round(cor(scale(painters), model.pca$x), 2) # Korelacja pomiedzy orginalnymi zmiennymi, a składowymi glownymi
round(cor(scale(painters), model.pca$x)^2, 2) # Korelacja^2: wielkość wariancji , każdej ze zmiennych, które są "wyjasnione"
#przez składowe głown; suma w wierszu rowna 1
# również tu widac, że najwiekszy wpły na zmienność maja PC1 i PC2, zaś najmniejszą, pomijalną PC4

# pierwsze 2 skladowe PC1,PC2 wyjasniaja zmiennosc w 84,5%
# zas pierwsze PC1,PC2,PC3 w 93,6%
#wykres osypiska -gdzie widac, ktore zmienne skladowe maja najwiekszy wplyw na zmiennosc
screeplot(model.pca, type = 'l', pch = 20)
# z wykresu widac, ze 2 pierwsze skladowe maja największy wpływ na zmiennosci



#biplot dla modelu
biplot(model.pca)

#biplot przenosi nam 4 wymiarowy uklad na 2 wymiarowy. Osie układu wspolrzędnych reprezentuja dwie pierwsze głowne składowe, na które
# zrzutowana została przestrzen obserwacji
#strzałki z nawami zmiennych pokazuja w jakim kierunku i jak bardzo wpływaja one na głowne skladowe. Ponadto ich wzajemne 
#połozenie mówi o korelacji miedzy nimi
#  - jeśli wektory są prostopadłe to zmienne nie są skorelowane
#  - mały kąt miedzy wektorami oznacza dużą korelację
#  - kat bliski pólpelnemu oznacza korelacje ujemna
# zaznaczone na wykresie punkty odpowiadaja poszczegłolnym obserwacją
# gdy rosnie zmienna Drawing maleje Colour, zmienne Drawing, Expression, Composition sa z soba skorelowane


 

#Badam ładunki 3 pierwszych 

model.pca$rotation[,1:3]

#zmienna składowa PC1 ma dodatni ładunek dla zmiennych "Composition", "Drawing", Expression", zaś ujemny dla "Colour"
# zmienna składowa PC2 ma ujemny ładunek dla "composition", "Colour", "Expresion" zas dodatni dla "Drawing"
# zmienna składowa PC3 ma dodatni ładunek dla "Drawing", "Colour", "Expression"

#wykres dla ładunków (loadings)

plot(model.pca$rotation[,1:3],   # x and y data
     pch=21,              # ksztalt
     bg="black",          # kolor
     cex=1,               # wielkosc punktu
     main="ladunki "      # tytul
)
text(model.pca$rotation[,1:3],             
     labels=rownames(model.pca$rotation))   # print labels



plot(model.pca$x, pch = 20, col = group, cex=1.5) # Only first two principal components
legend('left', 
       legend = c('A', 'B','C', 'D','E', 'F', 'G', 'H'), 
       pch = 20, 
       col = c('black', 'red','green', 'brown','cyan', 'coral','deeppink', 'blue'), 
       bty = 'y')







#####################################################################################
##                    ZADANIE 1                                                 #####
#####################################################################################

#2. Uøywajπc zmiennych ciπg≥ych oraz porzπdkowych ze zbioru danych Cars93 z pakietu
#MASS wykonaj analizÍ sk≥adowych g≥ównych. Porównaj (na osobnych wykresach):
#  • samochody amerykaÒskie i inne (Origin),
#• typy samochodów (Type).

#https://www.datacamp.com/community/tutorials/pca-analysis-r
