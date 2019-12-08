########################################################################################
##                       ZADANIE 1                                                  ####
#######################################################################################

#1. Uzywajac danych cars narysuj wykres rozrzutu, gdzie pierwsza zmienna to predkosc,
#a druga to droga hamowania. Dopasuj do tego prosta regresji i narysuj ja na wykresie.
#Spróbuj dopasowac i narysowac funkcje kwadratowa.




#cor.test(~ dist + speed, data = cars)

# buduje model liniowy gdzie y (opisywana) to droga hamowania, a x (opisująca) to prędkość.

model.lm <- lm(dist~speed, data = cars)

# obliczam wspólczynniki prostej, na potrzeby tytułu wykresu.
coeff <-coefficients(model.lm)
coeff

# wzór prostej uzyty w tytule wykresu

eq <- paste0("y = ",round(coeff[1],3),"+", round(coeff[2],3), "*x ")

# rysuje wyjres rozrzutu gdzie y - droga hamowania, x -predkosc

plot(dist ~ speed, data = cars, pch = 20, main=eq)

# kreślę prostą regresji, korzystajac z wcześniej stworzonego modelu model.lm

abline(model.lm, lwd = 2, col = 'red')


#kwadratowa <-lm(dist ~ 1 + speed + I(speed^2), data = data.set)
#plot(dist ~ speed, data = cars, pch = 20, main=eq)

########################################################################################
##                       ZADANIE 2                                                  ####
#######################################################################################


#2. Koszt domu zalezy od liczby pokoi. Ponizsze dane przedstawiaja dane dla pewnego
#miasta.
#Cena (w tys.) 300 250 400 550 317 389 425 289 389 559
#Liczba pokoi 3 3 4 5 4 3 6 3 4 5
#Wykonaj diagram korelacyjny oraz znajdz prosta regresji. Czy na poziomie istotnosci
#0,05 liczba pokoi ma istotny wpływ na cene? Jaka bedzie cena mieszkania dwupokojowego
#według oszacowanego modelu?

#wczytanie danych
Input <- ('Pokoje  Cena 
  3    300
  3    250
  4    400
  5    550
  4    317
  3    389
  6    425
  3    289
  4    389
  5    559
')
(data.set <- read.table(textConnection(Input), header = TRUE))


plot(Cena ~ Pokoje,  data = data.set, pch = 20)

#model liniowy
model.lm <- lm(Cena~Pokoje, data = data.set)

#prosta regresji
abline(model.lm, lwd = 2, col = 'red')

#korelacja
cor.test( ~ Pokoje + Cena, data = data.set)

# cor.test = 0.7361131 -> zaleznaść silna dodatnia, wzrost liczby pokoi powoduje wzrost ceny

summary(model.lm)

# H0 - hipoteza H0 zaklada, ze nie ma zwiazku miedzy iloscia pokoi a cena
# H1 - jako hipoteza alternatywna, zaklada zwiazek pomiedzy iloscia pokoi a cena
# p-value = 0.0152
# p-value < 0.05 -> 0.0152 < 0.05 -> odrzucamy hipotezę zerową z prawdopodobieństwem pomyłki mniejszym niż 0,05. 
# Wyniki są istotne statystycznie 
# Czyli na poziomie istotnosci 0,05 liczba pokoi ma wpływ na cene

# obliczam cene mieszkania 2-pokojowego na podstawie stworzonego modelu
new_data <- 3.5

predict(model.lm, data.frame(Pokoje = 2))

# przewidywana cena mieszkania 2 - pokojowego wynosi 240.6

########################################################################################
##                       ZADANIE 3                                                  ####
#######################################################################################


#3. W zbiorze danych emissions (emisja CO2 a poziom PKB (26 panstw)) z pakietu
#UsingR istnieje obserwacja odstajaca. Narysuj diagram korelacyjny dla zmiennych GDP
#oraz CO2. Na jego podstawie wyznacz obserwacje odstajaca. Przyjmujac zmienna CO2 za
#zmienna objasniana znajdz prosta regresji z obserwacja odstajaca jak równiez bez niej.
#Jak zmieniły sie wyniki? 

library(UsingR)
library(car)
?emissions
plot(emissions)



#diagram korelacyjny CO2~GDP (CO2 - y/ objasniana, GDP -x/ objasniajaca)
plot(CO2~GDP, data=emissions, pch = 20)

# buduje model liniowy (uwzgledniajcy wszystkie pomiary, rowniez te odstajace)
model.lm <- lm(CO2~GDP, data=emissions)
# rysuje prosta regresji (ktora uwzglednia rowniez pomiary odstajace)
abline(model.lm,col='red')

# Wyznaczam obserwacje odstajace

#wyznaczenie obserwacji odstajacej za pomoca regresji
regresja<-rstudent(model.lm)
regresja[which.max(abs(regresja))]

#wyznaczenie obserwacji odstajacej z wykorzystaniem biblioteki Car
outlierTest(model.lm)

# z obu testów wynika, ze pomiarem odstajacym jest Rosja (pomiar -7) Russia	692000 (GDP)	2000 (CO2)

#kresle diagram korelacji z pominieciem pomioaru odstajacego (Rosja)
emissions2 <- emissions[-7,]

# buduje model liniowy z danymi bez pomiaru odstajacego
model2.lm <- lm(CO2~GDP, data=emissions2)
#kresle nowa prosta regresji, ktora nie uwzgeldnia pomiaru odstajacego

abline(model2.lm,col='blue')

# porownuje oba modele
cor.test(~ GDP + CO2, data = emissions)
summary(model.lm)
cor.test(~ GDP + CO2, data = emissions2)
summary(model2.lm)

#model 1 ( z obserwacjami odstajacymi) : cor = 0.9501753; p-value = 1.197e-13
#model 2 ( bex obserwacji odstajacych) : cor = 0.9738918; p-value = 2.606e-16

########################################################################################
##                       ZADANIE 4                                                 ####
#######################################################################################

#4. Zbiór danych homeprice z pakietu UsingR zawiera informacje na temat domów sprzedanych
#w New Jersey w roku 2001. Czy liczba toalet (zmienna half) miała wpływ na
#cene (sale)?

library(UsingR)
?homeprice
homeprice

#diagram korelacyjny price~half (sale - y/ objasniana, half -x/ objasniajaca)
plot(sale~half, data=homeprice, pch = 20)


#test korelacji
cor.test(~ half + sale, data = homeprice)

# cor = 0.3941621 i jest 0,2<|r|≤0,4 - korelacja słaba dodatnią

#model
model.lm <-lm(sale~half,homeprice)
abline(model.lm, col="red")
summary(model.lm)

# H0 - hipoteza H0 zaklada, ze nie ma zwiazku miedzy iloscia lazienek a cena
# H1 - jako hipoteza alternatywna, zaklada zwiazek pomiedzy iloscia lazienek a cena
# p-value = 0.03436
# p-value < 0.05 -> 0.03436 < 0.05 -> odrzucamy hipotezę zerową z prawdopodobieństwem pomyłki mniejszym niż 0,05. 
# Wyniki są istotne statystycznie 
# Czyli na poziomie istotnosci 0,05 liczba lazienek ma wpływ na cene

#Wniosek: istnieje zwiazek miedzy ilościa lazienek a cena, przy czym  jest to korelacja slaba.

############################################################################################
##                        ZADANIE 5                                                   #####
###########################################################################################

#5. Klasycznym modelem wzrostu populacji jest model logistyczny postaci:
#        y =a/(1 + e^((b−x)/c)) 
#Dopasuj ten model do danych USPop z pakietu car, zawierajacych informacje na temat
#populacji USA od roku 1790 do 2000. Narysuj dopasowana funkcje regresji.

library(car)
?USPop
?SSlogis
?plot
USPop

plot(USPop,pch=20)
model.nlm<-nls(population ~ SSlogis(year,Asym,xmid,scal),data=USPop)
summary(model.nlm)
lines(USPop$year,fitted(model.nlm),col='blue')
#predict
predict(model.nlm, data.frame(year = 2015)) # 306mln vs 321 according Internet
predict(model.nlm, data.frame(year = 2018)) # 312mln vs 327 according Internet

############################################################################################
##                        ZADANIE 6                                                   #####
###########################################################################################

#6. Skoczek skacze ze spadochronem z balonu napełnionego goracym powietrzem. Ponizej
#znajduja sie jego predkosci [m/s] w kolejnych chwilach czasu [s] (poczynajac od 1s): 10,
##16,3, 23, 27,5, 31, 35,6, 39, 41,5, 42,9, 45, 46, 45,5, 46, 49, 50. Dopasuj do tych danych
#model:
#  v(t) = a*t / (b+t)

#Narysuj wykres rozrzutu wraz z dopasowana krzywa regresji. Jaka predkosc rozwinie w
#17 sekundzie lotu?
 
v<-c(16.3, 23.0, 27.5, 31.0, 35.6, 39.0, 41.5, 42.9, 45.0, 46.0, 45.5, 46.0, 49.0, 50.0)
t<-c(1:14)
lot=data.frame(v,t)

#wykres rozrzutu
plot(v~t,data = lot, pch=20)

#buduje model nieliniowy
# v(t) = a*t / (b+t) --- > SSmicmen
?SSmicmen
model.nlm <- nls(v~SSmicmen(t,Vm,K),data=lot)


lines(lot$t,fitted(model.nlm),col='blue')

predict(model.nlm, data.frame(t = 2)) # w danych bylo 23.0 vs obliczone 22,509
predict(model.nlm, data.frame(t = 6)) # w danych bylo 39.0 vs obliczone 38.84


predict(model.nlm, data.frame(t = 17)) # 50.75

#Odp. Przewidywaa predkosc w 17s to 50,75 [m/s]


















