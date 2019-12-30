#####################################################################################
##                    ZADANIE 1                                                 #####
#####################################################################################
  

#1. Zbiór danych Vehicle z pakietu mlbench zawiera informacje (18 cech, 846 obserwacji)
# o sylwetkach samochodów (4 typy).
# (a) Skonstruuj modele klasyfikacji: LDA, QDA, 1NN & NB dla tych danych.
# (b) Oszacuj b≥πd klasyfikacji dla skonstruowanych metod.
# (c) Który ze skonstruowanych klasyfikatorów jest godny polecenia dla tego zbioru danych?
#  Odpowiedü uzasadnij.
#(d*) Spróbuj zwizualizowaÊ wyniki w przestrzeni utworzonej przez dwie pierwsze sk≥adowe
#g≥ówne.

library(mlbench)
library(ipred)
library(class)
library(klaR) # For NaiveBayes() command
?Vehicle
data(Vehicle)

#        LDA - liniowa analiza dyskryminacyjna   ############################
# a) modele
model.lda <- lda(Class ~., data=Vehicle)
clasyfication.lda <-predict(model.lda,data=Vehicle)
contingency.table <-table(clasyfication.lda$class,Vehicle$Class)
print(contingency.table)

#      bus opel saab van
#bus  211    7   11   2
#opel   4  133   57   3
#saab   0   66  139   2
#van    3    6   10 192

#b) szacowane bledy
errorest(Class~.,data=Vehicle, model=lda, estimator='boot', predict=function(o,newdata) predict(o,newdata)$class,
         est.para = control.errorest(nboot=100))
# szacowany blad 0.2246  -> 22%


## #######                QDA  -kwadratowa analiza dyskryminacyjna   ############################
# a) model
 model.qda <-qda(Class ~.,data=Vehicle)
 classification.qda <-predict(model.qda)
 contingency.table <-table(classification.qda$class,Vehicle$Class)
 print(contingency.table)
 
#       bus opel saab van
# bus  215    0    2   0
# opel   0  175   25   1
# saab   0   31  187   0
# van    3    6    3 198
 
 #b) szacowane bledy
errorest(Class~.,data=Vehicle, model=qda, estimator='boot', predict=function(o,newdata) predict(o,newdata)$class,
         est.para = control.errorest(nboot=100))
# szacowany blad 0.1613  -> 16%

## #######                1NN  -metoda najblizszego sasiada   ############################
#a) model
training<-Vehicle[ ,-19] # w naszym przykladzie zbiory test i training sa takie same, usuwam z nich kolumne 19 gdzie sa dane opisowe
test<-Vehicle[ ,-19]
model.1NN <- knn(training,test, cl=Vehicle$Class, k=1)
contingency.table.knn <-table(model.1NN,Vehicle$Class)
print(contingency.table.knn)

# b) szacowane bledy
errorest(Class ~ .,data=Vehicle, model=ipredknn, estimator='boot', predict=function(o,newdata) predict(o,newdata, 'class'),
         est.para = control.errorest(nboot=100),k=1)
#szacowany blad 0.3632-> 36%



## #######                NB  -Naiwny klasyfikator bayesowski   ############################
# a - model
model.nb.normal <- NaiveBayes(Class ~ .,data = Vehicle)
classification.nb.normal <-table(predict(model.nb.normal,Vehicle)$class, Vehicle$Class)
print(classification.nb.normal)

#      bus opel saab van
#bus   39    1    2  10
#opel  35   93   58   3
#saab  11   55   90   8
#van  133   63   67 178

# b) szacowanie bledu
errorest(Class ~ .,data=Vehicle, model=NaiveBayes, estimator='boot', predict=function(o,newdata) predict(o,newdata)$class,
         est.para = control.errorest(nboot=100))

# szacowany blad 0.5449  ->54%

# c) wniosek, jako ze klasyfikator QDA obarczony jest najnizszym bledem (16%), to wydaje sie ze metoda klasyfikacji QDA jest najlepsza 
#  dla tego typu zbioru danych

# d) wykres
     plot(classification.qda$x,pch=20, col=Vehicle$Class)


