
#1. Zbiór danych Vehicle z pakietu mlbench zawiera informacje (18 cech, 846 obserwacji)
# o sylwetkach samochodów (4 typy).
# (a) Skonstruuj modele klasyfikacji: LDA, QDA, 1NN & NB dla tych danych.
# (b) Oszacuj b≥πd klasyfikacji dla skonstruowanych metod.
# (c) Który ze skonstruowanych klasyfikatorów jest godny polecenia dla tego zbioru danych?
#  Odpowiedü uzasadnij.
#(d*) Spróbuj zwizualizowaÊ wyniki w przestrzeni utworzonej przez dwie pierwsze sk≥adowe
#g≥ówne.

library(mlbench)
?Vehicle
data(Vehicle)

# czesc tteningowa 70% (846 obserwacji * 0.7)
 training <-Vehicle[1:592,]
 
# czesc testowa 30%
 test <-Vehicle[593:846,]



# a) modele
#LDA - liniowa analiza dyskryminacyjna
model.lda <- lda(Class ~., data=training) # buduje model w oparciu o zbiur traningowy
classification <-predict(model.lda,data=test) # prognozowanie w oparciu o zbior testowy

contingency.table <-table(classification$class,Vehicle$Class)
print(contingency.table)
#     bus opel saab van
#bus  211    7   11   2
#opel   4  133   57   3
#saab   0   66  139   2
#van    3    6   10 192

##   QDA
model.qda <-qda(Class ~.,data=training)
classification.qda <-predict(model.qda, data=test)
contingency.table.qda <-table(test$Class,classification.qda$class)
print(contingency.table.qda)

#     bus opel saab van
#bus  154    0    0   2
#opel   0  130   20   2
#saab   1   19  122   2
#van    1    1    0 138

## #######                1NN  -kwadratowa analiza dyskryminacyjna   ############################
#a) model
Vehicle <-Vehicle[ ,-19]
training<-Vehicle[1:592, ] # w naszym przykladzie zbiory test i training sa takie same, usuwam z nich kolumne 19 gdzie sa dane opisowe
test<-Vehicle[593:846, ]
model.1NN <- knn(training,test, cl=training$Class, k=1)
contingency.table.knn <-table(model.1NN,Vehicle$Class)
print(contingency.table.knn)

# b) szacowane bledy
errorest(Class~.,data=test, model=ipredknn, estimator='boot', predict=function(o,newdata) predict(o,newdata,'class'),
         est.para = control.errorest(nboot=100),k=1)

#szacowanie bled
# LDA
errorest(Class~.,data=test, model=lda, estimator='boot', predict=function(o,newdata) predict(o,newdata)$class,
         est.para = control.errorest(nboot=100))
# szacowany blad 0.2246  -> 22%

#QDA
errorest(Class~.,data=test, model=qda, estimator='boot', predict=function(o,newdata) predict(o,newdata)$class,
         est.para = control.errorest(nboot=100))


