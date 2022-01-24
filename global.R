source('dependencies.R')


data=read.table(file = "kaggle.txt" , 
                header=F, sep=" ",
                col.names=c("obs", "ser_delinquency", "RU_unsecuredlines", "age", "nb_3059days", "debt_ratio",
                            "income_month", "nb_creditloan", "nb_90days", "nb_realEloanlines", "nb_6089days",
                            "nb_dependents"))

data=data[,-1]


#traitement des doublons
doublons=which(duplicated(data))
data=data[-doublons,]

#traitement des valeurs manquantes
vmanq=which(is.na(data), arr.ind = T)

#que sur la variable 6 et 11 les valeurs manquantes
d=(vmanq[,2]==6)
length(d)
h=(vmanq[,2]==11)
length(h)

attach(data)


# remplacement des valeurs manquantes par la moyenne
data$income_month[is.na(data$income_month)] = round(mean(data$income_month, na.rm = TRUE))
data$nb_dependents[is.na(data$nb_dependents)] = round(mean(data$nb_dependents, na.rm = TRUE))

tab=head(data)
tab
#print(tab1)
#print(tab2)


# stat desc
statdes=summary(data)




X = as.factor(data$ser_delinquency)
data$ser_delinquency= as.factor(data$ser_delinquency)
# graphiques variable cible
graph1=ggplot(data, aes(x=X)) +
  geom_bar(col = "black", fill="#CC3333") +
  #ggtitle("R?partition de la variable cible") +
  xlab("Défaut") +
  ylab("Effectifs") + theme(panel.background = element_blank())
#evt rare

#r??chantillonnage
data1 <- SmoteClassif(ser_delinquency~. ,data, list("0" = 0.6, "1" = 4) )

graph2=ggplot(data1, aes(x=data1$ser_delinquency)) +
  geom_bar(col = "black", fill="#CC3333") +
  #ggtitle("R?partition de la variable cible") +
  xlab("Défaut") +
  ylab("Effectifs") + theme(panel.background = element_blank())
#r??chantillonage fait
#graph1
#graph2

# graphiques sur les autres variables
p1= ggplot(data1, aes(age)) + 
  geom_histogram(breaks=c(20,25,30,35,40,45,50,55,60,65,70,75,80,85,90), col="white") + 
  xlab("Age") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p2= ggplot(data1, aes(debt_ratio)) + 
  geom_histogram(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), col="white") + 
  xlab("Dette") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p3= ggplot(data1, aes(income_month)) + 
  geom_histogram(breaks=c(0,2000,4000,6000,8000,10000,12000,14000,16000,18000,20000), col="white") + 
  xlab("Revenu") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p4= ggplot(data1, aes(nb_creditloan)) + 
  geom_histogram(breaks=c(0,2,4,6,8,10,12,14,16,18,20,22,24,26,28,30), col='white') + 
  xlab("Nb crédits") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p5= ggplot(data1, aes(nb_dependents)) + 
  geom_histogram(breaks=c(0,1,2,3,4,5), col='white') + 
  xlab("Nb dépendances") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p6= ggplot(data1, aes(nb_realEloanlines)) + 
  geom_histogram(breaks=c(0,1,2,3,4,5), col='white') + 
  xlab("Nb crédits ouverts") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p7= ggplot(data1, aes(RU_unsecuredlines)) + 
  geom_histogram(breaks=c(0,0.2,0.4,0.6,0.8,1), col='white') + 
  xlab("Pourcentage") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p8= ggplot(data1, aes(nb_3059days)) + 
  geom_histogram(breaks=c(0,1,2,3,4,5), col="white") + 
  xlab("30-59 jours") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p9= ggplot(data1, aes(nb_6089days)) + 
  geom_histogram(breaks=c(0,1,2,3,4), col="white") + 
  xlab("60-89 jours") +
  ylab("Effectifs") + theme(panel.background = element_blank())

p10= ggplot(data1, aes(nb_90days)) + 
  geom_histogram(breaks=c(0,1,2,3,4), col="white") + 
  xlab("Plus de 90 jours") +
  ylab("Effectifs") + theme(panel.background = element_blank())


graph3=plot_grid(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10, ncol = 5, nrow = 2)
#graph3


detach(data)
attach(data1)
set.seed(3456)

#construction des ?chantillons apprentissage et test
trainIndex = sort(createDataPartition(ser_delinquency, p = .7,
                                      list = FALSE,
                                      times = 1))


train=data1[trainIndex,]
test=data1[-trainIndex,]



#Lasso, Elastic-Net, Adaptive Lasso, For?ts al?atoires, Boosting
#application des m?thodes 

#pr?paration des donn?es
x <- as.matrix(train[,-1])
y <- as.matrix(train[, 1])
x_test <- as.matrix(test[,-1])

#fr?quence n?cessaire pour modifier les matrices
freq1= sum(data$ser_delinquency==1)/(sum(data$ser_delinquency==1)+sum(data$ser_delinquency==0))
freq0=1-freq1


#rmq: alpha=1 pour lasso et adacptive lasso, alpha=0 pour ridge et alpha varie ]0,1[ pour elasticnet 

##### LASSO #####
# pas la meilleure m?thode car bien quand p>>n 

#on fixe d'abord l'hyperparam?tre 
#no stress le cv est un peu long 
#sinon on peut pr?d?finir des valeurs de lambdas pour faire une s?lection parmi ceux la
#cv.lasso=cv.glmnet(x, y, alpha = 1, family = "binomial")
lambda.bestlasso=0.0005832962 #cv.lasso$lambda.min

lambda.bestlasso

lasso= glmnet(x, y, alpha = 1, lamba=lambda.bestlasso , family = "binomial")

#coefficients
coeff.lasso=coef(lasso, lambda.bestlasso)
coeflasso=as.matrix(coeff.lasso)
a=c("Intercept","RU_unsecuredlines","age","nb_3059days","debt_ratio","income_month","nb_creditloan","nb_90days","nb_realEloanlines","nb_6089days","nb_dependents")
b=data.frame(x=a, y=coeflasso)
colnames(b)=c("Variables","Coefficients")


#matrice de confusion
y.lasso <- predict(lasso, x_test, type="class",s=c(0))
tab.lasso=as.matrix(table(test[,1], y.lasso))

#correction de la matrice
sp.lasso=tab.lasso[1]/(tab.lasso[1]+tab.lasso[3])
se.lasso=tab.lasso[4]/(tab.lasso[2]+tab.lasso[4])

name=c("Réponse 0", "Réponse 1")
lasso1=c(round(freq0*sp.lasso*length(test[,1])),round(freq0*(1-sp.lasso)*length(test[,1])))
lasso2=c(round(freq1*(1-se.lasso)*length(test[,1])),round(freq1*se.lasso*length(test[,1])))
tab2.lasso=data.frame(name,lasso1,lasso2)
colnames(tab2.lasso)=c(".","Prédiction 0", "Prédiction 1")
matlasso=as.matrix(tab2.lasso)


#tentative AUC
pred.lasso = as.numeric(y.lasso)
auc.lasso=auc(roc(test[,1], pred.lasso))

#auc pas top et pas sure des r?sultats

#accuracy
accu.lasso=mean(y.lasso==test[,1])


#train
y.lasso2 <- predict(lasso, x, type="class",s=c(0))
pred.lasso2=as.numeric(y.lasso2)
auc.lasso2=auc(roc(train[,1], pred.lasso2))
accu.lasso2=mean(y.lasso2==train[,1])



###################adaptive-lasso
#on r?alise d'abord un reg Ridge Regression pour cr?er le vecteur poids
cv.ridge = cv.glmnet(x, y, family='binomial', alpha=0, parallel=TRUE, standardize=TRUE)
w3 <- 1/abs(matrix(coef(cv.ridge, s=cv.ridge$lambda.min)[, 1][2:(ncol(x)+1)] ))^1 
#w3

#cv.alasso <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc', penalty.factor=w3)
#lambda.bestalasso=cv.alasso$lambda.min

lambda.bestalasso = 0.3858122

alasso <- glmnet(x, y, family='binomial', lambda=lambda.bestalasso, alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc', penalty.factor=w3)

#coefficients
coeff.alasso=coef(alasso, lambda.bestlasso)
coeff.alasso

#matrice de confusion
y.alasso <- predict(alasso, x_test, type="class",s=c(0))
tab.alasso=as.matrix(table(test[,1], y.alasso))



#correction de la matrice
sp.alasso=tab.alasso[1]/(tab.alasso[1]+tab.alasso[3])
se.alasso=tab.alasso[4]/(tab.alasso[2]+tab.alasso[4])

name=c("r?ponse 0", "r?ponse 1")
alasso1=c(round(freq0*sp.alasso*length(test[,1])),round(freq0*(1-sp.alasso)*length(test[,1])))
alasso2=c(round(freq1*(1-se.alasso)*length(test[,1])),round(freq1*se.alasso*length(test[,1])))
tab2.alasso=data.frame(name,alasso1,alasso2)
colnames(tab2.alasso)=c(".","Pr?diction 0", "Pr?diction 1")

#tentative AUC
pred.alasso = as.numeric(y.alasso)
auc.alasso=auc(roc(test[,1], pred.alasso))
#auc pas top et pas sure des r?sultats

#accuracy
accu.alasso=mean(y.alasso==test[,1])

#train
y.alasso2 <- predict(alasso, x, type="class",s=c(0))
pred.alasso2=as.numeric(y.alasso2)
auc.alasso2=auc(roc(train[,1], pred.alasso2))
accu.alasso2=mean(y.alasso2==train[,1])



###################elastic-net
#total=0
#alpha=0
#lambda=0

#for (i in 2:9) {
#  a=i/10
#  cv.elnet <- cv.glmnet(x, y, family='binomial', alpha=a, parallel=TRUE, 
#                        standardize=TRUE)
#  alpha[i-1]=i/10
#  lambda[i-1]=cv.elnet$lambda.min
#  total[i-1]=min(cv.elnet$cvm)
#}

#lambda.bestelnet=lambda[which.min(total)]
#alpha.bestelnet=alpha[which.min(total)]
lambda.bestelnet=0.0001611876
alpha.bestelnet=0.9

elnet <- glmnet(x, y, family='binomial', lambda=lambda.bestelnet, alpha=alpha.bestelnet, parallel=TRUE, standardize=TRUE, type.measure='auc')

#coefficients
coeff.elnet=coef(elnet, lambda.bestelnet)


#matrice de confusion
y.elnet <- predict(elnet, x_test, type="class",s=c(0))
tab.elnet=as.matrix(table(test[,1], y.elnet))


#correction de la matrice
sp.elnet=tab.elnet[1]/(tab.elnet[1]+tab.elnet[3])
se.elnet=tab.elnet[4]/(tab.elnet[2]+tab.elnet[4])

name=c("r?ponse 0", "r?ponse 1")
elnet1=c(round(freq0*sp.elnet*length(test[,1])),round(freq0*(1-sp.elnet)*length(test[,1])))
elnet2=c(round(freq1*(1-se.elnet)*length(test[,1])),round(freq1*se.elnet*length(test[,1])))
tab2.elnet=data.frame(name,elnet1,elnet2)
colnames(tab2.elnet)=c(".","Pr?diction 0", "Pr?diction 1")
print(as.matrix(tab2.elnet))

#tentative AUC
pred.elnet = as.numeric(y.elnet)
auc.elnet=auc(roc(test[,1], pred.elnet))
#auc pas top et pas sure des r?sultats

#accuracy
accu.elnet=mean(y.elnet==test[,1])

#train
y.elnet2 <- predict(elnet, x, type="class",s=c(0))
pred.elnet2=as.numeric(y.elnet2)
auc.elnet2=auc(roc(train[,1], pred.elnet2))
accu.elnet2=mean(y.elnet2==train[,1])


###################forets al?atoires
Rforest=randomForest(train$ser_delinquency~. , data=train, ntree=50, mtry=round(sqrt(ncol(x))), type=classification)


#matrice de confusion
y.Rforest <- predict(Rforest, x_test, type="class",s=c(0))
tab.Rforest=as.matrix(table(test[,1], y.Rforest))

#correction de la matrice
sp.Rforest=tab.Rforest[1]/(tab.Rforest[1]+tab.Rforest[3])
se.Rforest=tab.Rforest[4]/(tab.Rforest[2]+tab.Rforest[4])

name=c("r?ponse 0", "r?ponse 1")
Rforest1=c(round(freq0*sp.Rforest*length(test[,1])),round(freq0*(1-sp.Rforest)*length(test[,1])))
Rforest2=c(round(freq1*(1-se.Rforest)*length(test[,1])),round(freq1*se.Rforest*length(test[,1])))
tab2.Rforest=data.frame(name,Rforest1,Rforest2)
colnames(tab2.Rforest)=c(".","Pr?diction 0", "Pr?diction 1")


#tentative AUC
pred.Rforest = as.numeric(y.Rforest)
auc.Rforest=auc(roc(test[,1], pred.Rforest))
#auc pas top et pas sure des r?sultats

#accuracy
accu.Rforest=mean(y.Rforest==test[,1])


#train
y.Rforest2 <- predict(Rforest, x, type="class",s=c(0))
pred.Rforest2=as.numeric(y.Rforest2)
auc.Rforest2=auc(roc(train[,1], pred.Rforest2))
accu.Rforest2=mean(y.Rforest2==train[,1])



###################boosting
boosting=boosting(ser_delinquency~., data=train, boos=FALSE, mfinal=10, coeflearn="Breiman")
y.boosting <- predict(boosting, test[,-1])$class

#matrice de confusion
tab.boosting=as.matrix(table(test[,1], y.boosting))

#correction de la matrice
sp.boosting=tab.boosting[1]/(tab.boosting[1]+tab.boosting[3])
se.boosting=tab.boosting[4]/(tab.boosting[2]+tab.boosting[4])

name=c("réponse 0", "réponse 1")
boosting1=c(round(freq0*sp.boosting*length(test[,1])),round(freq0*(1-sp.boosting)*length(test[,1])))
boosting2=c(round(freq1*(1-se.boosting)*length(test[,1])),round(freq1*se.boosting*length(test[,1])))
tab2.boosting=data.frame(name,boosting1,boosting2)
colnames(tab2.boosting)=c(".","Prédiction 0", "Prédiction 1")


#tentative AUC
pred.boosting = as.numeric(y.boosting)
auc.boost=auc(roc(test[,1], pred.boosting))
#auc pas top et pas sure des r?sultats

#accuracy
accu.boost=mean(y.boosting==test[,1])


#train
y.boost2 <- predict(boosting, train[,-1])$class
pred.boost2=as.numeric(y.boost2)
auc.boost2=auc(roc(train[,1], pred.boost2))
accu.boost2=mean(y.boost2==train[,1])


#################comparaison

compare=data.frame(
  Méthodes = c("Lasso",
               "Adapative Lasso",
               "Elastic-Net",
               "Random Forest",
               "Boosting"),
  AUC.test = as.character(c(auc.lasso,
                            auc.alasso,
                            auc.elnet,
                            auc.Rforest,
                            auc.boost
  )),
  AUC.train = as.character(c(auc.lasso2,
                             auc.alasso2,
                             auc.elnet2,
                             auc.Rforest2,
                             auc.boost2
  )),
  ACCURACY.test = as.character(c(accu.lasso,
                                 accu.alasso,
                                 accu.elnet,
                                 accu.Rforest,
                                 accu.boost
  )),
  ACCURACY.train = as.character(c(accu.lasso2,
                                  accu.alasso2,
                                  accu.elnet2,
                                  accu.Rforest2,
                                  accu.boost2
  )),
  stringsAsFactors = FALSE)

accu.boost2
