#-------TP3 Analyse discriminante probabiliste

source("AFD_procedures_chavent.R")
source("LDA_procedures_chavent.R")

#--------Exemple des exploitations agricoles 

load("farms.Rdata")
head(farms)
colnames(farms)

#--------Code R de l'exercice 1

X <- farms[,c(3,8,13,21)] # R2, R7, R17, R32
colnames(X)
y <- farms[,1]

n <-nrow(X) # 1260 exploitations
table(y)
n1 <- 653 #saines
n2 <- 607 # dÃ©faillantes

# question 3

Lk <- linear_func(X,y,type="prob")$Lk
linear_func(X,y,type="geom")$Lk

Lk[1,1]-log(n1/n) # constante de L1 gÃ©om
Lk[1,1]-log(n2/n) # constante de L2 gÃ©om

# question 4
Delta21=Lk[,2]-Lk[,1]

# question 5

x <- X[1,]
delta <- sum(x * Delta21[-1])+Delta21[1] #score de la premiÃ¨re exploitation
# score < 0 donc affectation a G1 (saine).

# question 6

exp(delta)/(1+exp(delta)) #estimation de la proba Ã  posteriori
# proba < 0.5 donc affectation a G1 (saine).

# question 7
y[1] # vraie valeur est saine donc prÃ©diction correcte

# question 8
res <- linear_func(X,y,type="prob")
S <- res$S[,2]-res$S[,1] #score des des 1260 exploitations
yhat <- as.factor(S > 0)
levels(yhat) <- c("saine","dÃ©faillante")

T <- table(yhat,y) #matrice de confusion
sum(y != yhat)/n # taux d'erreur
sum(diag(T))/length(yhat) # taux de bon classement
diag(T)/apply(T,2,sum) # spÃ©cificitÃ© et sensibilitÃ©

#pour augmenter la sensiblitÃ© (taux de dÃ©faillante bien classÃ©s) il faut augmenter le seuil du score. 
#par exemple modifier le seuil et affecter Ã  G2 (dÃ©faillante) si proba > 0.4
#en consequence la spÃ©cificitÃ© diminue


#--------Code R de l'exercice 2

X <- farms[,c(3,4,8,12,13,14,16,21,22)] # R2,R3,R7,R14,R17,R18,R21,R32,R36
colnames(X)
y <- farms[,1]

# question 1
res <- linear_func(X,y,type="prob")
S <- res$S[,2]-res$S[,1] #score des  1260 exploitations

plot(S,rep(0,length(S)),xlab="Axe 1",ylab="",col=as.numeric(y))
legend("topleft",legend = levels(y), text.col = c(1:length(levels(y))),cex=0.8)
cor(X,S)

# question 2
library(MASS)
farms2 <- data.frame(y,X)
m <- lda(y~.,data=farms2)
m$scaling #facteur discriminant
pred <- predict(m)
pred$x #variable discriminante (canonique)
cor(pred$x,S)
plot(pred$x,rep(0,length(S)),xlab="Axe 1",ylab="",col=as.numeric(y))
legend("topleft",legend = levels(y), text.col = c(1:length(levels(y))),cex=0.8)
boxplot(pred$x~y)

# question 3
#les probas sont fonction logistique du score
head(exp(S)/(1+exp(S))) # estimation des proba d'etre dÃ©faillante
head(pred$posterior)

# question 4
#les scores sont les log odd ratio (fonction logit des probas)
head(log(pred$posterior[,2]/pred$posterior[,1]))
head(S)


# question 5
m1 <- lda(y~.,data=farms2,CV=TRUE)
yhat <- m1$class
T <- table(yhat,y) #matrice de confusion
sum(y != yhat)/length(y) # taux d'erreur
sum(diag(T))/length(yhat) # taux de bon classement
diag(T)/apply(T,2,sum) #spÃ©cificitÃ© et sensibilitÃ©

# question 6
m2 <- qda(y~.,data=farms2,CV=TRUE)
# head(m2$posterior)
# head(pred$posterior)

yhat <- m2$class
T <- table(yhat,y) #matrice de confusion
sum(y != yhat)/length(y) # taux d'erreur
sum(diag(T))/length(yhat) # taux de bon classement
diag(T)/apply(T,2,sum) #spÃ©cificitÃ© et sensibilitÃ©

# question 7

m3 <- glm(y~.,data=farms2, family=binomial) #regression logistique

m3$coefficients #coefficients de la fonction de score lineaire (log odd ratio)
S <- as.matrix(X)%*%m3$coefficients[-1] + m3$coefficients[1] #score lineaire ou encore logit
head(S)
head(m3$linear.predictors) #score linÃ©aire direct

prob <- 1/(1 + exp(-S)) #idem exps(S)/(1+exp(S))
head(prob) # score = proba a posteriori P(Y=1/X=x). Ici 1=defaillante
head(m3$fitted.values) #score proba a posteriori direct

yhat <- vector(length=nrow(X))
yhat[which(prob >=0.5)] = "dÃ©faillante"
yhat[which(prob <0.5)] = "saine"
sum(yhat != y)/length(yhat) #taux d'erreur apparent 

# question 8
library(ROCR) # 3 fonctions : prediction, performance, plot

# le score est la proba de dÃ©faillance (dÃ©faillance=1)

fr <- data.frame(score = prob, label = y) 

pred <- prediction(fr$score, fr$label)
?prediction #voir prediction-class pour la liste des sorties (slots)
#objet class S4 donc @ et non pas $ pour acceder aux slots

pred@labels #1=defaillance-positif 0=saine-negatif donc il faut changer l'ordre

pred <- prediction(fr$score, fr$label,label.ordering=c("saine","dÃ©faillante"))
pred@labels

pred@cutoffs #valeurs de seuils entre 0 et 1 car score est une proba
pred@tp   # nombre de vrais positifs pour chaque valeur de seuil

?performance
perf <- performance(pred, "acc")
plot(perf) #taux de bon classement en fonction du seuil

#on recommence avec le score linÃ©aire qui est le log odds (aussi appellÃ© proba on logit scale) 
fr <- data.frame(score = S, label = y) 
pred <- prediction(fr$score, fr$label,label.ordering=c("saine","dÃ©faillante"))
perf <- performance(pred, "acc")
plot(perf)

# question 9

fr <- data.frame(score = prob, label = y) 
pred <- prediction(fr$score, fr$label,label.ordering=c("saine","dÃ©faillante"))
perf <- performance(pred, "tpr", "fpr")
?plot.performance
plot(perf,colorize=TRUE) #courbe ROC avec couleur selon les seuils
auc <- performance(pred, "auc")@y.values[[1]]

# question 10
n <- nrow(X)
tr <- sample(1:n,900)

#train <- X[tr,] #echantillon test
#test <- X[-tr,] #echantillon d'apprentissage

train <- farms2[tr,]  #echantillon d'apprentissage pour glm
test <- farms2[-tr,] #echantillon test pour glm

#construction des 3 modÃ¨les sur l'Ã©chantillon d'apprentissage

m1 <- lda(y~.,data=train)
m2 <- qda(y~.,data=train)
m3 <- glm(y~.,data=train, family=binomial)

#construction des 3 scores sur l'Ã©chantillon test

prob1 <- predict(m1,test)$posterior[,2]
prob2 <- predict(m2,test)$posterior[,2]
prob3 <- predict(m3,test,type="response")

# trois taux d'erreurs sur l'Ã©chantillon test
yhat <- vector(length=nrow(test))
yhat[which(prob1 >=0.5)] = "dÃ©faillante"
yhat[which(prob1 <0.5)] = "saine"
sum(yhat != y[-tr])/length(yhat) #taux d'erreur LDA

yhat[which(prob2 >=0.5)] = "dÃ©faillante"
yhat[which(prob2 <0.5)] = "saine"
sum(yhat != y[-tr])/length(yhat) #taux d'erreur QDA

yhat[which(prob3 >=0.5)] = "dÃ©faillante"
yhat[which(prob3 <0.5)] = "saine"
sum(yhat != y[-tr])/length(yhat) #taux d'erreur logistique

#construction des 3 courbes ROC et AUC associÃ©s
fr1 <- data.frame(score = prob1, label = y[-tr]) 
fr2 <- data.frame(score = prob2, label = y[-tr]) 
fr3 <- data.frame(score = prob3, label = y[-tr]) 

pred1 <- prediction(fr1$score, fr1$label,label.ordering=c("saine","dÃ©faillante"))
pred2 <- prediction(fr2$score, fr2$label,label.ordering=c("saine","dÃ©faillante"))
pred3 <- prediction(fr3$score, fr3$label,label.ordering=c("saine","dÃ©faillante"))

perf1 <- performance(pred1, "tpr", "fpr")
plot(perf1,colorize=TRUE,main="LDA") #courbe ROC avec couleur selon les seuils
performance(pred1, "auc")@y.values[[1]] # auc LDA

perf2 <- performance(pred2, "tpr", "fpr")
plot(perf2,colorize=TRUE,main="QDA") #courbe ROC avec couleur selon les seuils
performance(pred2, "auc")@y.values[[1]] # auc QDA

perf3 <- performance(pred3, "tpr", "fpr")
plot(perf3,colorize=TRUE,main="Logistic regression") #courbe ROC avec couleur selon les seuils
performance(pred3, "auc")@y.values[[1]] # auc regression logistique