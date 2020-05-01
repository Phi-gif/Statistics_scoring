#Donn?es ----

load("farms.rdata")
head(farms)
colnames(farms) #non des diff?rentes variables explicatives et de la variable ? expliquer DIFF

#librairies
library(ROCR)
library(ggplot2)
library(GGally)
library(forestmodel)
library(effects)
library(ggeffects)
library(boot)
library(corrplot)
library(dplyr)
library(devtools)
library(caTools)
library(MASS)
library(klaR)
library(HistogramTools)

attach(farms)

summary(farms)
str(farms)    # 1: sain, 2:d?faillant

db_cor <- round(cor(farms[2:23]),1)
corrplot(db_cor)   #pas mal de correlation entre les variables (entre certaines, cf carr?s) 

#Etude statistique
#greedy.wilks(DIFF~., data=farms, niveau=0.01)

N = 1000
mod_ret = rep(0,N)

scores_A = rep(0,8)


for (k in 1:N){
  sample = sample.split(DIFF, SplitRatio = 0.8)
  train = subset(farms, sample == TRUE)
  test = subset(farms, sample == FALSE) 
  
  modele_1 = lda(DIFF~.,data=train)   #ceux de la r?gression logistique
  modele_2 = lda(DIFF ~ R1 + R3 + R14 + R17 + R36, data = train) 
  modele_3 = lda(DIFF ~ R1 + R3 + R17 + R36, data = train) 
  modele_4 = lda(DIFF ~ R1 + R14 + R17 + R36, data = train)
  modele_5 = lda(DIFF ~ R1 + R12 + R14 + R17 + R32 + R36,data=train)
  modele_6 = lda(DIFF ~ R2 + R7 + R17 + R32,data=train) #ceux du TP
  modele_7 = lda(DIFF ~ R1+R2+R3+R7+R14+R17+R18+R19+R21+R32+R36,data=train) #crit?re de Wilks lambda ? 0.05
  modele_8 = lda(DIFF ~ R1+R2+R3+R14+R17+R21+R32+R36,data=train) # idem mais ? 0.01
  
  liste_modeles = list(modele_1, modele_2, modele_3, modele_4,modele_5,modele_6,modele_7,modele_8)
  n = length(liste_modeles)
  
  A = matrix(0, nrow = 2, ncol=n)
  A[1,]= 1:n
  
  for (i in 1:n){
    diff.pred = predict(liste_modeles[[i]],test[,-1],method="predictive")$class #rend les classes pr?dites
    erreur_pred = prop.table(table(diff.pred, test$DIFF))[2]   #rend le taux de faux negatifs (0=sain, 1=defaillant)
    A[2,i] = erreur_pred
  }
  A_tri = A[,order(A[2,], decreasing = FALSE)]
  for (i in 1:n){ 
    scores_A[A_tri[1,i]] = scores_A[A_tri[1,i]] + (9-i)}
}

scores_A
#attribuer des scores: matrice deux lignes, une avec les modeles une avec les scores 


"Modèle final"
#C'est le mod?le 5 qui gagne

afd_fin = lda(DIFF ~ R1 + R12 + R14 + R17 + R32 + R36 ,data=farms)

pred=predict(afd_fin,farms[,-1])
prob_post=pred$posterior    #Quelle proba choisir ici --> voir avec Mr Pro?a

#point de vue on minimise les d?faillantes mal class?es (FN)
score_1 = filter(data.frame(prob_post[,2], farms$DIFF), farms$DIFF == "saine")  #vrai score, ceux qui sont 0 (1)
score_2 = filter(data.frame(prob_post[,2], farms$DIFF), farms$DIFF == "défaillante")  #vrai score, ceux qui sont 1 (2)

#score_1bis=filter(data.frame(prob_post[,1], farms$DIFF), farms$DIFF == "saine")
#score_2bis=filter(data.frame(prob_post[,1], farms$DIFF), farms$DIFF == "d?faillante")

hgB1 = hist(score_1$prob_post...2., breaks=100, plot=F) #histogramme des scores des vraies fermes saines
hgB2 = hist(score_2$prob_post...2., breaks=100, plot=F) #histogramme des scores des vraies fermes d?faillantes

#hgB1bis = hist(score_1bis$prob_post...1., breaks=20, plot=F) 
#hgB2bis = hist(score_2bis$prob_post...1., breaks=20, plot=F)

col_1 = rgb(1,0,0,0.5)
col_2 = rgb(0,0,1,0.5)

plot(hgB1, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,20))
plot(hgB2, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
lines(density(score_2$prob_post...2.), lwd=1.5)
lines(density(score_1$prob_post...2.), lwd=1.5)

ApproxQuantile(hgB2, 0.05)
ApproxQuantile(hgB2, 0.1)

#Pour n'avoir que 5% de risque de mal classer une ferme défaillante, il faudrait choisir un seuil de 0.148
#Pour n'avoir que 10% de risque de mal classer une ferme défaillante, il faudrait choisir un seuil de 0.305


#plot(hgB1bis, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,3))
#plot(hgB2bis, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
#lines(density(score_2bis$prob_post...1.), lwd=1.5)
#lines(density(score_1bis$prob_post...1.), lwd=1.5)

"Vérification des hypothèses d'homoscédasticité"
#package pas téléchargeable avec la version 3.6.1 de R

