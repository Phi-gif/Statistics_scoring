#Données ----

load("farms.rdata")
head(farms)
colnames(farms) #non des différentes variables explicatives et de la variable à expliquer DIFF

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

attach(farms)

summary(farms)
str(farms)    # 1: sain, 2:défaillant

db_cor <- round(cor(farms[2:23]),1)
corrplot(db_cor)   #pas mal de correlation entre les variables (entre certaines, cf carrés) 

#Etude statistique
Reg=glm(DIFF ~.,data=farms,family = binomial(link = "logit"))
summary(Reg)

N = 1000
mod_ret = rep(0,N)

scores_A = rep(0,5)


for (k in 1:N){
  sample = sample.split(DIFF, SplitRatio = 0.8)
  train = subset(farms, sample == TRUE)
  test = subset(farms, sample == FALSE) 
  
  modele_1 = glm(DIFF~.,data=train,family = binomial(link = "logit"))   #modÃ¨le complet "normal"
  modele_2 = glm(DIFF ~ R1 + R3 + R14 + R17 + R36, data = train, family=binomial(link="logit")) #modele complet avec sqrt(Insulin) au lieu de Insulin 
  modele_3 = glm(DIFF ~ R1 + R3 + R17 + R36, data = train, family = binomial(link = "logit")) #avec les variables significatives
  modele_4 = glm(DIFF ~ R1 + R14 + R17 + R36, data = train, family = binomial(link = "logit")) #avec les variables les plus significatives
  modele_5 = glm(DIFF ~ R1 + R12 + R14 + R17 + R32 + R36,data=train,family=binomial(link="logit"))
  
  liste_modeles = list(modele_1, modele_2, modele_3, modele_4,modele_5)
  n = length(liste_modeles)
  
  A = matrix(0, nrow = 2, ncol=n)
  A[1,]= 1:n
  
  for (i in 1:n){
    diff.pred = predict(liste_modeles[[i]], newdata=test, type="response")  #rend un score
    erreur_pred = prop.table(table(diff.pred>0.5, test$DIFF))[2]   #rend le taux de faux negatifs
    A[2,i] = erreur_pred
  }
  A_tri = A[,order(A[2,], decreasing = FALSE)]
  for (i in 1:n){ 
    scores_A[A_tri[1,i]] = scores_A[A_tri[1,i]] + (6-i)}
}

scores_A
#Le modèle 1 semble le meilleur ici


"ModÃ¨le final"

Reg_fin2 =  glm(DIFF ~ R1 + R3 +R8 + R12 + R14+ R17+ R22 + R28+ R30+ R36+R37, data = farms, family = binomial(link = "logit"))
Reg_fin1 =  glm(DIFF ~ ., data = farms, family = binomial(link = "logit"))

score_1 = filter(data.frame(Reg_fin2$fitted.values, farms$DIFF), farms$DIFF == "saine")  #vrai score, ceux qui sont 1
score_2 = filter(data.frame(Reg_fin2$fitted.values, farms$DIFF), farms$DIFF == "défaillante")  #vrai score, ceux qui sont 0


hgB1 = hist(score_1$Reg_fin2.fitted.values, breaks=20, plot=F) #histogramme des scores des vraies fermes saines
hgB2 = hist(score_2$Reg_fin2.fitted.values, breaks=20, plot=F) #histogramme des scores des vraies fermes défaillantes

col_1 = rgb(1,0,0,0.5)
col_2 = rgb(0,0,1,0.5)

plot(hgB1, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,12), main="Histogramme des scores", xlab="score en fonction des classes") #rajouter titre
plot(hgB2, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
lines(density(score_2$Reg_fin2.fitted.values), lwd=1.5)
lines(density(score_1$Reg_fin2.fitted.values), lwd=1.5)
