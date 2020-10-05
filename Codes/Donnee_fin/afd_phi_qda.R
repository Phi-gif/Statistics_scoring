#Données ----

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
N = 1000

scores_A = rep(0,8)

for (k in 1:N){
  sample = sample.split(DIFF, SplitRatio = 0.8)
  train = subset(farms, sample == TRUE)
  test = subset(farms, sample == FALSE) 
  
  modele_1 = qda(DIFF~.,data=train)   #ceux de la r?gression logistique
  modele_2 = qda(DIFF ~ R1 + R3 + R14 + R17 + R36, data = train) 
  modele_3 = qda(DIFF ~ R1 + R3 + R17 + R36, data = train) 
  modele_4 = qda(DIFF ~ R1 + R14 + R17 + R36, data = train)
  modele_5 = qda(DIFF ~ R1 + R12 + R14 + R17 + R32 + R36,data=train)
  modele_6 = qda(DIFF ~ R2 + R7 + R17 + R32,data=train) #ceux du TP
  modele_7 = qda(DIFF ~ R1+R2+R3+R7+R14+R17+R18+R19+R21+R32+R36,data=train) #crit?re de Wilks lambda ? 0.05
  modele_8 = qda(DIFF ~ R1+R2+R3+R14+R17+R21+R32+R36,data=train) # idem mais ? 0.01
  
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

#Le modèle 8 semble presque aussi bon que le modèle 1, je sais pas si ça mérite d'être mentionné...
"C'est cette fois-ci le modèle complet qui l'emporte pour la version quadratique."

#Modèle optimal

afd_fin = qda(DIFF~., data=farms)

pred=predict(afd_fin,farms[,-1])
prob_post=pred$posterior

score_1 = filter(data.frame(prob_post[,2], farms$DIFF), farms$DIFF == "saine")  
score_2 = filter(data.frame(prob_post[,2], farms$DIFF), farms$DIFF == "défaillante")

hgB1 = hist(score_1$prob_post...2., breaks=100, plot=F)
hgB2 = hist(score_2$prob_post...2., breaks=100, plot=F)


col_1 = rgb(1,0,0,0.5)
col_2 = rgb(0,0,1,0.5)

plot(hgB1, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,100))
plot(hgB2, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
# Pas sur qu'on affiche cet histogramme de score, on superposera plutot les courbes roc

ApproxQuantile(hgB2, 0.05)
ApproxQuantile(hgB2, 0.1)

#résultats très bizarres, quasi inexploitable ...

#Comparaison avec le modèle optimal de lda
"Comme les histogrammes de score et les seuils précédents sont inexploitables, nous comparons le modèle
optimal trouvé par lda au même modèle mais dans sa version quadratique "

afd_fin_comp = qda(DIFF~R1 + R12 + R14 + R17 + R32 + R36, data=farms)

pred2=predict(afd_fin_comp,farms[,-1])
prob_post2=pred2$posterior

score_1_comp = filter(data.frame(prob_post2[,2], farms$DIFF), farms$DIFF == "saine")  
score_2_comp = filter(data.frame(prob_post2[,2], farms$DIFF), farms$DIFF == "défaillante")

hgB1_comp = hist(score_1_comp$prob_post2...2., breaks=100, plot=F)
hgB2_comp = hist(score_2_comp$prob_post2...2., breaks=100, plot=F)

col_1 = rgb(1,0,0,0.5)
col_2 = rgb(0,0,1,0.5)

plot(hgB1_comp, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,60))
plot(hgB2_comp, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
"Les histogrammes de score sont déjà plus lisibles que précédemment"

ApproxQuantile(hgB2_comp, 0.05)
ApproxQuantile(hgB2_comp, 0.1)

"Avec la version lda, pour n'avoir que 5% de risque de mal classer une ferme défaillante, on trouvait
un seuil de 0.148, ici ce seuil est de 0.075. Le deux valeurs sont donc très différentes et il est difficilement
envisageable de prendre un seuil aussi bas.
Pour n'avoir que 10% de risque de mal classer une ferme défaillante, on trouvait un seuil de 0.305,
là où ce seuil vaut 0.21 maintenant. Ce seuil est déjà plus envisageable."

#Courbe ROC

pred_afd_fin=prediction(prob_post[,1], DIFF)
courbe=performance(pred_afd_fin,"tnr","fnr")
plot(courbe, colorize=TRUE)

"La courbe semble assez bonne mais moins que pour la version lda."
#reste à voir comment on les superpose...