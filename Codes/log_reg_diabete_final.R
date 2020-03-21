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

#fichier
diabete = read.table('/Users/Nadia/Documents/Maths/DATA_SCIENCES/PROJET_SCORING/diabetes.csv', header = TRUE, sep= ',')

#diabete = read.table('C:/Users/Philippine/Documents/Cours/Maths/M1/S2/TER_scoring/diabetes.csv', header = TRUE, sep= ',')
attach(diabete)

#Colonnes supp
Breaks_age = c(min(Age),24,29,41, max(Age))  #découpés selont les différents quartiles
Breaks_preg = c(0,1,3,6, max(Pregnancies))

diabete$Age_classe = cut(Age, breaks = Breaks_age, include.lowest = TRUE)
diabete$Pregnancies_classe = cut(Pregnancies, breaks = Breaks_preg, include.lowest = TRUE)
diabete$sqrtInsulin = (sqrt(Insulin)-1)*2
  #boxcox
# diabete$logInsulin = log(Insulin) pose probleme, les valeurs nulles

diabete
summary(diabete)

diabete[,9] <- factor(diabete[,9])
str(diabete)    # 0: pas de diabete, 1: diabete 


db_cor <- round(cor(diabete[1:8]),1)

corrplot(db_cor)   #pas beaucoup de correlation entre les variables 
N = 1000
mod_ret = rep(0,N)

scores_A = rep(0,10)


for (k in 1:N){
  sample = sample.split(Outcome, SplitRatio = 0.8)
  train = subset(diabete, sample == TRUE)
  test = subset(diabete, sample == FALSE) 
  
  modele_1 = glm(Outcome ~ . - Age_classe - sqrtInsulin - Pregnancies_classe , data = train, family = binomial(link = "logit"))   #modèle complet "normal"
  modele_2 = glm(Outcome ~ . -Insulin - Age_classe - Pregnancies_classe , data = train, family=binomial(link="logit")) #modele complet avec sqrt(Insulin) au lieu de Insulin 
  modele_3 = glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age, data = train, family = binomial(link = "logit")) #avec les variables significatives
  modele_4 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction , data = train, family = binomial(link = "logit")) #avec les variables les plus significatives
  modele_5 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Pregnancies, data = train, family = binomial(link = "logit")) #variables significatives sans Age car correle avec Pregnancies
  modele_6 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age, data = train, family = binomial(link = "logit")) #variables significatives sans Pregnancies car correle avec Age
  modele_7 = glm(Outcome ~  . - Age - sqrtInsulin - Pregnancies_classe , data = train, family = binomial(link = "logit"))  #modele complet avec les classes d'âge
  modele_8 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age_classe, data = train, family = binomial(link = "logit")) # même que 
  modele_9 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + Age_classe, data = train, family = binomial(link = "logit")) #les plus significatives + Age classe (étoiles-étoiles)
  modele_10 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Pregnancies_classe, data = train, family = binomial(link = "logit"))  #même que modele 8 avec Pregnancies classe au lieu de Age classe

  liste_modeles = list(modele_1, modele_2, modele_3, modele_4, modele_5, modele_6, modele_7, modele_8, modele_9, modele_10)
  n = length(liste_modeles)
  erreur = 1
  j = 0           #numéro du modèle retenu
  A = matrix(0, nrow = 2, ncol=n)
  A[1,]= 1:n
  
  for (i in 1:n){
    outcome.pred = predict(liste_modeles[[i]], newdata=test, type="response")  #rend un score
    erreur_pred = prop.table(table(outcome.pred>0.3, test$Outcome))[3]   #rend le taux de faux negatifs
    A[2,i] = erreur_pred
#    if (erreur_pred<erreur){  #on cherche à minimiser les faux négatifs 
    #  erreur<-erreur_pred  
    #  j = as.integer(i) #modele i retenu
     # } 
  }
  A_tri = A[,order(A[2,], decreasing = FALSE)]
  for (i in 1:n){ 
    scores_A[A_tri[1,i]] = scores_A[A_tri[1,i]] + (11-i)}
 #mod_ret[k]=j
}
scores_A
#attribuer des scores: matrice deux lignes, une avec les modeles une avec les scores 

res = as.data.frame(prop.table(table(mod_ret))); res
max_occ = which.max(res$Freq)
cat("Le modèle à retenir est le modèle", max_occ)

"Modèle final"

Reg_fin = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age_classe, data = diabete, family = binomial(link = "logit"))
outcome_pred = rep(0,dim(diabete)[1])     #vecteur de 0  

for (i in 1: dim(diabete)[1]){
  if (Reg_fin$fitted.values[i] >=0.3){
    outcome_pred[i] = 1
  }   #ce qu'on a prédit 
}
score_pred = filter(data.frame(Reg_fin$fitted.values, outcome_pred), outcome_pred == "1")  #score predit, ceuw qu'on a predit comme étant malades
score = filter(data.frame(Reg_fin$fitted.values, diabete$Outcome), diabete$Outcome == "1")  #vrai score, ceux qui sont 
score_1 = filter(data.frame(Reg_fin$fitted.values, diabete$Outcome), diabete$Outcome == "1")  #vrai score, ceux qui sont 
score_0 = filter(data.frame(Reg_fin$fitted.values, diabete$Outcome), diabete$Outcome == "0")  #vrai score, ceux qui sont 


hgA = hist(score_pred$Reg_fin.fitted.values, breaks=10, plot=F)   
hgB1 = hist(score_1$Reg_fin.fitted.values, breaks=20, plot=F)
hgB2 = hist(score_0$Reg_fin.fitted.values, breaks=20, plot=F)


col_1 = rgb(1,0,0,0.5)
col_2 = rgb(0,0,1,0.5)

plot(hgB1, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,3))
plot(hgB2, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
lines(density(score_0$Reg_fin.fitted.values), lwd=1.5)
lines(density(score_1$Reg_fin.fitted.values), lwd=1.5)



