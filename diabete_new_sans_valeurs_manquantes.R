library(DMwR)  #knn
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
library(bestglm)
library(car)
library(MASS)
diabete
diabete = read.table('/Users/Nadia/Documents/Maths/DATA_SCIENCES/PROJET_SCORING/diabetes.csv', header = TRUE, sep= ',')
variable.type <- lapply(diabete, class)

variable.description <- c("Number of times pregnant", 
                          "Plasma glucose concentration at 2 hours in an oral glucose tolerance test",
                          "Diastolic blood pressure", "Triceps skin fold thickness", "2-hour serum insulin (µU/ml)",
                          "Body Mass Index", "Synthesis of the history of Diabetes Mellitus in relatives, generic 
relationship of those relatives to the subject", "Age of the individual", 
                          "Occurrence of Diabetes")


variable.name <- colnames(diabete)

datadesc <- as_data_frame(cbind(variable.name, variable.type, variable.description))
colnames(datadesc) <- c("Variable Name","Data Type","Variable Description")


table(datadesc)


# Dealing with zeros
missing_data <- diabete[,setdiff(names(diabete), c('Outcome', 'Pregnancies'))]
features_miss_num <- apply(missing_data, 2, function(x) sum(x <= 0))
features_miss <- names(missing_data)[ features_miss_num > 0]

rows_miss <- apply(missing_data, 1, function(x) sum(x <= 0) >= 1) 
sum(rows_miss)

missing_data[missing_data <= 0] <- NA
diabete[, names(missing_data)] <- missing_data

# KNN imputation
orig_data <- diabete
colSums(is.na(diabete))

diabete[,c(-8,-9)] <- knnImputation(diabete[,c(-8,-9)], k = 5)

diabete_new <- diabete
str(diabete_new)

diabete_new$Outcome <- as.factor(diabete_new$Outcome)


#passer à sqrt insuline au lieu de insuline 
#classes d'age

modele_complet = glm(diabete_new$Outcome ~ .  , data = diabete_new, family = binomial(link = "logit"))
summary(modele_complet)
#preg Glucose  BMI   DiabetesPedigreeFunction

Anova(modele_complet, type = 3,test.statistic = "Wald")   
Anova(modele_complet, type = 3, test.statistic = "LR")

bestglm(diabete_new, family = binomial, IC = "AIC")  # Preg Glucose   BMI Diabetespedigreedunction  
step(modele_complet)                             # Preg Glucose  BMI Diabetespedigreedunction  
bestglm(diabete, family = binomial, IC = "BIC") 

db_cor <- round(cor(diabete_new[1:8]),1)
corrplot(db_cor)



list( Column = colSums(diabete_new==0), 
      Row = sum(rowSums(diabete_new==0)) )

diabete_new$Outcome <- factor(diabete_new$Outcome)

# 1. Outcome
ggplot(diabete_new,aes(Outcome,fill = Outcome)) +
  geom_bar() + 
  ggtitle("Distribution of Outcome variable")

p1 <- ggplot(diabete_new, aes(x = Outcome, y = Pregnancies,fill = Outcome)) +
  geom_boxplot() +
  theme(legend.position = "bottom") +
  ggtitle("Number of pregnancies Vs Diabetes")

p2 <- ggplot(diabete_new,aes(x = Pregnancies,fill = factor(Outcome))) + 
  geom_bar(position = "Dodge") + 
  scale_x_continuous(limits = c(0,16)) +
  theme(legend.position = "bottom") +
  labs(title = "Pregnancies Vs Outcome")

gridExtra::grid.arrange(p1, p2, ncol = 2)

boxplot(diabete_new)

#Breaks_age = c(min(Age),24,29,41, max(Age))  #découpés selont les différents quartiles

#diabete_new$Age_classe = cut(Age, breaks = Breaks_age, include.lowest = TRUE)



classes_age = cut(Age,c(min(Age)-1,quantile(Age, 0.25), quantile(Age,0.5), quantile(Age,0.75),max(Age)))
classes_preg = cut(Pregnancies,c(min(Pregnancies),quantile(Pregnancies, 0.25), quantile(Pregnancies,0.5), quantile(Pregnancies,0.75),max(Pregnancies)), include.lowest = TRUE)
modele_classes = glm(Outcome ~ Pregnancies + Glucose + Insulin + SkinThickness + BMI + DiabetesPedigreeFunction + diabete_new$classe_age, data = diabete_new, family=binomial(link="logit"))
modele_classe = glm(Outcome ~ classes_preg + Glucose + Insulin + SkinThickness + BMI + DiabetesPedigreeFunction + Age, data = diabete_new, family=binomial(link="logit"))


summary(modele_classes)

data$Outcome <- as.factor(data$Outcome)

str(diabete)
attach(diabete_new)
as.factor(Outcome)
list(Column = colSums(diabete==0))

diabete_new$classe_age <- classes_age
diabete_new$classe_preg <- classes_preg
N = 1000
mod_ret = rep(0,N)

scores_A = rep(0,10)


for (k in 1:N){
  sample = sample.split(Outcome, SplitRatio = 0.8)
  train = subset(diabete_new, sample == TRUE)
  test = subset(diabete_new, sample == FALSE) 
  
  modele_1 = glm(Outcome ~ . - classe_age - classe_preg , data = train, family = binomial(link = "logit"))   #modèle complet "normal"
  modele_2 = glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction , data = train, family=binomial(link="logit")) #modele avec variables significatives 
  modele_3 = glm(Outcome ~ Age + Glucose + BMI + DiabetesPedigreeFunction , data = train, family=binomial(link="logit"))
  modele_4 = glm(Outcome ~ Age + Insulin + SkinThickness + DiabetesPedigreeFunction , data = train, family=binomial(link="logit"))
  modele_5 = glm(Outcome ~ Pregnancies + Glucose + log(Insulin) + SkinThickness + BMI + DiabetesPedigreeFunction + Age, data = train, family=binomial(link="logit"))  #modele complet avec log(insuline) au lieu de insuline
  modele_6 = glm(Outcome ~ Pregnancies + Glucose + Insulin + SkinThickness + BMI + DiabetesPedigreeFunction + classe_age, data = train, family=binomial(link="logit"))
  modele_7 = glm(Outcome ~ classe_preg + Glucose + Insulin + SkinThickness + BMI + DiabetesPedigreeFunction + Age, data = train, family=binomial(link="logit"))
  
  # modele_3 = glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age, data = train, family = binomial(link = "logit")) #avec les variables significatives
  # modele_4 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction , data = train, family = binomial(link = "logit")) #avec les variables les plus significatives
 # modele_5 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Pregnancies, data = train, family = binomial(link = "logit")) #variables significatives sans Age car correle avec Pregnancies
  #modele_6 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age, data = train, family = binomial(link = "logit")) #variables significatives sans Pregnancies car correle avec Age
 # modele_7 = glm(Outcome ~  . - Age - sqrtInsulin - Pregnancies_classe , data = train, family = binomial(link = "logit"))  #modele complet avec les classes d'âge
#  modele_8 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Age_classe, data = train, family = binomial(link = "logit")) # même que 
  #modele_9 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + Age_classe, data = train, family = binomial(link = "logit")) #les plus significatives + Age classe (étoiles-étoiles)
 # modele_10 = glm(Outcome ~  Glucose + BMI + DiabetesPedigreeFunction + BloodPressure + Pregnancies_classe, data = train, family = binomial(link = "logit"))  #même que modele 8 avec Pregnancies classe au lieu de Age classe
  
  liste_modeles = list(modele_1, modele_2, modele_3, modele_4, modele_5, modele_6, modele_7) #, modele_8, modele_9, modele_10)
  n = length(liste_modeles)
  erreur = 1
  j = 0           #numéro du modèle retenu
  A = matrix(0, nrow = 2, ncol=n)
  A[1,]= 1:n
  
  for (i in 1:n){
    outcome.pred = predict(liste_modeles[[i]], newdata=test, type="response")  #rend un score
    erreur_pred = prop.table(table(outcome.pred>0.3, test$Outcome))[2]   #rend le taux de faux negatifs
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





# reg fin


Reg_fin = glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction , data = diabete_new, family=binomial(link="logit"))
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

library(ROCR)
sample = sample.split(Outcome, SplitRatio = 0.8)
train = subset(diabete_new, sample == TRUE)
test = subset(diabete_new, sample == FALSE) 








Reg_fin = glm(Outcome ~ Pregnancies + Glucose + BMI + DiabetesPedigreeFunction , data = diabete_new, family=binomial(link="logit"))
PredictTrain <- predict(Reg_fin, type = "response")

threshold_0.2 <- table(train$Outcome, PredictTrain > 0.2)
threshold_0.2

# Accuracy
accuracy_0.2 <- round(sum(diag(threshold_0.2))/sum(threshold_0.2),2)
sprintf("Accuracy is %s", accuracy_0.2)

# Mis-classification error rate
MC_0.2 <- 1-accuracy_0.2
sprintf("Mis-classification error is %s",MC_0.2)

sensitivity0.2 <- round(180/(21+180),2)
specificity0.2 <- round(215/(215+160),2)
sprintf("Sensitivity at 0.2 threshold: %s",sensitivity0.2)
sprintf("Specificity at 0.2 threshold: %s",specificity0.2)






ma.conf <- addmargins(table(Outcome.pred, train$Outcome))


summary(PredictTrain)

ROCRpred = prediction(Reg_fin$fitted.values, diabete_new$Outcome)
ROCRperf = performance(ROCRpred, "tnr", "fnr")

# Adding threshold labels
plot(ROCRperf, colorize=TRUE, print.cutoffs.at = seq(0,1,0.1))

auc_train <- round(as.numeric(performance(ROCRpred, "auc")@y.values),2)
legend(.8, .2, auc_train, title = "AUC", cex=1)















