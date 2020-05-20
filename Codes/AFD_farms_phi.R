#Données ----

load("farms.rdata")
head(farms)     #aperçu du jeu de données, toutes les variables sont quantitatives, sauf DIFF
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
library(MASS)
library(klaR)
library(HistogramTools)

attach(farms)

summary(farms) #653 fermes saines pour 607 fermes défaillantes
str(farms)    # 1: sain, 2:défaillant

"Le résumé du jeu de données montre que toutes les ratios sont des variables quantitatives et que seule
la variable à expliquer DIFF est un facteur à 2 modalités : commentaire ci-dessus"

db_cor <- round(cor(farms[2:23]),1)
corrplot(db_cor)
#ça il me semble que tu l'as déjà comme graphique non ?

"On remarque que beaucoup de ratios sont positivement ou négativement corrélés entre eux, c'est notamment
vrai pour les ratios d'une même 'classe' "

#Etude statistique
#greedy.wilks(DIFF~., data=farms, niveau=0.01) et niveau 0.05 pour trouver des modèles supplémentaires
#de ceux de la régression logistique. Je pense pas qu'il soit nécessaire d'afficher ça dans le rapport.


#Algorithme de séléction du meilleur modèle

N = 1000  #nombre de fois où l'on répète l'algorithme de sélection du meilleur modèle
scores_A = rep(0,8) #vecteur des scores obtenu par chaque modèle lors d'une répétition


for (k in 1:N){
  sample = sample.split(DIFF, SplitRatio = 0.8) #création de l'échantillon test et d'apprentissage (aléatoire)
  train = subset(farms, sample == TRUE)
  test = subset(farms, sample == FALSE) 
  
  modele_1 = lda(DIFF~.,data=train)   #ceux de la régression logistique
  modele_2 = lda(DIFF ~ R1 + R3 + R14 + R17 + R36, data = train) #idem
  modele_3 = lda(DIFF ~ R1 + R3 + R17 + R36, data = train) #idem
  modele_4 = lda(DIFF ~ R1 + R14 + R17 + R36, data = train) #idem
  modele_5 = lda(DIFF ~ R1 + R12 + R14 + R17 + R32 + R36,data=train) #idem
  modele_6 = lda(DIFF ~ R2 + R7 + R17 + R32,data=train) #ceux du TP (meilleures variables de l'ACP dans le plan F1/F2)
  modele_7 = lda(DIFF ~ R1+R2+R3+R7+R14+R17+R18+R19+R21+R32+R36,data=train) #critère de Wilks lambda à 0.05
  modele_8 = lda(DIFF ~ R1+R2+R3+R14+R17+R21+R32+R36,data=train) # idem mais à 0.01
  
  liste_modeles = list(modele_1, modele_2, modele_3, modele_4,modele_5,modele_6,modele_7,modele_8)
  n = length(liste_modeles)
  
  A = matrix(0, nrow = 2, ncol=n) #ça j'avoue que j'avais pas bien compris le principe
  A[1,]= 1:n
  
  for (i in 1:n){
    diff.pred = predict(liste_modeles[[i]],test[,-1],method="predictive")$class #rend les classes prédites par le modèle considéré
    erreur_pred = prop.table(table(diff.pred, test$DIFF))[2]   #rend le taux de faux negatifs (0=sain, 1=defaillant)
    A[2,i] = erreur_pred
  }
  A_tri = A[,order(A[2,], decreasing = FALSE)]
  for (i in 1:n){ 
    scores_A[A_tri[1,i]] = scores_A[A_tri[1,i]] + (9-i)}
}

scores_A
#attribuer des scores: matrice deux lignes, une avec les modeles une avec les scores 
"L'algorithme ci-dessus partitionne le jeu de données en un échantillon d'apprentissage et un échantillon
test à chaque passage dans la boucle.
A chaque répétition, plusieurs modèles sont créés et leurs capacités prédictives sont notés grâce à un 
score calculé en fonction de la capacité du modèle à minimiser le taux de faux négatif.
Tous les scores obtenus sont additionnés pour chaque modèle et le vecteur 'scores_A' contient
donc à la fin de l'algorithme la somme des scores obtenus pour tous les modèles.
On rentient alors celui ayant le score le plus élévé.
Dans notre exemple, nous retenons donc le modèle n°5 comme modèle optimal."


#Modèle final

afd_fin = lda(DIFF ~ R1 + R12 + R14 + R17 + R32 + R36 ,data=farms)

pred=predict(afd_fin,farms[,-1]) #objet ayant 3 paramètres, voir à quoi correspond x
prob_post=pred$posterior    # renvoi un dataframe avec les probas à posteriori d'appartenir à chacun des groupes

#point de vue on minimise les défaillantes mal classées (FN)
score_1 = filter(data.frame(prob_post[,2], farms$DIFF), farms$DIFF == "saine")  #score des vraies fermes saines, celles qui sont 0 (1)
score_2 = filter(data.frame(prob_post[,2], farms$DIFF), farms$DIFF == "défaillante")  #scoredes vraies fermes défaillantes, celles qui sont 1 (2)

"On veut pouvoir afficher les histogrammes de score pour ce modèle pour pouvoir déterminer le seuil à 
choisir donnant un taux d'erreur minimum à celui fixé par l'analyste pour ce genre détude.
Nous construisons donc 2 dataframes contenant les scores des fermes saines et des fermes défaillantes"

#point de vue on minimise les saines mal classées
#score_1bis=filter(data.frame(prob_post[,1], farms$DIFF), farms$DIFF == "saine")
#score_2bis=filter(data.frame(prob_post[,1], farms$DIFF), farms$DIFF == "d?faillante")

hgB1 = hist(score_1$prob_post...2., breaks=100, plot=F) #histogramme des scores des vraies fermes saines
hgB2 = hist(score_2$prob_post...2., breaks=100, plot=F) #histogramme des scores des vraies fermes défaillantes
"Nous créons donc les histogrammes correspondant"

#hgB1bis = hist(score_1bis$prob_post...1., breaks=20, plot=F) 
#hgB2bis = hist(score_2bis$prob_post...1., breaks=20, plot=F)

col_1 = rgb(1,0,0,0.5) #couleur pour les fermes saines
col_2 = rgb(0,0,1,0.5) #couleur pour les fermes défaillantes
"Nous ajoutons de jolies couleurs"

plot(hgB1, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,20)) #affichage des graphiques
plot(hgB2, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
lines(density(score_2$prob_post...2.), lwd=1.5) #affichage des densités
lines(density(score_1$prob_post...2.), lwd=1.5)
"Et nous affichons les histogrammes ainsi que les courbes de densité associées" #pas sûre que les courbes soient utiles au final

ApproxQuantile(hgB2, 0.05) #rend le seuil qu'il faut considérer pour n'avoir que 5% de risque de se tromper lors de l'affectation à un des deux groupes
ApproxQuantile(hgB2, 0.1) # iedm mais pour 10%

"Une fois les histogrammes affichés, on se rend bien compte qu'un seuil de 0.5 comme il est coutume de 
prendre de manière générale donnerai beaucoup d'erreurs de classement. Nous cherchons donc grâce aux
commandes précédente le seuil à choisir qui donne un tuax d'erreur fixé par l'analyste.
Pour n'avoir que 5% de risque de mal classer une ferme défaillante, il faudrait choisir un seuil de 0.148
Pour n'avoir que 10% de risque de mal classer une ferme défaillante, il faudrait choisir un seuil de 0.305"


#plot(hgB1bis, col= col_1, freq=FALSE, xlim=c(0,1), ylim= c(0,3))
#plot(hgB2bis, col=col_2, freq=FALSE, xlim=c(0,1), add=T)
#lines(density(score_2bis$prob_post...1.), lwd=1.5)
#lines(density(score_1bis$prob_post...1.), lwd=1.5)

#Vérification des hypothèses d'homoscédasticité
#package pas téléchargeable avec la version 3.6.1 de R

#Courbe ROC

pred_afd_fin=prediction(prob_post[,1], DIFF)
courbe=performance(pred_afd_fin,"tnr","fnr")
plot(courbe, colorize=TRUE)

