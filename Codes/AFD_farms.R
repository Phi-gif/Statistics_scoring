#Données ----

load("farms.rdata")
head(farms)
colnames(farms) #non des différentes variables explicatives et de la variable à expliquer DIFF

X = farms[,c(3,8,13,21)] # R2, R7, R17, R32 car on ne veut garder que ces ratios dans l'analyse discriminante
y = farms[,1]           #Y est bien un facteur

n = nrow(X) # 1260 exploitations
table(y)
n1 = 653 #saines
n2 = 607 # défaillantes

#Etude des nuages ----

par(mfrow=c(2,2))

for (k in 1:4) {boxplot(X[,k]~y,main=names(X)[k])}
# Les deux variable semblant le mieux discriminer les 2 groupes de fermes sont le ratio R2 et R32

lda(y~.,X)$means
# Les moyennes des 2 modalités sont assez différentes, sauf pour la variable R17,
# elle aura donc surement un faible pouvoir disciminant

for (k in 1:4)  {
  print(names(X)[k])
  print(anova(lm(X[,k]~y))) }
#On retrouve grâce aux valeurs de la statistique F que ce sont les ration R2 et R32 qui ont le 
#plus de pouvoir discriminant

plot(X,col=y)
#Les nuages sont quand même assez confondus

#ACP ----
library(FactoMineR)
library(factoextra)
par(mfrow=c(1,1))
acp=PCA(cbind(X,y),quali.sup=5)
plot.PCA(acp,choix='ind',habillage=5)

#La dimension 1 oppose les fermes ayant de fortes valeurs pour R2 et R32 et de faibles valeurs de R7 à celles ayant l'inverse
#La demension 2 met en opposition les fermes ayant une forte valeur de R17 et une faible valeur de R7 à celles ayant l'inverse

#Analyse discriminante

afd1=lda(y~.,X)
round(afd1$scaling,2)  #donne les coefficients de la foncion linéaire discriminante
anova(lm(as.matrix(X)%*%afd1$scaling~y))
#F* est bien supérieure à toutes les valeurs de F trouvées avec les précédentes anova donc la
#fonction discriminante aide bien à séparer les nuages

library(ade4)
afd2=discrimin(dudi.pca(X,scan=FALSE),y,scan=FALSE) 
anova(lm(as.matrix(afd2$li)~y)) 
plot(afd2)     #beaucoup d'outliers dans R7

afd2$eig #plus la valeur propre est proche de 1, mieux les nuages sont bien séparés
plot(as.matrix(afd2$li),as.matrix(as.matrix(X)%*%afd1$scaling),col=y)
#Zone tendancieuse au milieu

#Prédiction ----

table(predict(afd1,X)$class,y)
#En fonction de ce que l'on cherche à maximiser ou minimiser, on cherchera à réduire le nombre de faux
#négatifs ou de faux positifs

# Analyse article

## 1. ACP

acp_farms=PCA(farms,quali.sup=1,graph=F)
plot(acp,choix='ind',select="cos2 0.6")
plot(acp,choix='var',select="cos2 0.5")
# On retrouve les mêmes ratio que dans l'analyse précédente, ce sont ceux qui sont le mieux représentés
# dans le premier plan factoriel
# On voit d'ailleurs que le premier axe peut servir d'axe de classification à lui tout seul.

acp$quali.sup$coord[c(1,2)]
moy_seuil=(acp$quali.sup$coord[1]+acp$quali.sup$coord[2])/2;moy_seuil


## 2. Analyse discriminante
### 2.1 Choix des variables

library(klaR)
greedy.wilks(DIFF~., data=farms, niveau=0.01)

# L'utilisation du critère du lambda de Wilks nous dit qu'il faut garder 8 ratios pour minimiser la statistique de Wilks
# R1,R32,R14,R17,R2,R3,R36,R21

### 2.2 Analyse
afd=lda(DIFF~R1+R2+R3+R14+R17+R21+R32+R36,data=farms)
round(afd$scaling,2) #coefficients de la fonction linéaire discriminante
table(predict(afd,farms[,-1])$class,farms[,1])

afd_CV=lda(DIFF~R1+R2+R3+R14+R17+R21+R32+R36,data=farms,CV=T)
summary(afd_CV)
#predict(as.matrix(afd_CV),as.vector(farms[,1])) problème ici

afd_qd=qda(DIFF~R1+R2+R3+R14+R17+R21+R32+R36,data=farms)
round(afd_qd$scaling,2) #coefficients de la fonction linéaire discriminante

table(predict(afd_qd,farms[,-1])$class,farms[,1])

### 2.3 Vérification des postulats
Box.test(farms[,1])
#Les matrices de variance covariance des deux groupe semblent non égales --> analyse quadratique