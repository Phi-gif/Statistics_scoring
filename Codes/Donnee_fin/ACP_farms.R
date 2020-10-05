library(FactoMineR)
library(factoextra)
library("corrplot")

library(Factoshiny)



farms2
attach(farms2)

par(mfrow = c(1, 3))
boxplot(R11 ~ DIFF, data = farms2, xlab = "r11") 
boxplot(R12 ~ DIFF, data = farms2, xlab = "r12") 
boxplot(R14 ~ DIFF, data = farms2, xlab = "r14")

boxplot(DIFF, data = farms2)



acp = PCA(farms2, quali.sup = c(1,2,3,5,6), quanti.sup = c(4,7,8))
acp$eig
fviz_eig(acp,addlabels = TRUE)


acp$var$contrib
corrplot(acp$var$contrib,is.corr= FALSE)
fviz_pca_ind(acp,habillage = 2, addEllipses = TRUE, ellipse.type = "confidence",palette = "jco")
fviz_pca_ind(acp,habillage = 2, addEllipses = TRUE, axes = c(1,2))
abline(a = 0.000005, b = -1.07578, col = 2)

fviz_pca_var(acp,col.var="contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
fviz_pca_biplot(acp,habillage = 2, addEllipses = TRUE, axes = c(1,2),palette = "jco")


plot(c(-10,15), c(-10,7.5), type = "n", xlab = "x", ylab = "y", asp = 1)
abline(h = 0, v = 0, col = "gray60")
text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))

regline = lm(DIFF ~ . , data = farms2)
abline(a = 0.000005, b = -1.07578, col = 2)
plot(regline)

get_pca_ind(acp)[1]
get_pca_var(acp)[1]


fviz_contrib(acp, choice = "var", axes = c(1,2))
acp_dim1 = PCA(farms2, quali.sup = c(1,2,3,5,6), quanti.sup = c(4,7,8), ncp = 1)
coordonnees = acp_dim1[["ind"]][["coord"]]
acp_dim1$quali.sup$coord
#-2.16474485 + 2,32879

acp_dim1[["ind"]][["coord"]]   #pour coordonnées des points 
acp_dim1[["call"]][["quali.sup"]][["quali.sup"]][["DIFF"]]  #vrais résulats DIFF
DIFF_prevu_dim1 = rep(0,n)
for (i in 1:n){
  x = acp_dim1[["ind"]][["coord"]][i]
  point_pivot = 0.08202495
  if (x>point_pivot){  #si on est en dessous de la droite 
    DIFF_prevu_dim1[i] <- 2
  }
  if (x<point_pivot){  #on est au dessus de la droite 
    DIFF_prevu_dim1[i] <- 1
  }
}

obs_dim1 = DIFF_prevu_dim1
pred = acp_dim1[["call"]][["quali.sup"]][["quali.sup"]][["DIFF"]]

A_dim1 = matrix(0, nrow = 2, ncol=4)
A_dim1[1,] = 1:4

for (i in 1:n){
  if (obs_dim1[i] == 1 & pred[i] == 1){          # vrai: sain          observé: sain
    A_dim1[2,1] <- (A_dim1[2,1] + 1)
  } else if (obs_dim1[i] == 2 & pred[i] == 2){   # vrai: défaillant    observé: défaillant
    A_dim1[2,2] <- (A_dim1[2,2] + 1)
  } else if (obs_dim1[i] == 1 & pred[i] == 2){   # vrai: défaillant    observé: sain 
    A_dim1[2,3] <- (A_dim1[2,3] + 1)                   
  } else { A_dim1[2,4] <- (A_dim1[2,4] +1)}           # vrai: sain          observé: défaillant   
}

A_dim1

"""
     [,1] [,2] [,3] [,4]
[1,]    1    2    3    4
[2,]  599  483  124   54

fermes saines bien classées:        599/653:   91.73047  %
fermes défaillantes bien classées:  489/607:   79.57166 %

"""
summary(acp_dim1)
#PCAshiny(farms2)
n = length(farms2$DIFF)
acp_dim2 = PCA(farms2, quali.sup = c(1,2,3,5,6), quanti.sup = c(4,7,8), ncp = 2)
DIFF_prevu_dim2 = rep(0,n)

acp_dim2[["ind"]][["coord"]]
for (i in 1:n){
  x = acp_dim2[["ind"]][["coord"]][,1][i]
  y = acp_dim2[["ind"]][["coord"]][,2][i]
  point = -1.07578*x + 0.000005 - y
  if (point>0){  #si on est en dessous de la droite 
    DIFF_prevu_dim2[i] <- 1
  }
  if (point<0){  #on est au dessus de la droite 
    DIFF_prevu_dim2[i] <- 2
  }
}

obs_dim2 = DIFF_prevu_dim2
pred = acp_dim1[["call"]][["quali.sup"]][["quali.sup"]][["DIFF"]]

A_dim2 = matrix(0, nrow = 2, ncol=4)
A_dim2[1,] = 1:4

for (i in 1:n){
  if (obs_dim2[i] == 1 & pred[i] == 1){          # vrai: sain          observé: sain
    A_dim2[2,1] <- (A_dim2[2,1] + 1)
  } else if (obs_dim2[i] == 2 & pred[i] == 2){   # vrai: défaillant    observé: défaillant
    A_dim2[2,2] <- (A_dim2[2,2] + 1)
  } else if (obs_dim2[i] == 1 & pred[i] == 2){   # vrai: défaillant    observé: sain 
    A_dim2[2,3] <- (A_dim2[2,3] + 1)                   
  } else { A_dim2[2,4] <- (A_dim2[2,4] +1)}           # vrai: sain          observé: défaillant   
}


"""
[,1] [,2] [,3] [,4]
[1,]    1    2    3    4
[2,]  595  482  125   58

fermes saines bien classées:        595/653:   91.11792  %
fermes défaillantes bien classées:  482/607:   79.40692 %

"""
acp_dim3$quali.sup$coord
acp_dim3 = PCA(farms2, quali.sup = c(1,2,3,5,6), quanti.sup = c(4,7,8), ncp = 3)

acp_dim1[["call"]][["quali.sup"]][["quali.sup"]][["DIFF"]]  #vrais résulats DIFF

# DIFF.1  -2.16474485 -0.31915795 -0.047095986
# DIFF.2   2.32879471  0.34334455  0.050665039





n = length(farms2$DIFF)
DIFF_prevu_dim3 = rep(0,n)

acp_dim3[["ind"]][["coord"]]
for (i in 1:n){
  x = acp_dim3[["ind"]][["coord"]][,1][i]
  y = acp_dim3[["ind"]][["coord"]][,3][i]
  point = -1.07578*x + 0.000005 - y
  if (point>0){  #si on est en dessous de la droite 
    DIFF_prevu_dim3[i] <- 1
  }
  if (point<0){  #on est au dessus de la droite 
    DIFF_prevu_dim3[i] <- 2
  }
}

obs_dim3 = DIFF_prevu_dim3
pred = acp_dim1[["call"]][["quali.sup"]][["quali.sup"]][["DIFF"]]



A_dim3 = matrix(0, nrow = 2, ncol=4)
A_dim3[1,] = 1:4

for (i in 1:n){
  if (obs_dim3[i] == 1 & pred[i] == 1){          # vrai: sain          observé: sain
    A_dim3[2,1] <- (A_dim3[2,1] + 1)
  } else if (obs_dim3[i] == 2 & pred[i] == 2){   # vrai: défaillant    observé: défaillant
    A_dim3[2,2] <- (A_dim3[2,2] + 1)
  } else if (obs_dim3[i] == 1 & pred[i] == 2){   # vrai: défaillant    observé: sain 
    A_dim3[2,3] <- (A_dim3[2,3] + 1)                   
  } else { A_dim3[2,4] <- (A_dim3[2,4] +1)}           # vrai: sain          observé: défaillant   
}


"""
> A_dim3
     [,1] [,2] [,3] [,4]
[1,]    1    2    3    4
[2,]  556  473  134   97

fermes saines bien classées:        556/653:   85.14548 %
fermes défaillantes bien classées:  473/607:   77.92422 %

"""




confusionMatrix(obs,pred)

library(explor)
explor(acp)
summary(acp)
dimdesc(acp)

res <- explor::prepare_results(acp)
explor::PCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "DIFF", labels_size = 9, point_opacity = 0.5,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-9.03, 16.7), ylim = c(-14.4, 11.3))

explor::PCA_ind_plot(res, xax = 1, yax = 2, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "DIFF", labels_size = 9, point_opacity = 0.5,
                     opacity_var = "Cos2", point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-10.7, 19.7), ylim = c(-16.5, 13.8))


explor::PCA_var_plot(res, xax = 1, yax = 2, var_sup = TRUE, var_lab_min_contrib = 0,
                     col_var = "Type", labels_size = 9, scale_unit = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-2.46, 3.55), ylim = c(-3.52, 2.49))


res <- explor::prepare_results(acp)
explor::PCA_ind_plot(res, xax = 1, yax = 1, ind_sup = FALSE, lab_var = NULL,
                     ind_lab_min_contrib = 0, col_var = "DIFF", labels_size = 9, point_opacity = 0.48,
                     opacity_var = NULL, point_size = 64, ellipses = TRUE, transitions = TRUE,
                     labels_positions = NULL, xlim = c(-10.7, 19.7), ylim = c(-10.7, 19.7))
