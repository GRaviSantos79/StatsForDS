# Stats Project part 1

wines <- read.csv('C:/Users/User/Documents/Estatística Curso/5ºSemestre/Análise Multivariada/vinho/wines.csv', h=T, sep = ';')
wine.classes <- read.table('C:/Users/User/Documents/Estatística Curso/5ºSemestre/Análise Multivariada/vinho/wine_classes.txt', h=T)$x

# Measuring the Dimension 
dim(wines)

colnames(wines)

apply(wines, 2, range)

str(wines)
wines$magnesium = as.numeric(wines$magnesium)
wines$col..int. = as.numeric(wines$col..int.)


#
w <- scale(wines)
w

#Principal components analysis

w.pca <- princomp(w) 
w.pca
w.pca$scores
w.pca$loadings

#Var(Y)
w.pca$sdev^2

#Scores
head(w.pca$scores) 

#Loadings
w.pca$loading


sum( w.pca$sdev^2 ) # para ser exatamente p=13 requer divisor n-1 

#Lambda
lambda <- 177*w.pca$sdev^2/176

#Total Variance
Vtot <- sum(lambda)
Vtot

#Relative variance
Vrel <- lambda / Vtot
Vrel
#% of explained variance
100 * round(Vrel, digits = 2)

VAR <- 100 * Vrel
VAR

# CumSum Variance
cumsum( VAR )

#Plotting my lambda
plot(w.pca)

#"Elbow" plot to see how many components are necessary
plot(lambda, type='b')

#Biplot with each one the variables
x11()
biplot(w.pca)


#Scores graph with the group distinction (With the to components)

plot(w.pca$scores[,1:2], type = "n",xlab = paste("PC 1 (", VAR[1], "%)", sep = ""),
     ylab = paste("PC 2 (", VAR[2], "%)", sep = "")) 
 abline(h = 0, v = 0, col = "gray") 
 points(w.pca$scores[,1:2], pch = wine.classes) 
 legend(3,3,legend=unique(wine.classes), pch=1:3)
 
 
# Visualizing some specific variables that matters on the model
 
 
 boxplot(alcohol~wine.classes, data=cbind(w,wine.classes))
 boxplot(col..int.~wine.classes, data=cbind(w,wine.classes))
 boxplot(flavonoids~wine.classes, data=cbind(w,wine.classes))
 boxplot(tot..phenols~wine.classes, data=cbind(w,wine.classes)) 

 
#Let's decompose
 
S <- var(w)
S 
S.de <- eigen(S)
round(S.de$vec,3)

#Singular Value Decomposition

w.svd <- svd(w)
w.svd

w.svd$d^2 / (nrow(wines) - 1)

( lambda <- S.de$values )  # divisor is (n-1)

#V Matrix
round(w.svd$v, 3)

( Vtot <- sum(diag(S)) )

sum(diag(lambda))

Vrel <- lambda / Vtot
Vrel
100 * round(Vrel, digits = 3)

VAR <- 100 * round(Vrel, digits = 3) 
cumsum( VAR ) 

plot(lambda, type='b')

####################

mv1 = factanal(covmat = Sigma, n.obs = n, factors = m, rotation = "none")
mv1

mv2 = factanal(covmat = Sigma, n.obs = n, factors = m, rotation = "varimax")
mv2