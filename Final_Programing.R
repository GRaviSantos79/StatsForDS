#Stats Project

data <- read.csv('C:/Users/User/Documents/Portugal/NOVA/Disciplinas/Statistics for Data Science/Final Project/dataset.csv', h=T, sep = ',')
data

#Studying the data
summary(data)
str(data)

data
#####################
####BASIC ANALYSIS###
#####################
sum(is.na(data))

for(i in 1:ncol(data)){
  data[is.na(data[,i]), i] <- median(data[,i], na.rm = TRUE)
}

sum(is.na(data))

colnames(data) = c('Country','Co2','Poverty','LifeExpectancy','gini','BasicSanitation',
   'EnergyPerPerson','SugarPerPerson','GDD','SmokingPercapita',
   'Imunization','SuicideRate','WorkingHours','LiteracyRate',
   'CorruptionPerCapita','GovHealthSpend','FoodSupply','Unemployment',
   'TotaHealthSpend','AlcoholComsum','BasicWater')

colnames(data)

####################
####CORRELATION#####
####################
data1 = data[,-1]
require(corrplot)
x11()
COR <- cor(data1) 
?corrplot
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(COR, order="FPC", method="color", type="upper")

summary(data[,-1])
x11()

library(psych)
x11()
pairs.panels(data1, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
         

set.seed(137)
data[,]
summary(data[,-1])



# Analysing and transforming the main variables
summary(data1$LifeExpectancy)
y = data1$LifeExpectancy
ly = log(y) 
# Checking and treating the NA values for the training set
apply(Z, 2, function(X) sum(is.na(X)) )
X = ifelse(is.na(X), median(X, na.rm = TRUE, X))

X = subset(data1, select=-LifeExpectancy) 
X
###################
####TRAINING SET###
###################

Z = scale(X)
apply(Z, 2, function(x) sum(is.na(x)) ) #No NA's detected


train = sample(1:nrow(data1), size = 58, replace = FALSE) 
train;test = (-train) ;y.test = y[test] ;Xtreino = X[train,] ;Xtest = X[test,] 
# Matrix with the predictors, mean and sd by the training model
m.train = colMeans(Xtreino) 
sd.train = apply(Xtreino, 2, sd) 
Ztreino = as.data.frame( scale(Xtreino) ) 
Ztest = as.data.frame(scale(Xtest, m.train, sd.train) ) 
TRAIN = as.data.frame( cbind(ly=ly[train], Ztreino) ) 
colnames(TRAIN)
Ztest
TRAIN
library('caret')

# MultiColinearity for the numerical variables
COR <- cor(data1)
Multt = findCorrelation(COR, cutoff = 0.75)
data1 = data1[,-Multt]
Multt

################
####MODELING####
################
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

################
####LEAST#######
####SQUARE######
####METHOD######
################

m1 <- lm(ly ~ ., data=Ztest)
summary(m1)

# Plotting the model in a QQ-Plot
x11()
   plot(m.best, 2)
shapiro.test(TRAIN$ly)

## Final Model

lyZ <- data.frame(ly,Z) 
update(m1, data = lyZ)
summary(m1)   
plot(m1, 2)
                                   
####################
####STEPWISE########
####REGRESSION######
####################

##### Backward Method  #########


bic.bwd = step( m1, direction='backward', k=log(nrow(TRAIN)) )

## Updating the new Data
update(bic.bwd, data = lyZ)
summary(bic.bwd)

## Let's see how de model adjusted itself
summary(bic.bwd)$adj

##### Foward Method  #########

bic.fwd = step( m1, direction='forward', k=log(nrow(TRAIN)) )


## Updating the new Data
update(bic.fwd, data = lyZ)
summary(bic.fwd)

## Let's see how de model adjusted itself
summary(bic.fwd)$adj

##### Both Method  #########

bic.2 = step( m1, direction='both', k=log(nrow(TRAIN)) )
summary(bic.2)

## Updating the new Data
update(bic.2, data = lyZ)
summary(bic.2)

## Let's see how de model adjusted itself
summary(bic.2)$adj

########################################
####WHATS THE BEST VARIABLES?############
########################################

library(leaps) 

best = regsubsets(ly ~ ., data = TRAIN) 
best.summary = summary(best)
#names(best)
best.summary$adjr2
  
x11()
par(mfrow=c(2,2)) 
plot(best.summary$rss,xlab="No de Variaveis",ylab="SQR",type="l") 
plot(best.summary$adjr2,xlab="No de Variaveis",ylab="R2 ajustado",type="l") 
which.max(best.summary$adjr2)
points(15,best.summary$adjr2[15], col="red",cex=2,pch=20) 
plot(best.summary$cp,xlab="No de Variaveis",ylab="Cp",type='l') 
points(14,best.summary$cp[14],col="red",cex=2,pch=20) 
plot(best.summary$bic,xlab="No de Variaveis",ylab="BIC",type='l')
points(9,best.summary$bic[9],col="red",cex=2,pch=20)

#### Graph to help identify each one the variables that matter the most
x11()
par(mfrow=c(2,3)) 
plot(best, scale="r2")
plot(best, scale="adjr2")
plot(best, scale="Cp") 
plot(best, scale="bic") 
plot(best, scale="r2")

coef(best, 6)

## Selecting the best variables
#"Poverty"             "gini"                "BasicSanitation"     "CorruptionPerCapita"
#"AlcoholComsum" 

X.bic.best <- names(coef(best, 5))[-1] 
X.bic.best

## Building the final model

m.best <- lm( ly ~ ., data = TRAIN[,c('ly', X.bic.best)]) 
summary(m.best)$adj
summary(m.best)

###############
#####PCA#######
###############

library(pls)
m1
m.pcr <- pcr(formula(m.best), data=TRAIN, scale = TRUE, validation="CV") 
summary(m.pcr)
m.pcr$model

validationplot(m.pcr,val.type="MSEP")  ####### We will use 3 componentes 
ypred6 = exp( predict(m.pcr, newdata = Ztest, ncomp=5) ) 
sqrt( mean( (ypred6 - y.test)^2) )

##############################
#####Factorial Analysis#######
##############################

as.matrix(data1)
Sigma <- cor(as.matrix(data1))

m <- 2
de <- eigen(Sigma)
( lambda <- de$values )
sum(lambda)

rlam <- sqrt(lambda)

L <- sweep( de$vector[,1:m],  2, rlam[1:m], FUN = "*" )
LLt <- L %*% t(L)

com <- diag(LLt)                   # comunalidades estimadas
com
# ou 
rowSums(L^2)

psi <- diag(Sigma)-diag(LLt)       # var especificas 
psi 

options(digits=2)
data.frame(L,com,psi, row.names=colnames(Sigma))    

cumsum(lambda[1:m])/ncol(Sigma)

n <- 100
mv1 = factanal(covmat = Sigma, n.obs = 195, factors = m, rotation = "none")
mv1

mv2 = factanal(covmat = Sigma, n.obs = n, factors = m, rotation = "varimax")
mv2

cbind(psi, mv1$uniq, mv2$uniq)


