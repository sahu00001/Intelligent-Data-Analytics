install.packages("mlbench")
install.packages("scatterplot3d")
install.packages("rgl")
install.packages("devtools")
install.packages("corrplot")
install.packages("caret")
install.packages("MASS")
install.packages("HSAUR2")
install.packages("outliers")
install.packages("PreProcess")
install.packages("oompaBase")


##installing ggbiplot
remotes::install_github("vqv/ggbiplot")
Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
install_github("vqv/ggbiplot",force=TRUE)

library(mlbench)
library(scatterplot3d)
library(rgl)
library(ggplot2)
library(datasets)
library(devtools)
library(scales)
library(ggbiplot)
library(corrplot)
library(caret)
library(MASS)
library(PreProcess)
library(oompaBase)
library(HSAUR2)
library(outliers)
library(tidyverse)


####################################################################


# Problem 1(a): Mathematics of PCA


#i)

data("Glass")
GD<-(Glass[,-10])         ## Taking the numeric attributes from df
corMat<-cor(GD)           ## creating correlation matrix
View(corMat)


#ii)

corMat_ev<-eigen(corMat)       ## Computing eigen values and vectors of corMat
corMat_ev


#iii)

pcofGlass<-prcomp((GD),scale=T)     ## computing principal components.
pcofGlass
summary(pcofGlass)


#iv) Described in pdf file

print(evfrompc<-pcofGlass$sdev^2)   ##To get eigenvalues from principal component.


#v)

product<-t(pcofGlass$rotation[,2]) %*% 
pcofGlass$rotation[,1]
product 

#########################################################################

#Problem 1(b): Application of PCA

#i)

corrplot.mixed(corMat,lower = 'shade', upper = 'circle',      ## Visualization of correlation matrix
title="Visualization of correlation matrix",mar=c(0,0,1,0))                 
           
  
                                    
col<- colorRampPalette(c("blue", "white", "red"))(20)         ## Creating heatmap
heatmap(corMat, col = col, symm = TRUE)           
  

#ii)
  
## labeling the description based on type
  
levels(Glass$Type)[levels(Glass$Type)=='1'] <-'building windows float processed'
levels(Glass$Type)[levels(Glass$Type)=='2'] <-'building windows non-float processed'
levels(Glass$Type)[levels(Glass$Type)=='3'] <-'vehicle windows float processed'
levels(Glass$Type)[levels(Glass$Type)=='5'] <-'containers'
levels(Glass$Type)[levels(Glass$Type)=='6'] <-'tableware'
levels(Glass$Type)[levels(Glass$Type)=='7'] <-'headlamps'

ggbiplot(pcofGlass, obs.scale = 1, var.scale = 1,
         groups = Glass$Type, ellipse = TRUE, circle = TRUE) +
  theme(legend.direction = 'vertical', legend.position = 'right')+

  labs(title="Visualization of the prinipal components of glass dataset")+ 
  theme(plot.title=element_text(face="bold",hjust=0.5,size = 15), 
        axis.title.x = element_text(face="bold",size = 15), 
        axis.title.y = element_text(face="bold",size = 15))+ 
  theme(plot.background=element_rect(fill="#BFD5E3"))


#iii) Described on pdf file

summary(pcofGlass)


#iv)  Described on pdf file

#########################################################################

# Problem 1(c): Application of LDA

#i)

preProcess(Glass, method = c("center", "scale"))
LDA_Glass<-lda(Type ~ . , Glass)
LDA_Glass


#ii) Described on pdf file


#iii) 

Glass.lda.values <- predict(LDA_Glass)
Glass.lda.values
ldahist(Glass.lda.values$x[,1], g= Glass$Type)+      ## Histogram for LD1
title(main="Visualization of LD1")
  
ldahist(Glass.lda.values$x[,2], g= Glass$Type)+        ## Histogram for LD2
title(main="Visualization for LD2")


######################################################

# 2 Principal components for dimension reduction

#a) 

data("heptathlon")

tests = lapply(heptathlon,grubbs.test)   ## grubbs test
tests

x<-rm.outlier(heptathlon)                ##remove the outlier
x


#b)

hurdlemax <- max(heptathlon$hurdles)
hurdlemax
run200mmax <- max(heptathlon$run200m)
run200mmax
run800mmax <- max(heptathlon$run800m)
run800mmax

heptathlon$hurdles <- (hurdlemax-heptathlon$hurdles)
heptathlon$hurdles
heptathlon$run200m <- (run200mmax-heptathlon$run200m)
heptathlon$run200m
heptathlon$run800m <- (run800mmax-heptathlon$run800m)
heptathlon$run800m


#c)

Hpca<- prcomp(x[1:7],scale=TRUE)
Hpca


#d

ggbiplot(pcobj=Hpca,choices=c(1,2),obs.scale=1, var.scale=1,
          varname.size=5,varname.abbrev=FALSE)+
  labs(title="Visualization of the prinipal components")+ 
  theme(plot.title=element_text(face="bold",hjust=0.5,size = 15), 
        axis.title.x = element_text(face="bold",size = 15), 
        axis.title.y = element_text(face="bold",size = 15))+ 
  theme(plot.background=element_rect(fill="#BFD5E3"))

#e

ggplot(Hpca)+
geom_point(mapping=aes(x=Hpca$x[,1],y=heptathlon$score))


###################################################################

# 3. Housing data dimension reduction and exploration

housedata<-read.csv("housingData.csv")     ## reading "housingData" dataframe
view(housedata)


## select numeric columns

hd <- housedata %>%
  select_if(is.numeric) %>%  
  
##creates new variables age, ageSinceRemodel, and ageofGarage
  
  dplyr::mutate(age = YrSold - YearBuilt,
                ageSinceRemodel = YrSold - YearRemodAdd,
                ageofGarage = ifelse(is.na(GarageYrBlt), age, YrSold - GarageYrBlt)) %>%
  
##removes a few columns that are not needed
  
  dplyr::select(!c(Id,MSSubClass, LotFrontage, GarageYrBlt,
                   MiscVal, YrSold , MoSold, YearBuilt,
                   YearRemodAdd, MasVnrArea))

## PCA for dataset hd

pc=prcomp(hd, scale.=T)
pc

## Visualization of PCA 

ggbiplot(pcobj=pc,choices=c(1,2),varname.size=4,varname.abbrev=FALSE)+
  labs(title="Visualization of the prinipal components")+ 
  theme(plot.title=element_text(face="bold",hjust=0.5,size = 15), 
        axis.title.x = element_text(face="bold",size = 15), 
        axis.title.y = element_text(face="bold",size = 15))+ 
  theme(plot.background=element_rect(fill="#BFD5E3"))

## CRA

CRA<- cor(hd)
CRA

## plot the CRA visualisation 

corrplot(CRA)



##################################################################




# scree plot
qplot(c(1:9), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.5)+xlim(0,9)








