##LPCH Lek Analysis for Predicting Habitat Suitability--MGP
##STABLE LEKS ONLY VS RANDOM POINTS
##Morgan Solomon
## 11.09.21


library(tidyverse)  
library(raster)   
library(sf)        
library(rgdal)      
library(sp)         
library(psych)
library(lme4)       
library(AICcmodavg) 
library(mgcv)      
library(gridExtra)  
library(NLMR)   
library(DHARMa)     
library(ResourceSelection) 
library(ggpubr)
library(ROCR)


StableData<-read.csv("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models/StableData.csv")

str(StableData)


##################Habitat Modeling - Stable leks####

#Using gams to visualize trends/pattern in a univariate analysis--- leks vs random points####
gam1 <- gam(PT_Type ~ s(AveTree),family=binomial, data=StableData)
gam2 <- gam(PT_Type ~ s(AvePFG),family=binomial, data=StableData)
gam3 <- gam(PT_Type ~ s(AveAFG),family=binomial, data=StableData)
gam4 <- gam(PT_Type ~ s(AveBG),family=binomial, data=StableData)
gam5 <- gam(PT_Type ~ s(AveLit),family=binomial, data=StableData)
gam6 <- gam(PT_Type ~ s(AveShrub),family=binomial, data=StableData)
gam7 <- gam(PT_Type ~ s(AveCrop),family=binomial, data=StableData)
gam8 <- gam(PT_Type ~ s(AveElev),family=binomial, data=StableData)
gam9 <- gam(PT_Type ~ s(AvePrecip),family=binomial, data=StableData)
gam10<- gam(PT_Type ~ s(AveTemp),family=binomial, data=StableData)
gam11<- gam(PT_Type ~ s(Ruggedness),family=binomial, data=StableData)
gam12<- gam(PT_Type ~ s(DistOil),family=binomial, data=StableData)
gam13<- gam(PT_Type ~ s(DistHighway),family=binomial, data=StableData)
gam14<- gam(PT_Type ~ s(DistWind),family=binomial, data=StableData)
gam15<- gam(PT_Type ~ s(DistRoadway),family=binomial, data=StableData)
gam16<- gam(PT_Type ~ s(DensRoadway),family=binomial, data=StableData)
gam17<- gam(PT_Type ~ s(DensOil),family=binomial, data=StableData)
gam18<- gam(PT_Type ~ s(DensWind),family=binomial, data=StableData)
gam19<- gam(PT_Type ~ s(VarAFG),family=binomial, data=StableData)
gam20<- gam(PT_Type ~ s(VarPFG),family=binomial, data=StableData)
gam21<- gam(PT_Type ~ s(VarLit),family=binomial, data=StableData)
gam22 <- gam(PT_Type ~ s(VarBG),family=binomial, data=StableData)
gam24 <- gam(PT_Type ~ s(VarShrub),family=binomial, data=StableData)
gam25 <- gam(PT_Type ~ s(VarEVI),family=binomial, data=StableData)
gam26 <- gam(PT_Type ~ s(AveEVI),family=binomial, data=StableData)
gam27 <- gam(PT_Type ~ s(DistTrans),family=binomial, data=StableData)

#Plotting relationships of StableData vs random points####
plot(gam1, ylab='', xlab="Avg Tree", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam2, ylab='', xlab="Avg PFG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic increasing
plot(gam3, ylab='', xlab="Avg AFG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #ln(x)
plot(gam4, ylab='', xlab="Avg BG", cex.axis=1.5, cex.lab=1.5, yaxt='n')  #quadratic
plot(gam5, ylab='', xlab="Avg Litter", cex.axis=1.5, cex.lab=1.5, yaxt='n') # quadratic
plot(gam6, ylab='', xlab="Avg Shrub", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear/no effect
plot(gam7, ylab='', xlab="Avg Crop", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic
plot(gam8, ylab='', xlab="Avg Elev", cex.axis=1.5, cex.lab=1.5, yaxt='n') #No effect, remove
plot(gam9, ylab='', xlab="Avg Precip", cex.axis=1.5, cex.lab=1.5, yaxt='n') #No effect, linear
plot(gam10, ylab='', xlab="Avg Temp", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic
plot(gam11, ylab='', xlab="Ruggedness", cex.axis=1.5, cex.lab=1.5, yaxt='n') #Quadratic
plot(gam12, ylab='', xlab="Dist Oil", cex.axis=1.5, cex.lab=1.5, yaxt='n')   #Linear
plot(gam13, ylab='', xlab="Dist Highway", cex.axis=1.5, cex.lab=1.5, yaxt='n')#linear
plot(gam14, ylab='', xlab="Dist Wind", cex.axis=1.5, cex.lab=1.5, yaxt='n')    #ln(x)
plot(gam15, ylab='', xlab="Dist Roadway", cex.axis=1.5, cex.lab=1.5, yaxt='n') #ln(x)
plot(gam16, ylab='', xlab="Dens Roadway", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic decreasing
plot(gam17, ylab='', xlab="Dens Oil", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam18, ylab='', xlab="Dens Wind", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam19, ylab='', xlab="Var AFG", cex.axis=1.5, cex.lab=1.5, yaxt='n')  #linear decreasing
plot(gam20, ylab='', xlab="Var PFG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing; no effect
plot(gam21, ylab='', xlab="Var Lit", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic decreasing; no effect
plot(gam22, ylab='', xlab="Var BG", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing no effect
plot(gam24, ylab='', xlab="Var Shrub", cex.axis=1.5, cex.lab=1.5, yaxt='n') #linear decreasing
plot(gam25, ylab='', xlab="Var EVI", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic ELIMINATED
plot(gam26, ylab='', xlab="Ave EVI", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic ELIMNATED
plot(gam27, ylab='', xlab="DistTrans", cex.axis=1.5, cex.lab=1.5, yaxt='n') #quadratic



full<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveShrub + AveCrop + I(AveCrop^2) + AveTemp + I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DistTrans + I(DistTrans^2) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG + VarBG + VarShrub, family = binomial, data = StableData)

summary(full)

############Developing an AIC table for backwards selection process above####


Cand.models<-list()

Cand.models[[1]]<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveShrub + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DistTrans + I(DistTrans^2) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG  + I(VarLit^2) + VarBG + VarShrub, family = binomial, data = StableData) #all variables

Cand.models[[2]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DistTrans + I(DistTrans^2) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG + I(VarLit^2) + VarBG + VarShrub, family = binomial, data = StableData) #Ave Shrub dropped

Cand.models[[3]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DistTrans + I(DistTrans^2) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG + VarBG + VarShrub, family = binomial, data = StableData) #Ave shrub and VarLit dropped

Cand.models[[4]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DistTrans + I(DistTrans^2) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG + VarBG, family = binomial, data = StableData) #Ave Shrub, VarLit, VarShrub dropped

Cand.models[[5]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DistTrans + I(DistTrans^2) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG +  VarBG, family = binomial, data = StableData) #Ave Shrub, VarLit,  Var Shrub,Ruggedness, dropped

Cand.models[[6]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG  + VarBG, family = binomial, data = StableData) #AveShrub, VarLit, VarShrub, DistTrans dropped

Cand.models[[7]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + log(DistWind + 0.001) + log(DistRoadway + 0.001) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG , family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, VarBG dropped

Cand.models[[8]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway +  log(DistRoadway + 0.001) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG , family = binomial, data = StableData) #Ave Shrub, VarLit, VarShrub,, DistTrans,VarBG Dist Wind

Cand.models[[9]] <- glm(PT_Type ~  AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway +  log(DistRoadway + 0.001) + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG, family = binomial, data = StableData)  #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, AveTree dropped

Cand.models[[10]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistOil + DistHighway + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad dropped

Cand.models[[11]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + AveTemp + I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, DistOil dropped

Cand.models[[12]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, , VarPFG dropped

Cand.models[[13]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveBG + I(AveBG^2) + log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensRoadway + I(DensRoadway^2) + DensOil + VarAFG + VarPFG, family = binomial, data = StableData)#Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, DensWind dropped

Cand.models[[14]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG dropped

Cand.models[[15]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + Ruggedness + I(Ruggedness^2) + DistHighway + DensRoadway + I(DensRoadway^2) + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, AveCrop dropped

Cand.models[[16]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad dropped, FINAL MODEL?

Cand.models[[17]] <- glm(PT_Type ~ AveTree +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad, AvePFG dropped

Cand.models[[18]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad, AveAFG dropped

Cand.models[[19]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad, VarAFG dropped

Cand.models[[20]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind + VarAFG, family = binomial, data = StableData) #Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad, AveLit dropped

Cand.models[[21]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DistHighway  + DensWind + VarAFG, family = binomial, data = StableData)#Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad, DensOil dropped

Cand.models[[22]] <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + Ruggedness + I(Ruggedness^2) + DensOil + DensWind + VarAFG , family = binomial, data = StableData)#Ave Shrub, VarLit, DistTrans, VarShrub, Dist Wind, VarBG, DistRoad, AveBG, DensRoad, DistHighway dropped

Cand.models[[23]]<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + DistHighway + DensOil, family = binomial, data = StableData) #model that eliminates covariates of little significance

Cand.models[[24]]<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + DistHighway + DensOil, family = binomial, data = StableData) #model that eliminates covariates of little significance except for Ave crop


###AIC table

Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

aictable<-aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, nobs = NULL)
aictable
setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models")

AIC<-as.data.frame(aictable)


write.csv(AIC, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models/AICmodelselection_StableVRando.csv")

#### Test for spatial autocorrelation#### 


# fit a non-spatial global model; global means that it has an average value for the entire data set
GLOBAL <- glm(PT_Type ~ AveTree + AvePFG + AveAFG + AveBG + AveLit + AveShrub + AveCrop + AveElev + AvePrecip + AveTemp + DistHighway + DistOil + DistWind + DistRoadway + DistTrans + DensRoadway + DensOil + DensWind + VarAFG + VarPFG + VarBG + VarTree + VarShrub + VarLit, family=binomial, StableData)   #change these to paramaterize a global logistic regression
# plot residuals

head(StableData)
StableData$resid <- resid(GLOBAL)
StableData$resid_std <- rescale(StableData$resid, 1, 10)
ggplot(StableData, aes(x = x, y = y, size = resid)) +
  geom_point() +
  scale_size_continuous(range = c(1,10))

# The Formal test
sims <- simulateResiduals(GLOBAL)
testSpatialAutocorrelation(sims, plot = FALSE)  # Moran's I from package DHARMa  



### Habitat Modeling  ####
# Stable leks vs. Random points 
Cand.models <- list( )

#standard model
Cand.models[[1]] <- glm(PT_Type ~ AveTree + AvePFG + AveAFG + AveBG + AveLit + AveShrub + AveCrop + AvePrecip + AveTemp + DistHighway + DistOil + DistWind + DistRoadway + DensRoadway + DensOil + DensWind + VarPFG + VarBG + VarShrub + VarLit + DistTrans, family=binomial, StableData)

#model based on GAMs and model selection with AIC 
Cand.models[[2]] <-  glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + AveTemp + I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind, family = binomial, data = StableData)

##create a vector of names to trace back models in set
Modnames <- paste("mod", 1:length(Cand.models), sep = " ")

##round to 1 digits after decimal point and give log-likelihood
print(aictab(cand.set = Cand.models, modnames = Modnames, sort = TRUE, nobs = NULL),
      digits = 2, LL = TRUE)



####AUC####

pred <- predict(Cand.models[[2]], StableData, type = "response")


test_output <- cbind(StableData,pred)   # should be using a testing data set for first term

#Plotting ROC graph for data, if area more, then good fit
length(fitted(Cand.models[[2]]))==length(StableData$PT_Type)

preds <- prediction(as.numeric(pred), as.numeric(StableData$PT_Type))
perf <- performance(preds,"tpr","fpr") #tpr = true positivie rate, fpr = false positive rate
plot(perf)
abline(a=0, b=1)

auc_ROCR <- performance(preds, measure = "auc")
auc_ROCR <- auc_ROCR@y.values[[1]]  

auc_ROCR #0.91

boxplot(test_output$pred~test_output$PT_Type)

############### Cross validation of stable leks v randoms model ####
#    Cross validation - 500 folds - calculating AUC at each iteration
#    Put the AUC into a loop that resamples and calulates 95%CI

# Number of runs
num.sims <- 500

# Create place to store output from bootstrap
AUC <- rep(NA, num.sims)
#HL.P <- rep(NA, num.sims)

for (i in 1:num.sims){
  smp_size <- floor(0.80 * nrow(StableData)) # training data is 80%
  
  train_ind <- sample(seq_len(nrow(StableData)), size = smp_size)
  
  train <- StableData[train_ind, ]
  test <- StableData[-train_ind, ]
  
  ## Calculate AUC for test dataset using model calibrated for training set
  # Fit model to test dataset
  model1 <- glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + AveTemp + I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind, family = binomial, data = train)
  
  
  
  pred <- predict(model1, test, type = "response")
  
  test_output <- cbind(test,pred)  
  
  #Plotting ROC graph for data, if area more, then good fit:
  length(fitted(model1))==length(test$PT_Type)
  
  preds <- prediction(as.numeric(pred), as.numeric(test$PT_Type))
  perf <- performance(preds,"tpr","fpr")
  plot(perf)
  abline(a=0, b=1)
  
  auc_ROCR <- performance(preds, measure = "auc")
  
  AUC[i] <- auc_ROCR@y.values[[1]] 
} 

mean(AUC)  # 0.88
quantile(AUC, c(.025, .975))  #95%CI = 0.81 - 0.94



summary(full)

#Model coefficient to csv####

summary(model1)
std.err<-coef(summary(model1))[,2]
stderror<-as.data.frame((std.err))
write.csv(stderror, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models/StableVsRandoms_standardErrors.csv")

model1coef <- as.data.frame(model1$coefficients)
write.csv(model1coef, file ="C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/RSF models/StableVsRandoms_Coefficients.csv")


#Building stack for covariates included in model####



setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model....")


AvePFG<-raster("AvePFG.tif")
AveTree<-raster("AveTree.tif")
AveAFG<- raster("AveAFG.tif")
AveLit<-raster("AveLit.tif")

AveCrop<-raster("AveCrop.tif")
AveTemp<-raster("AveTemp.tif")


DensOil<-raster("DensOil.tif")
DensWind<-raster("DensWind.tif")

DistHighway<-raster("DistHighway.tif")


Ruggedness<-raster("Ruggedness.tif")


VarPFG<-raster("VarPFG.tif")
VarAFG<-raster("VarAFG.tif")


#Creating raster stack
stack<-stack(AveTree,AvePFG,AveAFG,AveLit,AveTemp,DistHighway,DensOil,DensWind,Ruggedness,AveCrop)

model1<-glm(PT_Type ~ AveTree + AvePFG + I(AvePFG^2) +log(AveAFG + 0.001) +  log(AveLit + 0.001) + AveCrop + I(AveCrop^2) + AveTemp + I(AveTemp^2) + Ruggedness + I(Ruggedness^2) + DistHighway + DensOil + DensWind, family = binomial, data = StableData)

#Making a habitat suitability raster for the MGP####


Suitability<-predict(stack, model1, filename = "RSF_Stable_NewData.tif" ) 

plot(Suitability)


