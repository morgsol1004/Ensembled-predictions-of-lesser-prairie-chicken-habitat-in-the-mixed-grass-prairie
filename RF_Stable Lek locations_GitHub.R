##LPCH Random Forest Lek Analysis for Predicting Habitat Suitability--MGP
##STABLE LEKS VS RANDOM POINTS
##Morgan Solomon



require(tidyverse)  
require(raster)    
require(sf)        
require(rgdal)     
require(sp)         
require(ggpubr)
require(ROCR)
require(pROC)
require(caret)
require(randomForest)




RFdata<-read.csv("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Random forest models/RFdata_StableOnly.csv")
str(RFdata)

########Using caret package to make random forest model####

#splitting data into training set and a testing set
set.seed(778)
smp_size <- floor(0.80 * nrow(RFdata)) # training data is 80%

train_ind <- sample(seq_len(nrow(RFdata)), size = smp_size)

train <- RFdata[train_ind, ]
test <- RFdata[-train_ind, ] #although we are already doing cross validation in the trainControl function below, I wanted to create a testing set as well


#Determing what mtry should be used in final model####
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=10, classProbs = TRUE, summaryFunction = twoClassSummary, search = 'grid') #see above but classProbs = TRUE allows you to have a continuous values when predicting lek occurrence instead of binary and search = 'random' allows us to randomly generate 15 random values to see what mtry provides the best accuracy (this takes a while)

set.seed(779)


tunegrid<- expand.grid(.mtry=(2:24)) #creating a grid that determines what mtry should be set to

mtry_fit <- train(POINT_TYPE ~ .,
                  data = train,
                  method = "rf",
                  verbose = FALSE,
                  metric = "ROC",
                  trControl = ctrl,
                  tuneGrid = tunegrid)

mtry_fit
plot(mtry_fit) #we will use mtry = 2



#Determining how many trees should be in the RF model####
tunegrid <- expand.grid(.mtry = c(sqrt(ncol(RFdata))))
modellist <- list()

#train with different ntree parameters
for (ntree in c(500,600,700,800,900)){
  set.seed(779)
  fit <- train(POINT_TYPE~.,
               data = RFdata,
               method = 'rf',
               metric = 'Accuracy',
               tuneGrid = data.frame(.mtry = 2),
               trControl = ctrl,
               ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

#Compare results
results <- resamples(modellist)
summary(results) #highest accuracy was at 500 trees which is caret packages default

dotplot(results)



#Fit model with all data
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=100, classProbs = TRUE, summaryFunction = twoClassSummary)

rf.fit.all <- train(POINT_TYPE ~ .,
                    data = RFdata,
                    method = "rf",
                    set.seed(7),
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl,
                    ntree = 500,
                    tuneGrid = data.frame(.mtry =2))



rf.fit.all
#AUC: 0.87
vi<-varImp(rf.fit.all)
vi
a<-plot(vi)
a

#Partial dependency plots
RF<-randomForest(POINT_TYPE~., data = RFdata, ntree = 500, mtry = 2)

line = 1
cex = 2
side = 3
adj=-0.05

par(mfrow = c(2, 5))
partialPlot(RF, RFdata, AveTree, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("A", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AvePFG, "LEK" , main = '', ylab = 'Probability of a lek')
mtext("B", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AvePrecip, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("C", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, DensOil, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("D", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AveCrop, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("E", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, DistHighway, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("F", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AveAFG, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("G", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, DistTrans, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("H", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, AveShrub, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("I", side=side, line=line, cex=cex, adj=adj)
partialPlot(RF, RFdata, Ruggedness, "LEK", main = '', ylab = 'Probability of a lek' )
mtext("J", side=side, line=line, cex=cex, adj=adj)

#Double check model on test data
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=100, classProbs = TRUE, summaryFunction = twoClassSummary)

rf.fit <- train(POINT_TYPE ~ .,
                data = train,
                method = "rf",
                verbose = FALSE,
                metric = "ROC",
                trControl = ctrl,
                tuneGrid = tunegrid)

# Build custom AUC function to extract AUC
# from the caret model object
test_roc <- function(model, data) {
  
  roc(data$POINT_TYPE,
      predict(model, data, type = "prob")[, "LEK"])
  
}

orig_fit %>%
  test_roc(data = test) %>%
  auc() 

rf.fit

###Cross validate####
AUC <- rep(NA, num.sims)
ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

for (i in 1:num.sims){
  smp_size <- floor(0.80 * nrow(RFdata)) # training data is 80%
  
  train_ind <- sample(seq_len(nrow(RFdata)), size = smp_size)
  
  train <- RFdata[train_ind, ]
  test <- RFdata[-train_ind, ]
  
  auc_fit <- train(POINT_TYPE ~ .,
                   data = train,
                   method = "rf",
                   verbose = FALSE,
                   metric = "ROC",
                   trControl = ctrl)
  
  # Build custom AUC function to extract AUC
  # from the caret model object
  
  test_roc <- function(model, data) {
    
    roc(data$POINT_TYPE,
        predict(model, data, type = "prob")[, "LEK"])
    
  }
  
  AUC[i]<-auc_fit %>%
    test_roc(data = test) %>%
    auc()
}

mean(AUC) #0.86
quantile(AUC, c(.025, .975)) #0.74 - 0.94; some uncertainty here

#Variable importance in final RF model####
VI<-varImp(rf.fit.all)
VI
#to



#Fit model with all data
ctrl <- trainControl(method = 'repeatedcv', number = 5, repeats=100, classProbs = TRUE, summaryFunction = twoClassSummary)

rf.fit.all <- train(POINT_TYPE ~ .,
                    data = RFdata,
                    method = "rf",
                    verbose = FALSE,
                    metric = "ROC",
                    trControl = ctrl,
                    tuneGrid = tunegrid)



#Building stack for covariates included in model####

setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Final Layers_rasterstack/Habitat Layers/CroppedResampled")


AvePFG<-raster("AvePFG.tif")
AveTree<-raster("AveTree.tif")
AveAFG<- raster("AveAFG.tif")
AveShrub<-raster("AveShrub.tif")
AveLit<-raster("AveLit.tif")
AveBG<-raster("AveBG.tif")


AveCrop<-raster("AveCrop.tif")
AveTemp<-raster("AveTemp.tif")
AvePrecip<-raster("AvePrecip.tif")
AveElev<-raster("AveElev.tif")


DensOil<-raster("DensOil.tif")
DensWind<-raster("DensWind.tif")
DensRoad<-raster("DensRoadway.tif")

DistHighway<-raster("DistHighway.tif")
DistWind<-raster("DistWind.tif")
DistTrans<-raster("DistTrans.tif")
DistRoadway<-raster("DistRoadway.tif")
DistOil<-raster("DistOil.tif")

Ruggedness<-raster("Ruggedness.tif")

VarBG<-raster("VarBG.tif")
VarPFG<-raster("VarPFG.tif")
VarShrub<-raster("VarShrub.tif")
VarAFG<-raster("VarAFG.tif")
VarLit<-raster("VarLit.tif")



stack<-stack(AveTree,AvePFG,AveAFG,AveShrub,AveLit,AveBG,DensOil,DensWind,DensRoad,DistHighway,DistWind,DistTrans,DistRoadway,DistOil,Ruggedness,VarPFG,VarAFG,VarLit,VarShrub,VarBG,AvePrecip,AveCrop,AveTemp,AveElev) #raster stack to make prediction on

stack

#Making a habitat suitability raster for the MGP####

setwd("C:/Users/morga/OneDrive - Montana State University/Solomon, Morgan/Habitat Model/Suitability rasters")

Suitability<-predict(stack, rf.fit.all, type = 'prob', filename = "HS_RF_StablevsRando_final_test.tif" ) #THIS WAS DONE WHEN YOU MADE THE FIRST PREDICTION; USE WHEN FIRST MAKING YOUR SUITABILITY MAPS

getwd()

#plotting raster
plot(Suitability)



#Arranging variable importance plots from both all leks and stable lek analysis


