
train<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TrainF.csv", header=TRUE, sep=",")
test<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TestFrv2.csv", header=TRUE, sep=",")


subtest<-subset(train, Id>=1001)
subtrain<-subset(train, Id<=1000)


## simple regression
testrun1<-lm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
               GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
               YrSold+ SaleType+SaleCondition , data=subtrain)
###code for checking MAEP
predicted.5<-exp(predict(log.test.gr1, subtest))
percent.errors.check4<- abs((subtest$SalePrice-predicted.5)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check4) 

logtestrun <-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF) +X2ndFlrSF + LowQualFinSF +
              log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
               YrSold+ SaleType+SaleCondition , data=subtrain)



## step backward
fit.log.step.b2<-step(lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                         +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                           +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                         +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF)+X2ndFlrSF + LowQualFinSF +
                           log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                           Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                           OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition+
                           GarageYrBlt, data=subtrain), direction="backward")
predicted.5<-exp(predict(log.test.gr1, subtest))
percent.errors.check4<- abs((subtest$SalePrice-predicted.5)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check4) #5.515891

## result from step backward
log.test.gr4<-lm(log(SalePrice) ~  MSZoning + log(LotArea) + Street + Utilities + 
                   LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                   Exterior1st + MasVnrArea + ExterCond + Foundation + BsmtQual + 
                   BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + 
                   HeatingQC + CentralAir + log(X1stFlrSF) + X2ndFlrSF + log(GrLivArea) + 
                   BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                   Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
                   GarageCond + WoodDeckSF + EnclosedPorch + ScreenPorch + PoolArea + 
                   PoolQC + SaleType + SaleCondition, data=subtrain)
predicted.prices9<-exp(predict(log.test.gr4,testv2))

write.csv(predicted.prices9, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v6.csv")

## result from step backward + MSSubclass 
log.test.gr5<-lm(log(SalePrice) ~ MSSubClass+ MSZoning + log(LotArea) + Street + Utilities + 
                   LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                   Exterior1st + MasVnrArea + ExterCond + Foundation + BsmtQual + 
                   BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + 
                   HeatingQC + CentralAir + log(X1stFlrSF) + X2ndFlrSF + log(GrLivArea) + 
                   BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                   Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
                   GarageCond + WoodDeckSF + EnclosedPorch + ScreenPorch + PoolArea + 
                   PoolQC + SaleType + SaleCondition, data=subtrain)
predicted.prices10<-exp(predict(log.test.gr5,testv2))

write.csv(predicted.prices10, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v7.csv")