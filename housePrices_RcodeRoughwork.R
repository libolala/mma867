
housetrain.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\train.csv", header=TRUE, sep=",")
head(housetrain.data)
str(housetrain.dathousetest.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\test.csv", header=TRUE, sep=",")a)

is.na(housetrain.data)
housetrain.data[!complete.cases(housetrain.data),]
summary(housetrain.data)
summary(housetest.data)+
housetrain1.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\train1.csv", header=TRUE, sep=",")
testrun1<-lm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
            +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
              +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
            +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
              GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
              Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
              OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
              YrSold+ SaleType+SaleCondition , data=housetrain1.data)
summary(testrun1)
housetrain2.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\train2.csv", header=TRUE, sep=",")

testrun2<-lm(SalePrice~MSSubClass+MSZoning+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
               GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
               YrSold+ SaleType+SaleCondition , data=housetrain2.data)
summary(testrun2)

par(mfrow=c(1,4)) # this command sets the plot window to show 1 row of 4 plots
plot(testrun2)


testrun3<-lm(SalePrice~MSSubClass+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
               GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
               YrSold+ SaleType+SaleCondition , data=housetrain2.data)
summary(testrun3)

test.log<-lm(log(SalePrice)~log(MSSubClass)+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
               GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
               YrSold+ SaleType+SaleCondition , data=housetrain2.data)
summary(test.log)
plot(test.log)
(housetrain2.data)
house.data.testing<-subset(housetrain2.data, Id>=1001)
house.data.training<-subset(housetrain2.data, Id<=1000)

predicted.Hprices.testing<-predict(testrun3, house.data.testing)


percent.errors.H <- abs((house.data.testing$SalePrice-predicted.Hprices.testing)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.H) 

predicted.Hprices.testing1<-exp(predict(test.log, house.data.testing))


percent.errors.H1 <- abs((house.data.testing$SalePrice-predicted.Hprices.testing1)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.H1) 


test.log3<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                       +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                         +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                       +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
                         log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                         Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                         OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
                         YrSold+ SaleType+SaleCondition, data=housetrain2.data)

predicted.Hprices.testing5<-exp(predict(test.log3, house.data.testing))


percent.errors.H5 <- abs((house.data.testing$SalePrice-predicted.Hprices.testing5)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.H5) 



test.log.n<-lm(log(SalePrice)~log(MSSubClass)+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
               GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
               YrSold+ SaleType+SaleCondition , data=housetrain2.data)

predicted.Hprices.testing.n<-exp(predict(test.log.n, house.data.testing))


percent.errors.n <- abs((house.data.testing$SalePrice-predicted.Hprices.testing.n)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.n)

housetest.data.new<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\test.csv", header=TRUE, sep=",")
summary(housetest.data.new)

predicted.prices<-exp(predict(test.log.n, housetest.data.new))
summary(preditcted.price)


housetest.data.new3<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\test1.csv", header=TRUE, sep=",")
summary(housetest.data.new1)



predicted.prices<-exp(predict(test.log.n, housetest.data.new1))
summary(preditcted.prices)

test.log.n1<-lm(log(SalePrice)~log(MSSubClass)+MSZoning+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                  +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF +X2ndFlrSF + LowQualFinSF +
                  GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                  Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                  OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+
                  YrSold+ SaleType+SaleCondition , data=housetrain2.data)

predicted.Hprices.testing.n1<-exp(predict(test.log.n1, house.data.testing))


percent.errors.n1 <- abs((house.data.testing$SalePrice-predicted.Hprices.testing.n1)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.n1)


predicted.prices<-exp(predict(test.log.n1, housetest.data.new1))
summary(preditcted.prices)



test.log.n2<-lm(log(SalePrice)~MSSubClass+MSZoning+log(LotFrontage)+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
               +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                 +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
               +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+log(X2ndFlrSF) + LowQualFinSF +
                 log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                 Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +log(GarageArea) +GarageQual+ GarageCond+ PavedDrive +log(WoodDeckSF)+
                log(OpenPorchSf)+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
               data=housetrain2.data)
housetrain3.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\train2.csv", header=TRUE, sep=",")
test.log.n3<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                  +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+X2ndFlrSF + LowQualFinSF +
                  log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                  Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                  OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
                data=housetrain3.data)
str(housetrain3.data)
summary(housetrain3.data)

house.data.testing2<-subset(housetrain3.data, Id>=1001)
house.data.training2<-subset(housetrain3.data, Id<=1000)

predicted.prices.test.log5<-exp(predict(test.log.n3, house.data.testing2))
percent.errors.n5 <- abs((house.data.testing$SalePrice-predicted.prices.test.log5)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.n5)


#predicted.prices1<-exp(predict(test.log.n2, housetest.data.new1))
summary(test.log.n2)
str(housetrain3.data)

housetrain3.data$MSSubClass=as.factor(as.integer(housetrain3.data$MSSubClass))


test.n3<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+LotArea+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                  +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF+X2ndFlrSF + LowQualFinSF +
                  GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                  Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                  OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
                data=housetrain3.data)
predicted.prices.test5<-exp(predict(test.n3, house.data.testing2))
percent.errors.n6 <- abs((house.data.testing$SalePrice-predicted.prices.test5)/house.data.testing$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.n6)


test.log.n3<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                  +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+X2ndFlrSF + LowQualFinSF +
                  log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                  Fireplaces +FireplaceQu+ GarageType+GarageYrBlt+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                  OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
                data=housetrain3.data)

predicted.prices1<-exp(predict(test.log.n3, housetest.data.new3))
str(housetest.data.new1)
housetest.data.new3$MSSubClass=as.factor(as.integer(housetest.data.new3$MSSubClass))
housetest.data.new3$MasVnrArea=as.integer(as.character(housetest.data.new3$MasVnrArea))
summary(housetest.data.new3$Neighborhood)
summary(housetrain3.data$Neighborhood)


housetrain4.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\train2.csv", header=TRUE, sep=",")

housetest4.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\test1.csv", header=TRUE, sep=",")


housetrain4.data$MSSubClass=as.factor(as.integer(housetrain4.data$MSSubClass))
housetrain4.data$MasVnrArea=as.integer(as.character(housetrain4.data$MasVnrArea))


housetest4.data$MSSubClass=as.factor(as.integer(housetest4.data$MSSubClass))
housetest4.data$MasVnrArea=as.integer(as.character(housetest4.data$MasVnrArea))

test.log.n4<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                  +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+X2ndFlrSF + LowQualFinSF +
                  log(GrLivArea)+ BsmtFullBatas.character()lfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                  Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                  OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
                data=housetrain4.data)

housetest4.data$LotFrontages=as.integer(as.character(housetest4.data$LotFrontage))
housetest4.data$BsmtFinSF1=as.integer(as.characteras.character()data$BsmtFinSF1))
housetest4.data$BsmtFinSF2=as.integer(mas.character(housetest4.data$BsmtFinSF2))
housetest4.data$BsmtUnfSF=as.integer(as.character(housetest4.data$BsmtUnfSF))
Summary(housetest4.data$LotFroas.character

housetest4.data$LotFrontage=as.integer(as.character(housetest4.data$LotFrontage))

predicted.prices4<-exp(predict(test.log.n4, housetest4.data))
str(housetrain4.data)
str(housetest4.data)
library(dplyr)
housetest4.data<-select(housetest4.data,housetest4.data$LotFrontages)

write.csv(housetest4.data, file ="C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\test2.csv")
housetest5.data<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\test2.csv", header=TRUE, sep=",")
str(housetest5.data)
housetest5.data$MSSubClass=as.factor(as.integer(housetest5.data$MSSubClass))
predicted.prices4<-exp(predict(test.log.n4, housetest5.data))

write.csv(predicted.prices4, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\housePrice.csv")
housetest.datasmall<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\smalltest.csv", header=TRUE, sep=",")

predicted.prices5<-exp(predict(test.log.n4,housetest.datasmall))
str(housetest.datasmall)
housetest.datasmall$MSSubClass=as.factor(as.integer(housetest.datasmall$MSSubClass))

write.csv(predicted.prices5, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\problemset.csv")
str(housetest4.data)
predicted.prices6<-exp(predict(test.log.n4, housetest5.data))
write.csv(predicted.prices6, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\housePricenew.csv")
summary(housetest4.data)
str(housetest5.data)
write.csv(housetest5.data, file ="C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TestF.csv")
str(housetrain4.data)
write.csv(housetrain4.data, file ="C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TrainF.csv")

train<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TrainF.csv", header=TRUE, sep=",")
test<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TestF.csv", header=TRUE, sep=",")
subtest<-subset(train, Id>=1001)
subtrain<-subset(train, Id<=1000)
log.test<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+X2ndFlrSF + LowQualFinSF +
               log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
             data=train)

predicted.1<-exp(predict(log.test, subtest))
percent.errors.check <- abs((subtest$SalePrice-predicted.1)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check)#6.012388


log.test1<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF+X2ndFlrSF + LowQualFinSF +
               log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
             data=train)
predicted.2<-exp(predict(log.test1, subtest))
percent.errors.check1 <- abs((subtest$SalePrice-predicted.2)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check1) # 6.044634

log.test2<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)*log(GrLivArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
              +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
              +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF+X2ndFlrSF + LowQualFinSF +
                 BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition,
              data=train)
predicted.3<-exp(predict(log.test2, subtest))
percent.errors.check2 <- abs((subtest$SalePrice-predicted.3)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check2)#6.046492

#change grbuiltyear
test1<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TestFr.csv", header=TRUE, sep=",")
log.test.gr<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
             +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
               +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
             +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+X2ndFlrSF + LowQualFinSF +
               log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
               Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
               OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition+
               GarageYrBlt,
             data=train)

predicted.4<-exp(predict(log.test.gr, subtest))
percent.errors.check3 <- abs((subtest$SalePrice-predicted.4)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check3) #5.5564

str(test1)
predicted.prices5<-exp(predict(log.test.gr,test1))


write.csv(predicted.prices5, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v2.csv")
smallv2<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\smalltestv2.csv", header=TRUE, sep=",")
str(smallv2)
smallv2$GarageYrBlt=as.factor(as.integer(smallv2$GarageYrBlt))
predicted.pricessmallv2<-exp(predict(log.test.gr,smallv2))
write.csv(predicted.pricessmallv2, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\problemset_v2.csv")
summary(test1$Utilities)
testv2<-read.csv("C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\TestFrv2.csv", header=TRUE, sep=",")
str(testv2)

testv2$MSSubClass=as.factor(as.integer(testv2$MSSubClass))

predicted.prices6<-exp(predict(log.test.gr,testv2))
write.csv(predicted.prices6, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v3.csv")
str(train)


train$MSSubClass=as.factor(as.integer(train$MSSubClass))

log.test.gr1<-lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                  +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF )+X2ndFlrSF + LowQualFinSF +
                  log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                  Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                  OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition+
                  GarageYrBlt,
                data=train)
predicted.prices6<-exp(predict(log.test.gr1,testv2))
write.csv(predicted.prices6, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v3.csv")
subtest$MSSubClass=as.factor(as.integer(subtest$MSSubClass))
predicted.5<-exp(predict(log.test.gr1, subtest))
percent.errors.check4<- abs((subtest$SalePrice-predicted.5)/subtest$SalePrice)*100 #calculate absolute percentage errors
mean(percent.errors.check4) #5.515891

fit.log.step.b<-step(lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                        +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                          +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                        +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF+X2ndFlrSF + LowQualFinSF +
                          GrLivArea+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                          Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                          OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition+
                          GarageYrBlt, data=train), direction="backward")

log.test.gr2<-lm(log(SalePrice) ~ MSSubClass + MSZoning + log(LotArea) + Street + 
                   LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                   Exterior1st + MasVnrArea + ExterCond + Foundation + BsmtQual + 
                   BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + 
                   HeatingQC + CentralAir + X1stFlrSF + X2ndFlrSF + LowQualFinSF + 
                   BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                   Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
                   GarageCond + WoodDeckSF + EnclosedPorch + X3SsnPorch + ScreenPorch + 
                   PoolArea + PoolQC + SaleType + SaleCondition, data=train)
predicted.prices7<-exp(predict(log.test.gr2,testv2))

write.csv(predicted.prices7, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v4.csv")

str(train)


fit.log.step.b<-step(lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                        +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                          +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                        +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ X1stFlrSF+X2ndFlrSF + LowQualFinSF +
                          log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                          Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                          OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition+
                          GarageYrBlt, data=train), direction="backward")



log.test.gr3<-lm(log(SalePrice) ~ MSSubClass + MSZoning + log(LotArea) + Street + 
                   Utilities + LotConfig + LandSlope + Neighborhood + Condition1 + 
                   Condition2 + OverallQual + OverallCond + YearBuilt + YearRemodAdd + 
                   RoofMatl + Exterior1st + MasVnrArea + ExterCond + Foundation + 
                   BsmtQual + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtFinSF2 + 
                   BsmtUnfSF + Heating + HeatingQC + CentralAir + X2ndFlrSF + 
                   log(GrLivArea) + BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + 
                   KitchenQual + Functional + Fireplaces + GarageCars + GarageArea + 
                   GarageQual + GarageCond + WoodDeckSF + EnclosedPorch + ScreenPorch + 
                   PoolArea + PoolQC + SaleType + SaleCondition, data=train)
predicted.prices8<-exp(predict(log.test.gr3,testv2))

write.csv(predicted.prices8, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v5.csv")

fit.log.step.b2<-step(lm(log(SalePrice)~MSSubClass+MSZoning+LotFrontage+log(LotArea)+ Street+ Alley + LotShape+ LandContour+ Utilities+ LotConfig+LandSlope
                        +Neighborhood+Condition1 + Condition2 +BldgType+HouseStyle+OverallQual+ OverallCond+YearBuilt +YearRemodAdd+RoofStyle +RoofMatl+
                          +  Exterior1st +Exterior2nd + MasVnrType+ MasVnrArea+ ExterQual+ ExterCond+  Foundation + BsmtQual+ BsmtCond +BsmtExposure +BsmtFinType1 + BsmtFinSF1
                        +BsmtFinType2+ BsmtFinSF2 +BsmtUnfSF + TotalBsmtSF+ Heating + HeatingQC +CentralAir+ Electrical+ log(X1stFlrSF)+X2ndFlrSF + LowQualFinSF +
                          log(GrLivArea)+ BsmtFullBath+ BsmtHalfBath +FullBath + HalfBath + BedroomAbvGr+ KitchenAbvGr + KitchenQual+TotRmsAbvGrd+Functional+
                          Fireplaces +FireplaceQu+ GarageType+ GarageFinish +GarageCars +GarageArea +GarageQual+ GarageCond+ PavedDrive +WoodDeckSF+
                          OpenPorchSF+ EnclosedPorch + X3SsnPorch + ScreenPorch  + PoolArea + PoolQC + Fence +MiscFeature+ MiscVal+ MoSold+YrSold+SaleType+SaleCondition+
                          GarageYrBlt, data=train), direction="backward")

log.test.gr4<-lm(log(SalePrice) ~  MSZoning + log(LotArea) + Street + Utilities + 
                   LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                   Exterior1st + MasVnrArea + ExterCond + Foundation + BsmtQual + 
                   BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + 
                   HeatingQC + CentralAir + log(X1stFlrSF) + X2ndFlrSF + log(GrLivArea) + 
                   BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                   Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
                   GarageCond + WoodDeckSF + EnclosedPorch + ScreenPorch + PoolArea + 
                   PoolQC + SaleType + SaleCondition, data=train)
predicted.prices9<-exp(predict(log.test.gr4,testv2))

write.csv(predicted.prices9, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v6.csv")


log.test.gr5<-lm(log(SalePrice) ~ MSSubClass+ MSZoning + log(LotArea) + Street + Utilities + 
                   LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
                   OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                   Exterior1st + MasVnrArea + ExterCond + Foundation + BsmtQual + 
                   BsmtExposure + BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + Heating + 
                   HeatingQC + CentralAir + log(X1stFlrSF) + X2ndFlrSF + log(GrLivArea) + 
                   BsmtFullBath + FullBath + HalfBath + KitchenAbvGr + KitchenQual + 
                   Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
                   GarageCond + WoodDeckSF + EnclosedPorch + ScreenPorch + PoolArea + 
                   PoolQC + SaleType + SaleCondition, data=train)
predicted.prices10<-exp(predict(log.test.gr5,testv2))

write.csv(predicted.prices10, file = "C:\\Users\\Libo\\Documents\\MMA\\867\\ass1\\HousePrice_v7.csv")