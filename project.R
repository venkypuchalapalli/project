#Setting the working directory'
setwd("D:/project work")

#Loading data into Workspace
test <- read.csv(file = "Test.csv", header = T)
train <- read.csv(file = "Train.csv", header = T)
dim(test)
dim(train)
summary(test)
class(test)
alg<- cbind.data.frame(test$Item_Identifier, test$Outlet_Identifier)
alg <- data.frame(alg)
class(alg)
#outlier analysis
library(dplyr)
summary(train$Item_Outlet_Sales)
hist(train$Item_Outlet_Sales, breaks = 20)
boxplot(train$Item_Outlet_Sales,horizontal = T)
#IQR Q3-Q1
IQR <- 3101.00 - 834.20
#upper limit <- Q3 + 1.5 *IQR
upper_limit <- 3101.00 + 1.5 * IQR
#Lower limit [Q1 - 1.5 * IQR]
lower_limit <- 834 - 1.5 * IQR
processed_train <- train %>% filter(Item_Outlet_Sales >= 0)
processed_train <- train %>% filter(Item_Outlet_Sales <= 6501.2)
boxplot(processed_train$Item_Outlet_Sales,horizontal = T)
#Filling dummy values in the test set adding test to train to do data preprocessing
test$Item_Outlet_Sales <- 0
df <- rbind(processed_train, test)

#Total number of categorical varibles
sapply(df, function(x) length(unique(x)))

#dealing with the Item fat content
levels(df$Item_Fat_Content)
df$Item_Fat_Content[df$Item_Fat_Content == 'LF'] <- 'Low Fat'
df$Item_Fat_Content[df$Item_Fat_Content == 'low fat'] <- 'Low Fat'
df$Item_Fat_Content[df$Item_Fat_Content == 'reg'] <- 'Regular'

#Fat levels for each Item type
itemfat <- as.data.frame(setNames(
aggregate(
  df$Item_Fat_Content,
  by = list(Category = df$Item_Type,
            Category = df$Item_Fat_Content),
  FUN = length),
c('Item_Type', 'Item_Fat_Content','num')
))
print(itemfat)


#health and hygiene, housegold, Others are not necessarily food Items
#So let's add another level of fat which is none.
levels(df$Item_Fat_Content) <- c(levels(df$Item_Fat_Content), 'None')
df[ which(df$Item_Type == "Health and Hygiene") ,]$Item_Fat_Content <- "None"
df[ which(df$Item_Type == "Household"), ]$Item_Fat_Content <- "None"
df[ which(df$Item_Type == "Others"), ]$Item_Fat_Content <- "None"
df$Item_Fat_Content <- factor(df$Item_Fat_Content)
str(df)

#now lets count the number of Item_Types with different fat
itemfat <- as.data.frame(setNames(
aggregate(
  df$Item_Fat_Content,
  by = list(Category = df$Item_Type,
            Category = df$Item_Fat_Content),
  FUN = length),
c('Item_Type', 'Item_Fat_Content','num')
))

print(itemfat)


#taking care of missing values
sapply(df, function(x) sum(is.na(x)))

library(ggplot2)
ggplot(df, aes(Item_Type, Item_Weight)) +
geom_boxplot()+
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) +
xlab("Item Type")+
ylab("Item Weight")+
ggtitle("Item Weight VS Item Type")
#this is cool
ggplot(df, aes(Outlet_Identifier, Item_Weight)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet_Identifier") + 
ylab("Item Weight") + 
ggtitle("Item Weight vs Outlet identifier")
#OUT018 and OUT019 didn't give any weight data
#Let us assume that Item identifier is giving us the unique Item
#Hence there sould be unique weight for wach Item
#Creating the mean data frame of the Item weights and Standard deviation
#data manuplating using dplyr and plyr
library(plyr)
WeightsByItem <- as.data.frame(ddply(na.omit(df),
                                   ~Item_Identifier,
                                   summarise,
                                   mean = mean(Item_Weight),
                                   sd = sd(Item_Weight)))

#We can use these values to fill in with the missing weights
df$Item_Weight <- ifelse(is.na(df$Item_Weight),
                       WeightsByItem$mean[
                         match(df$Item_Identifier, WeightsByItem$Item_Identifier)], df$Item_Weight)
table(is.na(df))

#lets check what we have now
ggplot(df, aes(Item_Type, Item_Weight)) +
geom_boxplot()+
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) +
xlab("Item Type")+
ylab("Item Weight")+
ggtitle("Item Weight VS Item Type")


ggplot(df, aes(Outlet_Identifier, Item_Weight)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet_Identifier") + 
ylab("Item Weight") + 
ggtitle("Item Weight vs Outlet identifier")
#Now we see all the Outet_items have weights



#Let's see what is the age of the Outlet till 2013
df$OutletAge <- 2013 - df$Outlet_Establishment_Year
df$Outlet_Establishment_Year <- NULL 



# how often does each Outlet_Identifier appear in the data
aggregate(df$Outlet_Identifier, by=list(Category=df$Outlet_Identifier), FUN=length)
aggregate(df$Item_Identifier, by=list(Category=df$Outlet_Identifier), FUN= length)


# these two Outlets have less Items compated to the others so we can say that
# they're less level of stores i.e, Grocery stores
# What else can we learn about the different types of shops?
# boxplot of  Sales vs. Outlet identifier
ggplot(df[1:nrow(train),], aes(Outlet_Identifier, Item_Outlet_Sales)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet identifier") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet identifier")

# boxplot of  Sales vs. Outlet Type
ggplot(df[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = OutletAge)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) +
xlab("Outlet Type") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet Type")

# Sales in the one type 2 supermarket appear a bit low,
# as one would expect them to be higher than in
# the type 1 supermarkets.
# Maybe it's because it's still fairly new, having
# been founded 4 years ago.

# boxplot of  Sales vs. Outlet Type as Outlet size
ggplot(df[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet type") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet type")

# count the number of others per Outlet_Identifier and Outlet_Type
otherShops <- as.data.frame( setNames(
aggregate(
  df$Outlet_Size, 
  by=list(Category=df$Outlet_Identifier, 
          Category=df$Outlet_Type,
          Category=df$Outlet_Location_Type,
          Category=df$Outlet_Size), 
  FUN= length),
c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

# a Grocery store certainly falls in the size category Small
# Type 1 supermarkets are most often classified as Small,
# i.e. the mode is "Small"
# Hence, we'll classify the two "Other" Type 1 supermarkets
# as "Small"
# so we only have to replace "Other" by "Small"
#levels(combi$Outlet_Size)[1] <- "Small"

# "OUT010" is clearly small
df[ which(df$Outlet_Identifier == "OUT010") ,]$Outlet_Size <- "Small"
# "OUT017" and "OUT045" could be small or medium
df[ which(df$Outlet_Identifier == "OUT017") ,]$Outlet_Size <- "Small"
df[ which(df$Outlet_Identifier == "OUT045") ,]$Outlet_Size <- "Small"

###count the number of others per Outlet_Identifier and Outlet_Type###
otherShops <- as.data.frame( setNames(
aggregate(
  df$Outlet_Size, 
  by=list(Category=df$Outlet_Identifier, 
          Category=df$Outlet_Type,
          Category=df$Outlet_Location_Type,
          Category=df$Outlet_Size), 
  FUN= length),
c("Outlet_Identifier","Outlet_Type", "Outlet_Location_Type", "Outlet_Size", "number")
))
otherShops

###dealing with zeros in Item visibility###
#using linear regression to replace the zeros in the Item_visibility
df_1 <- df %>% filter(Item_Visibility !=0)
Visibility_model <- lm(Item_Visibility ~ Item_Weight + Item_Fat_Content +
                         Item_Type + Item_MRP + OutletAge +
                         Outlet_Size + Outlet_Location_Type + Item_Outlet_Sales,
                       data = df_1)
df$Item_Visibility[df$Item_Visibility == 0] <- predict(Visibility_model,
                                                       newdata=df[df$Item_Visibility==0,])
#lets check what we have now
# apply factor to Outlet_Size in order to drop the now
# unused level "Other"
df$Outlet_Size <- factor(df$Outlet_Size)

str(df)

# boxplot of  Sales vs. Outlet location
ggplot(df[1:nrow(df),], aes(x = Outlet_Location_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet location") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet location")

# boxplot of  Sales vs. Outlet type
ggplot(df[1:nrow(df),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Outlet type") + 
ylab("Sales") + 
ggtitle("Sales vs Outlet type")

# boxplot of  Sales vs. Item type
ggplot(df[1:nrow(df),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Item type") + 
ylab("Sales") + 
ggtitle("Sales vs Item type")

# boxplot of  Sales vs. Item type
ggplot(df[1:nrow(df),], aes(x = Item_Type, y = Item_Outlet_Sales, fill = Outlet_Type)) +
geom_boxplot() +
theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black")) + 
xlab("Item type") + 
ylab("Sales") + 
ggtitle("Sales vs Item type")


###let's have a look at the numerical variables now###

###correlation between numerical variables###
corMatrix <- cor(df[1:nrow(processed_train),][sapply(df[1:nrow(processed_train),], is.numeric)])

corMatrix

avatrain <- df[1:nrow(processed_train),]
anova <- aov(avatrain$Item_Outlet_Sales ~ avatrain$Item_Weight +avatrain$Item_Fat_Content+ avatrain$Item_Visibility+
             avatrain$Item_Type+ avatrain$Item_MRP+ avatrain$Outlet_Size+
             avatrain$Outlet_Location_Type+ avatrain$Outlet_Type+
             avatrain$OutletAge)
summary(anova)

###As per anova test we found that Item_weight doesn't any significance

####dealing with the categories####
library(tidyr)
df$Outlet_Location_Type<- as.numeric(df$Outlet_Location_Type)
#Out_loc_Type <- spread(df,Outlet_Location_Type,Outlet_Location_Type)

#out_type <- spread(Out_loc_Type, Outlet_Type,Outlet_Type)
df$Outlet_Type <- as.numeric(df$Outlet_Type)

df$Item_Fat_Content <- as.numeric(df$Item_Fat_Content)
#ItemFat <- spread(out_type, Item_Fat_Content, Item_Fat_Content)

df$Item_Type <- as.numeric(df$Item_Type)
#ItemType <- spread(ItemFat, Item_Type, Item_Type)

df$Outlet_Size <- as.numeric(df$Outlet_Size)
#data  <- spread(ItemType, Outlet_Size, Outlet_Size)

main<- as.data.frame(df)
dependent <-main$Item_Outlet_Sales
dependent <- as.numeric(dependent)
main$Item_Outlet_Sales <- NULL
Main <- cbind.data.frame(main, dependent)

####Splitting again  'Turing Mode on'####
Ntrain <- Main[1:nrow(processed_train),]
Ntrain$Item_Identifier<- NULL
Ntrain$Outlet_Identifier <- NULL
Ntest <- Main[-(1:nrow(processed_train)), , drop=F]
Ntest$Item_Identifier<-NULL
Ntest$Outlet_Identifier <- NULL
Ntest$dependent <- NULL
Ntrain <- data.frame(Ntrain)
class(Ntrain)

####Data sampling####
set.seed(123)
index <- sample(1:nrow(Ntrain), round(0.8*nrow(Ntrain)))
train_fortrain <- Ntrain[index, ]
train_fortest <- Ntrain[-index, ]


####Multi linear regression ####
linearmodel <- glm(formula = dependent ~ Item_Fat_Content + Item_Visibility
                  + Item_Type + Item_MRP + Outlet_Size + Outlet_Location_Type+
                    + Outlet_Type + OutletAge, data = train_fortrain, family = gaussian(link="identity"))


Item_Outlet_Sales_Predict <- predict(linearmodel,train_fortest)

# library(caret)
# RMSE(pred= Item_Outlet_Sales_Predict, obs=train_fortest$dependent)
RMSE <- sqrt(sum((Item_Outlet_Sales_Predict - train_fortest$dependent)^2)/nrow(train_fortest))
RMSE## <1070.676538
Lm<-predict(linearmodel,Ntest)
ALG1 <- cbind.data.frame(alg, Lm)

#####Support vector regression#####
library(e1071)
regressor1 = svm(formula = dependent ~ Item_Weight + Item_Fat_Content + Item_Visibility
                 + Item_Type + Item_MRP + Outlet_Size + Outlet_Location_Type+
                   + Outlet_Type + OutletAge, data = train_fortrain,
                type = 'eps-regression')
SVM <- predict(regressor1, newdata = train_fortest)
#RMSE
RMSE<-sqrt(sum((SVM - train_fortest$dependent)^2)/nrow(train_fortest))
#986
SVMR<-predict(regressor1,Ntest)
ALG2 <- cbind.data.frame(alg, SVMR)

#####data scaling#####
#neuralnet

maxs <- apply(Ntrain, 2, max)
mins <-apply(Ntrain, 2, min)
scaled <- as.data.frame(scale(Ntrain, center= mins, scale = maxs - mins))
summary(scaled)
train_nn <- scaled[index,]
test_nn <- scaled[-index,]

# process in parallel
library(doParallel) 
cl <- makeCluster(detectCores(), type='PSOCK')
registerDoParallel(cl)

library(neuralnet)
n <- names(train_nn)
f <- as.formula(paste("dependent ~ ", paste(n[!n %in% "dependent"], collapse = " + ")))

nn <- neuralnet(f,data=train_nn,hidden=c(5,3),linear.output=T, stepmax = 1e+09)
nn$act.fct
nn$err.fct
nn$weights
#back to sequential processing
registerDoSEQ()
#nn with test_nn
s_pred_nn <- compute(nn, test_nn[,1:9])

pred_nn <- s_pred_nn$net.result*(max(Ntrain$dependent)-min(Ntrain$dependent)) + min(Ntrain$dependent)
test_nn_dependent <- test_nn$dependent*(max(Ntrain$dependent)-min(Ntrain$dependent)) + min(Ntrain$dependent)
RMSE <- sqrt(sum((pred_nn - test_nn_dependent)^2)/nrow(train_fortest))
RMSE ## <970

#This is looking cool ;)
#RMSE value of the neural net is 888.0496528
maxs_test<- apply(Ntest, 2, max)
mins_test<- apply(Ntest, 2, min)
scaled_test <- as.data.frame(scale(Ntest, center = mins_test, scale = maxs_test - mins_test))
summary(scaled_test)
f_test_pred <- compute(nn, scaled_test)
Neuralnet_pred <- f_test_pred$net.result*(max(Ntrain$dependent)-min(Ntrain$dependent)) + min(Ntrain$dependent)
ALG3 <- cbind.data.frame(alg, Neuralnet_pred )


##Exporting##
ALG1 <- as.data.frame(ALG1)
ALG2 <- as.data.frame(ALG2)
ALG3 <- as.data.frame(ALG3)
colnames(ALG1) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")
colnames(ALG2) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")
colnames(ALG3) <- c("Item_Identifier", "Outlet_Identifier", "Item_Outlet_Sales")
write.csv(ALG1, file = "alg1.csv", row.names = FALSE )
write.csv(ALG2, file = "alg2.csv", row.names = FALSE)
write.csv(ALG3, file = "alg3.csv", row.names = FALSE)

