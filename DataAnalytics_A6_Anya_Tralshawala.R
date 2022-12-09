library(ggplot2)

ems_data <- read.csv(file.choose(), header=TRUE, nrows=10000)
View(ems_data)
summary(ems_data)

#hist(ems_data$INITIAL_SEVERITY_LEVEL_CODE)
ggplot(ems_data, aes(x=INITIAL_SEVERITY_LEVEL_CODE)) + geom_histogram(binwidth=1, fill="light pink", color="dark blue") +
  labs(title="Histogram of Frequency per INITIAL_SEVERITY_LEVEL_CODE", x="INITIAL_SEVERITY_LEVEL_CODE", y="Frequency")

#hist(ems_data$FINAL_SEVERITY_LEVEL_CODE)
ggplot(ems_data, aes(x=FINAL_SEVERITY_LEVEL_CODE)) + geom_histogram(binwidth=1, fill="light pink", color="dark blue") +
  labs(title="Histogram of Frequency per FINAL_SEVERITY_LEVEL_CODE", x="FINAL_SEVERITY_LEVEL_CODE", y="Frequency")

#hist(ems_data$POLICEPRECINCT)
ggplot(ems_data, aes(x=POLICEPRECINCT)) + geom_histogram(binwidth=1, fill="pink", color="dark blue") +
    labs(title="Histogram of Frequency per POLICEPRECINCT", x="POLICEPRECINCT", y="Frequency")

summary(ems_data$INITIAL_SEVERITY_LEVEL_CODE)
summary(ems_data$FINAL_SEVERITY_LEVEL_CODE)

congress <- read.csv(file.choose(), header=TRUE)
View(congress)
head(congress)
summary(congress)

#plot(congress$CONGRESSIONALDISTRICT, congress$PoliticalPartyNumAssignment)

ems_data$POLICEPRECINCT <- as.factor(ems_data$POLICEPRECINCT)
ggplot(data = ems_data, aes(POLICEPRECINCT, INCIDENT_RESPONSE_SECONDS_QY)) + geom_boxplot()
wsummary(ems_data$INCIDENT_RESPONSE_SECONDS_QY)


# Multivariate Regression -------------------------
sample <- ems_data[sample(nrow(ems_data), 10000),]
model1 <- lm(INCIDENT_RESPONSE_SECONDS_QY ~ INITIAL_SEVERITY_LEVEL_CODE + FINAL_SEVERITY_LEVEL_CODE + INCIDENT_TRAVEL_TM_SECONDS_QY, data = sample)
summary(model1)

######################################################################################
#create subset and total combined dataset

ems.subset <- ems_data[c('INCIDENT_RESPONSE_SECONDS_QY', 'INITIAL_SEVERITY_LEVEL_CODE', 'FINAL_SEVERITY_LEVEL_CODE', 'INCIDENT_TRAVEL_TM_SECONDS_QY', 'POLICEPRECINCT', 'ZIPCODE','CONGRESSIONALDISTRICT', 'BOROUGH')]
head(ems.subset)
View(ems.subset)
summary(ems.subset)

total <- merge(ems.subset,congress,by="CONGRESSIONALDISTRICT")
View(total)

#response time and political party assignment
plot(total$PoliticalPartyNumAssignment,total$INCIDENT_RESPONSE_SECONDS_QY)

head(total)

total$INCIDENT_RESPONSE_SECONDS_QY = as.numeric(total$INCIDENT_RESPONSE_SECONDS_QY)
total <- total[!(is.na(total$INCIDENT_RESPONSE_SECONDS_QY)),]
total$INCIDENT_RESPONSE_SECONDS_QY

total$INITIAL_SEVERITY_LEVEL_CODE = as.numeric(total$INITIAL_SEVERITY_LEVEL_CODE)
total <- total[!(is.na(total$INITIAL_SEVERITY_LEVEL_CODE)),]
total$INITIAL_SEVERITY_LEVEL_CODE

total$FINAL_SEVERITY_LEVEL_CODE = as.numeric(total$FINAL_SEVERITY_LEVEL_CODE)
total <- total[!(is.na(total$FINAL_SEVERITY_LEVEL_CODE)),]
total$FINAL_SEVERITY_LEVEL_CODE

total$INCIDENT_TRAVEL_TM_SECONDS_QY = as.numeric(total$INCIDENT_TRAVEL_TM_SECONDS_QY)
total <- total[!(is.na(total$INCIDENT_TRAVEL_TM_SECONDS_QY)),]
total$INCIDENT_TRAVEL_TM_SECONDS_QY

total$POLICEPRECINCT = as.numeric(total$POLICEPRECINCT)
total <- total[!(is.na(total$POLICEPRECINCT)),]
total$POLICEPRECINCT

total$CONGRESSIONALDISTRICT = as.numeric(total$CONGRESSIONALDISTRICT)
total <- total[!(is.na(total$CONGRESSIONALDISTRICT)),]
total$CONGRESSIONALDISTRICT


plot1 <- ggplot(total, aes(INCIDENT_TRAVEL_TM_SECONDS_QY,INCIDENT_RESPONSE_SECONDS_QY,color=POLICEPRECINCT))
print(plot1 + geom_point(size=3))

plot2 <- ggplot(total, aes(CONGRESSIONALDISTRICT, PoliticalPartyNumAssignment, color=POLICEPRECINCT))
print(plot2 + geom_point(size=3))

plot3 <- ggplot(ems.subset, aes(CONGRESSIONALDISTRICT, INCIDENT_RESPONSE_SECONDS_QY, color=INCIDENT_TRAVEL_TM_SECONDS_QY))
print(plot3 + geom_point(size=3))

plot4 <- ggplot(total, aes(CONGRESSIONALDISTRICT,INCIDENT_RESPONSE_SECONDS_QY,color=BOROUGH))
print(plot4 + geom_point(size=3))

plot5 <- ggplot(total, aes(POLICEPRECINCT, INCIDENT_RESPONSE_SECONDS_QY, color=BOROUGH))
print(plot5 + geom_point(size=3))

plot5 <- ggplot(total, aes(ZIPCODE, PoliticalPartyNumAssignment, color=BOROUGH))
print(plot5 + geom_point(size=3))

total$responseSpeed <- cut(total$INCIDENT_RESPONSE_SECONDS_QY, br=c(3,300,900,22663), labels = c("fast", 'mid', 'slow'))
total$responseSpeed <- as.factor(total$responseSpeed)
total <- total[!(is.na(total$responseSpeed)),]

summary(total$responseSpeed)


######################################################################################
# K-means Clustering 
set.seed(101)
emsCluster <- kmeans(total[,1:6, 12], 3, nstart = 1) # nstart is the number of random start

emsCluster$cluster
total$INCIDENT_TRAVEL_TM_SECONDS_QY

table(emsCluster$cluster, total$INCIDENT_RESPONSE_SECONDS_QY)
# plotting the clusters
library(cluster)
clusplot(total,total$responseSpeed, color = TRUE, shade = TRUE, labels = 0, lines = 0)

######################################################################################
#prettier plot
library("ggplot2")
library("dplyr")
library("ggfortify")

summary(total[,1:6, 12])
head(total[,1:6, 12])


data <- total[,1:6, 12]

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}
wssplot(data)

kmean <- kmeans(data, 3)
kmean$centers

autoplot(kmean, data, frame = TRUE)



######################################################################################
#KNN ------------------------------------------------------------------

min <- min(total$INCIDENT_RESPONSE_SECONDS_QY)
min
max <- max(total$INCIDENT_RESPONSE_SECONDS_QY)
max
 
View(total)
total <- total[!(is.na(total$CONGRESSIONALDISTRICT)),]
total <- total[!(is.na(total$INCIDENT_RESPONSE_SECONDS_QY)),]
total <- total[!(is.na(total$INITIAL_SEVERITY_LEVEL_CODE)),]
total <- total[!(is.na(total$FINAL_SEVERITY_LEVEL_CODE)),]
total <- total[!(is.na(total$INCIDENT_TRAVEL_TM_SECONDS_QY)),]
total <- total[!(is.na(total$POLICEPRECINCT)),]
total <- total[!(is.na(total$PoliticalPartyNumAssignment)),]

# normalize the data using min max normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
total[,1:6] <- as.data.frame(lapply(total[,1:6], normalize))

ind <- sample(2, nrow(total), replace=TRUE, prob=c(0.7, 0.3))
#View(ind)
KNNtrain <- total[ind==1,]
KNNtest <- total[ind==2,]
KNNtestlabel <- total[ind==2, 16]
KNNtestlabel 

library(class)
KNNpred <- knn(train = KNNtrain[,1:6], test = KNNtest[,1:6], cl = KNNtrain$responseSpeed, k = 1)
KNNpred
table(KNNpred)
table(KNNpred, KNNtestlabel)

# error evaluation ----------------------------
missClassError <- mean(KNNtestlabel != KNNpred)
print(missClassError)

# Choosing the K value
# we can write a for-loop
KNNpred <- NULL
error_rate <- NULL

for (i in 1:10) {
  set.seed(101)
  KNNpred <- knn(train = KNNtrain[1:6], test = KNNtest[1:6], cl = KNNtrain$responseSpeed, k = i)
  error_rate[i] <- mean(KNNtestlabel != KNNpred)
}

print(error_rate)

# Plot the K value on a graph
library(ggplot2)
k_values <- 1:10

error_df <- data.frame(error_rate, k_values)
print(error_df)
ggplot(error_df,aes(k_values,error_rate)) + geom_point() + geom_line(lty='dotted', color='blue')





######################################################################################
# DID NOT USE FOR REPORT OR POSTER
# Decision Trees --
# Classification Tree
# Install the following libraries/packages
library(rpart)
library(rpart.plot)

dim(total)

# creating a sample from the total dataset
s_ems <- sample(150,100)
s_ems

# create testing and training sets
ems_train <- total[s_ems,]
ems_test <- total[-s_ems,]
dim(ems_train)
dim(ems_test)

# generate the decision tree model
decisionTreeModel <- rpart(total$responseSpeed~total$INITIAL_SEVERITY_LEVEL_CODE + total$CONGRESSIONALDISTRICT + total$PoliticalPartyNumAssignment + total$INCIDENT_RESPONSE_SECONDS_QY, ems_train, method = "class") # ~. includes remaining columns in data frame
decisionTreeModel

# plotting the decision tree model using 
par(mar = c(0.5, 0.5, 0.5, 0.5))
rpart.plot(decisionTreeModel)