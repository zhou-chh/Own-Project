setwd("E:/C/Documents/data/Own project")
##Loading the packages
# data wragling
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org") #dataframe manipulation
if(!require(varhandle)) install.packages("varhandle", repos = "http://cran.us.r-project.org") #inspect missing data

#data discription
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
#data visualization
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org") # to make correlation plots
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org") # to  thems 
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org") #to ensure labels don’t fall on top of each other. 
#main package for machine learning algorithms
if(!require(caret)) install.packages("caret", repos = "httpggthemes://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org") 
#working with functions, e.g. using map_df function to pick k in knn
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")

##Importing the data
fall <- read.csv("falldeteciton.csv") %>% mutate(ACTIVITY = as.factor(ACTIVITY))
levels(fall$ACTIVITY) <- c("Standing", "Walking", "Sitting", "Falling", "Cramps", "Running")
summary(fall) #summary of the data
names(fall) # the top 6rows of the data

# Validation set will be 20% data
set.seed(2020)
test_index <- createDataPartition(y = fall$ACTIVITY, times = 1, p = 0.2, list = FALSE)
trainset <- fall[-test_index,]
validset <- fall[test_index,]
summary(validset)

#2. data visualization
if(!require(graphics)) install.packages("graphics", repos = "http://cran.us.r-project.org")
# 2.1 distribution of each variable
#2.1.1 distribution of activity type
trainset %>% 
  ggplot(aes(ACTIVITY)) +
  geom_bar(color="cornflowerblue", fill="darkblue")+   
  ggtitle("Distribution of Acitivity") +
  labs(y = "Counts", x = "Activity Type") 
  
#2.1.2 histogram of TIME
trainset%>% 
  ggplot(aes(TIME)) + 
  geom_histogram(color="cornflowerblue", fill="darkblue") +
  scale_x_log10() +
  ggtitle("Distribution of Time") +
  labs(y = "Counts", x = "Time Value") 

#2.1.3 histogram of SL
trainset%>% 
  ggplot(aes(SL)) + 
  geom_histogram(color="cornflowerblue", fill="darkblue") +
  scale_x_log10() +
  ggtitle("Distribution of Sugar Level") +
  labs(y = "Counts", x ="Sugar Level") 
#2.1.4 histogram of EEG
trainset%>% 
  ggplot(aes(EEG)) + 
  geom_histogram(color="cornflowerblue", fill="darkblue") +
  scale_x_log10() +
  ggtitle("Distribution of EEG Rate") +
  labs(y = "Counts", x = "EEG Rate") 
#2.1.5 histogram of Blood Pressure
trainset%>% 
  ggplot(aes(BP)) + 
  geom_histogram(color="cornflowerblue", fill="darkblue") +
  scale_x_log10() +
  ggtitle("Distribution of Blood Pressure") +
  labs(y = "Counts", x = "Blood Pressure Level") 
#2.1.6 histogram of Heart Rate
trainset%>% 
  ggplot(aes(HR)) + 
  geom_histogram(color="cornflowerblue", fill="darkblue") +
  scale_x_log10() +
  ggtitle("Distribution of Heart Rate") +
  labs(y ="Counts", x =  "Heart Rate") 
#2.1.7 histogram of Blood Circulation
trainset%>% 
  ggplot(aes(CIRCLUATION)) + 
  geom_histogram(color="cornflowerblue", fill="darkblue") +
  scale_x_log10() +
  ggtitle("Distribution of Blood Circulation") +
  labs(y = "Counts", x = "Blood Circulation Rate") 

##2.2 plot the correlation between variables
#2.2.1 TIME VS activity
#boxplot
trainset%>% mutate(TIME_=TIME/1000) %>%
  ggplot(aes(ACTIVITY, TIME_)) + 
  labs(y = "Time (10^3s)", x = "Activity") +
  geom_boxplot()
#bar plot
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # group data by activity 
  summarise(M_TIME = mean(TIME)) %>% # mean time
  ggplot(aes(x = ACTIVITY, y = M_TIME)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  labs(y = "Time (s)", x = "Activity") +
  ggtitle("Time VS Activity") 
#histogram
trainset %>% ggplot(aes(x = TIME, fill = ACTIVITY)) +
  geom_histogram(bins = 200) +
  ggtitle("Time") +
  facet_wrap(~ACTIVITY, ncol = 2) 

#2.2.2 Sugar Level VS activity
#boxplot
trainset%>% 
  mutate(SL_=SL/100000)%>%
  ggplot(aes(ACTIVITY, SL_)) +
  labs(y = "Sugar Level (10^5mg/dL)", x = "Activity") +
  geom_boxplot()
#bar plot
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # group data by activity 
  summarise(M_SL = mean(SL)/1000) %>% # mean SL
  ggplot(aes(x = ACTIVITY, y = M_SL)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  labs(y = "Sugar Level (10^3mg/dL)", x = "Activity") +
  ggtitle("Sugar Level  VS Activity") 
#histogram
trainset %>%  mutate(SL=SL/100000)%>%
  ggplot(aes(x = SL, fill = ACTIVITY)) +
  geom_histogram(bins = 200) +
  ggtitle("Sugar Level") +
  labs(x = "Sugar Level (10^5mg/dL)")+
  facet_wrap(~ACTIVITY, ncol = 2) 

#2.2.3 EEG VS activity
#boxplot
trainset%>% 
  mutate(EEG_ = EEG*(-10^-6)) %>%
  ggplot(aes(ACTIVITY, EEG_)) +
  labs(y = "EEG (-10^6mV)", x = "Activity") +
  geom_boxplot()
#BAR PLOT
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # group data by activity 
  summarise(M_EEG = mean(EEG*(-1))) %>% # mean EEG
  ggplot(aes(x = ACTIVITY, y = M_EEG)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  labs(y = "EEG (-mV)", x = "Activity") +
  ggtitle("EEG  VS Activity") 
#histogram
trainset %>% ggplot(aes(x = EEG, fill = ACTIVITY)) +
  geom_histogram(bins = 200) +
  ggtitle("EEG") +
  facet_wrap(~ACTIVITY, ncol = 2) 

#2.2.4 Blood Pressure VS activity
#boxplot
trainset%>% 
  ggplot(aes(ACTIVITY, BP)) +
  geom_boxplot()
#barplot (with error bar)
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # group data by activity 
  summarise(M_BP = mean(BP), sd = sd(BP) ) %>% # count
  ggplot(aes(x = ACTIVITY, y = M_BP)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  geom_errorbar(aes(ymin=M_BP-sd, ymax=M_BP+sd),
                width=0.4, colour="darkblue", alpha=0.9, size=1)+
  labs(y = "Blood Pressure", x = "Activity") +
  ggtitle("Blood Pressure VS Activity")
#histogram
trainset %>% ggplot(aes(x = BP, fill = ACTIVITY)) +
  geom_histogram(bins = 200) +
  ggtitle("Blood Pressure") +
  facet_wrap(~ACTIVITY, ncol = 2) 

#2.2.5 Heart Rate VS activity
#boxplot
trainset%>% 
  ggplot(aes(ACTIVITY, HR)) +
  geom_boxplot()
#barplot (with error bar)
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # gHRroup data by activity 
  summarise(M_HR = mean(HR), sd = sd(HR)) %>% # count
  ggplot(aes(x = ACTIVITY, y = M_HR)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  geom_errorbar(aes(ymin=M_HR-sd, ymax=M_HR+sd), 
                width=0.4, colour="darkblue", alpha=0.9, size=1)+
  labs(y = "Heart Rate", x = "Activity") +
  ggtitle("Heart Rate VS Activity")
#histogram
trainset %>% ggplot(aes(x = HR, fill = ACTIVITY)) +
  geom_histogram(bins = 200) +
  ggtitle("Heart Rate") +
  facet_wrap(~ACTIVITY, ncol = 2) 

#2.2.6 circulation VS activity
#boxplot
trainset%>% 
  ggplot(aes(ACTIVITY, CIRCLUATION)) +
  geom_boxplot()
#barplot (with error bar)
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # group data by activity 
  summarise(M_CIRCLUATION = mean(CIRCLUATION), sd = sd(CIRCLUATION) ) %>% # count
  ggplot(aes(x = ACTIVITY, y = M_CIRCLUATION)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  geom_errorbar(aes(ymin=M_CIRCLUATION-sd, ymax=M_CIRCLUATION+sd), 
                width=0.4, colour="darkblue", alpha=0.9, size=1)+
  labs(y = "Circulation", x = "Activity") +
  ggtitle("Circulation VS Activity")
#barplot (without error bar)
trainset %>%
  na.omit() %>%
  group_by(ACTIVITY) %>% # group data by year 
  summarise(M_CIRCLUATION = mean(CIRCLUATION), sd = sd(CIRCLUATION) ) %>% # count
  ggplot(aes(x = ACTIVITY, y = M_CIRCLUATION)) +
  geom_bar(stat = "identity", fill="cornflowerblue",color="darkblue") + 
  labs(y = "Circulation", x = "Activity") +
  ggtitle("Circulation VS Activity") 
#histogram
trainset %>% mutate(CIRCLUATION = CIRCLUATION/1000) %>%
  ggplot(aes(x = CIRCLUATION, fill = ACTIVITY)) +
  geom_histogram(bins = 200) +
  ggtitle("Circulation ") +
  labs(x = "Circulation (*10^3)") +
  facet_wrap(~ACTIVITY, ncol = 2) 

#2.2.7 CORRELTION plot of variables
trainset %>% gather(Indicator, Value, -ACTIVITY) %>%
  ggplot(aes(ACTIVITY, Value, fill = ACTIVITY)) +
  geom_boxplot() +
  facet_wrap(~Indicator, scales = "free", ncol = col) +
  theme(axis.text.x = element_blank(), legend.position="bottom")

library(corrplot)
library(RColorBrewer)
pairs(trainset)
correlation <- cor(trainset[,2:7])
correlation
corrplot.mixed(correlation, lower.col = "black",upper = "ellipse", 
               order = "alphabet", number.cex = 1,  
               tl.col = "black", sig.level = .05, insig = "blank")  

#3 Machine Learning (predicting FALL)
#Activity type: "0=Standing", "1=Walking", "2=Sitting", "3=Falling","4=Cramps", "5=Running")
#3.1.1 knn1 OVERALL .6562977 (FALLING .7686)
train_knn <- train(ACTIVITY ~ ., method = "knn", data = trainset)
y_hat_knn <- predict(train_knn, validset, type = "raw")
confusionMatrix(y_hat_knn, validset$ACTIVITY)$overall["Accuracy"]
ggplot(train_knn, highlight =  TRUE)
#3.1.2 KNN MODEL2 OVERALL .6602623  (FALLING .7652)
set.seed(2020)
index <- sample(nrow(trainset), n)
control <- trainControl(method = "cv", number = 10, p = .9)
train_cv <- train(ACTIVITY ~ . ,method = 'knn', data = trainset,
                   tuneGrid = data.frame(k = seq(3,21,.5)),
                   trControl = control)
y_hat_cv <- predict(train_cv, validset, type = "raw")
confusionMatrix(y_hat_cv, validset$ACTIVITY)$overall["Accuracy"]
ggplot(train_cv, highlight =  TRUE)
train_cv$bestTune #k= 4.5
train_cv$finalModel

#3.1.3 KNN MODEL3. 
#knn3 OVERALL ACCURACY .6556877 (FALLING .7647)
ks <- seq(3,21,.5)

accuracy <- map_df(ks, function(K){
  fit <- knn3(ACTIVITY ~ ., data = trainset, k = K)
  
  y_hat <- predict(fit, trainset, type = "class")
  cm_train <- confusionMatrix(y_hat, trainset$ACTIVITY)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, validset, type = "class")
  cm_test <- confusionMatrix(y_hat,validset$ACTIVITY)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
  
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, validset$ACTIVITY),
       TPR = sensitivity(y_hat, validset$ACTIVITY))
})

ks[which.max(accuracy$test)] #pick the k maximize the accuracy k=4.5 
max(accuracy$test) #display the max OVERALL accuracy 
fit <- knn3(ACTIVITY ~ ., data = trainset, k = 4.5)
y_hat <- predict(fit, validset, type = "class")
confusionMatrix(y_hat,validset$ACTIVITY)$overall["Accuracy"]
confusionMatrix(y_hat,validset$ACTIVITY)

#3.2 random forest ACCURACY = .7770662
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
y<-as.factor(trainset$ACTIVITY)
x<-as.matrix(trainset[,2:7])
y.v<- as.factor(validset$ACTIVITY)
x.v<-validset[,2:7]
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
sds <- colSds(x)
qplot(sds, bins = 256)

nzv <- nearZeroVar(x)
col_index <- setdiff(1:ncol(x), nzv)
length(col_index)
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <-  train(x[, col_index], y, 
                   method = "rf", 
                   ntree = 150,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 5000)
rf <- randomForest(x, y)
pre<- predict(rf, x.v)
confusionMatrix(pre,y.v)$overall["Accuracy"] #.8648154 
confusionMatrix(pre,y.v)$byClass[7] #F1=.9813322

ggplot(train_rf, highlight=TRUE)
train_rf$bestTune #mtry=1


#roc PLOT FOR RANDOM FOREST
if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org")
# Perform training:
rf_classifier = randomForest(ACTIVITY ~ ., data=trainset,
                             ntree=100, mtry=1, importance=TRUE)
varImpPlot(rf_classifier)
rf_classifier
# Validation set assessment #1: looking at confusion matrix
prediction_for_table <- predict(rf_classifier, validset[,-1])
table(validset$ACTIVITY,predicted=prediction_for_table)
# Calculate the probability of new observations belonging to each class
# prediction_for_roc_curve will be a matrix with dimensions data_set_size x number_of_classes
prediction_for_roc_curve <- predict(rf_classifier,validset[,-1],type="prob")
# Use pretty colours:
pretty_colours <- c("blue","red","green", "yellow","pink", "purple")
# Specify the different classes 
classes <- levels(validset[,1])
# For each class
for (i in 1:6){
  # Define which observations belong to class[i]
  true_values <- ifelse(validset[,1]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen (FALLING .9597418)
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}
