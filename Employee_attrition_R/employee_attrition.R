rm(list = ls())
myLibraries<-c("scatterplot3d","caret",'mltools','ROSE',
               'randomForest','e1071','MLmetrics','pROC','ROCR','dplyr',
               'kernlab','naivebayes','yardstick','PRROC','tidyr','ggplot2',
               'lattice','repr','plyr','MASS','ggpubr','plotly','scales','rattle',
               'magrittr','rpart.plot','data.table')
library(pacman)
pacman::p_load(char=myLibraries,install=TRUE,character.only=TRUE)


#my constants
data.filename<-"10YearTerminationData.csv"
TYPE_DISCREET <- "Discreet"
TYPE_ORDINAL <- "Ordinal"
DiscreetBins <- 5
MAX_LITERALS      <- 50
cutoff <- 20
kfolds<-5



#user defined functions


#*****************************************************************
#
#NreadDataset() : Read the CSV file from working Directory
#
#Input :   string  -  file name  -  CSV file name
#
#Output :   data frame  -  Contents of the CSV file
#
#Reference - Dr.Nick Ryman-Tubb function from Lab 3
#
#*****************************************************************
NreadDataset<-function(filename){
  dataset<-read.csv(filename,encoding = 'UTF8',stringsAsFactors = F)
  names(dataset)<-gsub("[[:punct:][:blank:]]+", "", names(dataset))
  print(paste('CSV Dataset',filename,'has Records equal to',nrow(dataset)))
  return(dataset)
}
#*****************************************************************
# MissingValues() : Finding the missing values in the dataset
#
# Input:    data frame  -  dataset   
#
# Output 
#
#*****************************************************************
MissingValues <- function(dataset){
  if(any(is.na(dataset)) == FALSE){
    print('There are no missing values in the dataset')
  }
  else {
    print('There are missing values in the dataset')
  }
}

#*****************************************************************
# DataVisualization() : Exploratory Data Analysis
#
# Input:    data frame  -  dataset   
#
# Output 
#*****************************************************************
DataVisualization<- function(dataset){
  #output plot
  x<-ggplot(data = dataset,aes(x=as.factor(STATUS),fill = as.factor(STATUS))) + 
    geom_bar(aes(y=..count../sum(..count..)),color = 'black' )+ scale_y_continuous(labels = scales::percent)+ 
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y= ((..count..)/sum(..count..))), stat="count",vjust = -.25) + 
    labs(x="STATUS",y = "PERCENTAGE")+ ggtitle("TERMINATION")+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  # business unit & status(output)
  x<-ggplot(data = dataset,aes(x= as.factor(BUSINESSUNIT),y= ..count../sum(..count..),fill = as.factor(STATUS))) + 
    geom_bar(position = position_dodge()) + scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y= ((..count..)/sum(..count..))), stat="count",vjust = -0.25,position = position_dodge(0.9), check_overlap = TRUE) + 
    labs(x="BUSINESS_UNIT",y = "PERCENTAGE")+ ggtitle("Relationship between Business Unit and Status")+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  # age & status
  x<-ggplot(dataset,aes(x=age,color=as.factor(STATUS))) +
    scale_y_continuous(labels = scales::percent)+
    geom_density(position = 'identity',) + theme_grey()+
    labs(x="Age",y = "Termination Density")+
    ggtitle("Relationship between age and status")+theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  #length of service $status
  x<-ggplot(dataset,aes(x=lengthofservice,color=as.factor(STATUS))) +
    geom_density(position = 'identity')+scale_y_continuous(labels = scales::percent)+
    labs(x="Length of Service",y = "Termination Density")+
    ggtitle("Relationship between Length of Service and Status")+theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  
  #age & length correlation plot
  par(mar = c(4,4,1,1))
  plot(dataset$age,dataset$lengthofservice,frame.plot = T,xlab = 'AGE',ylab = 
         'LENGTH OF SERVICE', pch = 20,col= 'royalblue1',main = "Correlation between Age and Lenght of Service")
  abline(lm(dataset$lengthofservice~dataset$age))
  
  
  #gender & status
  x<-ggplot(data = dataset,aes(x= as.factor(gendershort),y= ..count../sum(..count..),fill = as.factor(STATUS))) + 
    geom_bar(position = position_dodge()) + theme_classic() + scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y= ((..count..)/sum(..count..))), stat="count",vjust = -0.25,position = position_dodge(0.9), check_overlap = TRUE) + 
    labs(x="GENDER_SHORT",y = "PERCENTAGE")+ ggtitle("Relationship between Gender and Status")+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  
  # termreason & output
  x<-ggplot(data = dataset,aes(x= as.factor(termreasondesc),y= ..count../sum(..count..),fill = as.factor(STATUS))) +
    geom_bar(position = position_dodge()) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y= ((..count..)/sum(..count..))), stat="count",vjust = -0.25,position = position_dodge(0.9), check_overlap = TRUE) + 
    labs(x="Termination Reason",y = "PERCENTAGE")+ ggtitle("Relationship between Termination reason and Status")+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  # status year & status
  x<-ggplot(data = dataset,aes(x= as.factor(STATUSYEAR),y= ..count../sum(..count..),fill = as.factor(STATUS))) + 
    geom_bar(position = position_dodge()) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)),y= ((..count..)/sum(..count..))), stat="count",vjust = -0.25,position = position_dodge(0.9), check_overlap = TRUE) + 
    labs(x="Status Year",y = "Percentage")+ ggtitle("Relationship between Status Year and Status")+
    theme_classic() + theme(plot.title = element_text(hjust = 0.5))
  print(x)
  
  
  # Jobtitles & STatus
  x<-ggplot(data = dataset,aes(x=as.factor(jobtitle),fill = as.factor(STATUS))) + 
    geom_bar(position = position_stack(reverse = T)) +
    labs(x="JOB_TITLE",y = "COUNT")+ ggtitle("Relationship between Job Title and Status")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5))
  print(x)
  
  # Termreason desc & Status
  x<-ggplot(data = dataset,aes(x=as.factor(cityname),fill = as.factor(STATUS))) + 
    geom_bar(position = position_stack(reverse = T)) +
    labs(x="Termination_Reason",y = "COUNT")+ ggtitle("Relationship between Termination Reason and Status")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5), plot.title = element_text(hjust = 0.5))
  print(x)
  
  #deptname & status
  x<-ggplot(data = dataset,aes(x=as.factor(departmentname),fill = as.factor(STATUS))) + 
    geom_bar(position = position_stack(reverse = T)) +
    labs(x="DEPARTMENT_NAME",y = "COUNT")+ ggtitle("Relationship between Department Name and Status")+
    theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),plot.title = element_text(hjust = 0.5))
  print(x)
  
}



#*****************************************************************
#DecisionTreeAGE():  Distribution of Status considering age
#
#Input: dataframe  -  dataset  
#
#Output:  Decision Tree PLot
#
#*****************************************************************
DecisionTreeAGE <- function(dataset){
  # selecting variables to include in model predicting terminations
  termination_variables <- c("age","lengthofservice","cityname", "departmentname",
                             "jobtitle","storename","genderfull","BUSINESSUNIT","STATUS")
  # Partitioning the data into training and test sets
  part_term_train <- subset(dataset, STATUSYEAR < 2015)
  part_term_test <- subset(dataset, STATUSYEAR == 2015)
  set.seed(99)  # setting a pre-defined value for the random seed to obtain results that are repeatable
  # Decision tree model
  rpart_model <- rpart(STATUS ~.,
                       data = part_term_train[termination_variables],
                       method = 'class',
                       parms = list(split='information'),
                       control = rpart.control(usesurrogate = 0,
                                               maxsurrogate = 0))
  # Plotting the decision tree
  rpart.plot(rpart_model, roundint = FALSE, type = 3)
  
}

#*****************************************************************
#
#NumField():    Separating the Numeric Fields form the dataframe
#
#Input :    data frame   - dataset   - dataset
#
#Output :   vector string   - NumField   - Numeric Fields
#
#*****************************************************************
NumField<-function(dataset){
  NumField<-names(which(sapply(dataset, is.numeric)))
  print(paste('Numeric Fields :',length(NumField)))
  print(NumField)
  return(NumField)
}
#*****************************************************************
#
#CategField():   Separating the Categorical Fields
#
#Input:  data frame  -   dataset   -  dataset
#
#Output:  vector  string   -    SymbField  -  Categorical Fields
#
#*****************************************************************
CategField <- function(dataset){
  SymbField<-names(which(sapply(dataset, is.character)))
  print(paste('Categorical Fields :',length(SymbField)))
  print(SymbField)
  return(SymbField)
}

#*****************************************************************
#Outliers():   Checking for Outliers
#
#Input : dataframe   -   dataset   -   dataset
#        vector string  - NumericFields   -  Numeric Fields
#
#Output
#*****************************************************************
Outliers <- function(dataset,NumericFields){
  df <- data.frame()
  for(i in 1: length(NumericFields)){
    val<-boxplot(dataset[,NumericFields[i]],
                 ylab= NumericFields[i],main = 'Boxplot To Determine Outliers')
    df[,i]<- val$out
  }
  if(any(is.na(df))==F){
    print('There are no outliers')
  }else{
    print('These are the outliers')
    print(head(df))
  }
}
#*****************************************************************
#
#Nrescale():   Scaling the Numerical Data between 0 and 1
#
#Input:  Input
#Output 
#
# Reference - Dr.Nick Ryman-Tubb function from Lab 3
#*****************************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  return((input-minv)/(maxv-minv))
}
#*****************************************************************
#Nrescaleentireframe():  Rescaling the entire dataframe between 0 and 1
#
#Input:    data frame  - dataset    -  Numeric Data
#
#Output: data frame   -  dataset    - z-scaled numeric data
#
#Reference - Dr.Nick Ryman-Tubb function from Lab 3
#
#*****************************************************************

Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}
#*****************************************************************
#
#NPREPROCESSING_categorical() : Pre-processing the categorical fields using one hot encoding
#
#Input :     dataframe    -    dataset      -    dataset
#            vetor string  -   Field_types   -    Categorical Fields
#
#Output :    dataframe   -    One hot encoded Categorical dataset
#
# Reference - Dr.Nick Ryman-Tubb function from Lab 3
#
#*****************************************************************
NPREPROCESSING_categorical<-function(dataset,field_types){
  
  catagorical<-data.frame()
  
  
  # for each field
  for (field in field_types){
    
    # Convert into factors. A level for each unique string
    ffield<-factor(dataset[,field])
    
    # Check if too many unique values to encode
    if (nlevels(ffield) > MAX_LITERALS) {
      stop(paste("Prof. Nick says - too many literals in:",
                 field,
                 nlevels(ffield)))
    }
    
    # Check if just one value!
    if (nlevels(ffield) ==1) {
      stop(paste("Prof. Nick says - field stuck at a single value:",
                 field))
    }
    
    # 1-hot encoding. A new column for each unique "level"
    xx<-data.frame(model.matrix(~ffield+0, data=ffield))
    
    names(xx)<-gsub("ffield",field,names(xx))
    
    # If 2 unique values, then can encode as a single "binary" column
    if (ncol(xx)==2){
      xx<-xx[,-2,drop=FALSE]
      names(xx)<-field  # Field name without the value appended
    }
    
    catagorical<-as.data.frame(append(catagorical,xx))
    
  } #endof for()
  return (catagorical)
  
}
#*****************************************************************
#RandomisingDataset():  Randomizing the dataset
#
#Input:   data frame   -   dataset  
#
#Output:  data frame    -  dataset   - Randomised dataset
#
# 
#*****************************************************************
RandomisingDataset<- function(dataset){
  set.seed(101)
  rows <- sample(nrow(dataset))
  dataset <- dataset[rows,]
  return(dataset)
}
#*****************************************************************
#
#DataForPCA():  Principal component analysis for input variables
#
#Input:  data frame   -  dataset
#
#Output:  data frame  -  dataset   
#
#*****************************************************************
DataForPCA <- function(dataset){
  PCA <- preProcess(x = dataset,method = 'pca', thresh = 0.7)
  ReducedData <- predict(object = PCA,newdata = dataset)
  return(ReducedData)
}
#*****************************************************************
#
# PlotConfusionMatrix() : plot the confusion matrix
#
# Input : confusionMatrix  -  Result  -  Results of the confusion matrix 
#         String           -  title   - title of the plot
# Output : plot
# 
# Reference : https://stackoverflow.com/questions/37897252/plot-confusion-matrix-in-r-using-ggplot
#
#*****************************************************************
PlotConfusionMatrix<-function(Result,title){
  v <- vector()
  v[1]<-Result$table[1,1]
  v[2]<-Result$table[1,2]
  v[3]<-Result$table[2,1]
  v[4]<-Result$table[2,2]
  TClass <- factor(c(0, 0, 1, 1))
  PClass <- factor(c(0, 1, 0, 1))
  df <- data.frame(TClass, PClass,v)
  Data<- cbind(df,v)
  Plot<-ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
    geom_tile(aes(fill = v), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", v)), vjust = 1) +
    scale_fill_gradient(low = "greenyellow", high = "limegreen") +
    theme_bw() + theme(legend.position = "none") + ggtitle(title)
  print(Plot)
}
#*****************************************************************
#
# PlottingROC() : Plot the ROC
#
# Input:  factor  -  prediction  -  Predictions from the models
#         string  -  title       -  title for the plot
#
# Output: Plot of ROC
#
#*****************************************************************
PlottingROC<-function(predictions,title){
  a<-levels(predictions)[predictions]
  b<-as.numeric(a)
  c<-levels(TestingData$Output)[TestingData$Output]
  d<- as.numeric(c)
  print(paste('AUC:',auc(d,b)))
  pred <- prediction(b, d)
  perf <- performance(pred,"tpr","fpr")
  par(mar = c(4,4,1,4))
  plot(perf,colorize=TRUE,main=title,curve = TRUE)
}








main<-function(){
  #Reading the dataset
  dataset<-NreadDataset(data.filename)
  # Checking for missing values
  MissingValues(dataset)
  # Data Exploration
  DataVisualization(dataset)
  # Desicion Tree for STATUS considering AGE
  DecisionTreeAGE(dataset)
  #Separating the Numerical Fields
  NumericFields<- NumField(dataset)
  #Separating the Categorical Fields
  CategoricalFields<-CategField(dataset)
  #Adding the Numeric Discrete Fields to Categorical Fields
  CategoricalFields<-append(CategoricalFields,c('storename','STATUSYEAR'))
  #Removing the Numeric Discrete Fields from Numeric fields
  NumericFields<-NumericFields[! NumericFields %in% c('EmployeeID','storename','STATUSYEAR')]
  #Checking for outliers
  Outliers(dataset,NumericFields)
  #Removing all date columns
  CategoricalFields <- CategoricalFields[! CategoricalFields %in% c('recorddatekey','birthdatekey',
                                    'orighiredatekey','terminationdatekey','EmployeeID','genderfull')]
  # Selecting the Ordinal Dataset
  ordinals<-dataset[,NumericFields]
  print(head(ordinals))
  #Z-scaling the Ordinal Data
  ordinalDataML<-Nrescaleentireframe(ordinals)
  print(head(ordinalDataML))
  #Processing the Categorical data using Onehot encoding
  CategoricalData<-NPREPROCESSING_categorical(dataset,CategoricalFields)
  print(dim(CategoricalData))
  #Combining the Categorical Data and Numeric ordinal Data
  combinedDataML<- cbind(CategoricalData,ordinalDataML)
  print(dim(combinedDataML))
  #Randomising the Dataset
  RandomisedDataset<- RandomisingDataset(combinedDataML)
  #Removing the Output 'STATUS' for PCA 
  DataReadyForPCA<-subset(RandomisedDataset,select = -c(STATUS))
  #Principal Component Analysis 
  ReducedDataAfterPCA<- DataForPCA(DataReadyForPCA)
  print(dim(ReducedDataAfterPCA))
  Output<-as.factor(RandomisedDataset$STATUS)
  
  #Combining Output to the data after PCA
  DataReadyForPartition <- cbind(ReducedDataAfterPCA,Output)
  #Splitting the Data Into Train and Test
  DataReadyForPartition <- DataReadyForPartition[order(runif(nrow(DataReadyForPartition))),]
  training_records<-round(nrow(DataReadyForPartition)*(70/100))
  TestingData<<- DataReadyForPartition[-(1:training_records),]
  TrainingData <<- DataReadyForPartition[(1:training_records),]
  #Resampling the data(Undersampling)
  underSampledData<-ovun.sample(Output~.,data = TrainingData,
                                method = "under", p = 0.7)
  DataReadyForTraining <- underSampledData$data
  
  
  #Straitified k-fold
  CVindex<-createFolds(DataReadyForTraining$Output, k = kfolds)
  StratifiedKFoldtrain_control<-trainControl(method = "CVindex", 
                                             number = kfolds, search = 'random')
  
  # Random Forest Modeling 
  # Training the Model
  RandomForestFit<-randomForest(formula = Output~., data = DataReadyForTraining, ntree = 4,
                                mtry =2) 
  #Testing the Model and predicting the data
  RandomForestPredict<-predict(RandomForestFit,TestingData %>% select(-Output))
  # Confusion Matrix
  ResultRandomForest<-confusionMatrix(RandomForestPredict,TestingData$Output)
  # Results
  print('Random Forest Model')
  print(ResultRandomForest)
  #Plotiing Confusion Matrix
  PlotConfusionMatrix(ResultRandomForest,'Random Forest')
  #Plot ROC
  PlottingROC(RandomForestPredict,'Random Forest')
  
  # Random Forest Modeling with Stratified K-fold Cross Validation
  # Training the Model
  RandomForestKFoldFit<-randomForest(formula = Output~., data = DataReadyForTraining, ntree = 4,
                                mtry =2,trainControl=StratifiedKFoldtrain_control)
  # Testing the Model and predicting the data
  RandomForestKFoldPredict<-predict(RandomForestKFoldFit,TestingData %>% select(-Output))  
  # Results
  ResultRandomForestKfold<-confusionMatrix(RandomForestKFoldPredict,TestingData$Output) 
  print('Random Forest with stratified K-fold Cross Validation')
  
  print(ResultRandomForestKfold)
  #Plotting Confusion Matrix 
  PlotConfusionMatrix(ResultRandomForestKfold,'Random Forest with Stratified K-Fold')
  #Plotting ROC
  PlottingROC(RandomForestKFoldPredict,'Random Forest with Stratified K-Fold')
  
  # Naive Bayes Model
  # Training the model using the method 'naive_bayes'
  NaiveBayes<-train(form = Output~., data = DataReadyForTraining,
                            method = "naive_bayes")
  #Testing the model
  NaiveBayesPredict<-predict(NaiveBayes, 
                             TestingData %>% select(-Output), type = "raw")
  #Confusion Matrix
  ResultNB<- confusionMatrix(NaiveBayesPredict, TestingData$Output)
  # Results
  print('Naive Byaes Model')
  print(ResultNB)
  #Plotting the Confusion Matrix
  PlotConfusionMatrix(ResultNB,'Naive Bayes')
  # Plotting ROC
  PlottingROC(NaiveBayesPredict,'Naive Bayes')
  
  
  
  
  # Support Vector Machine Model 
  # Training the model using svmRadial
  SVM<-train(form = Output~., data = DataReadyForTraining, 
             method = "svmRadial",cost = 10,gamma = 0.1)
  # Testing the data
  predictSVM<-predict(object = SVM, newdata = TestingData %>% select(-Output), type = "raw")
  # Confusion Matrix
  ResultSVM<-confusionMatrix(predictSVM,TestingData$Output)
  #Results
  print('Support Vector Machine Model')
  print(ResultSVM)
  #Plotting the confusion matrix
  PlotConfusionMatrix(ResultSVM,'SVM')
  
  #Plotting ROC
  PlottingROC(predictSVM,'SVM')
  
}

main()



