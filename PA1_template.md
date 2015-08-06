# Project Write-Up

## Loading and preprocessing the data
There are 19622 observations and 160 variables that encompasses both continuous and categorical variables.



```r
x_train = getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv")
activity_train= read.csv(text = x_train,na.strings=c("NA","#DIV/0!", ""))
x_test = getURL("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv")
activity_test=read.csv(text = x_test,na.strings=c("NA","#DIV/0!", ""))
```
 
The goal of this project is to build a model to predict activity performance classes, which reflects how well an individual performs a set of exercises. Looking at the table below, we see that there are five classes labeled A-E, where class A corresponds to the correct execution of the exercise and the other 4 classes correspond to common mistakes.


```r
table(activity_train$classe)
```

```
## 
##    A    B    C    D    E 
## 5580 3797 3422 3216 3607
```

For each variable, we compute the percentage of missing values. We remove variables that have more than 30% of values missing. We also remove the first seven variables which are irrelevant to the analysis as they pertain to user information not related to fitness activity tracking. We end up with 53 variables.

```r
NA_pred=apply(activity_train,2,function(x){mean(is.na(x))})
activity_train=activity_train[,which(NA_pred<.3)]
activity_train=activity_train[,-(1:7)]
dim(activity_train)
```

```
## [1] 19622    53
```

We a random forest model and look at the cross-validation results where we use 5-fold CV.

```r
fitControl <- trainControl(method = "cv",number =5)
rfFit <- train(classe~ .,data=activity_train,method = "rf",trControl = fitControl)
```

Looking at the results from cross-validation, the accuracy is at least 99.3% so we expect the out-of-sample error rate to be less than 0.7%.

```r
rfFit$resample
```

```
##    Accuracy     Kappa Resample
## 1 0.9936322 0.9919452    Fold1
## 2 0.9964340 0.9954894    Fold3
## 3 0.9943920 0.9929065    Fold2
## 4 0.9936290 0.9919416    Fold5
## 5 0.9938822 0.9922601    Fold4
```

We use the chosen model to make predictions for the test set and show the predictions below.

```r
pred_test <- predict(rfFit, activity_test)
```

```
## Loading required package: randomForest
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
pred_test 
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
  }
pml_write_files(pred_test)
```







