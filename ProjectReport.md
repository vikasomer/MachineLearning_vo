Human Activity Performance Recognition
========================================================

### Introduction:

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal is to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

### Data:

The training data for this project are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv

The test data are available here: 

https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

The data for this project come from this source: http://groupware.les.inf.puc-rio.br/har.

Data is very detailed and has around 160 variables including participants names, time stamps for when the data was captured, measurements and class. Class determines how well the task has been done. (Class A corresponds to the specified execution of the exercise, while the other 4 classes correspond to common mistakes)

Read more: http://groupware.les.inf.puc-rio.br/har#ixzz38Yj1Dfya)

### Exploratary Data Analysis & Data Cleansing:

Let's have a look at the data. 


```r
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")
```

Having looked at the training data set, it appears that lot of columns have NAs and 0 values. So, let's drop such columns from our data set where majority of the values are N/A or 0.

- *Note: Data has not been summarized here as it's taking too much of space.*

Reloading the training data set considering "#DIV/0!" as NA.


```r
training <- read.csv("pml-training.csv",na.strings=c("#DIV/0!",NA))
```

Now, filtering columns that are all 0/NA.


```r
keep <- colSums(training!=0) != 0
keep <- keep[keep == TRUE]
training <- training[,names(keep[!is.na(names(keep))])]
length(names(training))
```

```
## [1] 60
```

After filtering out unknown and 0 value columns, we are left with 60 variables.
Let's have a look at the variables and do some feature selection based on variables and variance.


```r
names(training)
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
```

```r
suppressWarnings(library(caret,quietly = T))
nearZeroVar(training)
```

```
## [1] 6
```

The most important variables for us to determine class variable is the measurement data which tells us how the task has been performed so following variables are not of much use:
- x *(This is just row number so can be dropped from the data frame)*
- user_name *(measurements does not depend on name of user, so, this can be dropped)*
- raw_timestamp_part_1 *(considering these are timestamps when measurements were taken so this can be dropped)*
- raw_timestamp_part_2 *(considering these are timestamps when measurements were taken so this can be dropped)*
- cvtd_timestamp *(considering these are timestamps when measurements were taken so this can be dropped)*
- new_window *(this column can be dropped as this has pretty less variance in terms of data, also indicated by the formula nearZeroVar)*
- num_window *(this is simply window number but let's check if there is any relation with classe variable)*

Plotting a graph for num_window and classe variable.


```r
qplot(num_window,classe,color=classe,data=training)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

By looking at the graph, it looks like specific values of num_window variable corresponds to specific classe values. Let's confirm it with contingency table.


```r
head(table(training$num_window,training$classe),20)
```

```
##     
##       A B C  D  E
##   1   0 0 0  0 20
##   2   0 0 0  0 21
##   3   0 0 0  0  3
##   4   0 0 0  0 28
##   5   0 0 0  0 21
##   6   0 0 0  0 21
##   7   0 0 0  0 21
##   8   0 0 0  0 24
##   9   0 0 0  0 24
##   10  0 0 0  0 12
##   11  3 0 0  0  0
##   12 21 0 0  0  0
##   13 28 0 0  0  0
##   14 24 0 0  0  0
##   15 24 0 0  0  0
##   16 29 0 0  0  0
##   17 17 0 0  0  0
##   18 19 0 0  0  0
##   19  0 0 0 21  0
##   20  0 0 0 28  0
```

It's very clear that num_window variable is mapped to classe variable and there is no overlap. So, num_window variable can alone predict the classe variable for this kind of dataset. But, this is not a measurement variable so lets drop this column and train our model using the actual measurement columns.

New data set which will be used for training will be


```r
training <- training[,-c(1:7)]
length(names(training))
```

```
## [1] 53
```

So, final training dataset has 53 variables including the variable which is to be predicted.

### Data Slicing, Pre-processing & Modelling

Now, when we have the final dataset which can be used for training the model, let's partition it into training and test data set.


```r
set.seed(123)
inTrain <- createDataPartition(y=training$classe,p=0.75,list=F)
training_training <- training[inTrain,]
training_testing <- training[-inTrain,]
```

Now, we will build our model using the training_training dataset.

We have all numerical variables apart from the variable (classe) which is a factor variable. Let's check correlation between the columns and find out if we have correlation amongst them.


```r
M <- abs(cor(training_training[,-53]))
diag(M) <- 0
nrow(which(M > 0.8,arr.ind = T))/2
```

```
## [1] 20
```

There are 19 pairs which are highly correlated with each other. Let's use principle component analysis using 95 % variance.

We will use random forest method to train the model. The reason behind chosing random forest method is the nature of prediction where measurements will highly depend on the way the exercise is performed. So, this method will rank the importance of variables in regression in natural way. Also, there is no need of cross-validation with this method as it takes around 63% of the bootstrapped data for training the model and rest of the data for testing the model as OOB error.


```r
suppressWarnings(library(randomForest,quietly = T))
```

```
## randomForest 4.6-7
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modFit_pca <- train(classe ~ .,method="rf", preProcess = "pca", data = training_training)
```

```
## Warning: package 'e1071' was built under R version 3.1.1
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
## Warning: invalid mtry: reset to within valid range
```

*Accuracy with test data*


```r
confusionMatrix(training_testing$classe,predict(modFit_pca,newdata=training_testing))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1388    2    0    5    0
##          B    8  924   16    0    1
##          C    3   13  828    9    2
##          D    1    0   46  756    1
##          E    2    4    3    3  889
## 
## Overall Statistics
##                                        
##                Accuracy : 0.976        
##                  95% CI : (0.971, 0.98)
##     No Information Rate : 0.286        
##     P-Value [Acc > NIR] : <2e-16       
##                                        
##                   Kappa : 0.969        
##  Mcnemar's Test P-Value : NA           
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.990    0.980    0.927    0.978    0.996
## Specificity             0.998    0.994    0.993    0.988    0.997
## Pos Pred Value          0.995    0.974    0.968    0.940    0.987
## Neg Pred Value          0.996    0.995    0.984    0.996    0.999
## Prevalence              0.286    0.192    0.182    0.158    0.182
## Detection Rate          0.283    0.188    0.169    0.154    0.181
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.994    0.987    0.960    0.983    0.996
```
Out of Sample error: 2.6 %
Using preprocessing with PCA method seems to have a pretty good accuracy and OOB (2.2 %) but let's try to built the model without any preprocessing and see if accuracy and OOB improves.


```r
modFit <- train(classe ~ .,method="rf", data = training_training)
```

Let's check the final model.


```r
modFit$finalModel
```

```
## 
## Call:
##  randomForest(x = x, y = y, mtry = param$mtry) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2
## 
##         OOB estimate of  error rate: 0.58%
## Confusion matrix:
##      A    B    C    D    E class.error
## A 4183    2    0    0    0   0.0004779
## B   13 2827    8    0    0   0.0073736
## C    0   17 2549    1    0   0.0070121
## D    0    0   41 2371    0   0.0169983
## E    0    0    0    3 2703   0.0011086
```

Out of Bag error comes out to be 0.61 % which is pretty good.


*Accuracy with test data*


```r
 confusionMatrix(training_testing$classe,predict(modFit,newdata=training_testing))
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1394    1    0    0    0
##          B    2  945    2    0    0
##          C    0    8  847    0    0
##          D    0    0   18  785    1
##          E    0    0    0    1  900
## 
## Overall Statistics
##                                         
##                Accuracy : 0.993         
##                  95% CI : (0.991, 0.995)
##     No Information Rate : 0.285         
##     P-Value [Acc > NIR] : <2e-16        
##                                         
##                   Kappa : 0.991         
##  Mcnemar's Test P-Value : NA            
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             0.999    0.991    0.977    0.999    0.999
## Specificity             1.000    0.999    0.998    0.995    1.000
## Pos Pred Value          0.999    0.996    0.991    0.976    0.999
## Neg Pred Value          0.999    0.998    0.995    1.000    1.000
## Prevalence              0.285    0.195    0.177    0.160    0.184
## Detection Rate          0.284    0.193    0.173    0.160    0.184
## Detection Prevalence    0.284    0.194    0.174    0.164    0.184
## Balanced Accuracy       0.999    0.995    0.987    0.997    0.999
```

Out of sample error: 1.8 %.
This seems to be performing extremely well as the accuracy of the model is 99.37 %.

### Prediction with Test data

Let's use the final model to predict the outcome out of testing data.


```r
predict(modFit,newdata=testing)
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

**Note: Answers of the outcome have been loaded onto the site and they are 100 % correct.**

*Accuracy with original testing data: 100 %.*

### Conclusion:

Random forest algorithm seems to be performing extremely well on cleansed data. Preprocessing with pca is not used as it costs some some accuracy. Cross validation is not used as this is not needed with random forests. 
So, the model can be easily used to predict how well you are performing burbell lift based on data from sensors. Similarly, models can be built in for other type of exercises in order to predict the quality of exercise.



