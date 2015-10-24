#Author   : Rajasekhar Jetty 
#LinkedIN : https://www.linkedin.com/pub/rajasekhar-jetty/45/173/623
#set source directory to input folder with setwd() command
#Hyperparameter tuning on Caret for different classifiers 
library(caret)
	if (FALSE)
	{
	finalset<-read.csv(file="train_sample_sub2.csv",header=TRUE) #Input training dataset generated 
	finalset$response<-as.factor(finalset$response)

	finalset$uq_mean_unique_device<-finalset$uq_mean_unique_device/finalset$uniqueDevices
	finalset$uq_max_unique_device<-finalset$uq_max_unique_device/finalset$uniqueDevices


	finalset$mean_bids_auction<-finalset$mean_bids_auction/finalset$bids_user
	finalset$max_bids_auction<-finalset$max_bids_auction/finalset$bids_user
	finalset$median_bids_auction<-finalset$median_bids_auction/finalset$bids_user

	finalset$uq_mean_unique_country<-finalset$uq_mean_unique_country/finalset$uniqueCountries
	finalset$uq_max_unique_country<-finalset$uq_max_unique_country/finalset$uniqueCountries
	#finalset<-finalset[finalset$bids_user!=1,]
	fitControl <- trainControl(method = "repeatedcv",
							   number = 10,
							   repeats = 2,
							   ## Estimate class probabilities
							   classProbs = TRUE,
							   ## Evaluate performance using 
							   ## the following function
							   summaryFunction = twoClassSummary)
	gbmGrid <-  expand.grid(interaction.depth = c(5,8,10,11,12,13),
							n.trees = c(500),
							shrinkage = c(0.2),
							n.minobsinnode =c(5,7,9,10,12,14) )
	gbmFit<-train(response ~ ., data = finalset,
					 method = "gbm",
					 trControl = fitControl,
					 verbose = FALSE,
					 tuneGrid = gbmGrid,
					 ## Specify which metric to optimize
					 metric = "ROC")
	print (gbmFit)
	}
#pred<-predict(gbmFit, newdata =finalset[,-c(7,21)])

if (TRUE)
{
#randomForest Tuning
finalset<-read.csv(file="train_sample_sub5.csv",header=TRUE)
finalset<-finalset[finalset$bids_user!=1,]
finalset$response<-as.factor(finalset$response)
levels(finalset$response)<-c("Human","Robo")
# finalset$uq_mean_unique_device<-finalset$uq_mean_unique_device/finalset$uniqueDevices
# finalset$uq_max_unique_device<-finalset$uq_max_unique_device/finalset$uniqueDevices


# finalset$mean_bids_auction<-finalset$mean_bids_auction/finalset$bids_user
# finalset$max_bids_auction<-finalset$max_bids_auction/finalset$bids_user
# finalset$median_bids_auction<-finalset$median_bids_auction/finalset$bids_user

# finalset$uq_mean_unique_country<-finalset$uq_mean_unique_country/finalset$uniqueCountries
# finalset$uq_max_unique_country<-finalset$uq_max_unique_country/finalset$uniqueCountries

#finalset<-finalset[finalset$bids_user!=1,]
set.seed(19)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 2,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)
rfGrid <-  expand.grid(mtry=c(17:20))
rFit<-train(response ~ ., data = finalset[,-c(1)],
                 method = "rf",
                 trControl = fitControl,
                 verbose = FALSE,replace=TRUE,
                 tuneGrid = rfGrid,n.trees=1000,
                 ## Specify which metric to optimize
                 metric = "ROC")
print(rFit)
}
						   