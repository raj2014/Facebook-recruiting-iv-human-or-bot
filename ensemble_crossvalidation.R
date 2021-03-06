#Author   : Rajasekhar Jetty 
#LinkedIN : https://www.linkedin.com/pub/rajasekhar-jetty/45/173/623
#set source directory to input folder with setwd() command
#Crossvalidation of the Ensemble (GBM and RF ) with AUC as the evaluation metric
library(randomForest)
library(AUC)
library(gbm)
library(caret)
arearf<-c()
areagbm<-c()
areaEnsemble<-c()
finalset<-read.csv(file="train_sample_sub2.csv",header=TRUE) #Input training dataset generated 
#finalset<-finalset[finalset$bids_user!=1,]

finalset$response<-as.factor(finalset$response)
levels(finalset$response)<-c("Human","Robo")

finalset$uq_mean_unique_device<-finalset$uq_mean_unique_device/finalset$uniqueDevices
finalset$uq_max_unique_device<-finalset$uq_max_unique_device/finalset$uniqueDevices


finalset$mean_bids_auction<-finalset$mean_bids_auction/finalset$bids_user
finalset$max_bids_auction<-finalset$max_bids_auction/finalset$bids_user
finalset$median_bids_auction<-finalset$median_bids_auction/finalset$bids_user

finalset$uq_mean_unique_country<-finalset$uq_mean_unique_country/finalset$uniqueCountries
finalset$uq_max_unique_country<-finalset$uq_max_unique_country/finalset$uniqueCountries

#Declaring weights for two ensembles 
# w1=0.5,w2=0.5
maxarea<-0
#randomForest
for (w1 in seq(0.1,1,by=0.05))

	{				
			arearf<-c()
			areagbm<-c()
			areaEnsemble<-c()	
				for ( i in 1:10)
					{
						set.seed(i*11)
						index<-sample(1:nrow(finalset),size=0.9*nrow(finalset),replace=FALSE)
						#mySampSize <- ceiling(table(as.factor(finalset[index,21])))
						#rfmodel<-randomForest(x=finalset[index,-c(3,19)],y=as.factor(finalset[index,19]),replace=FALSE,sampsize=mySampSize,
									#strata=as.factor(finalset[index,19]),importance=TRUE,ntree=1000,mtry=7)
						rfmodel<-randomForest(x=finalset[index,-c(25)],y=(finalset[index,25]),replace=FALSE,importance=TRUE,ntree=1000,mtry=11)
						k<-predict(rfmodel,newdata=finalset[-index,-c(25)],type="prob")
						k<-as.data.frame(k)
						# adding gbm model
						fitControl <- trainControl(method = "none", classProbs = TRUE)
						gbmGrid <-  expand.grid(interaction.depth = c(10),
												n.trees = c(500),
												shrinkage = c(0.2),
												n.minobsinnode =c(14) )
						gbmFit<-train(response ~ ., data = finalset[index,],
										 method = "gbm",
										 trControl = fitControl,
										 verbose = FALSE,
										 tuneGrid = gbmGrid,
										 ## Specify which metric to optimize
										 metric = "ROC")
										 
						l<-predict(gbmFit, newdata =finalset[-index,-c(25)],type="prob")	
						#arearf<-append(arearf,auc(roc(k$Robo,as.factor(finalset[-index,21]))))
						#areagbm<-append(areagbm,auc(roc(l$Robo,as.factor(finalset[-index,21]))))
						#print(auc(roc(k$Robo,as.factor(finalset[-index,21]))))
						#print(auc(roc(l$Robo,as.factor(finalset[-index,21]))))	
						ens<-(w1)*k$Robo+(1-w1)*l$Robo
						#print(auc(roc(ens,as.factor(finalset[-index,21]))))	
						areaEnsemble<-append(areaEnsemble,auc(roc(ens,as.factor(finalset[-index,25]))))
						#print("----")
						flush.console()
					}
		print("----")
        if(mean(areaEnsemble)>maxarea)
          {
		     maxarea<-mean(areaEnsemble)
			 print(paste(w1,maxarea,sep='   '))
			 print("Update")
			 flush.console()
		  }		
	}			
#print(area)

flush.console()







