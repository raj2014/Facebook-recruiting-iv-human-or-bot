#Author   : Rajasekhar Jetty 
#LinkedIN : https://www.linkedin.com/pub/rajasekhar-jetty/45/173/623
#set source directory to input folder with setwd() command
#Script for generating the features from the training data
library(entropy)
bidder<-read.csv(file="train.csv",header=TRUE)
bids<-read.csv(file="bids.csv",header=TRUE)

#script for accumulating the auction count 
auction_data<-read.csv(file="auction_data.csv",header=TRUE)

bids_user<-c()
uniqueAuctions<-c()
uniqueMerchandize<-c() #per bidder
mean_bids_auction<-c()
sd_bids_auction<-c()
median_bids_auction<-c()
max_bids_auction<-c()
entropy_auction<-c()
uniqueCountries<-c()
entropy_country<-c()
uniqueDevices<-c()
uq_mean_unique_device<-c()
uq_sd_unique_device<-c()
uq_max_unique_device<-c()
uq_mean_unique_country<-c()
uq_sd_unique_country<-c()
uq_max_unique_country<-c()
uq_mean_unique_url<-c()
uq_sd_unique_url<-c()
uq_max_unique_url<-c()
prop_auction_mean<-c()
prop_auction_sd<-c()
prop_auction_max<-c()
prop_auction_min<-c()
response<-c()
user_ids<-c()
#reading bins for time distibution 
auction_bins<-read.csv(file="auction_bins_v3.csv",header=TRUE)
time_prop_bin1<-c()
time_prop_bin2<-c()
time_prop_bin3<-c()
time_prop_bin4<-c()
time_prop_bin5<-c()
time_prop_bin6<-c()
time_prop_bin7<-c()
time_prop_bin8<-c()
time_prop_bin9<-c()
time_prop_bin10<-c()
bin_entropy<-c()
avgbidTime<-c() #avg bid time features
maxbidTime<-c()
medianbidTime<-c()
avg_duration<-c()
sd_duration<-c()
max_duration<-c()
min_duration<-c()
start.time <- Sys.time()
for (id in (1:nrow(bidder)))
{
			  #status check 
			  if(id%%50==0)
			  {
				print(paste((id*100)/nrow(bidder),"%","completed",sep=" "))
				flush.console()
			  }
  
  user_id<-bidder[id,1]  
  id_bids<-bids[as.character(bids$bidder_id)== as.character(user_id),]
  id_numbids<-nrow(id_bids)
  if(id_numbids>=1){
			  response<-append(response,bidder[id,4])	
			  user_ids<-append(user_ids,as.character(user_id))
			  bids_user<-append(bids_user,id_numbids)
			  id_bids$auction<-as.character(id_bids$auction)
			  auction_id<-unique(id_bids$auction)
			  uniqueAuctions<-append(uniqueAuctions,length(auction_id))
			  uniqueMerchandize<-append(uniqueMerchandize,length(unique(as.character(id_bids$merchandise))))
			  t<-aggregate(id_bids$bid_id,list(id_bids$auction),FUN=length) #returns group by dataframe 
			  mean_bids_auction<-append(mean_bids_auction,mean(t$x))
			  max_bids_auction<-append(max_bids_auction,max(t$x))       # features for bids/auction
				  if (length(auction_id)<=1)
				  {
					   sd_bids_auction<-append(sd_bids_auction,0)
					   median_bids_auction<-append(median_bids_auction,min(t$x))
					   entropy_auction<-append(entropy_auction,0)
				  }
				  else
				  {

				  sd_bids_auction<-append(sd_bids_auction,sd(t$x))
				  median_bids_auction<-append(median_bids_auction,min(t$x))
				  entropy_auction<-append(entropy_auction,entropy(y=t$x,units="log",method="ML"))
				  }
				uniqueCountries<-append(uniqueCountries,length(unique(id_bids$country)))  				
				uniqueDevices<-append(uniqueDevices,length(unique(id_bids$device))) 
				# script to obtain unique device per auction 
				temp_unique_device<-c()				
				for (i in (1:length(auction_id)))
				{
				  temp_unique_device<-append(temp_unique_device,length(unique(id_bids[as.character(id_bids$auction)==auction_id[i],5])))
				}

						if (length(temp_unique_device)>1)
						{
							uq_mean_unique_device<-append(uq_mean_unique_device,mean(temp_unique_device))
							uq_sd_unique_device<-append(uq_sd_unique_device,sd(temp_unique_device))
							uq_max_unique_device<-append(uq_max_unique_device,max(temp_unique_device))
						}

						else
						{
						  uq_mean_unique_device<-append(uq_mean_unique_device,temp_unique_device[1])
						  uq_sd_unique_device<-append(uq_sd_unique_device,0)
						  uq_max_unique_device<-append(uq_max_unique_device,temp_unique_device[1])

						}
						#scripts to obtain unique country
				temp_unique_country<-c()
				for (i in (1:length(auction_id)))
				{
				  temp_unique_country<-append(temp_unique_country,length(unique(id_bids[as.character(id_bids$auction)==auction_id[i],7])))
				}

					if (length(temp_unique_country)>1)
					{
						uq_mean_unique_country<-append(uq_mean_unique_country,mean(temp_unique_country))
						uq_sd_unique_country<-append(uq_sd_unique_country,sd(temp_unique_country))
						uq_max_unique_country<-append(uq_max_unique_country,max(temp_unique_country))
					}

					else
					{
						uq_mean_unique_country<-append(uq_mean_unique_country,mean(temp_unique_country))
						uq_sd_unique_country<-append(uq_sd_unique_country,0)
						uq_max_unique_country<-append(uq_max_unique_country,max(temp_unique_country))

					}
				#scripts to obtain proportion vector
				auction_prop<-c()
				for (i in (1:length(auction_id)))
				{
				   totalcount<-auction_data[as.character(auction_data$id)==auction_id[i],2]
				   currentcount<-nrow(id_bids[as.character(id_bids$auction)==auction_id[i],])
				   auction_prop<-append(auction_prop,(currentcount/totalcount))

				}

				if (length(auction_id)!=1)
				{
					prop_auction_mean<-append(prop_auction_mean,mean(auction_prop))
					prop_auction_sd<-append(prop_auction_sd,sd(auction_prop))
					prop_auction_max<-append(prop_auction_max,max(auction_prop))
					prop_auction_min<-append(prop_auction_min,min(auction_prop))
				}
				else
				{
					prop_auction_mean<-append(prop_auction_mean,auction_prop[1])
					prop_auction_sd<-append(prop_auction_sd,0)
					prop_auction_max<-append(prop_auction_max,auction_prop[1])
					prop_auction_min<-append(prop_auction_min,auction_prop[1])

				}
				#bin generation
				  bins<-c(0,0,0,0,0,0,0,0,0,0)
		  if ((nrow(id_bids)/length(auction_id))>1) #more than one auction per bid
			 {
			     for (i in (1:length(auction_id)))
				 {
				    temp<-id_bids[id_bids$auction==auction_id[i],]
					bin1<-auction_bins[auction_bins$id==auction_id[i],2]
					bin2<-auction_bins[auction_bins$id==auction_id[i],3]
					bin3<-auction_bins[auction_bins$id==auction_id[i],4]
					bin4<-auction_bins[auction_bins$id==auction_id[i],5]
					bin5<-auction_bins[auction_bins$id==auction_id[i],6]
					bin6<-auction_bins[auction_bins$id==auction_id[i],7]
					bin7<-auction_bins[auction_bins$id==auction_id[i],8]
					bin8<-auction_bins[auction_bins$id==auction_id[i],9]
					bin9<-auction_bins[auction_bins$id==auction_id[i],10]
					bin10<-auction_bins[auction_bins$id==auction_id[i],11]
						for (j in 1:nrow(temp))
						{
							  if(temp[j,6]<=bin1)
							  {
								bins[1]<-bins[1]+1
							  }
							  else if(temp[j,6]<=bin2)
							  {
								bins[2]<-bins[2]+1
							  }
							   else if(temp[j,6]<=bin3)
							  {
								bins[3]<-bins[3]+1
							  }
							   else if(temp[j,6]<=bin4)
							  {
								bins[4]<-bins[4]+1
							  }
							   else if (temp[j,6]<=bin5)
							  {
								bins[5]<-bins[5]+1
							  }
							  else if (temp[j,6]<=bin6)
							  {
								bins[6]<-bins[6]+1
							  }
							  else if (temp[j,6]<=bin7)
							  {
								bins[7]<-bins[7]+1
							  }
							  else if (temp[j,6]<=bin8)
							  {
								bins[8]<-bins[8]+1
							  }
							  else if (temp[j,6]<=bin9)
							  {
								bins[9]<-bins[9]+1
							  }
							  else 
							  {
								bins[10]<-bins[10]+1
							  }
						  
						
						}
				 }
			 
			 bins<-bins/nrow(id_bids)
			 } 
			
		  
		  
		  
		  else #if only one auction
		  {
				for (i in (1:length(auction_id)))
				 {
				    temp<-id_bids[id_bids$auction==auction_id[i],]
					bin1<-auction_bins[auction_bins$id==auction_id[i],2]
					bin2<-auction_bins[auction_bins$id==auction_id[i],3]
					bin3<-auction_bins[auction_bins$id==auction_id[i],4]
					bin4<-auction_bins[auction_bins$id==auction_id[i],5]
					bin5<-auction_bins[auction_bins$id==auction_id[i],6]
					bin6<-auction_bins[auction_bins$id==auction_id[i],7]
					bin7<-auction_bins[auction_bins$id==auction_id[i],8]
					bin8<-auction_bins[auction_bins$id==auction_id[i],9]
					bin9<-auction_bins[auction_bins$id==auction_id[i],10]
					bin10<-auction_bins[auction_bins$id==auction_id[i],11]
					
						for (j in 1:nrow(temp))
						{
							  if(temp[j,6]<=bin1)
							  {
								bins[1]<-bins[1]+1
							  }
							  else if(temp[j,6]<=bin2)
							  {
								bins[2]<-bins[2]+1
							  }
							   else if(temp[j,6]<=bin3)
							  {
								bins[3]<-bins[3]+1
							  }
							   else if(temp[j,6]<=bin4)
							  {
								bins[4]<-bins[4]+1
							  }
							   else if (temp[j,6]<=bin5)
							  {
								bins[5]<-bins[5]+1
							  }
							  else if (temp[j,6]<=bin6)
							  {
								bins[6]<-bins[6]+1
							  }
							  else if (temp[j,6]<=bin7)
							  {
								bins[7]<-bins[7]+1
							  }
							  else if (temp[j,6]<=bin8)
							  {
								bins[8]<-bins[8]+1
							  }
							  else if (temp[j,6]<=bin9)
							  {
								bins[9]<-bins[9]+1
							  }
							  else 
							  {
								bins[10]<-bins[10]+1
							  }
						  
						
						}
				 }
			 
			 bins<-bins/nrow(id_bids)
		  
		  }
		  
		  #final writing of proportions for a bidder
		  time_prop_bin1<-append(time_prop_bin1,bins[1])
		  time_prop_bin2<-append(time_prop_bin2,bins[2])
		  time_prop_bin3<-append(time_prop_bin3,bins[3])
		  time_prop_bin4<-append(time_prop_bin4,bins[4])
		  time_prop_bin5<-append(time_prop_bin5,bins[5])
		  time_prop_bin6<-append(time_prop_bin6,bins[6])
		  time_prop_bin7<-append(time_prop_bin7,bins[7])
		  time_prop_bin8<-append(time_prop_bin8,bins[8])
		  time_prop_bin9<-append(time_prop_bin9,bins[9])
		  time_prop_bin10<-append(time_prop_bin10,bins[10])
		  bin_entropy<-append(bin_entropy,entropy(y=bins,units="log2",method="ML"))
			avgtime_diff<-c()
			maxtime_diff<-c()
			mediantime_diff<-c()
			for (i in (1:length(auction_id)))
				{
				  time<-id_bids[as.character(id_bids$auction)==auction_id[i],6]
				  if(length(time)>1)
				  {
					temp<-diff(time,1)
					avgtime_diff<-append(avgtime_diff,mean(temp))
					maxtime_diff<-append(maxtime_diff,max(temp))
					mediantime_diff<-append(mediantime_diff,min(temp))
				  
				  }
				  
				  else
				  {
					avgtime_diff<-append(avgtime_diff,0)
					maxtime_diff<-append(maxtime_diff,0)
					mediantime_diff<-append(mediantime_diff,0)
				  
				  }
				
				
				}
			avgbidTime<-append(avgbidTime,mean(avgtime_diff))
			maxbidTime<-append(maxbidTime,mean(maxtime_diff))
			medianbidTime<-append(medianbidTime,mean(mediantime_diff))
			
			#Duration feature generation
			duration_bucket<-c()
			for (i in (1:length(auction_id)))
			{
			dur<-(auction_bins[auction_bins$id==auction_id[i],17]-auction_bins[auction_bins$id==auction_id[i],16])/10^8
			duration_bucket<-append(duration_bucket,dur)
			}
			if (length(auction_id)>1)
			{
			avg_duration<-append(avg_duration,mean(duration_bucket))
			sd_duration<-append(sd_duration,sd(duration_bucket))
			max_duration<-append(max_duration,max(duration_bucket))
			min_duration<-append(min_duration,min(duration_bucket))
			}
			else
			{
			  avg_duration<-append(avg_duration,mean(duration_bucket))
			  sd_duration<-append(sd_duration,0)
			  max_duration<-append(max_duration,max(duration_bucket))
			  min_duration<-append(min_duration,min(duration_bucket))
			}
				

	}
	 else
	{
	  
	}
  #avgbids<-append(avgbids,(id_numbids)/length(auction_id))  
}
end.time <- Sys.time()
time.taken <- difftime(end.time,start.time,units="mins")
print(time.taken)
data<-data.frame(user_ids,bids_user,uniqueAuctions,prop_auction_mean,prop_auction_sd,prop_auction_max,prop_auction_min,entropy_auction,bin_entropy,time_prop_bin1,
time_prop_bin2,time_prop_bin3,time_prop_bin4,time_prop_bin5,time_prop_bin6,time_prop_bin7,time_prop_bin8,time_prop_bin9,time_prop_bin10,avgbidTime,maxbidTime,medianbidTime,
uniqueDevices,uq_mean_unique_device,uq_sd_unique_device,uq_max_unique_device,avg_duration,sd_duration,max_duration,min_duration,
mean_bids_auction,sd_bids_auction,median_bids_auction,max_bids_auction,uniqueCountries,uq_mean_unique_country,uq_sd_unique_country,uq_max_unique_country,response)
write.csv(file="train_sample_sub5.csv",data,row.names=FALSE)



