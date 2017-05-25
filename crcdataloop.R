############for power
text2 <- readLines("crc2.txt")
totallength<-length(text2)#total length of file 
range_indices <- grep("Power_hist",text2) #get indices of all lines with Power_hist

#now go through each of the collected indices of lines saying power
hist_data<-NULL
#for each entry of power indices
for(i in 1:length(range_indices))
{
  print(i)
  start_hist <- range_indices[i]+1 #hist starts in the next line because the index is of the label line
  next_hist <- grep("hist",text2[start_hist:totallength]) #find the next occurence of hist from the current start index of hist
  fromstart<- min(next_hist) #the length till the next line saying hist
  end_histindex<- (start_hist+fromstart)-2 #the current hist measurement indices
  #get data 
  name<-paste("hist_data",i,sep="")
  assign(name,text2[start_hist:end_histindex])
}
  ########
###########
#########
############for SNR
range_indices_SNR <- grep("SNR_hist",text2) #get indices of all lines with SNR_hist

#now go through each of the collected indices of lines saying SNR
hist_data_SNR<-NULL
#for each entry of SNR indices
for(i in 1:length(range_indices_SNR))
{
  print(i)
  start_hist_SNR <- range_indices_SNR[i]+1 #hist starts in the next line because the index is of the label line
  next_hist_SNR <- grep("hist",text2[start_hist_SNR:totallength]) #find the next occurence of hist from the current start index of hist
  fromstart_SNR<- min(next_hist_SNR) #the length till the next line saying hist
  end_histindex_SNR<- (start_hist_SNR+fromstart_SNR)-2 #the current hist measurement indices
  #get data 
  name<-paste("SNR_hist_data",i,sep="")
  assign(name,text2[start_hist_SNR:end_histindex_SNR])
}
######
#######
############for CF_offset_hist
range_indices_CF <- grep("CF_offset_hist",text2) #get indices of all lines with SNR_hist

#now go through each of the collected indices of lines saying SNR
hist_data_CF<-NULL
#for each entry of SNR indices
for(i in 1:length(range_indices_CF))
{
  print(i)
  start_hist_CF <- range_indices_CF[i]+1 #hist starts in the next line because the index is of the label line
  next_hist_CF <- grep("hist",text2[start_hist_CF:totallength]) #find the next occurence of hist from the current start index of hist
  fromstart_CF<- min(next_hist_CF) #the length till the next line saying hist
  end_histindex_CF<- (start_hist_CF+fromstart_CF)-2 #the current hist measurement indices
  #get data 
  name<-paste("CF_hist_data",i,sep="")
  assign(name,text2[start_hist_CF:end_histindex_CF])
}
######
#######
############for Bandwidth_hist
range_indices_bw <- grep("Bandwidth_hist",text2) #get indices of all lines with SNR_hist

#now go through each of the collected indices of lines saying SNR
hist_data_bw<-NULL
#for each entry of SNR indices
for(i in 1:length(range_indices_bw))
{
  print(i)
  start_hist_bw <- range_indices_bw[i]+1 #hist starts in the next line because the index is of the label line
  next_hist_bw <- grep("hist",text2[start_hist_bw:totallength]) #find the next occurence of hist from the current start index of hist
  fromstart_bw<- min(next_hist_bw) #the length till the next line saying hist
  end_histindex_bw<- (start_hist_bw+fromstart_bw)-2 #the current hist measurement indices
  #get data 
  name<-paste("Bandwidth_hist_data",i,sep="")
  assign(name,text2[start_hist_bw:end_histindex_bw])
}
######
##
#######
############for Burst_on_hist
range_indices_bon <- grep("Burst_on_hist",text2) #get indices of all lines with SNR_hist

#now go through each of the collected indices of lines saying SNR
hist_data_bon<-NULL
#for each entry of SNR indices
for(i in 1:length(range_indices_bon))
{
  print(i)
  start_hist_bon <- range_indices_bon[i]+1 #hist starts in the next line because the index is of the label line
  next_hist_bon <- grep("hist",text2[start_hist_bon:totallength]) #find the next occurence of hist from the current start index of hist
  fromstart_bon<- min(next_hist_bon) #the length till the next line saying hist
  end_histindex_bon<- (start_hist_bon+fromstart_bon)-2 #the current hist measurement indices
  #get data 
  name<-paste("Burston_hist_data",i,sep="")
  assign(name,text2[start_hist_bon:end_histindex_bon])
}
######
#######
############for Burst_off_hist
range_indices_boff <- grep("Burst_off_hist",text2) #get indices of all lines with SNR_hist

#now go through each of the collected indices of lines saying SNR
hist_data_boff<-NULL
#for each entry of SNR indices
for(i in 1:length(range_indices_boff))
{
  print(i)
  start_hist_boff <- range_indices_boff[i]+1 #hist starts in the next line because the index is of the label line
  next_hist_boff <- grep("---",text2[start_hist_boff:totallength]) #find the next occurence of hist from the current start index of hist
  fromstart_boff<- min(next_hist_boff) #the length till the next line saying hist
  end_histindex_boff<- (start_hist_boff+fromstart_boff)-2 #the current hist measurement indices
  #get data 
  name<-paste("Burstoff_hist_data",i,sep="")
  assign(name,text2[start_hist_boff:end_histindex_boff])
}
######
##########
range_indices <- grep("_range",text2)

range_entries <- grep("-?[0-9]+/-?[0-9]+/-?[0-9]+",text2[range_indices],value=TRUE) #find indices that contain range values

range_names <- regmatches(range_entries,regexpr("^.*:",range_entries))
range_names <- substr(range_names,1,nchar(range_names)-1)
range_names

range_values <- regmatches(range_entries,regexpr("-?[[:digit:]]+/-?[[:digit:]]+/-?[[:digit:]]+",range_entries)) 
range_values

range_start <- regmatches(range_values,regexpr("-?[[:digit:]]+/",range_values)) #extracts the first part of the range XXX/./.
range_start <- as.numeric(regmatches(range_start,regexpr("-?[[:digit:]]+",range_start))) #clears the / signs and converts to number
range_start

range_increment <- regmatches(range_values,regexpr("/-?[[:digit:]]+/",range_values)) #extracts the middle part of the range ./XXX/.
range_increment <- as.numeric(regmatches(range_increment,regexpr("-?[[:digit:]]+",range_increment))) #clears the / signs and converts to number
range_increment

range_end <- regmatches(range_values,regexpr("/-?[[:digit:]]+$",range_values)) #extracts the last part of the range ././XXX
range_end <- as.numeric(regmatches(range_end,regexpr("-?[[:digit:]]+",range_end))) #clears the / signs and converts to number
range_end

for(i in 1:length(range_entries))
{
  print(i)
  print(range_names[i])
 
  names<-paste("range",i,sep="")
  
  assign(names,seq(range_start[i],range_end[i],range_increment[i]))
}
#########
######

######hist data for hour 1 
########split the measurements
temp <- strsplit(hist_data1, " ")#split rows of data file according to space 
temp # you get channelid, count, and then the histogram measurements 
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf1   <- as.data.frame(mat)#make a data frame 
#split the last column in substrings separated by comma 
#first convert last column to string 
histdf1$V3<-as.character(histdf1$V3)
histdf1$hist_measurements<- strsplit(histdf1$V3, ",")#split 
histdf1$num_meas<-lapply(histdf1$hist_measurements, as.numeric)#convert the last column to numeric list
histdf1$multi<-lapply(histdf1$num_meas, "*" , range1)
for(i in 1:nrow(histdf1))
{
histdf1$avg[i]<-do.call(sum, histdf1$multi[i])
histdf1$avg[i]<-histdf1$avg[i]/(as.numeric(as.character(histdf1$V2[i])))
histdf1$time<- as.numeric(1)
}

####
####
######hist data for hour 2 
########split the measurements
temp <- strsplit(hist_data2, " ")#split rows of data file according to space 
temp # you get channelid, count, and then the histogram measurements 
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf2   <- as.data.frame(mat)#make a data frame 
#split the last column in substrings separated by comma 
#first convert last column to string 
histdf2$V3<-as.character(histdf2$V3)
histdf2$hist_measurements<- strsplit(histdf2$V3, ",")#split 
histdf2$num_meas<-lapply(histdf2$hist_measurements, as.numeric)#convert the last column to numeric list
histdf2$multi<-lapply(histdf2$num_meas, "*" , range1)
for(i in 1:nrow(histdf2))
{
  histdf2$avg[i]<-do.call(sum, histdf2$multi[i])
  histdf2$avg[i]<-histdf2$avg[i]/(as.numeric(as.character(histdf2$V2[i])))
  histdf2$time<- as.numeric(2)
}


######
#######

######hist data for hour 3 
########split the measurements
temp <- strsplit(hist_data3, " ")#split rows of data file according to space 
temp # you get channelid, count, and then the histogram measurements 
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf3   <- as.data.frame(mat)#make a data frame 
#split the last column in substrings separated by comma 
#first convert last column to string 
histdf3$V3<-as.character(histdf3$V3)
histdf3$hist_measurements<- strsplit(histdf3$V3, ",")#split 
histdf3$num_meas<-lapply(histdf3$hist_measurements, as.numeric)#convert the last column to numeric list
histdf3$multi<-lapply(histdf3$num_meas, "*" , range1)
for(i in 1:nrow(histdf3))
{
  histdf3$avg[i]<-do.call(sum, histdf3$multi[i])
  histdf3$avg[i]<-histdf3$avg[i]/(as.numeric(as.character(histdf3$V2[i])))
  histdf3$time<- as.numeric(3)
}
##### for 4
temp <- strsplit(hist_data4, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf4   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf4$V3<-as.character(histdf4$V3)
histdf4$hist_measurements<- strsplit(histdf4$V3, ",")#split
histdf4$num_meas<-lapply(histdf4$hist_measurements, as.numeric)#convert the last column to numeric list
histdf4$multi<-lapply(histdf4$num_meas, "*" , range1)
for(i in 1:nrow(histdf4))
{
  histdf4$avg[i]<-do.call(sum, histdf4$multi[i])
  histdf4$avg[i]<-histdf4$avg[i]/(as.numeric(as.character(histdf4$V2[i])))
  histdf4$time<- as.numeric(4)
}

###################
temp <- strsplit(hist_data5, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf5   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf5$V3<-as.character(histdf5$V3)
histdf5$hist_measurements<- strsplit(histdf5$V3, ",")#split
histdf5$num_meas<-lapply(histdf5$hist_measurements, as.numeric)#convert the last column to numeric list
histdf5$multi<-lapply(histdf5$num_meas, "*" , range1)
for(i in 1:nrow(histdf5))
{
  histdf5$avg[i]<-do.call(sum, histdf5$multi[i])
  histdf5$avg[i]<-histdf5$avg[i]/(as.numeric(as.character(histdf5$V2[i])))
  histdf5$time<- as.numeric(5)
}
#####
temp <- strsplit(hist_data6, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf6   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf6$V3<-as.character(histdf6$V3)
histdf6$hist_measurements<- strsplit(histdf6$V3, ",")#split
histdf6$num_meas<-lapply(histdf6$hist_measurements, as.numeric)#convert the last column to numeric list
histdf6$multi<-lapply(histdf6$num_meas, "*" , range1)
for(i in 1:nrow(histdf6))
{
  histdf6$avg[i]<-do.call(sum, histdf6$multi[i])
  histdf6$avg[i]<-histdf6$avg[i]/(as.numeric(as.character(histdf6$V2[i])))
  histdf6$time<- as.numeric(6)
}


####
temp <- strsplit(hist_data7, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf7   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf7$V3<-as.character(histdf7$V3)
histdf7$hist_measurements<- strsplit(histdf7$V3, ",")#split
histdf7$num_meas<-lapply(histdf7$hist_measurements, as.numeric)#convert the last column to numeric list
histdf7$multi<-lapply(histdf7$num_meas, "*" , range1)
for(i in 1:nrow(histdf7))
{
  histdf7$avg[i]<-do.call(sum, histdf7$multi[i])
  histdf7$avg[i]<-histdf7$avg[i]/(as.numeric(as.character(histdf7$V2[i])))
  histdf7$time<- as.numeric(7)
}
######

temp <- strsplit(hist_data8, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf8   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf8$V3<-as.character(histdf8$V3)
histdf8$hist_measurements<- strsplit(histdf8$V3, ",")#split
histdf8$num_meas<-lapply(histdf8$hist_measurements, as.numeric)#convert the last column to numeric list
histdf8$multi<-lapply(histdf8$num_meas, "*" , range1)
for(i in 1:nrow(histdf8))
{
  histdf8$avg[i]<-do.call(sum, histdf8$multi[i])
  histdf8$avg[i]<-histdf8$avg[i]/(as.numeric(as.character(histdf8$V2[i])))
  histdf8$time<- as.numeric(8)
}
###

temp <- strsplit(hist_data9, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf9   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf9$V3<-as.character(histdf9$V3)
histdf9$hist_measurements<- strsplit(histdf9$V3, ",")#split
histdf9$num_meas<-lapply(histdf9$hist_measurements, as.numeric)#convert the last column to numeric list
histdf9$multi<-lapply(histdf9$num_meas, "*" , range1)
for(i in 1:nrow(histdf9))
{
  histdf9$avg[i]<-do.call(sum, histdf9$multi[i])
  histdf9$avg[i]<-histdf9$avg[i]/(as.numeric(as.character(histdf9$V2[i])))
  histdf9$time<- as.numeric(9)
}


###

temp <- strsplit(hist_data10, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf10   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf10$V3<-as.character(histdf10$V3)
histdf10$hist_measurements<- strsplit(histdf10$V3, ",")#split
histdf10$num_meas<-lapply(histdf10$hist_measurements, as.numeric)#convert the last column to numeric list
histdf10$multi<-lapply(histdf10$num_meas, "*" , range1)
for(i in 1:nrow(histdf10))
{
  histdf10$avg[i]<-do.call(sum, histdf10$multi[i])
  histdf10$avg[i]<-histdf10$avg[i]/(as.numeric(as.character(histdf10$V2[i])))
  histdf10$time<- as.numeric(10)
}

###
temp <- strsplit(hist_data11, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf11   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf11$V3<-as.character(histdf11$V3)
histdf11$hist_measurements<- strsplit(histdf11$V3, ",")#split
histdf11$num_meas<-lapply(histdf11$hist_measurements, as.numeric)#convert the last column to numeric list
histdf11$multi<-lapply(histdf11$num_meas, "*" , range1)
for(i in 1:nrow(histdf11))
{
  histdf11$avg[i]<-do.call(sum, histdf11$multi[i])
  histdf11$avg[i]<-histdf11$avg[i]/(as.numeric(as.character(histdf11$V2[i])))
  histdf11$time<- as.numeric(11)
}


##
temp <- strsplit(hist_data12, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf12   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf12$V3<-as.character(histdf12$V3)
histdf12$hist_measurements<- strsplit(histdf12$V3, ",")#split
histdf12$num_meas<-lapply(histdf12$hist_measurements, as.numeric)#convert the last column to numeric list
histdf12$multi<-lapply(histdf12$num_meas, "*" , range1)
for(i in 1:nrow(histdf12))
{
  histdf12$avg[i]<-do.call(sum, histdf12$multi[i])
  histdf12$avg[i]<-histdf12$avg[i]/(as.numeric(as.character(histdf12$V2[i])))
  histdf12$time<- as.numeric(12)
}

##
temp <- strsplit(hist_data13, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf13   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf13$V3<-as.character(histdf13$V3)
histdf13$hist_measurements<- strsplit(histdf13$V3, ",")#split
histdf13$num_meas<-lapply(histdf13$hist_measurements, as.numeric)#convert the last column to numeric list
histdf13$multi<-lapply(histdf13$num_meas, "*" , range1)
for(i in 1:nrow(histdf13))
{
  histdf13$avg[i]<-do.call(sum, histdf13$multi[i])
  histdf13$avg[i]<-histdf13$avg[i]/(as.numeric(as.character(histdf13$V2[i])))
  histdf13$time<- as.numeric(13)
}


###
temp <- strsplit(hist_data14, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf14   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf14$V3<-as.character(histdf14$V3)
histdf14$hist_measurements<- strsplit(histdf14$V3, ",")#split
histdf14$num_meas<-lapply(histdf14$hist_measurements, as.numeric)#convert the last column to numeric list
histdf14$multi<-lapply(histdf14$num_meas, "*" , range1)
for(i in 1:nrow(histdf14))
{
  histdf14$avg[i]<-do.call(sum, histdf14$multi[i])
  histdf14$avg[i]<-histdf14$avg[i]/(as.numeric(as.character(histdf14$V2[i])))
  histdf14$time<- as.numeric(14)
}

###

temp <- strsplit(hist_data15, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf15   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf15$V3<-as.character(histdf15$V3)
histdf15$hist_measurements<- strsplit(histdf15$V3, ",")#split
histdf15$num_meas<-lapply(histdf15$hist_measurements, as.numeric)#convert the last column to numeric list
histdf15$multi<-lapply(histdf15$num_meas, "*" , range1)
for(i in 1:nrow(histdf15))
{
  histdf15$avg[i]<-do.call(sum, histdf15$multi[i])
  histdf15$avg[i]<-histdf15$avg[i]/(as.numeric(as.character(histdf15$V2[i])))
  histdf15$time<- as.numeric(15)
}

####

temp <- strsplit(hist_data16, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf16   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf16$V3<-as.character(histdf16$V3)
histdf16$hist_measurements<- strsplit(histdf16$V3, ",")#split
histdf16$num_meas<-lapply(histdf16$hist_measurements, as.numeric)#convert the last column to numeric list
histdf16$multi<-lapply(histdf16$num_meas, "*" , range1)
for(i in 1:nrow(histdf16))
{
  histdf16$avg[i]<-do.call(sum, histdf16$multi[i])
  histdf16$avg[i]<-histdf16$avg[i]/(as.numeric(as.character(histdf16$V2[i])))
  histdf16$time<- as.numeric(16)
}

##
temp <- strsplit(hist_data17, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf17   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf17$V3<-as.character(histdf17$V3)
histdf17$hist_measurements<- strsplit(histdf17$V3, ",")#split
histdf17$num_meas<-lapply(histdf17$hist_measurements, as.numeric)#convert the last column to numeric list
histdf17$multi<-lapply(histdf17$num_meas, "*" , range1)
for(i in 1:nrow(histdf17))
{
  histdf17$avg[i]<-do.call(sum, histdf17$multi[i])
  histdf17$avg[i]<-histdf17$avg[i]/(as.numeric(as.character(histdf17$V2[i])))
  histdf17$time<- as.numeric(17)
}

##
temp <- strsplit(hist_data18, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf18   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf18$V3<-as.character(histdf18$V3)
histdf18$hist_measurements<- strsplit(histdf18$V3, ",")#split
histdf18$num_meas<-lapply(histdf18$hist_measurements, as.numeric)#convert the last column to numeric list
histdf18$multi<-lapply(histdf18$num_meas, "*" , range1)
for(i in 1:nrow(histdf18))
{
  histdf18$avg[i]<-do.call(sum, histdf18$multi[i])
  histdf18$avg[i]<-histdf18$avg[i]/(as.numeric(as.character(histdf18$V2[i])))
  histdf18$time<- as.numeric(18)
}

##
temp <- strsplit(hist_data19, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf19   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf19$V3<-as.character(histdf19$V3)
histdf19$hist_measurements<- strsplit(histdf19$V3, ",")#split
histdf19$num_meas<-lapply(histdf19$hist_measurements, as.numeric)#convert the last column to numeric list
histdf19$multi<-lapply(histdf19$num_meas, "*" , range1)
for(i in 1:nrow(histdf19))
{
  histdf19$avg[i]<-do.call(sum, histdf19$multi[i])
  histdf19$avg[i]<-histdf19$avg[i]/(as.numeric(as.character(histdf19$V2[i])))
  histdf19$time<- as.numeric(19)
}

####
temp <- strsplit(hist_data20, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf20   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf20$V3<-as.character(histdf20$V3)
histdf20$hist_measurements<- strsplit(histdf20$V3, ",")#split
histdf20$num_meas<-lapply(histdf20$hist_measurements, as.numeric)#convert the last column to numeric list
histdf20$multi<-lapply(histdf20$num_meas, "*" , range1)
for(i in 1:nrow(histdf20))
{
  histdf20$avg[i]<-do.call(sum, histdf20$multi[i])
  histdf20$avg[i]<-histdf20$avg[i]/(as.numeric(as.character(histdf20$V2[i])))
  histdf20$time<- as.numeric(20)
}

###
temp <- strsplit(hist_data21, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf21   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf21$V3<-as.character(histdf21$V3)
histdf21$hist_measurements<- strsplit(histdf21$V3, ",")#split
histdf21$num_meas<-lapply(histdf21$hist_measurements, as.numeric)#convert the last column to numeric list
histdf21$multi<-lapply(histdf21$num_meas, "*" , range1)
for(i in 1:nrow(histdf21))
{
  histdf21$avg[i]<-do.call(sum, histdf21$multi[i])
  histdf21$avg[i]<-histdf21$avg[i]/(as.numeric(as.character(histdf21$V2[i])))
  histdf21$time<- as.numeric(21)
}

###
temp <- strsplit(hist_data22, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf22   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf22$V3<-as.character(histdf22$V3)
histdf22$hist_measurements<- strsplit(histdf22$V3, ",")#split
histdf22$num_meas<-lapply(histdf22$hist_measurements, as.numeric)#convert the last column to numeric list
histdf22$multi<-lapply(histdf22$num_meas, "*" , range1)
for(i in 1:nrow(histdf22))
{
  histdf22$avg[i]<-do.call(sum, histdf22$multi[i])
  histdf22$avg[i]<-histdf22$avg[i]/(as.numeric(as.character(histdf22$V2[i])))
  histdf22$time<- as.numeric(22)
}

##
temp <- strsplit(hist_data23, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf23   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf23$V3<-as.character(histdf23$V3)
histdf23$hist_measurements<- strsplit(histdf23$V3, ",")#split
histdf23$num_meas<-lapply(histdf23$hist_measurements, as.numeric)#convert the last column to numeric list
histdf23$multi<-lapply(histdf23$num_meas, "*" , range1)
for(i in 1:nrow(histdf23))
{
  histdf23$avg[i]<-do.call(sum, histdf23$multi[i])
  histdf23$avg[i]<-histdf23$avg[i]/(as.numeric(as.character(histdf23$V2[i])))
  histdf23$time<- as.numeric(23)
}

####
temp <- strsplit(hist_data24, " ")#split rows of data file according to space
temp # you get channelid, count, and then the histogram measurements
mat  <- matrix(unlist(temp), ncol=3, byrow=TRUE)#make a matrix with 3 col
histdf24   <- as.data.frame(mat)#make a data frame
#split the last column in substrings separated by comma
#first convert last column to string
histdf24$V3<-as.character(histdf24$V3)
histdf24$hist_measurements<- strsplit(histdf24$V3, ",")#split
histdf24$num_meas<-lapply(histdf24$hist_measurements, as.numeric)#convert the last column to numeric list
histdf24$multi<-lapply(histdf24$num_meas, "*" , range1)
for(i in 1:nrow(histdf24))
{
  histdf24$avg[i]<-do.call(sum, histdf24$multi[i])
  histdf24$avg[i]<-histdf24$avg[i]/(as.numeric(as.character(histdf24$V2[i])))
  histdf24$time<- as.numeric(24)
}

################################
##get the unique channel names MASTER DIRECTORY OF CHANNELS
############################################################
############################################################
#convert the first columns to numeric
histdf1$V1<-as.numeric(as.character(histdf1$V1))
histdf2$V1<-as.numeric(as.character(histdf2$V1))
histdf3$V1<-as.numeric(as.character(histdf3$V1))
histdf4$V1<-as.numeric(as.character(histdf4$V1))
histdf5$V1<-as.numeric(as.character(histdf5$V1))
histdf6$V1<-as.numeric(as.character(histdf6$V1))
histdf7$V1<-as.numeric(as.character(histdf7$V1))
histdf8$V1<-as.numeric(as.character(histdf8$V1))
histdf9$V1<-as.numeric(as.character(histdf9$V1))
histdf10$V1<-as.numeric(as.character(histdf10$V1))
histdf11$V1<-as.numeric(as.character(histdf11$V1))
histdf12$V1<-as.numeric(as.character(histdf12$V1))
histdf13$V1<-as.numeric(as.character(histdf13$V1))
histdf14$V1<-as.numeric(as.character(histdf14$V1))
histdf15$V1<-as.numeric(as.character(histdf15$V1))
histdf16$V1<-as.numeric(as.character(histdf16$V1))
histdf17$V1<-as.numeric(as.character(histdf17$V1))
histdf18$V1<-as.numeric(as.character(histdf18$V1))
histdf19$V1<-as.numeric(as.character(histdf19$V1))
histdf20$V1<-as.numeric(as.character(histdf20$V1))
histdf21$V1<-as.numeric(as.character(histdf21$V1))
histdf22$V1<-as.numeric(as.character(histdf22$V1))
histdf23$V1<-as.numeric(as.character(histdf23$V1))
histdf24$V1<-as.numeric(as.character(histdf24$V1))




########################################
#make a final heatmap matrix
########################################
unique<- data.frame(matrix(ncol = 25, nrow = 7088))
colnames(unique) <- c("unique_channels",paste("avg", 1:24, sep = ""))
unique$unique_channels<-unique(c(histdf1$V1, histdf2$V1,histdf3$V1,histdf4$V1,histdf5$V1,histdf6$V1,histdf7$V1, histdf8$V1,histdf9$V1,
                                 histdf10$V1, histdf11$V1,histdf12$V1,histdf13$V1, histdf14$V1,histdf15$V1,histdf16$V1, histdf17$V1,histdf18$V1,
                                 histdf19$V1, histdf20$V1,histdf21$V1,histdf22$V1, histdf23$V1,histdf24$V1)) 
#unique$unique_channels 7088 obs
######################
#for each channel
#get avg value for T1, T2 

for(eachrow in 1:length(unique$unique_channels))
{
  #take the channel value 
  channel<- unique$unique_channels[eachrow]
  for(iter in 1:24)
  {
    #match with the df1 and get avg 
    dfname<-paste("histdf",iter,sep="")
    tempdf<-get(dfname)
    isEmpty <- function(x) {
    return(length(x)==0)
    }
    resultavg<-tempdf[tempdf$V1==channel, "avg"]
    if((isEmpty(resultavg))==FALSE) {
      unique[eachrow,iter+1]<-tempdf[tempdf$V1==channel, "avg"]
    }
    if((isEmpty(resultavg))==TRUE)
    {
      unique[eachrow,iter+1]<-as.numeric(0)
    }
  }
}

power_matrix<-unique
power_matrix <- power_matrix[order(power_matrix$unique_channels),]

power_matrix$unique_channels<-as.character(power_matrix$unique_channels)


row.names(power_matrix) <- power_matrix$unique_channels
power_matrix<- power_matrix[,2:25]
try_matrix<-try_matrix[,2:25]
power_finalmatrix <- data.matrix(power_matrix)

power_heatmap <- heatmap(power_finalmatrix, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))


library(plotly)
library(ggplot2)

plot_ly(x=colnames(power_finalmatrix), y=rownames(power_finalmatrix), z = power_finalmatrix, type = "heatmap") %>%
  layout(title = "Power Histogram Day2",yaxis=list(type="category",title="ChannelID"),xaxis=list(title="Time - Hourly",tickmode = 'array',
                                                                  tickvals = c(0,1, 2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23),
                                                                  ticktext = c('5<br>Min', 6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,1,2,3,'4<br>Max')))
plot_ly(x=colnames(try_matrix), y=as.character(rownames(try_matrix)), z = try_matrix, type = "heatmap") %>%
  layout(title = "Power Histogram Day2",yaxis=list(type="category",title="Channel"))     
xaxis = list(title = 'Recency Class',
             tickmode = 'array',
             tickvals = c(1, 2,3,4,5),
             ticktext = c('1<br>Min', 2, 3, 4, '5<br>Max')
),
yaxis = list(title = 'Frequency Class',
             tickmode = 'array',
             tickvals = c(1, 2,3,4,5),
             ticktext = c('1<br>Min', 2, 3, 4, '5<br>Max')
)