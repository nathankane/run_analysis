#The following script merges the provided Samsung Smartphone data into a Tidy Data set that displays 
#the average value of each mean and standard deviation variable for each activity and subject.

#Before running this script, make sure all related files have been downloaded to the working directory.

subject_train<-read.table("subject_train.txt")  #imports all files
x_train<-read.table("x_train.txt")
y_train<-read.table("y_train.txt")
subject_test<-read.table("subject_test.txt")
x_test<-read.table("y_test.txt")
y_test<-read.table("y_test.txt")
features<-read.table("features.txt")

y<-rbind(y_train,y_test)                        #binds files into one data set
x<-rbind(X_train,x_test)
subject<-rbind(subject_train,subject_test)
colnames(x)<- features[,2]                      #names all columns
colnames(y)<- "Activity"
colnames(subject)<- "Subject"
for (i in 1:10299){                             #defines all activity types
  if(y[i,]==1){
    y[i,]="Walking"
  }else if(y[i,]==2){
    y[i,]="Walking Up"
  }else if(y[i,]==3){
    y[i,]="Walking Down"
  }else if(y[i,]==4){
    y[i,]="Sitting"
  }else if(y[i,]==5){
    y[i,]="Standing"
  }else if(y[i,]==6){
    y[i,]="Laying"
  }
}  
for(i in 561:1){                               #removes all rows not related to standard deviation or mean
  if(length(grep("mean()",features[i,2]))==0 & length(grep("std",features[i,2]))==0){
    x[i]<-NULL
    }else if(length(grep("Freq",features[i,2]))!=0){
    x[i]<-NULL
    }    
}
alldata<-cbind(subject,y,x)
sp <- split(alldata[,3:68], list(alldata$Activity, alldata$Subject))
TidyData<-sapply(sp, colMeans)                #creates Tidy Data set
TidyData