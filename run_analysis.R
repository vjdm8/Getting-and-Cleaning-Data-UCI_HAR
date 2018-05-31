#Peer Review Assignment

library(dplyr)

# Read in data
X_train<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/train/X_train.txt')
y_train<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/train/y_train.txt')
X_test<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/test/X_test.txt')
y_test<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/test/y_test.txt')
subj_tr<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/train/subject_train.txt')
subj_ts<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/test/subject_test.txt')

#Bind Columns
Train<-bind_cols(X_train,y_train,subj_tr)
Test<-bind_cols(X_test,y_test,subj_ts)

#Bind rows
HAR_df<-bind_rows(Train,Test)
names(HAR_df)
#rm('X_test','X_train','y_test','y_train','subj_tr','subj_ts')

#read in feature table
features<-read.table('C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/features.txt')
#features$V2

#create new row for Activity Label and Subject
newrow<-cbind(1100,'ActivityLabel')
newrow2<-cbind(1101,'Subject')
#Row bind
features<-rbind(features,newrow,newrow2)
#Replace Column Names
names(HAR_df)<-features$V2

#STEP 4
#Make column names valid
valid_column_names <- make.names(names=names(HAR_df), unique=TRUE, allow_ = TRUE)
names(HAR_df)<-valid_column_names 

#Step 2 & 3
HAR_df<-HAR_df%>%
  select(matches("Activity|mean|std|standard|Subject"))%>%
  select(-matches("Freq"))%>%
  mutate(ActivityLabel=recode_factor(HAR_df$ActivityLabel,'1'= 'WALKING',
                                     '2'='WALKING_UPSTAIRS',
                                     '3'='WALKING_DOWNSTAIRS',
                                     '4'='SITTING',
                                     '5'='STANDING',
                                     '6'='LAYING'
))%>%
  mutate(Subject=factor(Subject))


#step 5: Create Grouped by data set
HAR_df2<-HAR_df %>% group_by(ActivityLabel,Subject) %>% summarize_all(mean)

#Write out file
write.table(HAR_df, "C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/TIDY_SET.txt", sep="\t")
write.table(HAR_df2, "C:/Users/i59206/Documents/JHDS/Getting Data/UCI HAR Dataset/GROUPED_SET.txt", sep="\t")