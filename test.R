#FILE LOADING################################################################################
#DO NOT FORGET TO REPLACE THE CURRENT VALUE OF DATA FRAME ACCORDING TO THE COLUMN YOU EXAMINE
data_assists <- data.frame(read.csv("C:\\Users\\GR828ZN\\Desktop\\test.csv", header = T, sep = ";"))

#Check that everything is ok
head(data_assists)

#DATA PREPARATION################
#Check for Outliers using boxplot
outlier_values <- boxplot.stats(data_assists$assists)$out  # outlier values.
print(outlier_values)
graph <- boxplot(data_assists$assists, main="assists", boxwex=0.1)
mtext(paste("Outliers: ", paste(outlier_values, collapse=", ")), cex=0.6)

#means per team (please adjust the column you want to test within the [])
for (i in 1:nrow(data_assists)) {
  data_assists$min_team_assists[i]<-min(subset(data_assists, data_assists$team==data_assists$team[i])['assists'])
  data_assists$max_team_assists[i]<-max(subset(data_assists, data_assists$team==data_assists$team[i])['assists'])
  summy<-sum(subset(data_assists, data_assists$team==data_assists$team[i])['assists'])
  count<-nrow(subset(data_assists, data_assists$team==data_assists$team[i]))
  data_assists$aver_team_assists[i] <- summy/count
}

#DATA MANIPULATION######################################################
#means per age (please adjust the column you want to test within the [])
for (i in 1:nrow(data_assists)) {
  data_assists$min_age_assists[i]<-min(subset(data_assists, data_assists$age==data_assists$age[i])['assists'])
  data_assists$max_age_assists[i]<-max(subset(data_assists, data_assists$age==data_assists$age[i])['assists'])
  summy<-sum(subset(data_assists, data_assists$age==data_assists$age[i])['assists'])
  count<-nrow(subset(data_assists, data_assists$age==data_assists$age[i]))
  data_assists$aver_age_assists[i] <- summy/count
}

#find the lowest score and the added range for age
for (i in 1:nrow(data_assists)) {
  data_assists$range_age_assists[i] <- data_assists$max_age_assists[i]-data_assists$min_age_assists[i]
  data_assists$step_age_assists[i] <- (data_assists$max_age_assists[i]-data_assists$min_age_assists[i])/10
}

#create the age list and age score
for (j in 1:nrow(data_assists)) {
  age_list<-list()
  for (i in 1:10) {
    if (i==1) {
      k<-data_assists$min_age_assists[j]+data_assists$step_age_assists[j]
      age_list[i]<-k
    }
    if (i>1) {
      k<- k+data_assists$step_age_assists[j]
      age_list[i]<-k
    }
    #print(age_list)
  }
  #Create the age score
  if (data_assists$assists[j]<=age_list[1]) {
    data_assists$score_age_assists[j]<-1
  }
  else if(data_assists$assists[j]<=age_list[2]) {
    data_assists$score_age_assists[j]<-2
  }
  else if (data_assists$assists[j]<=age_list[3]) {
    data_assists$score_age_assists[j]<-3
  }
  else if (data_assists$assists[j]<=age_list[4]) {
    data_assists$score_age_assists[j]<-4
  }
  else if (data_assists$assists[j]<=age_list[5]) {
    data_assists$score_age_assists[j]<-5
  }
  else if (data_assists$assists[j]<=age_list[6]) {
    data_assists$score_age_assists[j]<-6
  }
  else if (data_assists$assists[j]<=age_list[7]) {
    data_assists$score_age_assists[j]<-7
  }
  else if (data_assists$assists[j]<=age_list[8]) {
    data_assists$score_age_assists[j]<-8
  }
  else if (data_assists$assists[j]<=age_list[9]) {
    data_assists$score_age_assists[j]<-9
  }
  else {
    data_assists$score_age_assists[j]<-10
  }
}

#find the lowest score and the added range for team
for (i in 1:nrow(data_assists)) {
  data_assists$range_team_assists[i] <- data_assists$max_team_assists[i]-data_assists$min_team_assists[i]
  data_assists$step_team_assists[i] <- (data_assists$max_team_assists[i]-data_assists$min_team_assists[i])/10
  
}

#create the team list and team score
for (j in 1:nrow(data_assists)) {
  team_list<-list()
  for (i in 1:10) {
    if (i==1) {
      k<-data_assists$min_team_assists[j]+data_assists$step_team_assists[j]
      team_list[i]<-k
    }
    if (i>1) {
      k<- k+data_assists$step_team_assists[j]
      team_list[i]<-k
    }
    #print(team_list)
  }
  #Create the age score
  if (data_assists$assists[j]<=team_list[1]) {
    data_assists$score_team_assists[j]<-1
  }
  else if(data_assists$assists[j]<=team_list[2]) {
    data_assists$score_team_assists[j]<-2
  }
  else if (data_assists$assists[j]<=team_list[3]) {
    data_assists$score_team_assists[j]<-3
  }
  else if (data_assists$assists[j]<=team_list[4]) {
    data_assists$score_team_assists[j]<-4
  }
  else if (data_assists$assists[j]<=team_list[5]) {
    data_assists$score_team_assists[j]<-5
  }
  else if (data_assists$assists[j]<=team_list[6]) {
    data_assists$score_team_assists[j]<-6
  }
  else if (data_assists$assists[j]<=team_list[7]) {
    data_assists$score_team_assists[j]<-7
  }
  else if (data_assists$assists[j]<=team_list[8]) {
    data_assists$score_team_assists[j]<-8
  }
  else if (data_assists$assists[j]<=team_list[9]) {
    data_assists$score_team_assists[j]<-9
  }
  else {
    data_assists$score_team_assists[j]<-10
  }
}

#drop ranges and steps
data_assists$range_age_assists <- NULL
data_assists$step_age_assists <- NULL
data_assists$range_team_assists <- NULL
data_assists$step_team_assists <- NULL

#Create the differences of scores
data_assists$diff_age_assists <- 10 - data_assists$score_age_assists
data_assists$diff_team_assists <- 10 - data_assists$score_team_assists

#Create percentages from average
data_assists$percentage_age_assists <- (data_assists$assists - data_assists$aver_age_assists)/data_assists$aver_age_assists
data_assists$percentage_team_assists <- (data_assists$assists - data_assists$aver_team_assists)/data_assists$aver_team_assists

#DATA EXPORTING###################
#Export the complete category file
#write.csv(data_assists, "C:\\Users\\GR828ZN\\Desktop\\data_assists.csv", row.names = F)
#Scores category output (to be exported into .csv)
output_assists<-cbind(data_assists[,1:3], data_assists['assists'],data_assists['score_age_assists'], data_assists['score_team_assists'])
print(head(output_assists))
#write.csv(output_assists, "C:\\Users\\GR828ZN\\Desktop\\output_assists.csv", row.names = F)


#Combine all files into one file to use it for visuals (Uses cbind and take the whole first table)
#full_data <- data_assists[1:3]
full_data <- cbind(full_data, data_assists['assists'],data_assists[6:ncol(data_assists)])
full_data
write.csv(full_data, "C:\\Users\\GR828ZN\\Desktop\\full_data.csv", row.names = F)
