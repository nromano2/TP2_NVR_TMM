library(tidyverse)
library(glue) #acted as a way to do an f-string
library(reshape2) #correlation matrix
library(patchwork) #subplots

##
#Reading in and Merging Datasets
##

#Reading in the student-mat and student-por csv files
df_math <- read.csv("student-mat.csv", header=TRUE, sep=';')
df_math <- add_column(df_math, subject="Mathematics", .before = "school")
head(df_math)
tail(df_math)

df_portuguese <- read.csv("student-por.csv", header=TRUE, sep = ';')
df_portuguese <- add_column(df_portuguese, subject="Portuguese", .before = "school")
head(df_portuguese)
tail(df_portuguese)

#Checking if the column names of df_math and df_portugeuse are equal
colnames(df_math)
sapply(df_math, typeof)

colnames(df_portuguese)
sapply(df_portuguese, typeof)

column_check = all.equal(colnames(df_math), colnames(df_portuguese))
dtypes_check = all.equal(sapply(df_math, typeof), sapply(df_portuguese, typeof))
glue("Same Columns: {column_check}")
glue("Same Data types: {dtypes_check}")

#Merging df_math and df_portuguese dataframes
df<- rbind(df_math, df_portuguese)
head(df)
tail(df)

##
#Processing and Cleaning Dataframe
##
# Dropped duplicated students by certain variables
df<-df[!duplicated(df[ , c("school","sex","age","address","famsize",
                           "Pstatus","Medu","Fedu","Mjob","Fjob",
                           "reason","nursery","internet")]), ]

#resetting index of merged dataframe and checking if it worked
rownames(df)<-NULL
head(df)
tail(df)

#Expanding the school name values in the school variable column
df$school[df$school=="GP"] <- "Gabriel Pereira"
df$school[df$school=="MS"] <- "Mousinho da Silveira"

#Expanding the address type values in the address variable column
df$address[df$address=="U"] <- "Urban"
df$address[df$address=="R"] <- "Rural"

#Expanding the parent status values in the Pstatus variable column
df$Pstatus[df$Pstatus=="A"] <- "Living Apart"
df$Pstatus[df$Pstatus=="T"] <- "Living Together"

colnames(df)
#Dropping columns by assignning a subset of the columns that we decided to keep
df<- df[c("subject", "school", "sex", "age", "address", 
          "Pstatus", "Medu", "Fedu", "traveltime", "studytime", 
          "failures", "schoolsup", "famsup", "paid", "activities",
          "higher", "internet", "famrel", "freetime", "goout", "Dalc", 
          "Walc", "health", "absences", "G1", "G2", "G3")]
colnames(df)

#Renaming a few column names
df <- rename(df, c("M_edu" = "Medu", "F_edu" = "Fedu", "weekday_alc"= "Dalc", 
                   "weekend_alc"= "Walc", "period1_grade" = "G1", 
                   "period2_grade" = "G2","final_grade"= "G3"))

colnames(df)


##
#Dataset Description
##
head(df)
tail(df)

dim(df)

#Function to check for the missing values for a given column, then using a for loop to return the missing columns for each of the columns
missing_values <- function(x){
  na_values <- sum(is.na(x))
  return(na_values)
}

for (x in colnames(df)){
  print(glue("Column name: {x}"))
  print(glue("Missing values: {missing_values(x)}"))
  print(glue("Total value count: {nrow(df[x])}"))
  print(glue("Missing values percentage: {missing_values(x) / nrow(df[x])}%"), "\n")
}

str(df)

#
##Summary Statistics
#

#Summary Statistics of the continuous data within the data set

#Creating the correlation matrix, its heat map and saving it
continuous_columns <- df[c("age", "failures", "absences", "period1_grade", "period2_grade", "final_grade")]
summary(continuous_columns)

correlation_matrix <- cor(continuous_columns)
print(correlation_matrix)
correlation_matrix <- melt(correlation_matrix)
print(correlation_matrix)

figure1 <- ggplot(data = correlation_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="black") + 
  scale_fill_distiller(palette = "Reds", direction = +1)+
  ggtitle("Correlation Matrix Heat Map")
figure1

ggsave("correlation_matrix", plot = figure1, device = "png")

#Value Frequency and Proportions of Categorical Columns
categorical_columns <- df[c("subject", "school", "sex", "address", "Pstatus", "M_edu", "F_edu", 
                          "traveltime", "studytime", "schoolsup", "famsup", "paid",
                          "activities", "higher", "internet", "famrel", "freetime", "goout",
                          "weekday_alc", "weekend_alc", "health")]

head(categorical_columns)

#value_stats function returns a table that contains the value count and value frequency for a given column
value_stats <- function(x){
  table1 <- table(df[x])
  table2 <- prop.table(table(df[x]))
  table3 <- rbind(table1, table2)
  rownames(table3) <- c("table1" = "Value Count", "table2" = "Proportion")
  return(table3)
}

for (x in colnames(categorical_columns)){
  print(x)
  print(lapply(x, value_stats))
}

##
#Data Set Graphical Exploration
##

#Distribution Graphs
#Grade Distribution
grade1 <- ggplot(data=df, aes(x=period1_grade)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, by=1)) +
  ggtitle("Period 1 Grade Distibutions")

grade2 <-ggplot(data=df, aes(x=period2_grade)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, by=1)) +
  ggtitle("Period 2 Grade Distibutions")

grade3 <- ggplot(data=df, aes(x=final_grade)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 20, by=1)) +
  ggtitle("Final Grade Distibutions")

figure2 <- grade1 / grade2 / grade3 +
  plot_annotation(title = "Grade Distributions", 
                  theme = theme(plot.title = element_text(hjust = 0.5)))

figure2
ggsave("grade_distributions", plot = figure2, device = "png")

#Age Distribution
figure3 <- ggplot(data=df, aes(x=age)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(15, 23, by=1)) +
  ggtitle("Age Distibutions")
figure3  
ggsave("age_distributions", plot = figure3, device = "png")

#Absences Distribution
figure4 <- ggplot(data=df, aes(x=absences)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 80, by=10)) +
  ggtitle("Absences Distibutions")
figure4
ggsave("absences_distributions", plot = figure4, device = "png")

#Failure Distribution
figure5 <-  ggplot(data=df, aes(x=failures)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  ggtitle("Failures Distibutions")
figure5
ggsave("failures_distributions", plot = figure5, device = "png")

#Scatterplots
#Grade by Course Scatterplot
grades1_2 <- ggplot(data=df) +
  geom_point(mapping = aes(x=period1_grade, y=period2_grade), color="blue") +
  xlab("Period 1 Grade") +
  ylab("Period 2 Grade") +
  scale_x_continuous(breaks = seq(0, 20, by=2)) +
  scale_y_continuous(breaks = seq(0, 20, by=2)) +
  facet_wrap(~ subject, nrow=2)

grades1_3 <- ggplot(data=df) +
  geom_point(mapping = aes(x=period1_grade, y=final_grade), color="blue") +
  xlab("Period 1 Grade") +
  ylab("Final Grade") +
  scale_x_continuous(breaks = seq(0, 20, by=2)) +
  scale_y_continuous(breaks = seq(0, 20, by=2)) +
  facet_wrap(~ subject, nrow=2)

grades2_3 <- ggplot(data=df) +
  geom_point(mapping = aes(x=period2_grade, y=final_grade), color="blue") +
  xlab("Period 2 Grade") +
  ylab("Final Grade") +
  scale_x_continuous(breaks = seq(0, 20, by=2)) +
  scale_y_continuous(breaks = seq(0, 20, by=2)) +
  facet_wrap(~ subject, nrow=2)

figure6 <- (grades1_2 + grades1_3 + grades2_3) +
  plot_annotation(title = "Grade Comparison Scatterplots by Course") &
  theme(plot.title = element_text(hjust = 0.5))

figure6 
ggsave("grade_scatterplot", plot = figure6, device = "png")

#Bar Charts and Boxplots
nominal_graphs <- function(columnname, overall_title, title1, title1_size, title2, title2_size, title3, title3_size, title4, title4_size, labelx, label_y1, label_y2, label_y3, axis_size, fill_value){
  figure7_bar <- ggplot(data=df) +
    geom_bar(mapping=aes(x=columnname), fill="blue") +
    ggtitle(title1) +
    theme(plot.title = element_text(size = title1_size),
          axis.title = element_text(size = axis_size)) +
    xlab(labelx) +
    ylab("Student Count")
  
  figure7_box1 <- ggplot(data=df) +
    geom_boxplot(mapping=aes(x=columnname, y=period1_grade), fill=fill_value) +
    scale_y_continuous(breaks = seq(0, 20, by=2))+
    ggtitle(title2) +
    theme(plot.title = element_text(size = title2_size),
          axis.title = element_text(size = axis_size)) +
    xlab(labelx) +
    ylab(label_y1)
  
  figure7_box2 <- ggplot(data=df) +
    geom_boxplot(mapping=aes(x=columnname, y=period2_grade), fill=fill_value) +
    scale_y_continuous(breaks = seq(0, 20, by=2)) +
    ggtitle(title3) +
    theme(plot.title = element_text(size = title3_size),
          axis.title = element_text(size = axis_size)) +
    xlab(labelx) +
    ylab(label_y2)
  
  figure7_box3 <- ggplot(data=df) +
    geom_boxplot(mapping=aes(x=columnname, y=final_grade), fill=fill_value) +
    scale_y_continuous(breaks = seq(0, 20, by=2)) +
    ggtitle(title4) +
    theme(plot.title = element_text(size = title4_size),
          axis.title = element_text(size = axis_size)) +
    xlab(labelx) +
    ylab(label_y3)
  
  figure7 <- (figure7_bar + figure7_box1 + figure7_box2 + figure7_box3) +
    plot_annotation(title = overall_title, 
                    theme = theme(plot.title = element_text(hjust = 0.5)))
  return(figure7)
}

ordinal_graphs <- function(columnname, overall_title,  title1, title1_size, title2, title2_size, title3, title3_size, title4, title4_size, labelx, label_y1, label_y2, label_y3, axis_size, fill_value){
  figure8_bar <- ggplot(data = df)+
    geom_bar(mapping = aes(x=columnname), fill="blue")+
    ggtitle(title1) +
    theme(plot.title = element_text(size = title1_size),
          axis.title = element_text(size = axis_size)) +
    xlab(labelx)+
    ylab("Student Count")
  
  figure8_box1 <- ggplot(data=df) +
    geom_boxplot(mapping=aes(x=factor(columnname), y=period1_grade), fill=fill_value) +
    ggtitle(title2) +
    theme(plot.title = element_text(size = title2_size),
          axis.title = element_text(size = axis_size)) +
    scale_y_continuous(breaks = seq(0, 20, by=2))+
    xlab(labelx) +
    ylab(label_y1)
  
  figure8_box2 <- ggplot(data=df) +
    geom_boxplot(mapping=aes(x=factor(columnname), y=period2_grade),fill=fill_value) +
    ggtitle(title3) +
    theme(plot.title = element_text(size = title2_size),
          axis.title = element_text(size = axis_size)) +
    scale_y_continuous(breaks = seq(0, 20, by=2))+
    xlab(labelx) +
    ylab(label_y2)
  
  figure8_box3 <- ggplot(data=df) +
    geom_boxplot(mapping=aes(x=factor(columnname), y=final_grade), fill=fill_value) +
    ggtitle(title4) +
    theme(plot.title = element_text(size = title2_size),
          axis.title = element_text(size = axis_size)) +
    scale_y_continuous(breaks = seq(0, 20, by=2))+
    xlab(labelx) +
    ylab(label_y3)
  
  figure8 <- (figure8_bar + figure8_box1 + figure8_box2 + figure8_box3) +
    plot_annotation(title = overall_title, 
                    theme = theme(plot.title = element_text(hjust = 0.5)))
  
  return(figure8)
}

student_sex <- nominal_graphs(df$sex, "Student Sex/Gender", "Student Count by Sex", 10, "Period 1 Grade by Sex", 10, "Period 2 Grade by Sex", 10, "Final Grade by Sex", 10,"Student Sex", "Period 1 Grade", "Period 2 Grade", "Final Grade", 8, c("steelblue", "orange"))
student_sex
ggsave("student_sex", plot = student_sex, device = "png")

student_address <- nominal_graphs(df$address, "Student Address Type", "Student Count by Address Type", 9, "Period 1 Grade by Address Type", 9, "Period 2 Grade by Address Type", 9, "Final Grade by Address Type", 9,"Student Address", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("orange", "steelblue"))
student_address
ggsave("student_address_type", plot = student_address, device = "png")

student_parent_status <- nominal_graphs(df$Pstatus, "Student Parent Status", "Student Count by Parent Status", 9, "Period 1 Grade by Parent Status", 9, "Period 2 Grade by Parent Status", 9, "Final Grade by Parent Status", 9,"Student Parent Status", "Period 1 Grade", "Period 2 Grade", "Final Grade", 9, c("steelblue", "orange"))
student_parent_status
ggsave("student_parentstatus", plot = student_parent_status, device = "png")

mother_education_level <- ordinal_graphs(df$M_edu, "Mother Educational Level", "Student Count by Mother Educational Level", 8, "Period 1 Grade by Mother Educational Level", 8, "Period 2 Grade by Mother Educational Level", 8, "Final Grade by Mother Educational Level", 8, "Educational Level", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red", "purple"))
mother_education_level
ggsave("m_education", plot = mother_education_level, device = "png")

father_education_level <- ordinal_graphs(df$F_edu, "Father Educational Level", "Student Count by Father Educational Level", 8, "Period 1 Grade by Father Educational Level", 8, "Period 2 Grade by Father Educational Level", 8, "Final Grade by Father Educational Level", 8, "Educational Level", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red", "purple"))
father_education_level
ggsave("f_education", plot = father_education_level, device = "png")

student_traveltime <- ordinal_graphs(df$traveltime, "Student Travel Time", "Student Count by Travel Time", 9, "Period 1 Grade by Travel Time", 9, "Period 2 Grade by Travel Time", 9, "Final Grade by Travel Time", 9, "Travel Time", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red"))
student_traveltime
ggsave("student_traveltime", plot = student_traveltime, device = "png")


student_studytime <- ordinal_graphs(df$studytime, "Student Study Time", "Student Count by Study Time", 9, "Period 1 Grade by Study Time", 9, "Period 2 Grade by Study Time", 9, "Final Grade by Study Time", 9, "Study Time", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red"))
student_studytime
ggsave("student_studytime", plot = student_studytime, device = "png")


school_support <- nominal_graphs(df$schoolsup, "Student School Educational Support", "Student Count by School Educational Support", 8, "Period 1 Grade by School Educational Support", 8, "Period 2 Grade by School Educational Support", 8, "Final Grade by School Educational Support", 8, "School Educational Support", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("orange", "steelblue"))
school_support
ggsave("student_schoolsupport", plot = school_support, device = "png")


family_support <- nominal_graphs(df$famsup, "Student Family Educational Support", "Student Count by Family Educational Support", 9, "Period 1 Grade by Family Educational Support", 9, "Period 2 Grade by Family Educational Support", 9, "Final Grade by Family Educational Support", 9,"Family Educational Support", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange"))
family_support
ggsave("student_familysupport", plot = family_support, device = "png")


extra_classes <- nominal_graphs(df$paid, "Extra Classes Paid", "Student Count by Those Who Paid for Extra Classes", 8, "Period 1 Grade by Those Who Paid for Extra Classes", 8, "Period 2 Grade by Those Who Paid for Extra Classes", 8, "Final Grade by Those Who Paid for Extra Classes", 8,"Paid for Extra Classes", "Period 1 Grade", "Period 2 Grade", "Final Grade", 6, c("steelblue", "orange"))
extra_classes
ggsave("student_paid", plot = extra_classes, device = "png")


activities <- nominal_graphs(df$activities, "Student Extra Curricular Activities", "Student Count by Those Who Participate in Extra Curricular Activities", 6, "Period 1 Grade by Those Who Participate in Extra Curricular Activities", 6, "Period 2 Grade by Those Who Participate in Extra Curricular Activities", 6, 
                             "Final Grade by Those Who Participate in Extra Curricular Activities", 6,"Participate in Extra Curricular Activities", "Period 1 Grade", "Period 2 Grade", "Final Grade", 6, c("steelblue", "orange"))
activities
ggsave("student_activites", plot = activities, device = "png")


higher_edu <- nominal_graphs(df$higher, "Pursuit of Higher Education", "Student Count by Those Considering to Seek Hihger Education", 7, "Period 1 Grade by Those Considering to Seek Hihger Education", 7, 
                             "Period 2 Grade by Those Considering to Seek Hihger Education", 7, "Final Grade by Those Considering to Seek Hihger Education", 7, "Considering to Seek Hihger Education", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("orange", "steelblue"))
higher_edu
ggsave("student_highereducation", plot = higher_edu, device = "png")


internet_access <- nominal_graphs(df$internet, "Internet Access", "Student Count by Those Who Have Access to Internet", 7, "Period 1 Grade by Those Who Have Access to Internet", 7, "Period 2 Grade by Those Who Have Access to Internet", 7, "Final Grade by Those Who Have Access to Internet", 7,"Internet Access", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange"))
internet_access
ggsave("student_internetaccess", plot = internet_access, device = "png")


family_relationships <- ordinal_graphs(df$famrel, "Quality of Family Relationships", "Student Count by Quality of Family Relationships", 8, "Period 1 Grade by Quality of Family Relationships", 8, "Period 2 Grade by Quality of Family Relationships", 8, "Final Grade by Quality of Family Relationships", 8, "Quality of Family Relationships", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red", "purple"))
family_relationships
ggsave("student_familyrelationships", plot = family_relationships, device = "png")


student_freetime <- ordinal_graphs(df$freetime, "Amount of Free Time", "Student Count by Amount of Free Time", 9, "Period 1 Grade by Amount of Free Time", 9, "Period 2 Grade by Amount of Free Time", 9, "Final Grade by Amount of Free Time", 9, "Amount of Free Time", "Period 1 Grade", "Period 2 Grade", "Final Grade", 8, c("steelblue", "orange", "dark green", "dark red", "purple"))
student_freetime
ggsave("student_freetime", plot = student_freetime, device = "png")


student_timespentout <- ordinal_graphs(df$goout, "Time Spent Out with Friends", "Student Count by Time Spent Out with Friends", 8, "Period 1 Grade by Time Spent Out with Friends", 8, "Period 2 Grade by Time Spent Out with Friends", 8, "Final Grade by Time Spent Out with Friends", 8, "Time Spent Out with Friends", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red", "purple"))
student_timespentout
ggsave("student_goout", plot = student_timespentout, device = "png")


weekday_alc <- ordinal_graphs(df$weekday_alc, "Student Weekday Alcohol Consumption", "Student Count by Weekday Alcohol Consumption", 8, "Period 1 Grade by Weekday Alcohol Consumption", 8, "Period 2 Grade by Weekday Alcohol Consumption", 8, "Final Grade by Weekday Alcohol Consumption", 8, "Weekday Alcohol Consumption", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red", "purple"))
weekday_alc
ggsave("student_weekdayalc", plot = weekday_alc, device = "png")


weekend_alc <- ordinal_graphs(df$weekend_alc, "Student Weekend Alcohol Consumption", "Student Count by Weekend Alcohol Consumption", 8, "Period 1 Grade by Weekend Alcohol Consumption", 8, "Period 2 Grade by Weekend Alcohol Consumption", 8, "Final Grade by Weekend Alcohol Consumption", 8, "Weekend Alcohol Consumption", "Period 1 Grade", "Period 2 Grade", "Final Grade", 7, c("steelblue", "orange", "dark green", "dark red", "purple"))
weekend_alc
ggsave("student_weekendalc", plot = weekend_alc, device = "png")


health_status <- ordinal_graphs(df$health, "Health Status", "Student Count by Health Status", 9, "Period 1 Grade by Health Status", 9, "Period 2 Grade by Health Status", 9, "Final Grade by Health Status", 9, "Health Status", "Period 1 Grade", "Period 2 Grade", "Final Grade", 8, c("steelblue", "orange", "dark green", "dark red", "purple"))
health_status
ggsave("health_status", plot = health_status, device = "png")


