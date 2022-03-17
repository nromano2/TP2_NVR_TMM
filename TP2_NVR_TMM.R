library(dplyr) #imported for rename function
library(plyr)
library(tidyverse)
library(glue) #acted as a way to do an f-string
library(ggplot2)
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

#Dropping columns by assignning a subset of the columns that we decided to keep
df<- df[c("subject", "school", "sex", "age", "address", 
          "Pstatus", "Medu", "Fedu", "traveltime", "studytime", 
          "failures", "schoolsup", "famsup", "paid", "activities",
          "higher", "internet", "famrel", "freetime", "goout", "Dalc", 
          "Walc", "health", "absences", "G1", "G2", "G3")]
colnames(df)

#Renaming a few column names
df <- rename(df, c("Medu" = "M_edu", "Fedu" = "F_edu", "Dalc" = "weekday_alc", 
            "Walc"= "weekend_alc", "G1" = "period1_grade", 
            "G2"= "period2_grade", "G3"= "final_grade"))

colnames(df)


##
#Dataset Description
##
head(df)
tail(df)

dim(df)

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

#Creating the correlation matrix, its heatmap and saving it
continuous_columns <- df[c("age", "failures", "absences", "period1_grade", "period2_grade", "final_grade")]
summary(continuous_columns)

correlation_matrix <- cor(continuous_columns)
print(correlation_matrix)
correlation_matrix <- melt(correlation_matrix)
print(correlation_matrix)

ggplot(data = correlation_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color="white") + 
  ggtitle("Correlation Matrix Heat Map")

ggsave("correlation_matrix", plot = last_plot(), device = "png")



categorical_columns <- df[c("subject", "school", "sex", "address", "Pstatus", "M_edu", "F_edu", 
                          "traveltime", "studytime", "schoolsup", "famsup", "paid",
                          "activities", "higher", "internet", "famrel", "freetime", "goout",
                          "weekday_alc", "weekend_alc", "health")]

head(categorical_columns)


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
figure2 <- grade1 / grade2 / grade3
figure2
ggsave("grade_distributions", plot = last_plot(), device = "png")

figure3 <- ggplot(data=df, aes(x=age)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(15, 23, by=1)) +
  ggtitle("Age Distibutions")
figure3  
ggsave("age_distributions", plot = last_plot(), device = "png")

figure4 <- ggplot(data=df, aes(x=absences)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  scale_x_continuous(breaks = seq(0, 80, by=10)) +
  ggtitle("Absences Distibutions")
figure4
ggsave("absences_distributions", plot = last_plot(), device = "png")


figure5 <-  ggplot(data=df, aes(x=failures)) + 
  geom_histogram(color="black", fill="blue", binwidth = 1) +
  ggtitle("Failures Distibutions")
figure5
ggsave("failures_distributions", plot = figure5, device = "png")


grades1_2 <- ggplot(data=df) +
  geom_point(mapping = aes(x=period1_grade, y=period2_grade), color="blue") +
  scale_x_continuous(breaks = seq(0, 20, by=2)) +
  scale_y_continuous(breaks = seq(0, 20, by=2)) +
  facet_wrap(~ subject, nrow=2)
grades1_3 <- ggplot(data=df) +
  geom_point(mapping = aes(x=period1_grade, y=final_grade), color="blue") +
  scale_x_continuous(breaks = seq(0, 20, by=2)) +
  scale_y_continuous(breaks = seq(0, 20, by=2)) +
  facet_wrap(~ subject, nrow=2)
grades2_3 <- ggplot(data=df) +
  geom_point(mapping = aes(x=period2_grade, y=final_grade), color="blue") +
  scale_x_continuous(breaks = seq(0, 20, by=2)) +
  scale_y_continuous(breaks = seq(0, 20, by=2)) +
  facet_wrap(~ subject, nrow=2)
figure6 <- grades1_2 + grades1_3 + grades2_3
figure6 