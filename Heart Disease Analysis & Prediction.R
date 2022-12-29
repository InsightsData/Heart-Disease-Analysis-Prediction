
# Read in the dataset
library(foreign)
CDC <- read.xport("/Users/luzhang/Desktop/Data Analysis/Final Project/LLCP2020.XPT")
# Check dimension & structure
dim(CDC)
str(CDC)
# Get 401,958 records and 279 variables

# Select records from completed surveys
# Get 330,619 records
CDC <- CDC[CDC$DISPCODE == 1100,]

library(dplyr)
attach(CDC)
# Keep 29 columns after careful selection
CDC <- select(CDC, X_AGE_G, X_SEX, X_IMPRACE, X_BMI5CAT, X_RFHLTH, X_TOTINDA, SLEPTIM1, X_MICHD, CVDSTRK3, X_LTASTH1, CHCSCNCR, CHCOCNCR, CHCCOPD2, HAVARTH4, ADDEPEV3, CHCKDNY2, DIABETE4, RMVTETH4, MARITAL, VETERAN3, DIFFWALK, DIFFDRES, DIFFALON, SMOKE100, X_RFSMOK3, X_RFDRHV7, FALL12MN, HAVEHEPC, HAVEHEPB)
# Change column names for better readability
colnames(CDC) <- c("Age", "Gender", "Race", "BMI", "GeneralHealth",  "Exercise", "SleepTime", "HeartDisease", "Stroke", "Asthma", "SkinCancer", "OtherCancer", "PulmonaryDisease", "Arthritis", "Depression",  "KidneyDisease", "Diabetes", "Teeth",  "Marital", "Military", "DiffWalk", "DiffDress",  "DiffAlone", "Smoke100", "CurrentSmoker", "HeavyDrinker", "Fall", "HepatitisC", "HepatitisB")
# Check numbers of NAs in each column
sapply(CDC,function(x) sum(is.na(x))) 
# Remove columns containing NAs of more than a half
CDC <- CDC[,!sapply(CDC, function(x) mean(is.na(x)))>0.5]
# The last two variables are gone
# Now the data set has 27 variables

# Replace “88” in Fall with zero
# “88” represents “None”: didn’t fall in the past 12 years
CDC["Fall"][CDC["Fall"] == 88] <- 0
# Delete “77”/”99” in SleepTime & Fall
# “77”/”99” represent answers of “Not sure”/”Refused(to answer)”
# Keeping them may influence the calculation of the mean
CDC["SleepTime"][CDC["SleepTime"] == 77] <- NA
CDC["SleepTime"][CDC["SleepTime"] == 99] <- NA
CDC["Fall"][CDC["Fall"] == 77] <- NA
CDC["Fall"][CDC["Fall"] == 99] <- NA
# Remove NAs in HeartDisease & SleepTime & Marital & Fall & BMI
CDC <- subset(CDC, !is.na(HeartDisease))
CDC <- subset(CDC, !is.na(Marital))
CDC <- subset(CDC, !is.na(SleepTime))
CDC <- subset(CDC, !is.na(Fall))
CDC <- subset(CDC, !is.na(BMI))
# Get a data set of 213k records and 27 variables
dim(CDC)
str(CDC)

write.csv(CDC, "/Users/luzhang/Desktop/Data Analysis/Final Project/CDC.csv", row.names=FALSE)

detach(CDC)

# Create a new dataset with different data types 
# (for better visualization)
CDC_des <- CDC
# Convert numbers to factors
# Add labels
# Keep “SleepTime” & “Fall” as numeric ones
CDC_des[,'Age'] <- factor(CDC_des[,'Age'], labels = c("18-24", "25-34", "35-44", "45-54", "55-64", ">65"))
CDC_des[,'Gender'] <- factor(CDC_des[,'Gender'], labels = c("Male", "Female"))
CDC_des[,'Race'] <- factor(CDC_des[,'Race'], labels = c("White", "Black", "Asian", "American Indian/Alaskan Native", "Hispanic", "Other race"))
CDC_des[,'BMI'] <- factor(CDC_des[,'BMI'], labels = c("Underweight", "Normal weight", "Overweight", "Obese"))
CDC_des[,'GeneralHealth'] <- factor(CDC_des[,'GeneralHealth'], labels = c("Good or Better", "Fair or Poor", "Not sure"))
CDC_des[,'Exercise'] <- factor(CDC_des[,'Exercise'], labels = c("Yes", "No", "Not sure"))
CDC_des[,'HeartDisease'] <- factor(CDC_des[,'HeartDisease'], labels = c("Yes", "No"))
CDC_des[,'Stroke'] <- factor(CDC_des[,'Stroke'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'Asthma'] <- factor(CDC_des[,'Asthma'], labels = c("Yes", "No", "Not sure"))
CDC_des[,'SkinCancer'] <- factor(CDC_des[,'SkinCancer'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'OtherCancer'] <- factor(CDC_des[,'OtherCancer'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'PulmonaryDisease'] <- factor(CDC_des[,'PulmonaryDisease'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'Arthritis'] <- factor(CDC_des[,'Arthritis'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'Depression'] <- factor(CDC_des[,'Depression'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'KidneyDisease'] <- factor(CDC_des[,'KidneyDisease'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'Diabetes'] <- factor(CDC_des[,'Diabetes'], labels = c("Yes", "Yes(pregnant)", "No", "No(prediabetes)", "Not sure", "Refused"))
CDC_des[,'Teeth'] <- factor(CDC_des[,'Teeth'], labels = c("1-5", ">6", "All", "Not sure", "0", "Refused"))
CDC_des[,'Marital'] <- factor(CDC_des[,'Marital'], labels = c("Married", "Divorced", "Widowed", "Separated", "Never married", "Domestic relation", "Refused"))
CDC_des[,'Military'] <- factor(CDC_des[,'Military'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'DiffWalk'] <- factor(CDC_des[,'DiffWalk'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'DiffDress'] <- factor(CDC_des[,'DiffDress'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'DiffAlone'] <- factor(CDC_des[,'DiffAlone'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'Smoke100'] <- factor(CDC_des[,'Smoke100'], labels = c("Yes", "No", "Not sure", "Refused"))
CDC_des[,'CurrentSmoker'] <- factor(CDC_des[,'CurrentSmoker'], labels = c("No", "Yes", "Not sure"))
CDC_des[,'HeavyDrinker'] <- factor(CDC_des[,'HeavyDrinker'], labels = c("No", "Yes", "Not sure"))
dim(CDC_des)
str(CDC_des)
# Summary
summary(CDC_des)

write.csv(CDC_des, "/Users/luzhang/Desktop/Data Analysis/Final Project/CDC_des.csv", row.names=FALSE)

# Graphs
#----
library(ggplot2)
library(scales)
library(lessR)
library(tidyverse)
attach(CDC_des)
# p1 - Age
Age_vec <- data.frame(Age)
Age_count <- data.frame(count(Age_vec, Age))
p1 <- ggplot(Age_count, aes(x=n, y=Age)) +
      geom_col(fill="7ECDBB") +
      theme_classic() +
      labs(title="Bar Chart of Age Group", x="Count", y="Age Groups")

# p2 - Gender - Pie chart
Gender_pie <- CDC_des %>% 
              group_by(Gender) %>%
              count() %>% 
              ungroup() %>% 
              mutate(perc = `n` / sum(`n`)) %>% 
              arrange(perc) %>%
              mutate(labels = scales::percent(perc))
p2 <- ggplot(Gender_pie, aes(x ="", y=perc, fill=Gender)) +
             geom_col() +
             geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
             coord_polar(theta = "y") +
             theme(panel.background = element_rect(fill = "white"),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank()) +
                   labs(title="Pie Chart of Gender Distribution")

# p3 - Race - Pie chart
Race_vec <- data.frame(Race)
Race_count <- data.frame(count(Race_vec, Race))
cols <-  hcl.colors(length(levels(Race)), "Fall")
Race_count %>% 
          dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = .1, trim = FALSE))
p3 <- PieChart(Race, data = Race_vec, hole = 0,
               fill = cols, main = "Pie Chart of Race Distribution",
               labels_cex = 0.6)

# p4 - BMI - Bar chart
BMI_sum <- CDC_des %>% 
           dplyr::count(BMI, sort = TRUE) %>% 
           dplyr::mutate(BMI = forcats::fct_rev(forcats::fct_inorder(BMI)),)
           BMI_sum <- BMI_sum %>% 
           dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = .1, trim = FALSE))
colnames(BMI_sum)
p4 <- ggplot(BMI_sum, aes(x = n, y = BMI)) +
             geom_col(fill = "Orange") +
             geom_text(aes(label = perc), hjust = 1, nudge_x = -.5) +
             theme_minimal() +
             labs(title="Bar Chart of BMI", x="Count", y="BMI") 

# p5 - GeneralHealth
p5 <- ggplot(CDC_des, aes(x=as.factor(GeneralHealth), fill=as.factor(GeneralHealth) )) + 
             geom_bar( ) +
             scale_fill_brewer(palette = "Set1") +
             coord_flip() +
             theme_minimal() + 
             theme(legend.position="none") +
             labs(title="Bar Chart of General Health", y="Count", x="General Health Type")

# p6 - Exercise - Bar chart
p6 <- ggplot(CDC_des, aes(x=as.factor(Exercise), fill=as.factor(Exercise) )) + 
             geom_bar( ) +
             scale_fill_hue(c = 30) +
             coord_flip() +
             theme_minimal() + 
             theme(legend.position="none") +
             labs(title="Bar Chart of Exercise", y="Count", x="Did you exercise in the last 30 days?")

# p7 - SleepTime - Histogram with mean
plot(1:30)
p7 <- ggplot(CDC_des, aes(x=SleepTime, fill = SleepTime)) +  
             geom_histogram(binwidth=1) +
             theme(legend.position="none") +
             geom_vline(aes(xintercept=mean(SleepTime), color="red", linetype="dashed", linewidth=1)) +
             annotate("text",  
                      x = mean(SleepTime) * 2.3,
                      y = mean(SleepTime) * 3000,
                      label = paste("Mean =", round(mean(SleepTime),2)),
                      col = "red",
                      size = 6) + 
             theme_minimal() +
             theme(legend.position="none") +
             labs(title="Histogram of Sleep Time", x="Sleep Time (Hour)", y="Count")

# p8 - HeartDisease - Donut chart
Heart_vec <- data.frame(HeartDisease)
Heart_count <- data.frame(count(Heart_vec, HeartDisease))
hsize <- 2
Heart_count <- Heart_count %>% mutate(x = hsize)
p8 <- ggplot(Heart_count, aes(x = hsize, y = n, fill = HeartDisease)) +
             geom_col() + 
             coord_polar(theta = "y") +
             xlim(c(0.2, hsize + 0.5)) +
             scale_fill_brewer(palette = "Set2") +
             geom_text(aes(label = n),
             position = position_stack(vjust = 0.5)) +
             theme(panel.background = element_rect(fill = "white"),
                   panel.grid = element_blank(),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank()) + 
                   labs(title="Donut Chart of Heart Disease Distribution")

# p9 - Stroke
Stroke_pie <- CDC_des %>% 
              group_by(Stroke) %>%
              count() %>% 
              ungroup() %>% 
              mutate(perc = `n` / sum(`n`)) %>% 
              arrange(perc) %>%
              mutate(labels = scales::percent(perc))
p9 <- ggplot(Stroke_pie, aes(x ="", y=perc, fill=Stroke)) +
             geom_col() +
             geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
             coord_polar(theta = "y") +
             theme(panel.background = element_rect(fill = "white"),
                   axis.title = element_blank(),
                   axis.ticks = element_blank(),
                   axis.text = element_blank()) +
             labs(title="Pie Chart of Stroke")

# p10 - Asthma
Asthma_pie <- CDC_des %>% 
              group_by(Asthma) %>%
              count() %>% 
              ungroup() %>% 
              mutate(perc = `n` / sum(`n`)) %>% 
              arrange(perc) %>%
              mutate(labels = scales::percent(perc))
p10 <- ggplot(Asthma_pie, aes(x ="", y=perc, fill=Asthma)) +
              geom_col() +
              geom_text(aes(label = labels), position = position_stack(vjust = 0.5)) +
              coord_polar(theta = "y") +
              theme(panel.background = element_rect(fill = "white"),
                    axis.title = element_blank(),
                    axis.ticks = element_blank(),
                    axis.text = element_blank()) +
              labs(title="Pie Chart of Asthma")

# p11 - SkinCancer
p11 <- ggplot(CDC_des, aes(x=SkinCancer, fill=SkinCancer)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Skin Cancer", y="Count")
 
# p12 - OtherCancer
p12 <- ggplot(CDC_des, aes(x=OtherCancer, fill=OtherCancer)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Other Cancer", y="Count")

# p13 - PulmonaryDisease
p13 <- ggplot(CDC_des, aes(x=PulmonaryDisease, fill=PulmonaryDisease)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Pulmonary Disease", y="Count")

# p14 - Arthritis
p14 <- ggplot(CDC_des, aes(x=Arthritis, fill=Arthritis)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Arthritis", y="Count")

# p15 - Depression
p15 <- ggplot(CDC_des, aes(x=Depression, fill=Depression)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Depression", y="Count")

# p16 - KidneyDisease
p16 <- ggplot(CDC_des, aes(x=KidneyDisease, fill=KidneyDisease)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Kidney Disease", y="Count")

# p17 - Diabetes
p17 <- ggplot(CDC_des, aes(x=Diabetes, fill=Diabetes)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Diabetes", y="Count")

# p18 - Teeth - Bar chart with No.1/2/3 in different colors
Teeth_sum <- CDC_des %>% 
             dplyr::count(Teeth, sort = TRUE) %>% 
             dplyr::mutate(Teeth = forcats::fct_rev(forcats::fct_inorder(Teeth)),
                           Teeth = forcats::fct_relevel(Teeth, "Refused", after = 0))
Teeth_sum <- Teeth_sum %>% 
             dplyr::mutate(perc = scales::percent(n / sum(n), accuracy = .1, trim = FALSE))
pal <- c("gray85",
         rep("gray70", length(Teeth_sum$Teeth) - 4), "coral2", "mediumpurple1", "goldenrod1")
p18 <- ggplot(Teeth_sum, aes(x = n, y = Teeth, fill=Teeth)) +
              geom_col() +
              geom_text(aes(label = perc), hjust = 1, nudge_x = -.5) +
              scale_fill_manual(values = pal, guide = "none") +
              theme_minimal() +
              labs(title="Bar chart of Removed Teeth", x="Count", y="Removed Teeth")

# p19 - Marital - Donut chart
p19 <- PieChart(Marital, data = CDC_des,
                main = "Donut Chart of Marital Status" )

# p20 - Military
p20 <- ggplot(CDC_des, aes(x=Military, fill=Military)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Military", y="Count")

# p21 - DiffWalk
p21 <- ggplot(CDC_des, aes(x=DiffWalk, fill=DiffWalk)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of DiffWalk", y="Count")

# p22 - DiffDress
p22 <- ggplot(CDC_des, aes(x=DiffDress, fill=DiffDress)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of DiffDress", y="Count")

# p23 - DiffAlone
p23 <- ggplot(CDC_des, aes(x=DiffAlone, fill=DiffAlone)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of DiffAlone", y="Count")

# p24 - Smoke100
p24 <- ggplot(CDC_des, aes(x=Smoke100, fill=Smoke100)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Smoke 100+ cigarettes", y="Count")

# p25 - CurrentSmoker
p25 <- ggplot(CDC_des, aes(x=CurrentSmoker, fill=CurrentSmoker)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Current Smoker", y="Count")

# p26 - HeavyDrinker
p26 <- ggplot(CDC_des, aes(x=HeavyDrinker, fill=HeavyDrinker)) + 
              geom_bar( ) +
              scale_fill_hue(c = 30) +
              coord_flip() +
              theme_minimal() + 
              theme(legend.position="none") +
              labs(title="Bar Chart of Heavy Drinker", y="Count")

# p27 - Fall
plot(1:30)
p27 <- ggplot(CDC_des, aes(x=Fall, fill = Fall)) +  
              geom_histogram(binwidth=1) +
              theme(legend.position="none") +
              geom_vline(aes(xintercept=mean(SleepTime), color="red", linetype="dashed", linewidth=1)) +
              annotate("text",  
                       x = mean(Fall) * 20,
                       y = mean(Fall) * 150000,
                       label = paste("Mean =", round(mean(Fall),2)),
                       col = "red",
                       size = 6) + 
              theme_minimal() +
              theme(legend.position="none") +
              labs(title="Histogram of Fall", x="Fall (Times)", y="Count")

# Find out key indicators
# Method_1: glm()
#----
# Key indicators _ 1 - glm() 
LogModel_1 <- glm(HeartDisease ~., family=binomial(link='logit'), data=CDC_des)
summary(LogModel)

# Method_2: Graphs - 100% Stacked Bar

# Method_2: Graph - 100% Stacked Bar
#----
# 100% Stacked Bar for Gender
# Primitive haha
n1 <- sum(Gender=="Male" & HeartDisease=="Yes")
n2 <- sum(Gender=="Male" & HeartDisease=="No")
n3 <- sum(Gender=="Female" & HeartDisease=="Yes")
n4 <- sum(Gender=="Female" & HeartDisease=="No")
Gender_k <- c(rep("male",2), rep("female",2))
HD <- c("Y", "N", "Y", "N")
Number <- c(n1, n3, n2, n4)
gender_hd <- data.frame(Gender_k, HD, Number)
k1<- ggplot(gender_hd, aes(fill=HD, y=Number, x=Gender_k)) + 
     geom_bar(position="fill", stat="identity") +
     labs(title="Gender vs Heart Disease 100% Stacked Bar", x ="Gender", y="Percentage")

# Find a way out to save labour ;)
# Create a function
number.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- 0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="Yes" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="Yes" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="No" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="No" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    }
    i = i+1
  }
  print(paste("rY-HD-Y: ",n_1))
  print(paste("rY-HD-N: ",n_2))
  print(paste("rN-HD-Y: ",n_3))
  print(paste("rN-HD-N: ",n_4))
  Num <- c(n_1, n_2, n_3, n_4)
  return(Num)
}

# Asthma
# Begin to call the function
num_asthma = number.function(Asthma, HeartDisease)
asth_k <- c(rep("Asthma-Yes",2), rep("Asthma-No",2))
HD <- c("Y", "N", "Y", "N")
asthma_hd <- data.frame(asth_k, HD, num_asthma)
k2 <- ggplot(asthma_hd, aes(fill=HD, y=num_asthma, x=asth_k)) + 
      geom_bar(position="fill", stat="identity") +
      labs(title="Asthma vs Heart Disease 100% Stacked Bar", x ="Asthma", y="Percentage")

# Exercise
num_ex = number.function(Exercise, HeartDisease)
ex_k <- c(rep("Exercise-Yes",2), rep("Exercise-No",2))
HD <- c("Y", "N", "Y", "N")
exer_hd <- data.frame(ex_k, HD, num_ex)
k3 <- ggplot(exer_hd, aes(fill=HD, y=num_ex, x=ex_k)) + 
             geom_bar(position="fill", stat="identity") +
             labs(title="Exercise vs Heart Disease 100% Stacked Bar", x ="Exercise", y="Percentage")

# Stroke
num_strk = number.function(Stroke, HeartDisease)
str_k <- c(rep("Stroke-Yes",2), rep("Stroke-No",2))
HD <- c("Y", "N", "Y", "N")
str_hd <- data.frame(str_k, HD, num_strk)
k4 <- ggplot(str_hd, aes(fill=HD, y=num_strk, x=str_k)) + 
             geom_bar(position="fill", stat="identity") +
            labs(title="Stroke vs Heart Disease 100% Stacked Bar", x ="Stroke", y="Percentage")

# SkinCancer
num_skin = number.function(SkinCancer, HeartDisease)
skin_k <- c(rep("SkinCancer-Yes",2), rep("SkinCancer-No",2))
HD <- c("Y", "N", "Y", "N")
skin_hd <- data.frame(skin_k, HD, num_skin)
k5 <- ggplot(skin_hd, aes(fill=HD, y=num_skin, x=skin_k)) + 
             geom_bar(position="fill", stat="identity") +
             labs(title="Skin Cancer vs Heart Disease 100% Stacked Bar", x ="Skin Cancer", y="Percentage")

# OtherCancer
num_other = number.function(OtherCancer, HeartDisease)
other_k <- c(rep("OtherCancer-Yes",2), rep("OtherCancer-No",2))
HD <- c("Y", "N", "Y", "N")
other_hd <- data.frame(other_k, HD, num_other)
k6 <- ggplot(other_hd, aes(fill=HD, y=num_other, x=other_k)) + 
             geom_bar(position="fill", stat="identity") +
             labs(title="Other Cancer vs Heart Disease 100% Stacked Bar", x ="Other Cancer", y="Percentage")

# PulmonaryDisease
num_pulm = number.function(PulmonaryDisease, HeartDisease)
pulm_k <- c(rep("PulmonaryDisease-Yes",2), rep("PulmonaryDisease-No",2))
HD <- c("Y", "N", "Y", "N")
pulm_hd <- data.frame(pulm_k, HD, num_pulm)
k7 <- ggplot(pulm_hd, aes(fill=HD, y=num_pulm, x=pulm_k)) + 
             geom_bar(position="fill", stat="identity") +
             labs(title="Pulmonary Disease vs Heart Disease 100% Stacked Bar", x ="Pulmonary Disease", y="Percentage")

# Arthritis
num_arth = number.function(Arthritis, HeartDisease)
arth_k <- c(rep("Arthritis-Yes",2), rep("Arthritis-No",2))
HD <- c("Y", "N", "Y", "N")
arth_hd <- data.frame(arth_k, HD, num_arth)
k8 <- ggplot(arth_hd, aes(fill=HD, y=num_arth, x=arth_k)) + 
             geom_bar(position="fill", stat="identity") +
             labs(title="Arthritis vs Heart Disease 100% Stacked Bar", x ="Arthritis", y="Percentage")

# Depression
num_depr = number.function(Depression, HeartDisease)
depr_k <- c(rep("Depression-Yes",2), rep("Depression-No",2))
HD <- c("Y", "N", "Y", "N")
depr_hd <- data.frame(depr_k, HD, num_depr)
k9 <- ggplot(depr_hd, aes(fill=HD, y=num_depr, x=depr_k)) + 
             geom_bar(position="fill", stat="identity") +
             labs(title="Depression vs Heart Disease 100% Stacked Bar", x ="Depression", y="Percentage")

# KidneyDisease
num_kid = number.function(KidneyDisease, HeartDisease)
kid_k <- c(rep("KidneyDisease-Yes",2), rep("KidneyDisease-No",2))
HD <- c("Y", "N", "Y", "N")
kid_hd <- data.frame(kid_k, HD, num_kid)
k10 <- ggplot(kid_hd, aes(fill=HD, y=num_kid, x=kid_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Kidney Disease vs Heart Disease 100% Stacked Bar", x ="Kidney Disease", y="Percentage")

# Military
num_mili = number.function(Military, HeartDisease)
mili_k <- c(rep("Military-Yes",2), rep("Military-No",2))
HD <- c("Y", "N", "Y", "N")
mili_hd <- data.frame(mili_k, HD, num_mili)
k11 <- ggplot(mili_hd, aes(fill=HD, y=num_mili, x=mili_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Military vs Heart Disease 100% Stacked Bar", x ="Military", y="Percentage")

# DiffWalk
num_walk = number.function(DiffWalk, HeartDisease)
walk_k <- c(rep("DiffWalk-Yes",2), rep("DiffWalk-No",2))
HD <- c("Y", "N", "Y", "N")
walk_hd <- data.frame(walk_k, HD, num_walk)
k12 <- ggplot(walk_hd, aes(fill=HD, y=num_walk, x=walk_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Walking Difficulty vs Heart Disease 100% Stacked Bar", x ="Walking Difficulty", y="Percentage")

# DiffDress
num_dress = number.function(DiffDress, HeartDisease)
dress_k <- c(rep("DiffDress-Yes",2), rep("DiffDress-No",2))
HD <- c("Y", "N", "Y", "N")
dress_hd <- data.frame(dress_k, HD, num_dress)
k13 <- ggplot(dress_hd, aes(fill=HD, y=num_dress, x=dress_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Dressing Difficulty vs Heart Disease 100% Stacked Bar", x ="Dressing Difficulty", y="Percentage")

# DiffAlone
num_alone = number.function(DiffAlone, HeartDisease)
alone_k <- c(rep("DiffAlone-Yes",2), rep("DiffAlone-No",2))
HD <- c("Y", "N", "Y", "N")
alone_hd <- data.frame(alone_k, HD, num_alone)
k14 <- ggplot(alone_hd, aes(fill=HD, y=num_alone, x=alone_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Difficulty Being Alone vs Heart Disease 100% Stacked Bar", x ="Difficulty Being Alone", y="Percentage")

# Smoke100
num_smoke = number.function(Smoke100, HeartDisease)
smoke_k <- c(rep("Smoke100-Yes",2), rep("Smoke100-No",2))
HD <- c("Y", "N", "Y", "N")
smoke_hd <- data.frame(smoke_k, HD, num_smoke)
k15 <- ggplot(smoke_hd, aes(fill=HD, y=num_smoke, x=smoke_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Smoking >100 ciga vs Heart Disease 100% Stacked Bar", x ="Smoke100", y="Percentage")

# CurrentSmoker
num_current = number.function(CurrentSmoker, HeartDisease)
current_k <- c(rep("CurrentSmoker-Yes",2), rep("CurrentSmoker-No",2))
HD <- c("Y", "N", "Y", "N")
current_hd <- data.frame(current_k, HD, num_current)
k16 <- ggplot(current_hd, aes(fill=HD, y=num_current, x=current_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Current Smoker vs Heart Disease 100% Stacked Bar", x ="Current Smoker", y="Percentage")

# HeavyDrinker
num_drinker = number.function(HeavyDrinker, HeartDisease)
drinker_k <- c(rep("HeavyDrinker-Yes",2), rep("HeavyDrinker-No",2))
HD <- c("Y", "N", "Y", "N")
drinker_hd <- data.frame(drinker_k, HD, num_drinker)
k17 <- ggplot(drinker_hd, aes(fill=HD, y=num_drinker, x=drinker_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Heavy Drinker vs Heart Disease 100% Stacked Bar", x ="Heavy Drinker", y="Percentage")

# Age
number2.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- n_5 <- n_6 <- n_7 <- n_8 <- n_9 <- n_10 <- n_11 <- n_12 <-0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="18-24" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="18-24" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="25-34" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="25-34" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    } else if (x[i]=="35-44" & HeartDisease[i] == "Yes"){
      n_5 = n_5 + 1
    } else if (x[i]=="35-44" & HeartDisease[i] == "No"){
      n_6 = n_6 + 1
    } else if (x[i]=="45-54" & HeartDisease[i] == "Yes"){
      n_7 = n_7 + 1
    } else if (x[i]=="45-54" & HeartDisease[i] == "No"){
      n_8 = n_8 + 1
    } else if (x[i]=="55-64" & HeartDisease[i] == "Yes"){
      n_9 = n_9 + 1
    } else if (x[i]=="55-64" & HeartDisease[i] == "No"){
      n_10 = n_10 + 1
    } else if (x[i]==">65" & HeartDisease[i] == "Yes"){
      n_11 = n_11 + 1
    } else if (x[i]==">65" & HeartDisease[i] == "No"){
      n_12 = n_12 + 1
    }
    i = i+1
  }
  print(paste("r18-HD-Y: ",n_1))
  print(paste("r18-HD-N: ",n_2))
  print(paste("r25-HD-Y: ",n_3))
  print(paste("r25-HD-N: ",n_4))
  print(paste("r35-HD-Y: ",n_5))
  print(paste("r35-HD-N: ",n_6))
  print(paste("r45-HD-Y: ",n_7))
  print(paste("r45-HD-N: ",n_8))
  print(paste("r55-HD-Y: ",n_9))
  print(paste("r55-HD-N: ",n_10))
  print(paste("r65-HD-Y: ",n_11))
  print(paste("r65-HD-N: ",n_12))
  Num <- c(n_1, n_2, n_3, n_4, n_5, n_6, n_7, n_8, n_9, n_10, n_11, n_12)
  return(Num)
}
num_age = number2.function(Age, HeartDisease)
age_k <- c(rep("18-24",2), rep("25-34",2), rep("35-44",2), rep("45-54",2), rep("55-64",2), rep(">65",2))
HD <- c("Y", "N", "Y", "N", "Y", "N", "Y", "N", "Y", "N", "Y", "N")
age_hd <- data.frame(age_k, HD, num_age)
k18 <- ggplot(age_hd, aes(fill=HD, y=num_age, x=age_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Age vs Heart Disease 100% Stacked Bar", x ="Age", y="Percentage")

# Race
number_race.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- n_5 <- n_6 <- n_7 <- n_8 <- n_9 <- n_10 <- n_11 <- n_12 <-0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="White" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="White" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="Black" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="Black" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    } else if (x[i]=="Asian" & HeartDisease[i] == "Yes"){
      n_5 = n_5 + 1
    } else if (x[i]=="Asian" & HeartDisease[i] == "No"){
      n_6 = n_6 + 1
    } else if (x[i]=="American Indian/Alaskan Native" & HeartDisease[i] == "Yes"){
      n_7 = n_7 + 1
    } else if (x[i]=="American Indian/Alaskan Native" & HeartDisease[i] == "No"){
      n_8 = n_8 + 1
    } else if (x[i]=="Hispanic" & HeartDisease[i] == "Yes"){
      n_9 = n_9 + 1
    } else if (x[i]=="Hispanic" & HeartDisease[i] == "No"){
      n_10 = n_10 + 1
    } else if (x[i]=="Other race" & HeartDisease[i] == "Yes"){
      n_11 = n_11 + 1
    } else if (x[i]=="Other race" & HeartDisease[i] == "No"){
      n_12 = n_12 + 1
    }
    i = i+1
  }
  print(paste("rW-HD-Y: ",n_1))
  print(paste("rW-HD-N: ",n_2))
  print(paste("rB-HD-Y: ",n_3))
  print(paste("rB-HD-N: ",n_4))
  print(paste("rAs-HD-Y: ",n_5))
  print(paste("rAs-HD-N: ",n_6))
  print(paste("rAIAN-HD-Y: ",n_7))
  print(paste("rAIAN-HD-N: ",n_8))
  print(paste("rH-HD-Y: ",n_9))
  print(paste("rH-HD-N: ",n_10))
  print(paste("rO-HD-Y: ",n_11))
  print(paste("rO-HD-N: ",n_12))
  Num <- c(n_1, n_2, n_3, n_4, n_5, n_6, n_7, n_8, n_9, n_10, n_11, n_12)
  return(Num)
}
num_race = number_race.function(Race, HeartDisease)
race_k <- c(rep("White",2), rep("Black",2), rep("Asian",2), rep("AIAN",2), rep("Hispanic",2), rep("Other race",2))
HD <- c("Y", "N", "Y", "N", "Y", "N", "Y", "N", "Y", "N", "Y", "N")
race_hd <- data.frame(race_k, HD, num_race)
k19 <- ggplot(race_hd, aes(fill=HD, y=num_race, x=race_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Race vs Heart Disease 100% Stacked Bar", x ="Race", y="Percentage")

# Marital
number_marital.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- n_5 <- n_6 <- n_7 <- n_8 <- n_9 <- n_10 <- n_11 <- n_12 <-0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="Married" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="Married" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="Divorced" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="Divorced" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    } else if (x[i]=="Widowed" & HeartDisease[i] == "Yes"){
      n_5 = n_5 + 1
    } else if (x[i]=="Widowed" & HeartDisease[i] == "No"){
      n_6 = n_6 + 1
    } else if (x[i]=="Separated" & HeartDisease[i] == "Yes"){
      n_7 = n_7 + 1
    } else if (x[i]=="Separated" & HeartDisease[i] == "No"){
      n_8 = n_8 + 1
    } else if (x[i]=="Never married" & HeartDisease[i] == "Yes"){
      n_9 = n_9 + 1
    } else if (x[i]=="Never married" & HeartDisease[i] == "No"){
      n_10 = n_10 + 1
    } else if (x[i]=="Domestic relation" & HeartDisease[i] == "Yes"){
      n_11 = n_11 + 1
    } else if (x[i]=="Domestic relation" & HeartDisease[i] == "No"){
      n_12 = n_12 + 1
    }
    i = i+1
  }
  print(paste("rM-HD-Y: ",n_1))
  print(paste("rM-HD-N: ",n_2))
  print(paste("rD-HD-Y: ",n_3))
  print(paste("rD-HD-N: ",n_4))
  print(paste("rW-HD-Y: ",n_5))
  print(paste("rW-HD-N: ",n_6))
  print(paste("rS-HD-Y: ",n_7))
  print(paste("rS-HD-N: ",n_8))
  print(paste("rN-HD-Y: ",n_9))
  print(paste("rN-HD-N: ",n_10))
  print(paste("rDo-HD-Y: ",n_11))
  print(paste("rDo-HD-N: ",n_12))
  Num <- c(n_1, n_2, n_3, n_4, n_5, n_6, n_7, n_8, n_9, n_10, n_11, n_12)
  return(Num)
}
num_mar = number_marital.function(Marital, HeartDisease)
mar_k <- c(rep("Married",2), rep("Divorced",2), rep("Widowed",2), rep("Separated",2), rep("Never married",2), rep("Dometic Relation",2))
HD <- c("Y", "N", "Y", "N", "Y", "N", "Y", "N", "Y", "N", "Y", "N")
mar_hd <- data.frame(mar_k, HD, num_mar)
k20 <- ggplot(mar_hd, aes(fill=HD, y=num_mar, x=mar_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Marital Status vs Heart Disease 100% Stacked Bar", x ="Marital Status", y="Percentage")

# BMI
number_bmi.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- n_5 <- n_6 <- n_7 <- n_8 <- 0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="Underweight" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="Underweight" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="Normal weight" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="Normal weight" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    } else if (x[i]=="Overweight" & HeartDisease[i] == "Yes"){
      n_5 = n_5 + 1
    } else if (x[i]=="Overweight" & HeartDisease[i] == "No"){
      n_6 = n_6 + 1
    } else if (x[i]=="Obese" & HeartDisease[i] == "Yes"){
      n_7 = n_7 + 1
    } else if (x[i]=="Obese" & HeartDisease[i] == "No"){
      n_8 = n_8 + 1
    }
    i = i+1
  }
  print(paste("rU-HD-Y: ",n_1))
  print(paste("rU-HD-N: ",n_2))
  print(paste("rN-HD-Y: ",n_3))
  print(paste("rN-HD-N: ",n_4))
  print(paste("rOv-HD-Y: ",n_5))
  print(paste("rOv-HD-N: ",n_6))
  print(paste("rOb-HD-Y: ",n_7))
  print(paste("rOb-HD-N: ",n_8))
  Num <- c(n_1, n_2, n_3, n_4, n_5, n_6, n_7, n_8)
  return(Num)
}
num_bmi = number_bmi.function(BMI, HeartDisease)
bmi_k <- c(rep("Underweight",2), rep("Normal weight",2), rep("Overweight",2), rep("Obese",2))
HD <- c("Y", "N", "Y", "N", "Y", "N", "Y", "N")
bmi_hd <- data.frame(bmi_k, HD, num_bmi)
k21 <- ggplot(bmi_hd, aes(fill=HD, y=num_bmi, x=bmi_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="BMI vs Heart Disease 100% Stacked Bar", x ="BMI", y="Percentage")

# GeneralHealth
number_general.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- 0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="Good or Better" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="Good or Better" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="Fair or Poor" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="Fair or Poor" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    }
    i = i+1
  }
  print(paste("rG-HD-Y: ",n_1))
  print(paste("rG-HD-N: ",n_2))
  print(paste("rF-HD-Y: ",n_3))
  print(paste("rF-HD-N: ",n_4))
  Num <- c(n_1, n_2, n_3, n_4)
  return(Num)
}
num_general = number_general.function(GeneralHealth, HeartDisease)
general_k <- c(rep("GeneralHealth-Good or better",2), rep("GeneralHealth-Fair or poor",2))
HD <- c("Y", "N", "Y", "N")
general_hd <- data.frame(general_k, HD, num_general)
k22 <- ggplot(general_hd, aes(fill=HD, y=num_general, x=general_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="General Health vs Heart Disease 100% Stacked Bar", x ="General Health", y="Percentage")

# Diabetes
number_diabetes.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- n_5 <- n_6 <- n_7 <- n_8 <- 0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="Yes" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="Yes" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]=="Yes(pregnant)" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]=="Yes(pregnant)" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    } else if (x[i]=="No" & HeartDisease[i] == "Yes"){
      n_5 = n_5 + 1
    } else if (x[i]=="No" & HeartDisease[i] == "No"){
      n_6 = n_6 + 1
    } else if (x[i]=="No(prediabetes)" & HeartDisease[i] == "Yes"){
      n_7 = n_7 + 1
    } else if (x[i]=="No(prediabetes)" & HeartDisease[i] == "No"){
      n_8 = n_8 + 1
    }
    i = i+1
  }
  print(paste("rY-HD-Y: ",n_1))
  print(paste("rY-HD-N: ",n_2))
  print(paste("rYP-HD-Y: ",n_3))
  print(paste("rYP-HD-N: ",n_4))
  print(paste("rN-HD-Y: ",n_5))
  print(paste("rN-HD-N: ",n_6))
  print(paste("rNP-HD-Y: ",n_7))
  print(paste("rNP-HD-N: ",n_8))
  Num <- c(n_1, n_2, n_3, n_4, n_5, n_6, n_7, n_8)
  return(Num)
}
num_diabetes = number_diabetes.function(Diabetes, HeartDisease)
dia_k <- c(rep("Yes",2), rep("Yes(pregnant)",2), rep("No",2), rep("No(prediabetes)",2))
HD <- c("Y", "N", "Y", "N", "Y", "N", "Y", "N")
dia_hd <- data.frame(dia_k, HD, num_diabetes)
k23 <- ggplot(dia_hd, aes(fill=HD, y=num_diabetes, x=dia_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Diabetes vs Heart Disease 100% Stacked Bar", x ="Diabetes", y="Percentage")

# Teeth
number_teeth.function <- function(x, HeartDisease){
  n_1 <- n_2 <- n_3 <- n_4 <- n_5 <- n_6 <- n_7 <- n_8 <- 0
  Number <- list()
  for (i in 1:length(x)){
    if (x[i]=="1-5" & HeartDisease[i] == "Yes"){
      n_1 = n_1 + 1
    } else if (x[i]=="1-5" & HeartDisease[i] == "No"){
      n_2 = n_2 + 1
    } else if (x[i]==">6" & HeartDisease[i] == "Yes"){
      n_3 = n_3 + 1
    } else if (x[i]==">6" & HeartDisease[i] == "No"){
      n_4 = n_4 + 1
    } else if (x[i]=="All" & HeartDisease[i] == "Yes"){
      n_5 = n_5 + 1
    } else if (x[i]=="All" & HeartDisease[i] == "No"){
      n_6 = n_6 + 1
    } else if (x[i]=="0" & HeartDisease[i] == "Yes"){
      n_7 = n_7 + 1
    } else if (x[i]=="0" & HeartDisease[i] == "No"){
      n_8 = n_8 + 1
    }
    i = i+1
  }
  print(paste("r1-HD-Y: ",n_1))
  print(paste("r1-HD-N: ",n_2))
  print(paste("r6-HD-Y: ",n_3))
  print(paste("r6-HD-N: ",n_4))
  print(paste("rA-HD-Y: ",n_5))
  print(paste("rA-HD-N: ",n_6))
  print(paste("r0-HD-Y: ",n_7))
  print(paste("r0-HD-N: ",n_8))
  Num <- c(n_1, n_2, n_3, n_4, n_5, n_6, n_7, n_8)
  return(Num)
}
num_teeth = number_teeth.function(Teeth, HeartDisease)
teeth_k <- c(rep("1-5",2), rep(">6",2), rep("All",2), rep("0",2))
HD <- c("Y", "N", "Y", "N", "Y", "N", "Y", "N")
teeth_hd <- data.frame(teeth_k, HD, num_teeth)
k24 <- ggplot(teeth_hd, aes(fill=HD, y=num_teeth, x=teeth_k)) + 
              geom_bar(position="fill", stat="identity") +
              labs(title="Teeth vs Heart Disease 100% Stacked Bar", x ="Teeth", y="Percentage")

# SleepTime
k25 <- ggplot(CDC_des, aes(SleepTime, fill = HeartDisease)) +  
              geom_histogram(binwidth=1) +
              labs(title="Sleep Time vs Heart Disease Histogram", x="Sleep Time", y="Count")

# Fall
k26 <- ggplot(CDC_des, aes(Fall, fill = HeartDisease)) +  
              geom_histogram(binwidth=1) +
              labs(title="Fall vs Heart Disease Histogram", x="Fall", y="Count")

library(gridExtra)
# Factors that have strong & positive relation with heart disease
grid.arrange(k1, k4, k7, k10, nrow = 2, ncol = 2)
grid.arrange(k12, k13, k14, k22, nrow = 2, ncol = 2)
grid.arrange(k23, k24, nrow = 1, ncol = 2)
# Factors that have positive relation with heart disease
grid.arrange(k3, k5, k6, k8, k9, k11, k15, k18, nrow = 4, ncol = 2)
# Factors that seem to have no or strange relation with heart disease
grid.arrange(k2, k16, k17, k19, k20, k21, k25, k26, nrow = 4, ncol = 2)

#----

# Method_3: cor()
#----
# Correlation
data_cor <- cor(CDC[ , colnames(CDC) != "HearDisease"], CDC$HeartDisease)
data_cor
cor_table <- data.frame(data_cor)
cor_table["Index"] <- rownames(cor_table)
newdata <- cor_table[order(data_cor),]
newdata
res_general <- cor.test(CDC$GeneralHealth, CDC$HeartDisease, method = "pearson")
res_general
res_age <- cor.test(CDC$Age, CDC$HeartDisease, method = "pearson")
res_age
res_exercise <- cor.test(CDC$Exercise, CDC$HeartDisease, method = "pearson")
res_exercise
res_fall <- cor.test(CDC$Fall, CDC$HeartDisease, method = "pearson")
res_fall
res_bmi <- cor.test(CDC$BMI, CDC$HeartDisease, method = "pearson")
res_bmi
res_gender <- cor.test(CDC$Gender, CDC$HeartDisease, method = "pearson")
res_gender
res_teeth <- cor.test(CDC$Teeth, CDC$HeartDisease, method = "pearson")
res_teeth
res_stroke <- cor.test(CDC$Stroke, CDC$HeartDisease, method = "pearson")
res_stroke
res_diffwalk <- cor.test(CDC$DiffWalk, CDC$HeartDisease, method = "pearson")
res_diffwalk
res_diabetes <- cor.test(CDC$Diabetes, CDC$HeartDisease, method = "pearson")
res_diabetes

key_indicators_1 <- c("Gender", "Stroke", "PulmonaryDisease", "KidneyDisease", "DiffWalk", "DiffDress", "DiffAlone", "GeneralHealth", "Diabetes", "Teeth", "Race", "Asthma", "SkinCancer", "Arthritis", "Marital", "Military", "Smoke100", "CurrentSmoker", "HeavyDrinker", NA, NA, NA, NA)
key_indicators_2 <- c("Gender", "Stroke", "PulmonaryDisease", "KidneyDisease", "DiffWalk", "DiffDress", "DiffAlone", "GeneralHealth", "Diabetes", "Teeth", rep(NA, 13))
key_indicators_3 <- c("Gender", "Stroke", "DiffWalk", "GeneralHealth", "Diabetes", "Teeth", "Age", "BMI", "Exercise", "Fall", rep(NA, 13))
key_indicators_all <- c("Gender", "Stroke", "PulmonaryDisease", "KidneyDisease", "DiffWalk", "DiffDress", "DiffAlone", "GeneralHealth", "Diabetes", "Teeth", "Race", "Asthma", "SkinCancer", "Arthritis", "Marital", "Military", "Smoke100", "CurrentSmoker", "HeavyDrinker", "Age", "BMI","Exercise", "Fall")
key_indicators <- data.frame(key_indicators_1, key_indicators_2, key_indicators_3, key_indicators_all)
colnames(key_indicators) <- c(" -1- From glm() ", " -2- From 100% stacked bar ", " -3- From cor() ", "All")
key_indicators

#----
# Create a new data set
# The new data set has 22 columns: 21 all the key indicators + 1 HeartDisease
col_test <- c("HeartDisease", "Gender", "Stroke", "PulmonaryDisease", "KidneyDisease", "DiffWalk", "DiffDress", "DiffAlone", "GeneralHealth", "Diabetes", "Teeth", "Race", "Asthma", "SkinCancer", "Arthritis", "Marital", "Military", "Smoke100", "CurrentSmoker", "HeavyDrinker", "Age", "BMI","Exercise", "Fall")
CDC_des_test <- CDC_des[,(names(CDC_des) %in% col_test)]
str(CDC_des_test)

# Compare three models for accuracy
# Base accuracy
BaseAccuracy = sum(HeartDisease == "No") / nrow(CDC_des_test)
BaseAccuracy

# Test models - logistic regression
# LogModel_1
LogModel_1 = glm(CDC_des_test$HeartDisease~. - Age - BMI - Exercise - Fall, data = CDC_des_test, family = binomial)
predict_1 = predict(LogModel_1, type = "response")
print(table(CDC_des_test$HeartDisease, predict_1 >= 0.5))
accuracy_1 = (2882 + 185575) / nrow(CDC_des_test)
accuracy_1

# LogModel_2
LogModel_2 = glm(CDC_des_test$HeartDisease ~ . - Race - Asthma - SkinCancer - Arthritis - Marital - Military - Smoke100 - CurrentSmoker - HeavyDrinker - Age - BMI - Exercise - Fall, data = CDC_des_test, family = binomial)
predict_2 = predict(LogModel_2, type = "response")
print(table(CDC_des_test$HeartDisease, predict_2 >= 0.5))
accuracy_2 = (2594 + 185796) / nrow(CDC_des_test)
accuracy_2

# LogModel_3
LogModel_3 = glm(CDC_des_test$HeartDisease ~ . - PulmonaryDisease - KidneyDisease - DiffDress - DiffAlone - Race - Asthma - SkinCancer - Arthritis - Marital - Military - Smoke100 - CurrentSmoker - HeavyDrinker, data = CDC_des_test, family = binomial)
predict_3 = predict(LogModel_3, type = "response")
print(table(CDC_des_test$HeartDisease, predict_3 >= 0.5))
accuracy_3 = (2258 + 185831) / nrow(CDC_des_test)
accuracy_3

