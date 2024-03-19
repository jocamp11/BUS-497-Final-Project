# Set working directory
setwd("/Users/amandaquan/Library/CloudStorage/OneDrive-CalPoly/BUS 497/Final Project")

# Import excel file
library(readxl)
ERIMData <- read_excel("ERIMData.xlsx")
View(ERIMData)


# ------------------------------------------------------
# DATA PREPARATION (OMITING DATA) ---
# -----------------------------------------------------
# Remove Missing Values From Dataset
length(which(is.na(ERIMData))) # 4
which(is.na(ERIMData)) # 3676 10663 12081 14658

# summary of ERIMData
summary(ERIMData)

# Remove N/As from the Dataset
ERIMDataOmit = na.omit(ERIMData)

# Note: using ERIMDATA, not Ommission_Data
# Updated to exclude the N/A data

# ------------------------------------------------------------------------------------------------
# SUBSETTING THE DATA 
# Subseting Residence Status & Education Level (Male & Female) 
myData1 = ERIMDataOmit[,c("ResStatus","MEdu")]
myData2 = ERIMDataOmit[,c("ResStatus","FEdu")]

# ------------------------------------------------------------------------------------------------
# Create Custom Binning the Data Based the Education level to make it easier to interpret
# Subsetting Male Education
myData1$Binning = cut(ERIMDataOmit$MEdu, breaks = c(-Inf, 8, 9, 10, 11),
                      labels = c("No College", "College", "Graduate", "Post-Graduate"))

# Count the number of Bins
length(which(myData1$Binning == "No College")) # 2474
length(which(myData1$Binning == "College")) # 414
length(which(myData1$Binning == "Graduate")) # 102
length(which(myData1$Binning == "Post-Graduate")) # 199


# Subseting Female Education
myData2$Binning = cut(ERIMDataOmit$FEdu, breaks = c(-Inf, 8, 9, 10, 11),
                      labels = c("No College", "College", "Graduate", "Post-Graduate"))

length(which(myData2$Binning == "No College")) # 2444
length(which(myData2$Binning == "College")) # 451
length(which(myData2$Binning == "Graduate")) # 141
length(which(myData2$Binning == "Post-Graduate")) # 149



# ----------------------------------------------------------------------------------------------------------------
# MODELING 1 --- Does gender affecting the amount of house income earned based on predictors varaibles such as education level and average hours worked?

model1 <- lm(HHInc ~ MEdu + MWrkHrs, data = ERIMDataOmit)
summary(model1)
options(scipen = 999)

model2 <- lm(HHInc ~ FEdu + FWrkHrs, data = ERIMDataOmit)
summary(model2)

model3 <- lm(HHInc ~ MEdu + MWrkHrs + FEdu + FWrkHrs, data = ERIMDataOmit)
summary(model3)

# MALE education of college degree (9) and works 40 hrs
test = 3.441818 + (0.299144* 9) + (0.036294 * 40) # 7.5858

# FEMALE education of a college degree (9) and works 40 hrs
test2 = 3.163006 + (0.348224*9) + (0.023917*40) # 7.2537

# MALE & FEMALE HOUSEHOLD
test3 = 1.548705 + (0.295379 * 9) + (0.030447 * 40) + (0.237449 * 9) + (0.022081 * 40) # 8.4452



# BOXPLOTS ---
# Create a Boxplot for Male/Female Avg Working Hours

# Male Avg Working Hours
boxplot(ERIMDataOmit$MWrkHrs, ERIMDataOmit$FWrkHrs, 
        main = "Boxplots for Avg Working Hours",
        xlab = "Avg Working Hours",
        names = c("Male", "Female"),
        horizontal = TRUE,
        col = "purple")

summary(ERIMDataOmit$MWrkHrs) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00   40.00   26.34   44.00   80.00

mean(ERIMDataOmit$MWrkHrs) # 26.338 (male)
mean(ERIMDataOmit$FWrkHrs) # 22.10 (female)

summary(ERIMDataOmit$FWrkHrs) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00   25.00   22.1   40.00   90.00 

# Based on observation: On average, males work more than females. 
# Males working on avg of 26.34 hrs compared to females where they work 22.11 hours.


# ----------------------------------------------------------------------------------------------------------------
# MODELING 2 - Determine whether education, work hours, influences residency status
# Determining whether residency status is influenced by education, working hours, etc.

length(which(ERIMDataOmit$ResStatus == 3)) # 40

ERIMDataOmit$homeOwner <- ifelse(ERIMDataOmit$ResStatus == 1, 1, 0)


# bar graph of the residency status
counts = table(ERIMDataOmit$ResStatus)
barplot(counts, main = "Barplot Distribution of Residency Status",
        xlab = "Residency Status",
        ylab = "# of People", 
        ylim = c(0, 3000),
        col= c("blue", "green", "red"))


# We want to classify residency status into binary variables (1 = owned home, 2 = other aka renting, etc.)

# What is the residency status of a male who works 40 hrs and has an education of 11
model4 <- lm(homeOwner~ HHInc + MEdu + MWrkHrs, data = ERIMDataOmit)
summary(model4)

model5 <- lm(homeOwner ~ HHInc + FEdu + FWrkHrs, data = ERIMDataOmit)
summary(model5)

model6 <- lm(homeOwner ~ HHInc + MEdu + MWrkHrs + FEdu + FWrkHrs, data = ERIMDataOmit)
summary(model6)

# predict the residency status of a male who has an education level of 9, and works an avg of 40 hrs per week
status_test4 = 0.6106038 + (0.0247759 * Income) + (0.0189088 * MEdu) + (-0.0004368 * MWrkHrs) 

# predict the residency status of a female who has an education level of 9, and works an avg of 40 hrs per week
status_test5 = 0.6488107 + (0.0384782 * 9) + (-0.0010606 * 100)

# predict the residency status of both male & female who has an education level of 4, and works an avg of 40 hrs per week
status_test6 = 0.6692530 + (0.0244472 * 9) + (0.0006898 * 40) + (0.0075484 * 9) + (-0.0008819 * 40)


# -------------------------------------------------------------------------------------------------------
# MODELING 3 --- We want to determine whether income is determines whether a household is likely to own a


# Subseting Income 
myData3 = ERIMDataOmit[,c("HHInc", "HHNbr", "MWrkHrs", "FWrkHrs", "Cats", "Dogs", "ResStatus", "ResType")]
myData3$Pets = myData3$Cats + myData3$Dogs

myData3$Income = cut(ERIMDataOmit$HHInc, breaks = c(-Inf, 6, 9, 14),
                     labels = c("Low Income", "Middle Income", "High Income"))

myData4 = myData3[myData3$Pets == 1 & myData3$Income == "High Income",]

# we want to know how many cats and dogs are owned by each income level
counts1 = table(myData3$Income)
barplot(counts1, main = "Pet Ownership",
        xlab = "Income Levels", 
        ylab = "# of Pets Owned",
        ylim = c(0, 2000),
        col= c("purple", "orange", "pink"))


# KEEP FOR NOW --
# length(which(myData3$Income == "High Income" & myData3$Pets <= 2)) # 406
# length(which(myData3$Income == "High Income" & myData3$Pets <= 1)) # 344
# length(which(myData3$Income == "High Income" & myData3$Pets == 0)) # 161
# length(which(myData3$Income == "Low Income" & myData3$Pets >= 4)) # 55


cat_model = lm(Cats ~ HHInc + ResStatus + ResType, data = ERIMDataOmit)
summary(cat_model)
# standard error = 0.7953, adjusted r^2 = 0.003115

dog_model = lm(Dogs ~ HHInc + ResStatus + ResType, data = ERIMDataOmit)
summary(dog_model)
# standard error = 0.7326, Adjusted R-squared:  0.02434

pet = lm(Pets ~ HHInc + ResStatus + ResType, data = myData4)
summary(pet)







