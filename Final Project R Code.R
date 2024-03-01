# Set working directory
setwd("/Users/amandaquan/Library/CloudStorage/OneDrive-CalPoly/BUS 497/Final Project")

# Import excel file
library(readxl)
ERIMData <- read_excel("ERIMData.xlsx")
View(ERIMData)

# Remove Missing Values From Dataset
length(which(is.na(ERIMData))) # 4
which(is.na(ERIMData)) # 3676 10663 12081 14658

# summary of ERIMData
summary(ERIMData)

# Remove NAs from the Dataset
ERIMDataOmit = na.omit(ERIMData)

# Note using ERIMDATA, not Ommission_Data

# Updated to exclude the N/A data
# Subseting Residence Status & Education Level (Male & Female) 
myData1 = ERIMDataOmit[,c("ResStatus","MEdu")]
myData2 = ERIMDataOmit[,c("ResStatus","FEdu")]


# Create Custom Binning the Data Based the Education level 
# Subseting Male Education
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

summary(ERIMDataOmit$FWrkHrs) # Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00   25.00   22.1   40.00   90.00 

# Based on observation: On average, males work more than females. 
# Males working on avg of 26.34 hrs compared to females where they work 22.11 hours.


# What is the average HHInc
model1 <- lm(HHInc ~ MEdu + MWrkHrs, data = ERIMDataOmit)
summary(model1)
options(scipen = 999)

# Male education of college degree and works 50 hrs
test = 3.441818 + (0.299144* 9) + (0.036294 * 50) # 7.948814


model2 <- lm(HHInc ~ FEdu + FWrkHrs, data = ERIMDataOmit)
summary(model2)

# Female education of a graduate college degree and works 50 hrs
test2 = 3.163006 + (0.348224*11) + (0.023917*50) # 8.18932

model3 <- lm(ResStatus ~ MEdu + MWrkHrs + DinExp, data = ERIMDataOmit)
summary(model3)

model4 <- lm(ResStatus ~ MEdu + MWrkHrs + FEdu + FWrkHrs, data = ERIMDataOmit)
summary(model4)

model5 <- lm(ResStatus ~ MEdu + MWrkHrs + FEdu + FWrkHrs + HHNbr, data = ERIMDataOmit)
summary(model5)

model6 <- lm(MWrkHrs ~ MEdu, data = ERIMDataOmit)
summary(model6)



#plot(ERIMData$MEdu ~ ERIMData$MWrkHrs, data = ERIMData, ylab = 'Education', 
#     xlab = 'Avg Working Hrs')

















