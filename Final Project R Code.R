# Set working directory
setwd("/Users/amandaquan/Library/CloudStorage/OneDrive-CalPoly/BUS 497/Final Project")

# Import excel file
library(readxl)
ERIMData <- read_excel("ERIMData.xlsx")
View(ERIMData)

# Remove Missing Values From Dataset
length(which(is.na(ERIMData))) # 4
which(is.na(ERIMData)) # 3676 10663 12081 14658

# Remove NAs from the Dataset
Omission_Data = na.omit(ERIMData)

# Subseting Residence Status & Education Level (Male & Female) 
myData1 = ERIMData[,c("ResStatus","MEdu")]
myData2 = ERIMData[,c("ResStatus","FEdu")]


# Create Custom Binning the Data Based the Education level 
# Subseting Male Education
myData1$Binning = cut(ERIMData$MEdu, breaks = c(-Inf, 8, 9, 10, 11),
                    labels = c("No College", "College", "Graduate", "Post-Graduate"))

# Subseting Female Education
myData2$Binning = cut(ERIMData$FEdu, breaks = c(-Inf, 8, 9, 10, 11),
                      labels = c("No College", "College", "Graduate", "Post-Graduate"))


# 




