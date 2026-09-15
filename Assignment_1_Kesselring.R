#The below data set includes information about Cuban Brown Anoles (Anolis sagrei) collected from field sites across peninsular Florida, USA. 
#The data was collected by J. Kesselring, and is aiming to explore differences in Raillietiella orientalis infections across urban and nonurban habitats.
#This data is uploaded to my personal Github profile, and can be found at https://github.com/jasminekesselring/Anole_Biometry_Practice. 
library(readxl) #Reading in data. Run this line first.
anoles<-read_excel("Anole Dissections COPY.xlsx",
                   sheet="Fort De Soto") #Selecting a specific sheet within the Excel file.
str(anoles) #Identifying data class. This data set is a dataframe, more specifically a tibble.
#For this assignment, the variables of interest will be Mass, Snout-Vent Length (SVL), and Sex. 
#The variable class for Mass and SVL is numerical, and the class for Sex is character.
#The str command shows that our variables of interest do not have any formatting errors, and the data classes are correct. 
summary(anoles$Mass) #Summary statistics for Mass.
summary(anoles$SVL) #Summary statistics for SVL.
summary(anoles$Sex) #Summary statistics for Sex. These are less interesting since Sex isn't a numerical class. 
Body_Morphometrics<-anoles[c("Mass", "SVL")] #Assigned a working name to chosen variables.
pairs(x=Body_Morphometrics,
      pch=16,
      cex=0.75,
      col=rgb(red=0,
              green=0,
              blue=0,
              alpha=0.5)) #FIGURE 1: Made a scatterplot that compares both variables on each axis.
#FIGURE 1 DESCRIPTION: This figure shows the relationships between anole mass and snout-vent length on a non-logarithmic scale, with the variables displayed on both axes. As expected, there is a strong correlation between these variables. 
plot(x=log10(anoles$Mass), 
     y=log10(anoles$SVL),
     xlab="Anole Mass (log10 g)", #Renaming x-axis. 
     ylab="SVL (log10 cm)", #Renaming y-axis. 
     main="Snout-Vent Length (SVL) Scaled with Anole Mass", #Naming the figure. 
     pch=16,
     cex=1,
     col=rgb(red=0,
             green=0,
             blue=0,
             alpha=0.5)) #FIGURE 2: Made a standard scatterplot.
#FIGURE 2 DESCRIPTION: This figure shows the logarithmic relationship between anole mass and snout-vent length. As expected, there is still a strong correlation between these variables. 
boxplot(log10(Mass)~`Sex`, data=anoles)
Males<-anoles[which(anoles$Sex=="M"),]
Females<-anoles[which(anoles$Sex=="F"),]
par(mfrow=c(1, 2))
hist(log10(Males$`Mass`),
     xlab="Anole Mass (log10 g)", #Renaming x-axis. 
     main="Male") #Naming this figure. 
hist(log10(Females$`Mass`),
     xlab="Anole Mass (log10 g)", #Renaming y-axis. 
     main="Female") #Naming this figure. 
par(mfrow=c(1,1)) #FIGURE 3: Made a double-panel barplot. 
#FIGURE 3 DESCRIPTION: This figure shows a side-by-side barplot displaying mass values for each sex on a logarithmic scale, and how many times each mass occurred for each sex. 
