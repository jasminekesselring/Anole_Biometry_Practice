#PART 1: 
#EXAMPLE 1) Number of surviving individuals within a forest plot, out of a known starting number
#Discrete 
#Binomial: There are a fixed number of trials, each with independent chances of success or failure. 

#EXAMPLE 2) Species abundance (counts of individuals in a plot)
#Discrete 
#Poisson: Assuming individuals are randomly distributed, the Poisson allows for discrete, unbounded data like species abundance. 

#EXAMPLE 3) Pollinator visitation rate (visits per flower per unit time)
#Continuous 
#Gamma: Allows for continuous, positive, right-skewed data. Also considers units of time until an event occurs. 

#EXAMPLE 4) Ages (in years) of surviving individuals within a reserve
#Continuous 
#Normal: If the ages are roughly symmetrical around the mean, then this could be used for continuous data. However if skewed, you could opt for a Gamma or Exponential instead. 

#EXAMPLE 5) Body size (e.g., body mass, length)
#Continuous 
#Lognormal: A good choice for sizes and masses of individuals, especially if rapidly growing. Also a good fit since data would be right-skewed. 

#EXAMPLE 6) Biomass per unit area
#Continuous 
#Lognormal: Similar reasoning to the previous example. The data is continuous, right-skewed, and relates to body size. 

#EXAMPLE 7) Time to germination
#Continuous 
#Gamma: Considers units of time until the germination occurs. Allows for continuous, positive, right-skewed data. 

#EXAMPLE 8) Proportion of habitat covered by vegetation
#Continuous 
#Beta: One of the best options for modeling proportions and probabilities. Also bounds data between 0-1. 

#EXAMPLE 9) Larval settlement success (number settled out of larvae released)
#Discrete
#Binomial: Fixed number of trials, with independent chances of settlement success.

#EXAMPLE 10) Proportion of infected fish in a population (parasite prevalence)
#Continuous 
#Beta: Prevalence is a proportion bound between 0 and 1, and beta distributions are one of the best options for proportions. 

#EXAMPLE 11) Daily temperature at a field site
#Continuous 
#Normal: Good for symmetrical, unbounded data, and is frequently used for temperature data. 

#EXAMPLE 12) Population growth rate, meaning next year’s population size divided by this year’s
#Continuous 
#Lognormal: Data would be continuous and positive and would accommodate data with size ratios. 



#PART 2: 
library(readxl)
library(tidyverse)
anoles<-read_excel("Anole Dissections COPY.xlsx", 
                   sheet="Fort De Soto")
#VARIABLE 1: Mass: Continuous; NORMAL DISTRIBUTION. Normal distributions are a good fit for body size data since body size is a continuous trait that can influenced by many outside variables.  
x<-anoles$Mass #Assign variable. 
hist(x, freq=FALSE, 
     breaks=30) #Plot histogram.
mean(x, na.rm=TRUE) #Calculate mean.
median(x, na.rm=TRUE) #Calculate median. 
#This histogram is right skewed, so let's log transform.
hist(log10(x), freq=FALSE, 
     breaks=30,
     main="Distribution of anole mass (log10)",
     xlab="Mass log10(g)",
     ylab="Density",
     col="azure2") #Plot histogram.
mean(log10(x), na.rm=TRUE) #Find new log transformed mean.
median(log10(x), na.rm=TRUE) #Find new log transformed median.
sd(log10(anoles$Mass), na.rm=TRUE) #Find standard deviation of log transformed data. 
curve(dnorm(x, mean=0.5510243, sd=0.2023386), add=TRUE, col="red", lwd=2) #Plot curve. 

#VARIABLE 2: SVL of Male Anoles: Continuous; NORMAL DISTRIBUTION. Normal distributions are a good fit for body size data since body size is a continuous trait that can influenced by many outside variables. However, the fit of this distribution could be improved.  
x<-anoles$SVL #Assign variable. 
anoles_males<- anoles |>
  filter(Sex=="M") #Filter out Females. 
hist(anoles_males$SVL, freq=FALSE, 
     breaks=30, 
     main="Distribution of SVL in male anoles",
     xlab="SVL (cm)", 
     ylab="Density",
     col="azure2") #Plot histogram.
mean(anoles_males$SVL, na.rm=TRUE) #Calculate mean.
sd(anoles_males$SVL, na.rm=TRUE) #Calculate sd. 
median(x, na.rm=TRUE) #Calculate median. 
curve(dnorm(x, mean=5.583051, sd=0.6292175), add=TRUE, col="red", lwd=2) #Plot curve. 
#Attempted to log transform data, but the fit of the curve was not improved. Data were left in their original format. 


#VARIABLE 3: Ro_Count: Discrete: NEGATIVE BINOMIAL DISTRIBUTION (ZERO-TRUNCATED). A negative binomial distribution is a good fit for this data since the data is overdispersed (variance is quite larger than the mean), and there is no upper limit. It is zero-truncated because all uninfected anoles have been removed from the data set.   
anoles$Ro_Count<-as.numeric(as.character(anoles$Ro_Count)) #Convert class to numeric. 
anoles_positive<-anoles |> #Keep only anoles who were positive for Ro larvae. 
  filter(Ro_Count > 0)
x<-anoles_positive$Ro_Count #Isolate filtered data. 
median(x) #Calculate median.
mu<-mean(x) #Calculate mean. 
v<-var(x) #Calculate variance. 
size<-mu^2/(v-mu) #Calculate size, a negative binomial parameter. 
prob<-size/(size+mu) #Calculate prob, a negative binomial parameter. 
k<-1:max(x) #Create integer count values. 
p_truncated<-dnbinom(k, size=size, prob=prob)/
  (1-dnbinom(0, size=size, prob=prob)) #Zero-truncated probabilities. 
hist(x, 
     freq=FALSE, 
     breaks=seq(0.5, max(x)+0.5, by=1), 
     main="Infection intensity of pentastome larvae",
     xlab="Number of pentastome larvae", 
     ylab="Density", 
     col="azure2") #Plot histogram.
lines(k, p_truncated, col="red", type="l", lwd=3) #Plot curve. 