#PART 1: WRITING ABOUT DATA
#Load packages. 
library(readxl)
library(dplyr)
library(ggplot2)

#Load spreadsheet. 
anoles <- read_excel("Anole Dissections COPY.xlsx", sheet = "Pinellas")

#Clean data.
anoles_clean<-anoles |>
  mutate(
    SVL=as.numeric(SVL), 
    Mass=as.numeric(Mass), 
    Site_Class=factor(Site_Class, levels=c("Natural", "Anthropogenic"))
  )|>
  filter(!is.na(SVL), !is.na(Mass), !is.na(Site_Class), SVL>0, Mass>0)

#Take a quick look at the data to ensure it looks generally correct. 
summary(anoles_clean[, c("SVL", "Mass", "Site_Class")])
str(anoles_clean)

#Assign data by site class.
natural<-anoles_clean[anoles_clean$Site_Class=="Natural",]
anthro<-anoles_clean[anoles_clean$Site_Class=="Anthropogenic",]

#Plot data. This plot shows a logged Mass against logged SVL, and is highly correlated as expected. 
ggplot(anoles_clean, aes(x = log(SVL), y = log(Mass), color = Site_Class)) +
  geom_point(alpha = 0.7, size = 1.25) +
  geom_smooth(method = "lm", se = TRUE, lwd =1) +
  scale_color_manual(values = c("Natural" = "chartreuse4",
                                "Anthropogenic" = "darkorange2")) +
  labs(
    x = "log(SVL, cm)",
    y = "log(Mass, g)",
    color = "Site class",
    title = "Body mass scales with SVL in A. sagrei",
    subtitle = "Pinellas County sites, by anthropogenic vs. natural classification"
  ) +
  theme_classic(base_size = 12)

#Plot a boxplot of SVL separated by site class. 
ggplot(anoles_clean, aes(x = Site_Class, y = SVL)) +
  geom_boxplot(fill = c("chartreuse4", "darkorange2")) +
  labs(
    x = "Site class", 
    y = "SVL (cm)", 
    title= "A. sagrei SVL by site classification")+
  theme_classic(base_size = 12)

#Check whether or not gamma distribution is a good fit for this SVL data. Plot a histogram and a gamma distribution curve. 
x<-anoles_clean$SVL 
hist(anoles_clean$SVL, freq=FALSE, 
     breaks=30, 
     main="Distribution of SVL in A. sagrei",
     xlab="SVL (cm)", 
     ylab="Density",
     col="lightgray")
m<-mean(anoles_clean$SVL)
s2<-var(anoles_clean$SVL)
shape<-m^2/s2
rate<-m/s2
curve(dgamma(x, shape = shape, rate = rate),
      add = TRUE,
      col = "red",
      lwd = 2)



#PART 2: DISTRIBUTIONS AND FUNCTIONS
library(readr)
library(ggplot2)
twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2a.RDS")
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2b.RDS")
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/b_2c.RDS")

#Inspect data set 2a.
head(twoa)
str(twoa)
summary(twoa)

#Plot a scatterplot to choose a function. 
plot(log(twoa$Adult.Body.Mass.g), log(twoa$Egg.Mass.g), 
     xlab = "log(adult body mass, g)",
     ylab = "log(egg mass, g)",
     main = "Egg mass vs. adult body mass")
#The linear shape of the data in the scatterplot implies a power law function, especially when considering that these variables could be used to describe allometric growth. 
#These variables could reflect allometric growth due to the physiological constraints of egg mass and its interactions with body mass.

#Now let's plot a histogram to choose a distribution. 
hist(twoa$Egg.Mass.g,
     breaks = 25,
     col = "gray",
     xlab = "Egg mass (g)",
     ylab = "Frequency",
     main = "Distribution of egg mass")
mean(twoa$Egg.Mass.g)
var(twoa$Egg.Mass.g)

#The shape of the data in the histogram could imply a gamma distribution. Plot a curve to check. 
hist(twoa$Egg.Mass.g,
     probability = TRUE,
     breaks = 25,
     col = "gray",
     xlab = "Egg mass (g)",
     ylab = "Frequency",
     main = "Distribution of egg mass")

m  <- mean(twoa$Egg.Mass.g)
s2 <- var(twoa$Egg.Mass.g)
shape <- m^2 / s2
rate  <- m / s2

curve(dgamma(x, shape = shape, rate = rate),
      add = TRUE, col = "red", lwd = 2)
#The gamma distribution curve is a reasonable fit for this data. 
#The use of a gamma distribution can also be justified by some of the data's other properties. 
#The data is continuous, positive, and is noticeably right skewed. Gamma curves are also a commonly used distribution for body size data, including egg mass or body mass.

#Inspect data set 2b. 
head(twob)
str(twob)
summary(twob)

#Plot a scatterplot to choose a function. 
plot(twob$Total.Fish.Abundance, twob$Species.Richness,
     xlab = "Total fish abundance",
     ylab = "Species richness",
     main = "Species richness vs. fish abundance")
#The shape of the data in the scatterplot looks like it could be a Michaelis-Menten function. The shape of the curve increases initially, but then levels off. 
#The Michaelis-Menten function is logical because species richness will eventually plateau in a finite area (such as the reef in this case). Once you've recorded most of the species, adding more individuals adds no/few new species. 

#Now let's plot a histogram to choose a distribution. 
hist(twob$Species.Richness,
     breaks = 25,
     col = "gray",
     xlab = "Species richness (number of species)",
     ylab = "Frequency",
     main = "Distribution of species richness")
mean(twob$Species.Richness)
var(twob$Species.Richness)

#The shape of the data in the histogram could a negative binomial distribution. Plot a curve to check. 
hist(twob$Species.Richness,
     probability = TRUE,
     breaks = 25,
     col = "gray",
     xlab = "Species Richness",
     ylab = "Frequency",
     main = "Distribution of species richness")

m  <- mean(twob$Species.Richness)
s2 <- var(twob$Species.Richness)
mu<-m
k<-m/(s2/m-1)
x<-0:max(twob$Species.Richness)
lines(x,
      dnbinom(x, size=k, mu=mu),
      type="h", col="red", lwd=2)
#This negative binomial distribution curve is imperfect, but is still a reasonable fit for this data. 
#A negative binomial distribution is also reasonable for this data because it is discrete and has a variance that is much larger than the mean.  
#Biologically, a negative binomial distribution makes sense because the rate of encountering new species will differ from reef to reef.
#If species richness/detection of new species were to remain constant from reef to reef, a Poisson distribution would be a more reasonable fit instead.

#Inspect data set 2c. 
head(twoc)
str(twoc)
summary(twoc)

#Plot a scatterplot to choose a function. 
plot(twoc$Rugosity, twoc$Hard.Bottom.Cover.Pct,
     xlab = "Rugosity",
     ylab = "Hard bottom cover (%)",
     main = "Hard bottom cover vs. rugosity")
#The shape of the data in the scatterplot implies a logarithmic function. 
#The shape of the data shows a steep curve at low x values, and rapidly begins to level off. 
#Logarithmic functions can begin above zero (appropriate since rugosity never reaches zero in this data set), whereas some of the other flattening curves in the bestiary begin at zero. 

#Now let's plot a histogram to choose a distribution. 
hist(twoc$Hard.Bottom.Cover.Pct,
     breaks = 20,
     col = "gray",
     xlab = "Hard bottom cover (%)",
     ylab = "Frequency",
     main = "Distribution of hard bottom cover")
mean(twoc$Hard.Bottom.Cover.Pct)
var(twoc$Hard.Bottom.Cover.Pct)

#The shape of the data in the histogram could imply a beta distribution. Plot a curve to check. 
#Rescale first to consider that beta is in a 0-1 proportion. 
p<-twoc$Hard.Bottom.Cover.Pct/100

hist(p,
     probability = TRUE,
     breaks = 20,
     col = "gray",
     xlab = "Hard Bottom Cover",
     ylab = "Frequency",
     main = "Distribution of hard bottom cover")

m  <- mean(p)
s2 <- var(p)
common <- m * (1 - m) / s2 - 1
a <- m * common
b <- (1 - m) * common
curve(dbeta(x, shape1 = a, shape2 = b),
      add = TRUE, col = "red", lwd = 2)
#This beta distribution curve is a reasonable fit for this data, as long as you remember to scale the data from 0-1 to accommodate the beta distribution's 0-1 range. 
#The rescaled data is bounded from 0 to 1, and beta distributions are useful for defining continuous data over this finite range. 
#In this case, we treat proportions as continuous data, and because hard bottom cover is a proportion, a beta distribution is the most appropriate fit. 



#PART 3: POWER ANALYSIS
#Part 3, Figure 1: 
sample_size <- 20
a <- 2
b<-1

nsim <- 400
sdvec <- seq(1, 20, by = 1)
power.sd <- numeric(length(sdvec))
pval <- numeric(nsim)

for(j in 1:length(sdvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    sd <- sdvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    m <- lm(y ~ x)
    #get p-value
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
  }#end i lloop
  
  power.sd[j] <- sum(pval< 0.05)/nsim
}#end j loop

plot(power.sd ~ sdvec,
     type="l", 
     ylim=c(0, 1), 
     xlab="Standard Deviation", 
     ylab="Estimated power", 
     main=paste("Sample size=", sample_size), 
     col="black", 
     lwd=2)
abline(h=0.8, lty=2, col="red")

#Part 3, Figure 2: 
sample_size <- 50
a <- 2
b<-1

nsim <- 400
sdvec <- seq(1, 20, by = 1)
power.sd <- numeric(length(sdvec))
pval <- numeric(nsim)

for(j in 1:length(sdvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    sd <- sdvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    m <- lm(y ~ x)
    #get p-value
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
  }#end i lloop
  
  power.sd[j] <- sum(pval< 0.05)/nsim
}#end j loop

plot(power.sd ~ sdvec,
     type="l", 
     ylim=c(0, 1), 
     xlab="Standard Deviation", 
     ylab="Estimated power", 
     main=paste("Sample size=", sample_size), 
     col="black", 
     lwd=2)
abline(h=0.8, lty=2, col="red")

#Part 3, Figure 3: 
sample_size <- 200
a <- 2
b<-1

nsim <- 400
sdvec <- seq(1, 20, by = 1)
power.sd <- numeric(length(sdvec))
pval <- numeric(nsim)

for(j in 1:length(sdvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    sd <- sdvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    m <- lm(y ~ x)
    #get p-value
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
  }#end i lloop
  
  power.sd[j] <- sum(pval< 0.05)/nsim
}#end j loop

plot(power.sd ~ sdvec,
     type="l", 
     ylim=c(0, 1), 
     xlab="Standard Deviation", 
     ylab="Estimated power", 
     main=paste("Sample size=", sample_size), 
     col="black", 
     lwd=2)
abline(h=0.8, lty=2, col="red")
#How do noise and sample size impact power? 
#The power for detecting our slope of 1 decreased as noise increased. 
#At smaller sample sizes, this decline for detection was also steeper. 
#This means that overall, power increases with decreasing noise, and power is stronger at larger sample sizes. 