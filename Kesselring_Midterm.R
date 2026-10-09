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
twoa <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/2a.RDS")
twob <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/2b.RDS")
twoc <- read_rds("https://github.com/bmaitner/biometry_course/raw/refs/heads/main/midterm/2c.RDS")

#Inspect data set 2a.
head(twoa)
str(twoa)
summary(twoa)

#Plot a scatterplot to choose a function. 
plot(Wing.Length~Mass, data=twoa)
#The shape of the data in the scatterplot implies a power law function, especially when considering that these variables could be used to describe allometric growth. 
#These variables could reflect potential allometric growth, considering mass as a function of wing length. 

#Now let's plot a histogram to choose a distribution. 
hist(twoa$Wing.Length,
     probability = TRUE,
     breaks = 25,
     col = "gray",
     xlab = "Wing length",
     ylab = "Density",
     main = "Distribution of wing length")

#The shape of the data in the histogram could imply a gamma distribution. Plot a curve to check. 
m <- mean(twoa$Wing.Length)
s2 <- var(twoa$Wing.Length)
shape <- m^2 / s2
rate  <- m / s2
curve(dgamma(x, shape = shape, rate = rate),
      add = TRUE, col = "red", lwd = 2)
#The gamma distribution curve is imperfect, but still a reasonable fit for this data. 
#The use of a gamma distribution can also be justified by some of the data's other properties. 
#The data is continuous, positive, and has a noticeable right tail. Gamma curves are also a commonly used distribution for body size data, such as wing length. 

#Inspect data set 2b. 
head(twob)
str(twob)
summary(twob)

#Plot a scatterplot to choose a function. 
plot(Beak.Width ~ Beak.Length, data = twob)
#The shape of the data in the scatterplot implies a linear function, especially when considering that these variables are both measurements of the same body part.
#Because the variables are from the same anotomical structure, it is reasonable to assume they would scale proportionally.

#Now let's plot a histogram to choose a distribution. 
hist(twob$Beak.Width,
     probability = TRUE,
     breaks = 25,
     col = "gray",
     xlab = "Beak width",
     ylab = "Density",
     main = "Distribution of beak width")

#The shape of the data in the histogram could imply another gamma distribution. Plot a curve to check. 
m  <- mean(twob$Beak.Width)
s2 <- var(twob$Beak.Width)
shape <- m^2 / s2
rate  <- m / s2
curve(dgamma(x, shape = shape, rate = rate),
      add = TRUE, col = "red", lwd = 2)
#This gamma distribution curve is also imperfect, but as before is still a reasonable fit for this data. 
#Like before, the use of a gamma distribution can also be justified by some of the data's other properties. 
#The data is continuous, positive, and has a noticeable right tail. Gamma curves are also a commonly used distribution for body size data, such as beak width. 

#Inspect data set 2c. 
head(twoc)
str(twoc)
summary(twoc)

#Plot a scatterplot to choose a function. 
plot(Functional.Evenness ~ Functional.Richness, data = twoc)
#The shape of the data in the scatterplot implies a negative linear function, especially when considering that functional richness measures the volume of trait space filled, whereas functional evenness measures how uniformly species are distributed within the trait space. 
#When richness is high, many species will occupy a trait space, forcing occupation at the edges of the space, or overlapping. 
#This effect therefore decreases evenness, which contributes to the inverse relationship we see on the scatterplot. 

#Now let's plot a histogram to choose a distribution. 
hist(twoc$Functional.Evenness,
     probability = TRUE,
     breaks = 20,
     col = "gray",
     xlab = "Functional evenness",
     ylab = "Density",
     main = "Distribution of functional evenness")

#The shape of the data in the histogram could imply a beta distribution. Plot a curve to check. 
m  <- mean(twoc$Functional.Evenness)
s2 <- var(twoc$Functional.Evenness)
common <- m * (1 - m) / s2 - 1
shape1 <- m * common
shape2 <- (1 - m) * common
curve(dbeta(x, shape1 = shape1, shape2 = shape2),
      add = TRUE, col = "red", lwd = 2)
#This beta distribution curve is imperfect, but is still a reasonable fit for this data. 
#Like the previous examples, there are other properties that can help us justify this distribution. 
#The evenness data is bounded from 0 to 1, and beta distributions are useful for defining continuous data over a finite range.  



#PART 3: POWER ANALYSIS
#Part 3, Figure 1: 
sample_size <- 10
a <- 2
sd <- 8
nsim <- 400
bvec <- seq(-2, 2, by = 0.1)

power.b <- numeric(length(bvec))
pval <- numeric(nsim)

for(j in 1:length(bvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    b <- bvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    m <- lm(y ~ x)
    #get p-value
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
  }#end i lloop
  
  power.b[j] <- sum(pval< 0.05)/nsim
}#end j loop

plot(power.b ~ bvec,
     type="l", 
     ylim=c(0, 1), 
     xlab="Slope (b)", 
     ylab="Estimated power", 
     main=paste("Sample size=", sample_size), 
     col="black", 
     lwd=2)
abline(h=0.8, lty=2, col="gray")

#Part 3, Figure 2: 
sample_size <- 50
a <- 2
sd <- 8
nsim <- 400
bvec <- seq(-2, 2, by = 0.1)

power.b <- numeric(length(bvec))
pval <- numeric(nsim)

for(j in 1:length(bvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    b <- bvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    m <- lm(y ~ x)
    #get p-value
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
  }#end i lloop
  
  power.b[j] <- sum(pval< 0.05)/nsim
}#end j loop

plot(power.b ~ bvec,
     type="l", 
     ylim=c(0, 1), 
     xlab="Slope (b)", 
     ylab="Estimated power", 
     main=paste("Sample size=", sample_size), 
     col="black", 
     lwd=2)
abline(h=0.8, lty=2, col="gray")

#Part 3, Figure 3: 
sample_size <- 200
a <- 2
sd <- 8
nsim <- 400
bvec <- seq(-2, 2, by = 0.1)

power.b <- numeric(length(bvec))
pval <- numeric(nsim)

for(j in 1:length(bvec)){
  for(i in 1:nsim){
    
    x <- sample(x = 1:20,
                size = sample_size,
                replace = TRUE)
    b <- bvec[j]
    y_det <- a + b*x
    y <- rnorm(n = length(y_det),
               mean = y_det,
               sd = sd)
    m <- lm(y ~ x)
    #get p-value
    pval[i] <- coef(summary(m))["x","Pr(>|t|)"]
  }#end i lloop
  
  power.b[j] <- sum(pval< 0.05)/nsim
}#end j loop

plot(power.b ~ bvec,
     type="l", 
     ylim=c(0, 1), 
     xlab="Slope (b)", 
     ylab="Estimated power", 
     main=paste("Sample size=", sample_size), 
     col="black", 
     lwd=2)
abline(h=0.8, lty=2, col="gray")
#How do slope and sample size impact power? 
#Lower slope values result in lower overall power, and therefore higher slopes also result in higher power.
#This is because at lower slopes, there is almost no effect to detect, but as slope rises, the effect becomes easier to detect and distinguish from noise. 
#Increasing the sample size results in a higher power at every slope, and also causes the "U" shape surrounding the slope of zero to narrow. 
