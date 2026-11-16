library(MASS)
library(bbmle)      
library(tidyverse)
library(readxl)

#Loading and cleaning data.
anoles <- read_excel("Anole Data Master Sheet.xlsx")
names(anoles) <- trimws(names(anoles))
anoles <- anoles %>%
  filter(!is.na(Sample_ID), Sample_ID != "") %>%
  mutate(
    Sex        = factor(Sex),
    Site_Class = factor(Site_Class, levels = c("Natural", "Anthropogenic")),
    Ro_Visual  = factor(Ro_Visual,  levels = c("N", "Y")),
    SVL        = as.numeric(SVL),
    Mass       = as.numeric(Mass),
    Ro_Count   = as.numeric(Ro_Count)
  ) %>%
  filter(!is.na(Ro_Count)) #Drop the 2 non-numeric rows

#Making sure there are no NAs in columns I plan to model with.
sum(is.na(anoles[, c("Ro_Count","Ro_Visual","SVL","Sex","Site_Class")]))

#QUESTION 1: Does site class affect R. orientalis prevalence, controlling for host size (SVL) and sex?
prev1 <- glm(Ro_Visual ~ Site_Class, family = binomial, data = anoles)             #Baseline: Does habitat alone predict infection (ignoring size)?
prev2 <- glm(Ro_Visual ~ Site_Class + SVL, family = binomial, data = anoles)       #Adding SVL: Does size explain infection beyond habitat?
prev3 <- glm(Ro_Visual ~ Site_Class + SVL + Sex, family = binomial, data = anoles) #Adding Sex: Do males and females differ in prevalence after habitat and size are accounted for?
prev4 <- glm(Ro_Visual ~ Site_Class * SVL + Sex, family = binomial, data = anoles) #Interaction: Does the size effect itself differ by habitat type?

#Why were these variables chosen? 
#Binomial family was chosen due to infection status being a noncontinuous binary result (infected vs. not infected).
#SVL was chosen as an additional predictor since it is likely that larger lizards are exposed to more infected prey items in their diets. 
#Sex was chosen as a predictor because the anoles are sexually dimorphic (males are larger) and therefore may show similar patterns as those seen in the SVL predictor.

#Comparing models.
AICtab(prev1, prev2, prev3, prev4)
#prev2 (Site_Class + SVL) had the lowest AIC; adding Sex did not improve fit.

summary(prev2)
exp(coef(prev2)); exp(confint(prev2))
#This model assumes that the probability of R. orientalis infection is a function of site class and host body size (SVL in cm).
#This is an additive model, so it assumes that the intercept varies by site class, while the effect of SVL on the log-odds is the same in both site types. 
#So biologically, site class is the baseline that shifts infection probability up or down. 
#Body size shifts infection probability up by a constant amount regardless of site class.

#Running predicted probabilities (original scale) with plausible SVL values (4cm, 5cm, 6cm). 
plogis(-9.2116 + 1.2986 * c(4, 5, 6)) #Natural
plogis(-9.2116 - 1.7123 + 1.2986 * c(4, 5, 6)) #Anthropogenic
#Predicted infection probability at SVL = 4, 5, 6 cm:
#Natural: 1.8%, 6.2%, 19.5%
#Anthropogenic: 0.3%, 1.2%, 4.2%

#CHECK WITH FIGURE: Predicted probability of R. orientalis infection across SVL, by site class, shown over raw 0/1 observations.
library(ggplot2)
grid <- expand.grid(
  SVL        = seq(min(anoles$SVL), max(anoles$SVL), length = 100),
  Site_Class = c("Natural", "Anthropogenic")
)
grid$Site_Class <- factor(grid$Site_Class, levels = c("Natural", "Anthropogenic"))

p <- predict(prev2, newdata = grid, type = "link", se.fit = TRUE)
grid$prob <- plogis(p$fit)
grid$lwr  <- plogis(p$fit - 1.96 * p$se.fit)
grid$upr  <- plogis(p$fit + 1.96 * p$se.fit)

ggplot(grid, aes(x = SVL, y = prob, fill = Site_Class)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.3) +
  geom_line(aes(color = Site_Class)) +
  geom_jitter(data = anoles,
              aes(x = SVL, y = as.integer(Ro_Visual == "Y"), color = Site_Class),
              inherit.aes = FALSE,
              height = 0.02, width = 0, alpha = 0.7) +
  labs(x = "SVL (cm)", y = "Probability of R. orientalis infection",
       fill = "Site class", color = "Site class") +
  theme_minimal()
#The anthropogenic curve always lies below the natural curve. 
#The curves do not cross, and the bands widen at extreme SVLs where the data are more sparse. 

#Big takeaways for Question 1: 
#Odds of R. orientalis infection are roughly 82% lower in anthropogenic sites. 
#Odds of R. orientalis infection also steeply increase with body size. 
#Each cm in body length multiplies the odds of infection by about 3.7. 
#This can probably be attributed to larger lizards having increased exposure and/or having a larger foraging area.
#Therefore, this suggests habitat type is associated with R. orientalis prevalence, and larger hosts are more likely to be infected. 

#QUESTION 2: Does site class affect R. orientalis intensity (the amount of larvae in a single host), controlling for host size (SVL) and sex?
mean(anoles$Ro_Count); var(anoles$Ro_Count) #Check for overdispersion
#Count data is overdispersed. We will run a negative binomial model instead of Poisson.

intensity1 <- glm.nb(Ro_Count ~ Site_Class, data = anoles)             #Baseline: Does habitat alone predict Ro larval abundance (ignoring size)?
intensity2 <- glm.nb(Ro_Count ~ Site_Class + SVL, data = anoles)       #Adding SVL: Does size explain Ro larval abundance beyond habitat?
intensity3 <- glm.nb(Ro_Count ~ Site_Class + SVL + Sex, data = anoles) #Adding Sex: Do males and females differ in larval abundance after habitat and size are accounted for?
intensity4 <- glm.nb(Ro_Count ~ Site_Class * SVL + Sex, data = anoles) #Interaction: Does the size effect itself differ by habitat type?

#Why were these variables chosen? 
#Negative binomial was chosen because Ro_Count is count data (aka no negative values), and because the data is strongly overdispersed.  
#SVL was chosen as an additional predictor since it is likely that larger lizards are exposed to more infected prey items in their diets. 
#Sex was chosen as a predictor because the anoles are sexually dimorphic (males are larger) and therefore may show similar patterns as those seen in the SVL predictor.

#Comparing models. 
AICtab(intensity1, intensity2, intensity3, intensity4)
#intensity4 had the lowest AIC, but all top 3 models have a low AIC with no dramatic difference in value.
#Adding the extra parameters doesn't improve the model by a huge amount, so let's just stick with the simplest of the top three models.
#This is intensity2, which parallels the same effects seen in the prevalence models. 

summary(intensity2)
intensity2$theta
exp(coef(intensity2)); exp(confint(intensity2))
#This model assumes that the intensity of R. orientalis larvae is a function of site class and host body size (SVL in cm).
#This is an additive model, so it assumes that the intercept varies by site class, while the effect of SVL on the log expected count is the same in both site types. 
#So biologically, site class sets the baseline expected parasite burden that scales the intensity up or down. 
#Body size multiplies expected parasite burden by a constant amount regardless of site class.

#Predicted counts at plausible SVLs (original scale):
pred <- data.frame(SVL = c(4,5,6),
                 Site_Class = factor("Natural", levels = c("Natural","Anthropogenic")))
predict(intensity2, newdata = pred, type = "response")   # Natural
pred$Site_Class <- factor("Anthropogenic", levels = c("Natural","Anthropogenic"))
predict(intensity2, newdata =pred, type = "response")   # Anthropogenic
#Predicted expected larval counts at SVL = 4, 5, 6 cm:
#Natural: 0.051, 0.309, 1.871
#Anthropogenic: 0.002, 0.014, 0.084

#CHECK: Proportion of observed counts inside the 95% range:
mu <- predict(intensity2, type = "response")
lo <- qnbinom(0.025, mu = mu, size = intensity2$theta)
hi <- qnbinom(0.975, mu = mu, size = intensity2$theta)
mean(anoles$Ro_Count >= lo & anoles$Ro_Count <= hi)
#98% of the observed counts fall inside the 95% range.
#The few data points that lie outside of the range are likely just extremely high larval counts. 
#For example, anole #CW077 had over 120 larvae, when most infected anoles have less than 10 larvae. 

#Big takeaways for Question 2: 
#Expected R. orientalis counts are roughly 22 times higher in natural sites than in anthropogenic sites. 
#Each cm in body length multiplies the expected larval count by about 6. 
#The strong size effect can probably be attributed to larger lizards having increased exposure and/or having a larger foraging area.
#Overall, habitat type is associated with R. orientalis abundance, and larger hosts carry heavier burdens. 
#This mirrors the pattern seen for infection probability. 
