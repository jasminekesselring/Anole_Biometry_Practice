#PART 1: MODEL MODIFICATION AND SELECTION

library(bbmle)
library(biometrydata)

#1A: Competition between bean beetles 
beetles <- beetle_competition_interspecific
beetles <- beetles[beetles$pair == "Cmac_Csub" & !is.na(beetles$progeny_1), ]

#Plot: offspring per female against the number of competitors
plot(beetles$start_females_1 + beetles$start_females_2,
     beetles$progeny_1 / beetles$start_females_1,
     xlab = "females in the dish, both species",
     ylab = "C. maculatus offspring per female")

#The starting model:
beetle.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                         (1 + (a11 * start_females_1 +
                                                 a12 * start_females_2)^d),
                                       size = k),
                   start = list(R = 20, a11 = 0.1, a12 = 0.1, d = 1, k = 2),
                   lower = c(R = 0.01, a11 = 0, a12 = 0, d = 0.1, k = 0.01),
                   upper = c(R = 1000, a11 = 10, a12 = 10, d = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)

coef(beetle.fit)
AIC(beetle.fit)

#Now, let's start building alternative models. First, is d needed? This is the classic Beverton-Holt model. 
bh.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                     (1 + (a11 * start_females_1 +
                                             a12 * start_females_2)),
                                   size = k),
               start = list(R = 20, a11 = 0.1, a12 = 0.1, k = 2),
               lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
               upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
               method = "L-BFGS-B",
               data = beetles)

#Another alternative model. What happens when there is no interspecific competition (fixing a12 at zero)?
noa12.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                       (1 + (a11 * start_females_1)^d),
                                     size = k),
                 start = list(R = 20, a11 = 0.1, d = 1, k = 2),
                 lower = c(R = 0.01, a11 = 0, d = 0.1, k = 0.01),
                 upper = c(R = 1000, a11 = 10, d = 10, k = 100),
                 method = "L-BFGS-B",
                 data = beetles)

#Another alternative model. What happens when a11=a12? Aka, is a single female as harmful as a single male?
equal.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 /
                                        (1 + (a * (start_females_1 +
                                                     start_females_2))^d),
                                      size = k),
                  start = list(R = 20, a = 0.1, d = 1, k = 2),
                  lower = c(R = 0.01, a = 0, d = 0.1, k = 0.01),
                  upper = c(R = 1000, a = 10, d = 10, k = 100),
                  method = "L-BFGS-B",
                  control = list(parscale = c(R = 20, a = 0.1, d = 1, k = 2)),
                  data = beetles)

#Last alternative model for 1a. Let's try a different shape, the Ricker model. 
#This model isn't nested in the Beverton-Holt, but it does share the same parameters and data. 
ricker.fit <- mle2(progeny_1 ~ dnbinom(mu = R * start_females_1 *
                                         exp(-a11 * start_females_1 -
                                               a12 * start_females_2),
                                       size = k),
                   start = list(R = 20, a11 = 0.1, a12 = 0.1, k = 2),
                   lower = c(R = 0.01, a11 = 0, a12 = 0, k = 0.01),
                   upper = c(R = 1000, a11 = 10, a12 = 10, k = 100),
                   method = "L-BFGS-B",
                   data = beetles)

#Now, let's compare all of these models. 
AICtab(beetle.fit, bh.fit, noa12.fit, equal.fit, ricker.fit,
       weights = TRUE, delta = TRUE, sort = TRUE)

#Based on this comparison, the best models are ricker.fit and bh.fit. 
#However, the provided beans are a finite resource for the beetles. 
#Because the larvae develop within the beans, adding more adult beetles is pointless once all of the bean space is occupied. 
#The beetles will saturate, therefore meaning that bh.fit is the most appropriate model to keep. 

#1B: Wing length in hawks and eagles
hawks <- avonet
hawks <- hawks[hawks$Family1 == "Accipitridae", c("Species1", "Wing.Length", "Mass", "Migration")]
hawks <- na.omit(hawks)
hawks$migration <- factor(hawks$Migration, levels = 1:3,
                          labels = c("resident", "partial", "migrant"))

#Plot:
plot(log(hawks$Mass), log(hawks$Wing.Length), col = hawks$migration,
     xlab = "log body mass", ylab = "log wing length")
legend("topleft", levels(hawks$migration), col = 1:3, pch = 1)

#The starting model:
hawk.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                      sdlog = exp(log_s)),
                 start = list(int = 3, b = 0.3, log_s = -2),
                 parameters = list(int ~ migration,
                                   b ~ migration,
                                   log_s ~ migration),
                 data = hawks)

coef(hawk.fit)
AIC(hawk.fit)

#Now, let's start building alternative models. First, does length scale differently with mass in residents compared to migrants? 
hawk_bcommon.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                              sdlog = exp(log_s)),
                         start = list(int = 3, b = 0.3, log_s = -2),
                         parameters = list(int ~ migration,
                                           log_s ~ migration),
                         data = hawks)

#Another alternative model. Is a shift in wing length for a given mass the only effect caused by migration? 
hawk_intonly.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                              sdlog = exp(log_s)),
                         start = list(int = 3, b = 0.3, log_s = -2),
                         parameters = list(int ~ migration),
                         data = hawks)

#Another alternative model. Does migration even matter at all?
hawk_nomig.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                            sdlog = exp(log_s)),
                       start = list(int = 3, b = 0.3, log_s = -2),
                       data = hawks)

#Last alternative model. Is distinction between partial migrants and full migrants needed, or would migrant vs. resident do? 
hawks$mig_binary <- factor(ifelse(hawks$migration == "migrant",
                                  "migrant", "not_migrant"))
hawk_binary.fit <- mle2(Wing.Length ~ dlnorm(meanlog = int + b * log(Mass),
                                             sdlog = exp(log_s)),
                        start = list(int = 3, b = 0.3, log_s = -2),
                        parameters = list(int ~ mig_binary),
                        data = hawks)

#Now, let's compare all of these models. 
AICtab(hawk.fit, hawk_bcommon.fit, hawk_intonly.fit,
       hawk_nomig.fit, hawk_binary.fit,
       weights = TRUE, delta = TRUE, sort = TRUE)

#Based on this comparison, the best model is hawk_intonly.fit. 
#Migrating Accipitridae do have longer wings for their body mass. 
#This does support that longer wings are better for reducing the energetic costs of sustained flight. 
#However, all 249 species listed here are not each their own data point.
#Closely related species have shared traits and ancestry, but accounting for this is beyond the scope of our course. 

#1C: Reef fish species richness
reef <- reef_fish_surveys
reef$depth <- reef$DEPTH / 10
reef$visibility <- reef$UNDERWATER_VISIBILITY / 10
reef$year <- reef$YEAR - 2014
reef$region <- factor(reef$REGION_DESCRIPTION,
                      levels = c("Dry Tortugas",
                                 "Florida Keys: from Key West to Miami",
                                 "Miami to North Martin County"),
                      labels = c("Tortugas", "Keys", "North"))
reef$protected <- ifelse(reef$MPA_NAME == "Unprotected", "no", "yes")

#Plot:
plot(reef$DEPTH, reef$species_richness, col = rgb(0, 0, 0, 0.1),
     xlab = "depth (m)", ylab = "species recorded")

#The starting model:
reef.fit <- mle2(species_richness ~ dpois(lambda = exp(int +
                                                         d1 * depth +
                                                         d2 * depth^2 +
                                                         v * visibility +
                                                         t * year)),
                 start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0),
                 parameters = list(int ~ region + protected),
                 data = reef)

coef(reef.fit)
AIC(reef.fit)

#First we need to address if Poisson is the right distribution. 
var(reef$species_richness) / mean(reef$species_richness)

#The data are overdispersed. This means we should probably try switching to a negative binomial distribution instead. 
reef_nb.fit <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                          d1 * depth +
                                                          d2 * depth^2 +
                                                          v * visibility +
                                                          t * year),
                                               size = k),
                    start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 1),
                    method = "L-BFGS-B",
                    lower = c(k = 0.001),
                    parameters = list(int ~ region + protected),
                    data = reef)

#Now, let's start building alternative models. First, does protection matter if region is included?
reef_nb_noprot.fit <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                                 d1 * depth +
                                                                 d2 * depth^2 +
                                                                 v * visibility +
                                                                 t * year),
                                                      size = k),
                           start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 1),
                           method = "L-BFGS-B",
                           lower = c(k = 0.001),
                           parameters = list(int ~ region),
                           data = reef)

#Another alternative model. Does region matter if protection is included?
reef_nb_noregion.fit <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                                   d1 * depth +
                                                                   d2 * depth^2 +
                                                                   v * visibility +
                                                                   t * year),
                                                        size = k),
                             start = list(int = 3, d1 = 0, d2 = 0, v = 0, t = 0, k = 1),
                             method = "L-BFGS-B",
                             lower = c(k = 0.001),
                             parameters = list(int ~ protected),
                             data = reef)

#Another alternative model. Let's try dropping depth^2.
reef_nb_linear.fit <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                                 d1 * depth +
                                                                 v * visibility +
                                                                 t * year),
                                                      size = k),
                           start = list(int = 3, d1 = 0, v = 0, t = 0, k = 1),
                           method = "L-BFGS-B",
                           lower = c(k = 0.001),
                           parameters = list(int ~ region + protected),
                           data = reef)

#Last alternative model. Let's also try dropping visibility.
reef_nb_novis.fit <- mle2(species_richness ~ dnbinom(mu = exp(int +
                                                                d1 * depth +
                                                                d2 * depth^2 +
                                                                t * year),
                                                     size = k),
                          start = list(int = 3, d1 = 0, d2 = 0, t = 0, k = 1),
                          method = "L-BFGS-B",
                          lower = c(k = 0.001),
                          parameters = list(int ~ region + protected),
                          data = reef)

#Now, let's compare all of these models. 
AICtab(reef.fit, reef_nb.fit,
       reef_nb_noprot.fit, reef_nb_noregion.fit,
       reef_nb_linear.fit, reef_nb_novis.fit,
       weights = TRUE, delta = TRUE, sort = TRUE)

#As expected, this comparison shows that the Poisson distribution was inappropriate for this data.
#It looks like the best model is reef_nb_linear.fit. But as expected, AIC kept every term. Let's back-transform to take a better look at this: 

exp(coef(reef_nb_linear.fit))

#Now we can confirm that reef_nb_linear.fit was the best model choice. Removal of most of the parameters worsened the model. 
#The Poisson model didn't work because it assumes that every species was independently recorded. In reality, species cluster within reefs. 
#Visibility had a very strong effect, but this is likely attributed to the divers being able to see better than a reflection of the reef's overall health. 


#PART 2: FOCAL DATASET MODEL SELECTION
#Response variable: Ro_Count (count of Raillietiella orientalis parasites in a single anole)
#Predictors: Site_Class, Sex, and SVL

#Load in data.
library(bbmle)
library(readxl)
anoles <- read_excel("Herpeton Anole Data.xlsx")

#Inspect data to fix any incorrect classes. 
str(anoles)
head(anoles)

#Convert the Ro_Count column to numeric.
anoles$Ro_Count_num <- suppressWarnings(as.numeric(anoles$Ro_Count))

#Did any values fail to convert/became NAs? If so, remove them.
sum(is.na(anoles$Ro_Count_num))
table(anoles$Ro_Count, useNA = "ifany")
anoles_model <- anoles[!is.na(anoles$Ro_Count_num), ]

#Distribution: Check if the data is overdispersed. 
var(anoles$Ro_Count_num, na.rm = TRUE) / mean(anoles$Ro_Count_num, na.rm = TRUE)

#The data is overdispersed, so we'll use a negative binomial distribution as a baseline. 
#Let's briefly inspect our possible predictors. 
table(anoles$Sex, useNA = "ifany")
table(anoles$Site_Class, useNA = "ifany")

#Starting model: Includes Sex, Site Class, and SVL.
nb_full <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int + b_svl * SVL), size = k),
                start = list(int = -1, b_svl = 0, k = 1),
                method = "L-BFGS-B",
                lower = c(k = 0.001),
                parameters = list(int ~ Sex + Site_Class),
                data = anoles_model)

#Warning message implies there's an issue with k being pushed to its lower bound. 
#Let's inspect this: 
coef(nb_full)

#Yes, k is sitting too close to the lower bound, so let's fit it on a log scale. 
#This is now our starting model: 
nb_everything <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int + b_svl * SVL),
                                       size = exp(log_k)),
                start = list(int = -1, b_svl = 0, log_k = 0),
                parameters = list(int ~ Sex + Site_Class),
                data = anoles_model)

#Now, let's build some alternative models. First, let's try dropping Site Class. 
nb_nosite <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int + b_svl * SVL),
                                         size = exp(log_k)),
                  start = list(int = -1, b_svl = 0.1, log_k = 0),
                  parameters = list(int ~ Sex),
                  data = anoles_model)

#Another alternative model. Let's drop Sex now. 
nb_nosex <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int + b_svl * SVL),
                                        size = exp(log_k)),
                 start = list(int = -1, b_svl = 0.1, log_k = 0),
                 parameters = list(int ~ Site_Class),
                 data = anoles_model)

#Another alternative model. Can parasite burden be explained with just sex and site class (aka dropping SVL)?
nb_nosvl <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int), size = exp(log_k)),
                 start = list(int = -1, log_k = 0),
                 parameters = list(int ~ Sex + Site_Class),
                 data = anoles_model)

#Another alternative model. Can SVL alone help us interpret parasite burden?
nb_svlonly <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int + b_svl * SVL),
                                        size = exp(log_k)),
                 start = list(int = -1, b_svl = 0.1, log_k = 0),
                 data = anoles_model)

#Another alternative model. Let's include a null model without any predictors too. 
nb_null <- mle2(Ro_Count_num ~ dnbinom(mu = exp(int), size = exp(log_k)),
                start = list(int = -1, log_k = 0),
                data = anoles_model)

#Now, let's compare all our models. 
AICtab(nb_everything, nb_nosite, nb_nosex, nb_nosvl,
       nb_svlonly, nb_null,
       weights = TRUE, delta = TRUE, sort = TRUE)

#Based on this comparison, the best model to keep is nb_nosex. 
#This means parasite burden was best predicted by SVL and site class. 
#Sex did not seem to have an important effect on parasite burden. 
#Biologically, this means that bigger lizards collected from Natural sites are likely to carry more parasites. 