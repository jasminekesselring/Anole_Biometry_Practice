library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
anoles <- read_excel("Herpeton Anole Data.xlsx")
anoles <- anoles %>%
  mutate(
    Ro_Visual_clean = case_when(
      Ro_Visual %in% c("Y")        ~ "Y",
      Ro_Visual %in% c("N", "N?")  ~ "N",
      TRUE                         ~ NA_character_
    ),
    infected = case_when(
      Ro_Visual_clean == "Y" ~ 1L,
      Ro_Visual_clean == "N" ~ 0L,
      TRUE                   ~ NA_integer_
    )
  )

sum(is.na(anoles$infected))
table(anoles$Site_Class, anoles$infected, useNA = "ifany")

# 2x2 table (Site_Class x infected)
tab <- table(anoles$Site_Class, anoles$infected)
tab

# Fisher's exact test
ft <- fisher.test(tab)
ft

# Extract the key numbers for reporting
ft$p.value                    # p-value
ft$estimate                   # conditional MLE odds ratio
ft$conf.int                   # 95% CI for the odds ratio

install.packages("logistf")
library(logistf)
m_firth <- logistf(infected ~ Site_Class, data = anoles)
summary(m_firth)
exp(coef(m_firth))            # penalized OR
exp(m_firth$ci.lower)         # CI lower
exp(m_firth$ci.upper)         # CI upper


library(readxl)
library(dplyr)

anoleglm <- read_excel("Herpeton Anole Data.xlsx", sheet = "Anole Master Sheet")

anoleglm_clean <- anoleglm %>%
  mutate(
    Infected_Ro = case_when(
      Ro_Visual == "Y" ~ 1,
      Ro_Visual == "N" ~ 0,
      TRUE             ~ NA_real_      # blanks, "Y?", "N?"
    ),
    Site_Class = factor(Site_Class, levels = c("Natural", "Anthropogenic"))
  ) %>%
  filter(!is.na(Infected_Ro), !is.na(Site_Class))

# ---- Crosstab ----
tab <- table(anoleglm_clean$Site_Class, anoleglm_clean$Infected_Ro)
print(tab)
print(prop.table(tab, 1))   # row proportions (prevalence by site class)

# ---- GLM ----
m <- glm(Infected_Ro ~ Site_Class, data = anoleglm_clean, family = binomial)
summary(m)
anova(m, test = "Chisq")

# Odds ratio + 95% CI
exp(cbind(OR = coef(m), confint(m)))

# ---- Robustness: Fisher's exact test ----
fisher.test(tab)

# ---- Predicted prevalence ----
predict(m, newdata = data.frame(Site_Class = factor(c("Natural","Anthropogenic"),
                                                    levels = c("Natural","Anthropogenic"))),
        type = "response", se.fit = TRUE)

#PLOTTING FIGURE

install.packages("emmeans")
install.packages("ggtext")
library(ggtext)
library(ggplot2)
library(dplyr)
library(emmeans)   # for model-based CIs
# install.packages(c("ggplot2","dplyr","emmeans","scales")) if needed

# Rebuild your clean data frame if not already in memory
# (same code as before)
anoleglm_clean <- readxl::read_excel("Herpeton Anole Data.xlsx",
                                sheet = "Anole Master Sheet") %>%
  mutate(
    Infected_Ro = case_when(Ro_Visual == "Y" ~ 1,
                            Ro_Visual == "N" ~ 0,
                            TRUE ~ NA_real_),
    Site_Class = factor(Site_Class, levels = c("Natural","Anthropogenic"))
  ) %>%
  filter(!is.na(Infected_Ro), !is.na(Site_Class))

m <- glm(Infected_Ro ~ Site_Class, data = anoleglm_clean, family = binomial)

emm <- emmeans(m, ~ Site_Class, type = "response")   # returns probabilities
emm_df <- as.data.frame(emm)
print(emm_df)

ggplot(emm_df, aes(x = Site_Class, y = prob, fill = Site_Class)) +
  geom_col(width = 0.6, colour = "black") +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.15, linewidth = 0.7) +
  geom_richtext(
    aes(label = sprintf("<i>Ro</i> %.1f%%", prob * 100)),
    position = position_nudge(x = 0.36),
    hjust = 0, size = 4.5,
    fill = NA, label.colour = NA         # no box, just text
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.12))
  ) +
  scale_x_discrete(expand = expansion(add = c(0.4, 1.1))) +
  coord_cartesian(ylim = c(0, 0.14)) +
  labs(x = "Site class",
       y = "Predicted *Ro* prevalence (%, ±95% CI)",
       title = "Model-predicted *R. orientalis* prevalence") +
  theme_classic(base_size = 14) +
  theme(
    legend.position = "none",
    axis.title.y    = element_markdown(),
    plot.title      = element_markdown()
  )
