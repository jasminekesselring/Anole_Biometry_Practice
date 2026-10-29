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