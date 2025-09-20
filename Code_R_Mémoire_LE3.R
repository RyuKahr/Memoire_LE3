install.packages("readxl")
install.packages("dplyr")
library(readxl)
library(dplyr)

# Charger les données 
df <- read_excel("Base de données.xlsx", sheet = "Réponses au formulaire 1", skip =1)
View(df)

 


# Renommer les colonnes
colnames(df) <- c("Horodatage", "Mois de naissance", "Q1_CF", "Q2_CF", "Q3_CF", 
                  "Q1_BFNI", "Q2_BFNI", "Q3_BFNI", "Q1_CF_nudge", "Q2_CF_nudge", 
                  "Q1_BFNI_nudge", "Q2_BFNI_nudge", "genre", "année naissance", 
                  "niveau etude", "filière", "niveau aversion", "selfconfidence", "competitif")

View(df)

# Garder uniquement les colonnes numériques
df_num <- df %>%
  select(where(is.numeric))
View(df_num)


# Moyenne et écart-type, format lignes
stats <- bind_rows(summarise(df_num, across(everything(), ~mean(.x, na.rm = TRUE))) %>% mutate(Statistique = "Moyenne"),
  summarise(df_num, across(everything(), ~sd(.x, na.rm = TRUE))) %>% mutate(Statistique = "Ecart-type"))

# Réorganisation 
stats <- stats %>% select(Statistique, everything())

View(stats)

# Test de corrélation
cor.test(df$Q1_CF, df$selfconfidence, use = "complete.obs")
cor.test(df$Q1_BFNI, df$selfconfidence, use = "complete.obs")
cor.test(df$Q1_CF_nudge, df$selfconfidence, use = "complete.obs")
cor.test(df$Q1_BFNI_nudge, df$selfconfidence, use = "complete.obs")
