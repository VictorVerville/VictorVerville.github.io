# Aller chercher les packages disponibles avec la tidyverse
library(tidyverse)

# Importer la base de données
CSES <- readRDS("~/Desktop/Science Politique/A25/Outils numériques en sciences sociales/VictorVerville.github.io/projet_session/donnees/CSES.rds")

str(CSES$int_pol)
str(CSES$att_elites)

# Régression linéaire simple

modele1 <- lm(att_elites ~ int_pol, data = CSES)
summary(modele1)

# Régression quadratique (non linéraire)

modele_quad <- lm(att_elites ~ int_pol + I(int_pol^2), data = CSES)
summary(modele_quad)

sum(!is.na(CSES$att_elites) & !is.na(CSES$int_pol))

# Supprimer les lignes avec NA pour ne pas avoir de problème
CSES_plot <- CSES[!is.na(CSES$int_pol) & !is.na(CSES$att_elites), ]

# Créer un dataframe avec les valeurs prédites
pred_quad <- data.frame(
  int_pol = seq(min(CSES_plot$int_pol), max(CSES_plot$int_pol), length.out = 100)
)
pred_quad$att_elites <- predict(modele_quad, newdata = pred_quad)

# Créer une suite de valeurs pour int_pol de 1 à 4
pred_quad <- data.frame(int_pol = seq(1, 4, length.out = 100))
pred_quad$att_elites <- predict(modele_quad, newdata = pred_quad)

# Graphique
ggplot(CSES_plot, aes(x = int_pol, y = att_elites)) +
  geom_jitter(width = 0.1, height = 0, alpha = 0.2, color = "grey50") +  # points individuels
  geom_line(data = pred_quad, aes(x = int_pol, y = att_elites), color = "steelblue", size = 1.5) + # quadratique
  geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", size = 1, se = TRUE) + # linéaire
  scale_x_continuous(breaks = 1:4, labels = c("Pas du tout", "Un peu", "Assez", "Très")) +
  scale_y_continuous(limits = c(0,28)) +
  labs(
    title = "Intérêt politique et confiance envers les politiciens",
    x = "Intérêt politique",
    y = "Confiance envers les politiciens",
    caption = "Ligne bleue : régression quadratique\nLigne noire (dashed) : régression linéaire"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )

