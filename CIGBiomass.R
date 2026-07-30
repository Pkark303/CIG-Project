####monthlyfigure
library(dplyr)
library(tidyr)
library(ggplot2)

lb_ac_to_kg_ha <- 1.12085116

trt_levels <- c("Solid seeded","SCCS","SCCS-SB","SCCS-CP","Mono cowpea","Mono soybean")

## Matching color style
cols <- c(
  "Solid seeded" = "#6A4C93",  # muted violet
  "SCCS"         = "#4E79A7",  # soft blue
  "SCCS-SB"      = "#59A14F",  # muted green
  "SCCS-CP"      = "#B6992D",  # soft mustard
  "Mono cowpea"  = "#E17C05",  # warm orange (not bright)
  "Mono soybean" = "#B55D60"   # muted red
)

CIG_long <- CIGBiomass %>%
  rename(Treatment = any_of(c("Treatments", "Treatment"))) %>%
  pivot_longer(cols = c(December, January, February, March),
               names_to = "Month", values_to = "Biomass_lb_ac") %>%
  mutate(
    Biomass_kg_ha = Biomass_lb_ac * lb_ac_to_kg_ha,
    Month = factor(Month, levels = c("December","January","February","March")),
    Treatment = factor(Treatment, levels = trt_levels)
  )

ggplot(CIG_long, aes(Treatment, Biomass_kg_ha, fill = Treatment)) +
  geom_col(width = 0.8) +
  facet_wrap(~ Month, ncol = 2) +
  scale_fill_manual(values = cols, name = "Treatments") +
  labs(x = NULL, y = expression("Forage Production (kg ha"^-1*")")) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1),
    strip.background = element_rect(fill = "grey85", color = "grey60"),
    strip.text = element_text(face = "bold")
  )

library(dplyr)

lb_ac_to_kg_ha <- 1.12085116

CIGBiomass_kg_ha <- CIGBiomass %>%
  rename(Treatment = any_of(c("Treatments", "Treatment"))) %>%
  mutate(across(
    .cols = c(December, January, February, March),
    .fns  = ~ .x * lb_ac_to_kg_ha
  ))


#### totalbiomassfigure
library(dplyr)
library(ggplot2)

# lb/ac -> kg/ha
lb_ac_to_kg_ha <- 1.12085116

df <- CIGBiomass %>%
  rename(Treatment = any_of(c("Treatments", "Treatment")),
         Total_Biomass_lb_ac = any_of(c("Total Biomass", "Total_Biomass", "TotalBiomass"))) %>%
  mutate(
    Total_Biomass_kg_ha = Total_Biomass_lb_ac * lb_ac_to_kg_ha,
    Treatment = factor(Treatment, levels = c(
      "Solid seeded","SCCS","SCCS-SB","SCCS-CP","Mono cowpea","Mono soybean"
    ))
  )

ggplot(df, aes(x = Treatment, y = Total_Biomass_kg_ha)) +
  geom_col(width = 0.8, fill = "grey35") +
  labs(
    x = NULL,
    y = expression("Total Forage Production (kg ha"^-1*")")
  ) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 16, face = "bold"),
    axis.text.x  = element_text(size = 12, angle = 25, hjust = 1, face = "bold"),
    axis.text.y  = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank()
  )

library(dplyr)

lb_ac_to_kg_ha <- 1.12085116

CIGBiomass_kg_ha <- CIGBiomass %>%
  rename(Treatment = any_of(c("Treatments", "Treatment"))) %>%
  mutate(across(
    .cols = c(`Total Biomass`),   # use backticks because of space
    .fns  = ~ .x * lb_ac_to_kg_ha
  ))

