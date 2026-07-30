# ============================================================
# SCCS RAW DATA BOXPLOT FIGURES
#
# Figure 1:
# POXC, Mineralizable C, ACE protein
#
# Figure 2:
# BG and NAG
#
# Columns:
# December and March
#
# Uses raw data directly
# ============================================================

################Boxplot##########################################################
# ============================================================
# 1. INSTALL PACKAGES
# Run only once if needed
# ============================================================

# install.packages(c(
#   "readxl",
#   "dplyr",
#   "tidyr",
#   "ggplot2"
# ))


# ============================================================
# 2. LOAD PACKAGES
# ============================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)


# ============================================================
# 3. IMPORT DATA
# ============================================================

data_file <- paste0(
  "C:/Users/pooon/OneDrive/Documents/",
  "CIG_Project/CIG SCCS Year 2 Soil Data.xlsx"
)

SCCS_data <- read_excel(
  path = data_file,
  sheet = "Sheet1"
)

# Remove extra spaces from column names
names(SCCS_data) <- trimws(names(SCCS_data))


# ============================================================
# 4. CLEAN AND RENAME COLUMNS
# ============================================================

SCCS_data <- SCCS_data %>%
  
  dplyr::rename(
    SCCS = Treament,
    Cover = `Cover crop`,
    Mineralizable_C = `Mineralizable C`,
    ACE_Protein = `ACE Protein`
  ) %>%
  
  dplyr::mutate(
    Time = trimws(as.character(Time)),
    SCCS = trimws(as.character(SCCS)),
    Cover = trimws(as.character(Cover)),
    
    SCCS = dplyr::recode(
      SCCS,
      "152 cm GS+CP" = "152 cm GS + CP",
      "152 cm GS + CP" = "152 cm GS + CP",
      "152 cm GS+FSB" = "152 cm GS + SB",
      "152 cm GS + FSB" = "152 cm GS + SB",
      "152 cm GS+SB" = "152 cm GS + SB",
      "152 cm GS + SB" = "152 cm GS + SB"
    ),
    
    Cover = dplyr::recode(
      Cover,
      "CC" = "CC",
      "No CC" = "No-CC",
      "No-CC" = "No-CC"
    ),
    
    POXC = as.numeric(POXC),
    Mineralizable_C = as.numeric(Mineralizable_C),
    ACE_Protein = as.numeric(ACE_Protein),
    BG = as.numeric(BG),
    NAG = as.numeric(NAG)
  )


# ============================================================
# 5. DEFINE ORDERS
# ============================================================

time_levels <- c("December", "March")

sccs_levels <- c(
  "76 cm GS",
  "152 cm GS",
  "152 cm GS + CP",
  "152 cm GS + SB",
  "Monoculture CP",
  "Monoculture FSB"
)

cover_levels <- c("CC", "No-CC")

SCCS_data <- SCCS_data %>%
  
  dplyr::mutate(
    Time = factor(Time, levels = time_levels),
    SCCS = factor(SCCS, levels = sccs_levels),
    Cover = factor(Cover, levels = cover_levels),
    Block = factor(Block)
  )


# ============================================================
# 6. CHECK OBSERVATIONS
# ============================================================

observation_check <- SCCS_data %>%
  dplyr::count(Time, SCCS, Cover, name = "n_observations")

observation_check
View(observation_check)


# ============================================================
# 7. CONVERT RAW DATA TO LONG FORMAT
# ============================================================

Figure_raw_long <- SCCS_data %>%
  
  dplyr::select(
    Time,
    Block,
    SCCS,
    Cover,
    POXC,
    Mineralizable_C,
    ACE_Protein,
    BG,
    NAG
  ) %>%
  
  tidyr::pivot_longer(
    cols = c(
      POXC,
      Mineralizable_C,
      ACE_Protein,
      BG,
      NAG
    ),
    names_to = "Parameter_code",
    values_to = "Raw_value"
  ) %>%
  
  dplyr::filter(
    !is.na(Raw_value),
    !is.na(Time),
    !is.na(SCCS),
    !is.na(Cover)
  )


# ============================================================
# 8. CREATE PARAMETER LABELS
# These will appear on the left side of each row
# ============================================================

Figure_raw_long <- Figure_raw_long %>%
  
  dplyr::mutate(
    Parameter = dplyr::recode(
      Parameter_code,
      "POXC" = "POXC (mg kg\u207B\u00B9)",
      "Mineralizable_C" = "Mineralizable C (mg kg\u207B\u00B9)",
      "ACE_Protein" = "ACE protein (mg g\u207B\u00B9)",
      "BG" = "BG",
      "NAG" = "NAG"
    )
  )


# ============================================================
# 9. ONE-LINE X-AXIS LABELS
# ============================================================

sccs_axis_labels <- c(
  "76 cm GS" = "76 cm GS",
  "152 cm GS" = "152 cm GS",
  "152 cm GS + CP" = "152 cm GS + CP",
  "152 cm GS + SB" = "152 cm GS + SB",
  "Monoculture CP" = "Monoculture CP",
  "Monoculture FSB" = "Monoculture FSB"
)


# ============================================================
# 10. COLORS
# ============================================================

cover_fill_colors <- c(
  "CC" = "#4C78A8",
  "No-CC" = "#F58518"
)

cover_line_colors <- c(
  "CC" = "#2F5D9B",
  "No-CC" = "#C65F00"
)


# ============================================================
# 11. COMMON THEME
# ============================================================

common_theme <- theme_bw(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    
    strip.text.x = element_text(
      size = 13,
      face = "bold"
    ),
    
    strip.background.x = element_rect(
      fill = "grey92",
      color = "black",
      linewidth = 0.5
    ),
    
    strip.text.y.left = element_text(
      angle = 0,
      size = 11,
      face = "bold",
      margin = margin(r = 8)
    ),
    
    strip.background.y = element_rect(
      fill = "white",
      color = NA
    ),
    
    strip.placement = "outside",
    
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      vjust = 1,
      size = 9
    ),
    
    axis.text.y = element_text(size = 10),
    
    axis.title.x = element_text(
      size = 12,
      face = "bold",
      margin = margin(t = 10)
    ),
    
    axis.title.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.6
    ),
    
    panel.spacing.x = grid::unit(1, "lines"),
    panel.spacing.y = grid::unit(0.75, "lines"),
    
    plot.margin = margin(
      t = 8,
      r = 12,
      b = 8,
      l = 20
    )
  )


# ============================================================
# 12. FIGURE 1 DATA
# POXC, Mineralizable C, ACE protein
# ============================================================

Figure1_data <- Figure_raw_long %>%
  
  dplyr::filter(
    Parameter_code %in% c(
      "POXC",
      "Mineralizable_C",
      "ACE_Protein"
    )
  ) %>%
  
  dplyr::mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "POXC (mg kg\u207B\u00B9)",
        "Mineralizable C (mg kg\u207B\u00B9)",
        "ACE protein (mg g\u207B\u00B9)"
      )
    )
  )


# ============================================================
# 13. FIGURE 1
# ============================================================

Figure_1 <- ggplot(
  Figure1_data,
  aes(
    x = SCCS,
    y = Raw_value,
    fill = Cover,
    color = Cover
  )
) +
  
  geom_boxplot(
    position = position_dodge2(
      width = 0.8,
      preserve = "single"
    ),
    width = 0.65,
    linewidth = 0.5,
    outlier.shape = NA
  ) +
  
  facet_grid(
    rows = vars(Parameter),
    cols = vars(Time),
    scales = "free_y",
    switch = "y"
  ) +
  
  scale_x_discrete(
    labels = sccs_axis_labels
  ) +
  
  scale_fill_manual(
    values = cover_fill_colors,
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  scale_color_manual(
    values = cover_line_colors,
    guide = "none"
  ) +
  
  labs(
    x = "",
    y = NULL
  ) +
  
  common_theme


# Display Figure 1
Figure_1


# ============================================================
# 14. FIGURE 2 DATA
# BG and NAG
# ============================================================

Figure2_data <- Figure_raw_long %>%
  
  dplyr::filter(
    Parameter_code %in% c(
      "BG",
      "NAG"
    )
  ) %>%
  
  dplyr::mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "BG",
        "NAG"
      )
    )
  )


# ============================================================
# 15. FIGURE 2
# ============================================================

# ============================================================
# FIGURE 2
# BG AND NAG
# ============================================================


# ============================================================
# 1. PREPARE FIGURE 2 DATA
# ============================================================

Figure2_data <- Figure_raw_long %>%
  
  dplyr::filter(
    Parameter_code %in% c(
      "BG",
      "NAG"
    )
  ) %>%
  
  dplyr::mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "BG",
        "NAG"
      )
    )
  )


# Check Figure 2 data
Figure2_data

View(Figure2_data)


# ============================================================
# 2. ONE-LINE SCCS X-AXIS LABELS
# ============================================================

sccs_axis_labels <- c(
  "76 cm GS" = "76 cm GS",
  "152 cm GS" = "152 cm GS",
  "152 cm GS + CP" = "152 cm GS + CP",
  "152 cm GS + SB" = "152 cm GS + SB",
  "Monoculture CP" = "Monoculture CP",
  "Monoculture FSB" = "Monoculture FSB"
)


# ============================================================
# 3. COLORS FOR CC AND NO-CC
# ============================================================

cover_fill_colors <- c(
  "CC" = "#4C78A8",
  "No-CC" = "#F58518"
)

cover_line_colors <- c(
  "CC" = "#2F5D9B",
  "No-CC" = "#C65F00"
)


# ============================================================
# 4. CREATE FIGURE 2
# ============================================================

Figure_2 <- ggplot(
  data = Figure2_data,
  
  mapping = aes(
    x = SCCS,
    y = Raw_value,
    fill = Cover,
    color = Cover
  )
) +
  
  # Boxplots using raw data
  geom_boxplot(
    position = position_dodge2(
      width = 0.80,
      preserve = "single"
    ),
    
    width = 0.65,
    linewidth = 0.50,
    
    # Hide outlier points
    outlier.shape = NA
  ) +
  
  # Two rows: BG and NAG
  # Two columns: December and March
  facet_grid(
    rows = vars(Parameter),
    cols = vars(Time),
    scales = "free_y"
  ) +
  
  # One-line SCCS labels
  scale_x_discrete(
    labels = sccs_axis_labels
  ) +
  
  # Box fill colors
  scale_fill_manual(
    values = cover_fill_colors,
    
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  # Box outline colors
  scale_color_manual(
    values = cover_line_colors,
    guide = "none"
  ) +
  
  labs(
    x = "",
    y = NULL
  ) +
  
  theme_bw(
    base_size = 12
  ) +
  
  theme(
    # Legend
    legend.position = "top",
    
    # Remove legend title
    legend.title = element_blank(),
    
    legend.text = element_text(
      size = 12
    ),
    
    # December and March headings
    strip.text.x = element_text(
      size = 14,
      face = "bold"
    ),
    
    strip.background.x = element_rect(
      fill = "grey92",
      color = "black",
      linewidth = 0.50
    ),
    
    # Remove BG and NAG labels from the side
    strip.text.y = element_blank(),
    strip.text.y.left = element_blank(),
    strip.background.y = element_blank(),
    
    # X-axis labels in one line
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(
        t = 12
      )
    ),
    
    axis.title.y = element_blank(),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    
    # Panel borders
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.60
    ),
    
    # Space between panels
    panel.spacing.x = grid::unit(
      1,
      "lines"
    ),
    
    panel.spacing.y = grid::unit(
      0.75,
      "lines"
    ),
    
    plot.margin = margin(
      t = 8,
      r = 12,
      b = 8,
      l = 8
    )
  )


# Display Figure 2
Figure_2


# ============================================================
# 16. SAVE FIGURES
# ============================================================

output_folder <- dirname(data_file)

Figure1_png <- file.path(
  output_folder,
  "SCCS_POXC_MineralizableC_ACEProtein_boxplot.png"
)

Figure2_png <- file.path(
  output_folder,
  "SCCS_BG_NAG_boxplot.png"
)

Figure1_pdf <- file.path(
  output_folder,
  "SCCS_POXC_MineralizableC_ACEProtein_boxplot.pdf"
)

Figure2_pdf <- file.path(
  output_folder,
  "SCCS_BG_NAG_boxplot.pdf"
)

ggsave(
  filename = Figure1_png,
  plot = Figure_1,
  width = 16,
  height = 10,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = Figure2_png,
  plot = Figure_2,
  width = 16,
  height = 7.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = Figure1_pdf,
  plot = Figure_1,
  width = 16,
  height = 10,
  units = "in",
  bg = "white"
)

ggsave(
  filename = Figure2_pdf,
  plot = Figure_2,
  width = 16,
  height = 7.5,
  units = "in",
  bg = "white"
)

cat("\nFigure 1 PNG saved at:\n", Figure1_png, "\n")
cat("\nFigure 2 PNG saved at:\n", Figure2_png, "\n")
cat("\nFigure 1 PDF saved at:\n", Figure1_pdf, "\n")
cat("\nFigure 2 PDF saved at:\n", Figure2_pdf, "\n")

########################################################################

# ============================================================
# BAR GRAPHS USING RAW MEANS ± RAW SE
# ============================================================

library(dplyr)
library(ggplot2)


# ============================================================
# 1. CALCULATE RAW MEANS ± RAW SE
# ============================================================

Figure_bar_summary <- Figure_raw_long %>%
  
  dplyr::group_by(
    Time,
    Parameter,
    Parameter_code,
    SCCS,
    Cover
  ) %>%
  
  dplyr::summarise(
    n = sum(!is.na(Raw_value)),
    
    Raw_mean = mean(
      Raw_value,
      na.rm = TRUE
    ),
    
    Raw_SD = sd(
      Raw_value,
      na.rm = TRUE
    ),
    
    Raw_SE = Raw_SD / sqrt(n),
    
    Lower_SE = Raw_mean - Raw_SE,
    
    Upper_SE = Raw_mean + Raw_SE,
    
    .groups = "drop"
  )


Figure_bar_summary

View(Figure_bar_summary)


# ============================================================
# 2. ONE-LINE SCCS LABELS
# ============================================================

sccs_axis_labels <- c(
  "76 cm GS" = "76 cm GS",
  "152 cm GS" = "152 cm GS",
  "152 cm GS + CP" = "152 cm GS + CP",
  "152 cm GS + SB" = "152 cm GS + SB",
  "Monoculture CP" = "Monoculture CP",
  "Monoculture FSB" = "Monoculture FSB"
)


# ============================================================
# 3. BAR COLORS
# ============================================================

cover_fill_colors <- c(
  "CC" = "#4C78A8",
  "No-CC" = "#F58518"
)


cover_line_colors <- c(
  "CC" = "#2F5D9B",
  "No-CC" = "#C65F00"
)


# Position used for both bars and error bars
bar_position <- position_dodge(
  width = 0.80
)


# ============================================================
# 4. COMMON BAR-GRAPH THEME
# ============================================================

bar_theme <- theme_bw(
  base_size = 12
) +
  
  theme(
    # Legend
    legend.position = "top",
    
    # Remove legend title
    legend.title = element_blank(),
    
    legend.text = element_text(
      size = 12
    ),
    
    # December and March headings
    strip.text.x = element_text(
      size = 14,
      face = "bold"
    ),
    
    strip.background.x = element_rect(
      fill = "grey92",
      color = "black",
      linewidth = 0.50
    ),
    
    # X-axis treatment names
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      vjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(
      size = 11
    ),
    
    axis.title.x = element_text(
      size = 14,
      face = "bold",
      margin = margin(t = 12)
    ),
    
    axis.title.y = element_blank(),
    
    # Grid lines
    panel.grid.minor = element_blank(),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    
    # Panel border
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.60
    ),
    
    # Space between panels
    panel.spacing.x = grid::unit(
      1,
      "lines"
    ),
    
    panel.spacing.y = grid::unit(
      0.75,
      "lines"
    ),
    
    plot.margin = margin(
      t = 8,
      r = 12,
      b = 8,
      l = 8
    )
  )
# ============================================================
# 5. FIGURE 1 DATA
# ============================================================

Figure1_bar_data <- Figure_bar_summary %>%
  
  dplyr::filter(
    Parameter_code %in% c(
      "POXC",
      "Mineralizable_C",
      "ACE_Protein"
    )
  ) %>%
  
  dplyr::mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "POXC (mg kg\u207B\u00B9)",
        "Mineralizable C (mg kg\u207B\u00B9)",
        "ACE protein (mg g\u207B\u00B9)"
      )
    )
  )


# ============================================================
# 6. CREATE FIGURE 1 BAR GRAPH
# ============================================================

Figure_1_bar <- ggplot(
  data = Figure1_bar_data,
  
  mapping = aes(
    x = SCCS,
    y = Raw_mean,
    fill = Cover,
    color = Cover
  )
) +
  
  # Side-by-side bars
  geom_col(
    position = bar_position,
    width = 0.72,
    linewidth = 0.45
  ) +
  
  # Raw mean ± raw SE
  geom_errorbar(
    aes(
      ymin = Lower_SE,
      ymax = Upper_SE
    ),
    
    position = bar_position,
    width = 0.18,
    linewidth = 0.60
  ) +
  
  facet_grid(
    rows = vars(Parameter),
    cols = vars(Time),
    scales = "free_y",
    switch = "y"
  ) +
  
  scale_x_discrete(
    labels = sccs_axis_labels
  ) +
  
  scale_fill_manual(
    values = cover_fill_colors,
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  scale_color_manual(
    values = cover_line_colors,
    guide = "none"
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  
  labs(
    x = "",
    y = NULL
  ) +
  
  bar_theme +
  
  theme(
    # Show parameter labels for Figure 1
    strip.text.y.left = element_text(
      angle = 0,
      size = 11,
      face = "bold",
      margin = margin(r = 8)
    ),
    
    strip.background.y = element_blank(),
    
    strip.placement = "outside"
  )


Figure_1_bar

# ============================================================
# 7. FIGURE 2 DATA
# ============================================================

Figure2_bar_data <- Figure_bar_summary %>%
  
  dplyr::filter(
    Parameter_code %in% c(
      "BG",
      "NAG"
    )
  ) %>%
  
  dplyr::mutate(
    Parameter = factor(
      Parameter,
      levels = c(
        "BG",
        "NAG"
      )
    )
  )


# ============================================================
# 8. CREATE FIGURE 2 BAR GRAPH
# ============================================================

Figure_2_bar <- ggplot(
  data = Figure2_bar_data,
  
  mapping = aes(
    x = SCCS,
    y = Raw_mean,
    fill = Cover,
    color = Cover
  )
) +
  
  # Side-by-side bars
  geom_col(
    position = bar_position,
    width = 0.72,
    linewidth = 0.45
  ) +
  
  # Raw mean ± raw SE
  geom_errorbar(
    aes(
      ymin = Lower_SE,
      ymax = Upper_SE
    ),
    
    position = bar_position,
    width = 0.18,
    linewidth = 0.60
  ) +
  
  facet_grid(
    rows = vars(Parameter),
    cols = vars(Time),
    scales = "free_y"
  ) +
  
  scale_x_discrete(
    labels = sccs_axis_labels
  ) +
  
  scale_fill_manual(
    values = cover_fill_colors,
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  scale_color_manual(
    values = cover_line_colors,
    guide = "none"
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0,
        0.12
      )
    )
  ) +
  
  labs(
    x = "",
    y = NULL
  ) +
  
  bar_theme +
  
  theme(
    # Remove BG and NAG labels from the side
    strip.text.y = element_blank(),
    
    strip.text.y.left = element_blank(),
    
    strip.background.y = element_blank()
  )


Figure_2_bar
###############
#Soil Protein
# ============================================================
# DECEMBER FIGURES
# 1. ACE protein: point graph with error bars
# 2. BG: box plot
# 3. BG: bar graph
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(dplyr)
library(ggplot2)
library(readxl)


# ============================================================
# 2. IMPORT AND CLEAN DATA
# Skip this section if SCCS_data is already cleaned in R
# ============================================================

data_file <- paste0(
  "C:/Users/pooon/OneDrive/Documents/",
  "CIG_Project/CIG SCCS Year 2 Soil Data.xlsx"
)

SCCS_data <- read_excel(
  path = data_file,
  sheet = "Sheet1"
)

names(SCCS_data) <- trimws(names(SCCS_data))

SCCS_data <- SCCS_data %>%
  
  dplyr::rename(
    SCCS = Treament,
    Cover = `Cover crop`,
    Mineralizable_C = `Mineralizable C`,
    ACE_Protein = `ACE Protein`
  ) %>%
  
  dplyr::mutate(
    Time = trimws(as.character(Time)),
    SCCS = trimws(as.character(SCCS)),
    Cover = trimws(as.character(Cover)),
    
    SCCS = dplyr::recode(
      SCCS,
      "152 cm GS+CP" = "152 cm GS + CP",
      "152 cm GS + CP" = "152 cm GS + CP",
      "152 cm GS+FSB" = "152 cm GS + SB",
      "152 cm GS + FSB" = "152 cm GS + SB",
      "152 cm GS+SB" = "152 cm GS + SB",
      "152 cm GS + SB" = "152 cm GS + SB"
    ),
    
    Cover = dplyr::recode(
      Cover,
      "CC" = "CC",
      "No CC" = "No-CC",
      "No-CC" = "No-CC"
    ),
    
    POXC = as.numeric(POXC),
    Mineralizable_C = as.numeric(Mineralizable_C),
    ACE_Protein = as.numeric(ACE_Protein),
    BG = as.numeric(BG),
    NAG = as.numeric(NAG)
  )


# ============================================================
# 3. SET FACTOR ORDER
# ============================================================

sccs_levels <- c(
  "76 cm GS",
  "152 cm GS",
  "152 cm GS + CP",
  "152 cm GS + SB",
  "Monoculture CP",
  "Monoculture FSB"
)

cover_levels <- c("CC", "No-CC")

SCCS_data <- SCCS_data %>%
  
  dplyr::mutate(
    SCCS = factor(SCCS, levels = sccs_levels),
    Cover = factor(Cover, levels = cover_levels),
    Block = factor(Block)
  )


# ============================================================
# 4. FILTER DECEMBER DATA
# ============================================================

December_data <- SCCS_data %>%
  dplyr::filter(Time == "December")


# ============================================================
# 5. COMMON COLORS
# ============================================================

cover_colors <- c(
  "CC" = "#4C78A8",
  "No-CC" = "#F58518"
)

cover_fill_colors <- c(
  "CC" = "#4C78A8",
  "No-CC" = "#F58518"
)

cover_line_colors <- c(
  "CC" = "#2F5D9B",
  "No-CC" = "#C65F00"
)


# ============================================================
# 6. ACE PROTEIN SUMMARY
# Since interaction is significant, keep CC and No-CC separate
# Raw mean ± SE for each SCCS × Cover combination
# ============================================================

December_ACE_summary <- December_data %>%
  
  dplyr::group_by(SCCS, Cover) %>%
  
  dplyr::summarise(
    n = sum(!is.na(ACE_Protein)),
    Mean = mean(ACE_Protein, na.rm = TRUE),
    SD = sd(ACE_Protein, na.rm = TRUE),
    SE = SD / sqrt(n),
    Lower_SE = Mean - SE,
    Upper_SE = Mean + SE,
    .groups = "drop"
  )


December_ACE_summary
View(December_ACE_summary)


# ============================================================
# 7. ACE PROTEIN POINT GRAPH WITH ERROR BARS
# ============================================================

ace_dodge <- position_dodge(width = 0.20)

ACE_point_figure_December <- ggplot(
  December_ACE_summary,
  aes(
    x = SCCS,
    y = Mean,
    group = Cover,
    color = Cover,
    shape = Cover,
    linetype = Cover
  )
) +
  
  geom_line(
    linewidth = 0.75,
    position = ace_dodge
  ) +
  
  geom_errorbar(
    aes(
      ymin = Lower_SE,
      ymax = Upper_SE
    ),
    width = 0.10,
    linewidth = 0.60,
    position = ace_dodge
  ) +
  
  geom_point(
    size = 3.2,
    position = ace_dodge
  ) +
  
  scale_color_manual(
    values = cover_colors,
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  scale_shape_manual(
    values = c(
      "CC" = 16,
      "No-CC" = 17
    ),
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  scale_linetype_manual(
    values = c(
      "CC" = "solid",
      "No-CC" = "dashed"
    ),
    labels = c(
      "CC" = "CC",
      "No-CC" = "No-CC"
    )
  ) +
  
  labs(
    x = "",
    y = "ACE protein (mg g⁻¹)"
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(size = 11),
    
    axis.title.x = element_text(
      size = 13,
      face = "bold",
      margin = margin(t = 10)
    ),
    
    axis.title.y = element_text(
      size = 13,
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.60
    )
  )


ACE_point_figure_December

# ============================================================
# 8. BG BLOCK × SCCS MEANS
# Average CC and No-CC within each block first
# ============================================================

December_BG_block_means <- December_data %>%
  
  dplyr::group_by(Block, SCCS) %>%
  
  dplyr::summarise(
    BG_block_mean = mean(BG, na.rm = TRUE),
    .groups = "drop"
  )


December_BG_block_means
View(December_BG_block_means)

# ============================================================
# 8. BG BLOCK × SCCS MEANS
# Average CC and No-CC within each block first
# ============================================================

December_BG_block_means <- December_data %>%
  
  dplyr::group_by(Block, SCCS) %>%
  
  dplyr::summarise(
    BG_block_mean = mean(BG, na.rm = TRUE),
    .groups = "drop"
  )


December_BG_block_means
View(December_BG_block_means)


# ============================================================
# 9. BG BAR SUMMARY
# Mean ± SE across the four blocks
# ============================================================

December_BG_bar_summary <- December_BG_block_means %>%
  
  dplyr::group_by(SCCS) %>%
  
  dplyr::summarise(
    n = sum(!is.na(BG_block_mean)),
    Mean = mean(BG_block_mean, na.rm = TRUE),
    SD = sd(BG_block_mean, na.rm = TRUE),
    SE = SD / sqrt(n),
    Lower_SE = Mean - SE,
    Upper_SE = Mean + SE,
    .groups = "drop"
  )


December_BG_bar_summary
View(December_BG_bar_summary)

# ============================================================
# 10. BG BOX PLOT
# Uses Block × SCCS pooled values
# ============================================================

BG_boxplot_December <- ggplot(
  December_BG_block_means,
  aes(
    x = SCCS,
    y = BG_block_mean
  )
) +
  
  geom_boxplot(
    fill = "#FDE68A",
    color = "#D4A017",
    width = 0.65,
    linewidth = 0.55,
    outlier.shape = NA
  ) +
  
  labs(
    x = "",
    y = ""
  ) +
  
  theme_bw(base_size = 12) +
  
  theme(
    axis.text.x = element_text(
      angle = 35,
      hjust = 1,
      size = 10
    ),
    
    axis.text.y = element_text(size = 11),
    
    axis.title.x = element_text(
      size = 13,
      face = "bold",
      margin = margin(t = 10)
    ),
    
    axis.title.y = element_text(
      size = 13,
      face = "bold"
    ),
    
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      linewidth = 0.25,
      color = "grey85"
    ),
    
    panel.border = element_rect(
      color = "black",
      fill = NA,
      linewidth = 0.60
    )
  )


BG_boxplot_December
