# ============================================================
# INSTALL PACKAGES
# Run only once
# ============================================================

# install.packages(c(
#   "readxl",
#   "nlme",
#   "emmeans",
#   "multcomp",
#   "multcompView",
#   "ggResidpanel",
#   "dplyr",
#   "MASS"
# ))


# ============================================================
# LOAD PACKAGES
# ============================================================

library(readxl)
library(nlme)
library(emmeans)
library(multcomp)
library(multcompView)
library(ggResidpanel)
library(MASS)
library(dplyr)


# ============================================================
# IMPORT DATA
# ============================================================

data_file <- paste0(
  "C:/Users/pooon/OneDrive/Documents/",
  "CIG_Project/CIG SCCS Year 2 Soil Data.xlsx"
)

SCCS_data <- read_excel(
  data_file,
  sheet = "Sheet1"
)

# Remove extra spaces from column names
names(SCCS_data) <- trimws(names(SCCS_data))


# ============================================================
# CLEAN AND ORGANIZE DATA
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
    
    # Standardize SCCS names
    SCCS = dplyr::recode(
      SCCS,
      "152 cm GS+CP" = "152 cm GS + CP",
      "152 cm GS + CP" = "152 cm GS + CP",
      "152 cm GS+FSB" = "152 cm GS + SB",
      "152 cm GS + FSB" = "152 cm GS + SB",
      "152 cm GS+SB" = "152 cm GS + SB"
    ),
    
    # Make response variables numeric
    POXC = as.numeric(POXC),
    Mineralizable_C = as.numeric(Mineralizable_C),
    ACE_Protein = as.numeric(ACE_Protein),
    BG = as.numeric(BG),
    NAG = as.numeric(NAG)
  )


# Convert treatment variables to factors
SCCS_data$Time <- as.factor(SCCS_data$Time)
SCCS_data$SCCS <- as.factor(SCCS_data$SCCS)

SCCS_data$Cover <- as.factor(SCCS_data$Cover)
SCCS_data$Block <- as.factor(SCCS_data$Block)


# Set SCCS order
SCCS_data$SCCS <- factor(
  SCCS_data$SCCS,
  levels = c(
    "76 cm GS",
    "152 cm GS",
    "152 cm GS + CP",
    "152 cm GS + SB",
    "Monoculture CP",
    "Monoculture FSB"
  )
)


# Set cover-crop order
SCCS_data$Cover <- factor(
  SCCS_data$Cover,
  levels = c(
    "CC",
    "No-CC"
  )
)


# Contrasts for marginal/Type III-style tests
options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)


# View cleaned data
str(SCCS_data)



# ============================================================
# ============================================================
# DECEMBER ANALYSIS
# ============================================================
# ============================================================


# ============================================================
# 1. SELECT DECEMBER DATA
# ============================================================

December_data <- SCCS_data %>%
  dplyr::filter(Time == "December") %>%
  droplevels()


# ============================================================
# 2. SELECT ONE RESPONSE VARIABLE
# ============================================================

#December_data$response <- December_data$POXC

# Other choices:
#December_data$response <- December_data$Mineralizable_C
#December_data$response <- December_data$ACE_Protein
#December_data$response <- December_data$BG
December_data$response <- December_data$NAG


# ============================================================
# 3. DECEMBER RAW SCCS × COVER MEANS ± SE
# ============================================================

December_raw_summary <- December_data %>%
  
  dplyr::group_by(
    SCCS,
    Cover
  ) %>%
  
  dplyr::summarise(
    n = sum(!is.na(response)),
    Mean = mean(response, na.rm = TRUE),
    SD = sd(response, na.rm = TRUE),
    SE = SD / sqrt(n),
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    Mean_SE = paste0(
      round(Mean, 2),
      " ± ",
      round(SE, 2)
    )
  )


December_raw_summary
View(December_raw_summary)


# ============================================================
# 4. DECEMBER RAW SCCS MEANS ± SE
# Average CC and No-CC within each Block × SCCS first
# Then calculate mean ± SE across the four blocks
# ============================================================

# Step 1: Average CC and No-CC within each Block × SCCS
December_SCCS_block_means <- December_data %>%
  
  dplyr::group_by(
    Block,
    SCCS
  ) %>%
  
  dplyr::summarise(
    Block_mean = mean(
      response,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Step 2: Calculate SCCS mean ± SE across blocks
December_SCCS_summary <- December_SCCS_block_means %>%
  
  dplyr::group_by(SCCS) %>%
  
  dplyr::summarise(
    n = sum(!is.na(Block_mean)),
    
    Mean = mean(
      Block_mean,
      na.rm = TRUE
    ),
    
    SD = sd(
      Block_mean,
      na.rm = TRUE
    ),
    
    SE = SD / sqrt(n),
    
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    Mean_SE = paste0(
      round(Mean, 2),
      " ± ",
      round(SE, 2)
    )
  )


December_SCCS_summary

View(December_SCCS_summary)


# ============================================================
# 5. DECEMBER RAW COVER MEANS ± SE
# Averaged across SCCS treatments
# ============================================================

December_Cover_summary <- December_data %>%
  
  dplyr::group_by(Cover) %>%
  
  dplyr::summarise(
    n = sum(!is.na(response)),
    Mean = mean(response, na.rm = TRUE),
    SD = sd(response, na.rm = TRUE),
    SE = SD / sqrt(n),
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    Mean_SE = paste0(
      round(Mean, 2),
      " ± ",
      round(SE, 2)
    )
  )


December_Cover_summary
View(December_Cover_summary)


# ============================================================
# 6. FIT ORIGINAL DECEMBER SPLIT-PLOT MODEL
#
# Block/SCCS includes:
#   Block
#   SCCS main plots within Block
# ============================================================

model_December_original <- lme(
  response ~ SCCS * Cover,
  random = ~1 | Block/SCCS,
  data = December_data,
  na.action = na.omit,
  method = "REML"
)


summary(model_December_original)


# ============================================================
# 7. CHECK ORIGINAL DECEMBER RESIDUAL NORMALITY
# ============================================================

December_Shapiro_original <- shapiro.test(
  residuals(
    model_December_original,
    type = "pearson"
  )
)

December_Shapiro_original


resid_panel(
  model_December_original,
  plots = c(
    "qq",
    "hist",
    "resid"
  ),
  type = "pearson",
  smoother = TRUE,
  qqbands = TRUE
)


# ============================================================
# 8. CHOOSE WHETHER TO USE BOX-COX FOR DECEMBER
#
# Keep FALSE when residuals are acceptable.
# Change to TRUE when Box-Cox transformation is needed.
# ============================================================

use_boxcox_December <- FALSE


# Start with original model as the final model
final_model_December <- model_December_original

December_transformation <- "No transformation"


# ============================================================
# 9. OPTIONAL DECEMBER BOX-COX TRANSFORMATION
# Runs only when use_boxcox_December is TRUE
# ============================================================

if (use_boxcox_December == TRUE) {
  
  # Box-Cox requires positive values
  if (any(
    December_data$response <= 0,
    na.rm = TRUE
  )) {
    
    stop(
      paste(
        "December response contains zero or negative values.",
        "A standard Box-Cox transformation cannot be used."
      )
    )
  }
  
  
  # Temporary fixed-effects model for estimating lambda
  # Block and Block:SCCS represent the experimental structure
  boxcox_lm_December <- lm(
    response ~
      SCCS * Cover +
      Block +
      Block:SCCS,
    data = December_data,
    na.action = na.omit
  )
  
  
  # Estimate and display Box-Cox lambda
  boxcox_profile_December <- MASS::boxcox(
    boxcox_lm_December,
    lambda = seq(
      -2,
      2,
      by = 0.05
    ),
    plotit = TRUE
  )
  
  
  lambda_December <-
    boxcox_profile_December$x[
      which.max(
        boxcox_profile_December$y
      )
    ]
  
  
  cat(
    "\nDecember Box-Cox lambda:",
    lambda_December,
    "\n"
  )
  
  
  # Apply transformation
  if (abs(lambda_December) < 0.05) {
    
    December_data$response_BoxCox <-
      log(December_data$response)
    
    December_transformation <-
      "Natural-log transformation because lambda was near zero"
    
  } else {
    
    December_data$response_BoxCox <-
      (
        December_data$response^lambda_December - 1
      ) / lambda_December
    
    December_transformation <- paste0(
      "Box-Cox transformation with lambda = ",
      round(lambda_December, 2)
    )
  }
  
  
  # Fit transformed split-plot model
  model_December_BoxCox <- lme(
    response_BoxCox ~ SCCS * Cover,
    random = ~1 | Block/SCCS,
    data = December_data,
    na.action = na.omit,
    method = "REML"
  )
  
  
  summary(model_December_BoxCox)
  
  
  # Check transformed residual normality
  December_Shapiro_BoxCox <- shapiro.test(
    residuals(
      model_December_BoxCox,
      type = "pearson"
    )
  )
  
  December_Shapiro_BoxCox
  
  
  resid_panel(
    model_December_BoxCox,
    plots = c(
      "qq",
      "hist",
      "resid"
    ),
    type = "pearson",
    smoother = TRUE,
    qqbands = TRUE
  )
  
  
  # Use transformed model for ANOVA and post-hoc tests
  final_model_December <- model_December_BoxCox
}


cat(
  "\nDecember final analysis:",
  December_transformation,
  "\n"
)


# ============================================================
# 10. FINAL DECEMBER ANOVA
# Uses original or Box-Cox model according to the selection
# ============================================================

December_ANOVA <- anova(
  final_model_December,
  type = "marginal"
)


December_ANOVA

View(
  as.data.frame(
    December_ANOVA
  )
)


# ============================================================
# DECEMBER MANUAL POST-HOC TESTS
#
# Keep all sections FALSE initially.
#
# After checking December_ANOVA, change only the appropriate
# post-hoc section from FALSE to TRUE and run that section.
# ============================================================


# ============================================================
# DECEMBER OPTION 1:
# SCCS SIGNIFICANT
# INTERACTION NOT SIGNIFICANT
# ============================================================

if (TRUE) {
  
  em_sccs_December <- emmeans(
    final_model_December,
    ~ SCCS
  )
  
  
  # Estimated marginal means
  em_sccs_December
  
  
  # Tukey pairwise comparisons
  December_SCCS_pairs <- pairs(
    em_sccs_December,
    adjust = "tukey"
  )
  
  December_SCCS_pairs
  
  
  # Tukey grouping letters
  cld_sccs_December <- multcomp::cld(
    em_sccs_December,
    Letters = letters,
    adjust = "tukey",
    sort = FALSE
  )
  
  cld_sccs_December
  
  
  # Extract letters
  December_SCCS_letters <- as.data.frame(
    cld_sccs_December
  ) %>%
    
    dplyr::transmute(
      SCCS,
      Letter = trimws(.group)
    )
  
  
  # Combine raw mean ± SE with Tukey letters
  December_SCCS_final_table <-
    December_SCCS_summary %>%
    
    dplyr::left_join(
      December_SCCS_letters,
      by = "SCCS"
    ) %>%
    
    dplyr::mutate(
      Mean_SE_Letter = paste(
        Mean_SE,
        Letter
      )
    )
  
  
  December_SCCS_final_table
  
  View(
    December_SCCS_final_table
  )
}


# ============================================================
# DECEMBER OPTION 2:
# COVER SIGNIFICANT
# INTERACTION NOT SIGNIFICANT
# ============================================================

if (FALSE) {
  
  em_cover_December <- emmeans(
    final_model_December,
    ~ Cover
  )
  
  
  # Estimated marginal means
  em_cover_December
  
  
  # CC versus No-CC comparison
  December_Cover_pairs <- pairs(
    em_cover_December,
    adjust = "none"
  )
  
  December_Cover_pairs
  
  
  # Grouping letters
  cld_cover_December <- multcomp::cld(
    em_cover_December,
    Letters = letters,
    adjust = "none",
    sort = FALSE
  )
  
  cld_cover_December
  
  
  # Extract letters
  December_Cover_letters <- as.data.frame(
    cld_cover_December
  ) %>%
    
    dplyr::transmute(
      Cover,
      Letter = trimws(.group)
    )
  
  
  # Combine raw mean ± SE with letters
  December_Cover_final_table <-
    December_Cover_summary %>%
    
    dplyr::left_join(
      December_Cover_letters,
      by = "Cover"
    ) %>%
    
    dplyr::mutate(
      Mean_SE_Letter = paste(
        Mean_SE,
        Letter
      )
    )
  
  
  December_Cover_final_table
  
  View(
    December_Cover_final_table
  )
}


# ============================================================
# DECEMBER OPTION 3:
# SCCS × COVER INTERACTION SIGNIFICANT
# ============================================================

if (TRUE) {
  
  # ----------------------------------------------------------
  # Compare SCCS treatments within each Cover level
  # ----------------------------------------------------------
  
  em_sccs_within_cover_December <- emmeans(
    final_model_December,
    ~ SCCS | Cover
  )
  
  
  # Estimated marginal means
  em_sccs_within_cover_December
  
  
  # Tukey comparisons among SCCS treatments within each Cover
  December_SCCS_within_Cover_pairs <- pairs(
    em_sccs_within_cover_December,
    adjust = "tukey"
  )
  
  December_SCCS_within_Cover_pairs
  
  
  # Tukey letters within each Cover level
  cld_sccs_within_cover_December <- multcomp::cld(
    em_sccs_within_cover_December,
    Letters = letters,
    adjust = "tukey",
    sort = FALSE
  )
  
  cld_sccs_within_cover_December
  
  
  # Extract letters
  December_interaction_letters <- as.data.frame(
    cld_sccs_within_cover_December
  ) %>%
    
    dplyr::transmute(
      SCCS,
      Cover,
      Letter = trimws(.group)
    )
  
  
  # Combine 12 raw means ± SE with Tukey letters
  December_interaction_final_table <-
    December_raw_summary %>%
    
    dplyr::left_join(
      December_interaction_letters,
      by = c(
        "SCCS",
        "Cover"
      )
    ) %>%
    
    dplyr::mutate(
      Mean_SE_Letter = paste(
        Mean_SE,
        Letter
      )
    )
  
  
  December_interaction_final_table
  
  View(
    December_interaction_final_table
  )
  
  
  # ----------------------------------------------------------
  # Also compare CC versus No-CC within each SCCS treatment
  # ----------------------------------------------------------
  
  em_cover_within_sccs_December <- emmeans(
    final_model_December,
    ~ Cover | SCCS
  )
  
  
  December_Cover_within_SCCS_pairs <- pairs(
    em_cover_within_sccs_December
  )
  
  
  # Apply Holm correction across the six comparisons
  December_Cover_within_SCCS_Holm <- summary(
    December_Cover_within_SCCS_pairs,
    by = NULL,
    adjust = "holm"
  )
  
  
  December_Cover_within_SCCS_Holm
  
  View(
    as.data.frame(
      December_Cover_within_SCCS_Holm
    )
  )
}



# ============================================================
# ============================================================
# MARCH ANALYSIS
# ============================================================
# ============================================================


# ============================================================
# 1. SELECT MARCH DATA
# ============================================================

March_data <- SCCS_data %>%
  dplyr::filter(Time == "March") %>%
  droplevels()


# ============================================================
# 2. SELECT ONE RESPONSE VARIABLE
# Use the same response selected for December
# ============================================================

#March_data$response <- March_data$POXC

# Other choices:
#March_data$response <- March_data$Mineralizable_C
#March_data$response <- March_data$ACE_Protein
#March_data$response <- March_data$BG
 March_data$response <- March_data$NAG


# ============================================================
# 3. MARCH RAW SCCS × COVER MEANS ± SE
# ============================================================

March_raw_summary <- March_data %>%
  
  dplyr::group_by(
    SCCS,
    Cover
  ) %>%
  
  dplyr::summarise(
    n = sum(!is.na(response)),
    Mean = mean(response, na.rm = TRUE),
    SD = sd(response, na.rm = TRUE),
    SE = SD / sqrt(n),
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    Mean_SE = paste0(
      round(Mean, 2),
      " ± ",
      round(SE, 2)
    )
  )


March_raw_summary
View(March_raw_summary)


# ============================================================
# MARCH RAW SCCS MEANS ± SE
# ============================================================

March_SCCS_block_means <- March_data %>%
  
  dplyr::group_by(
    Block,
    SCCS
  ) %>%
  
  dplyr::summarise(
    Block_mean = mean(
      response,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


March_SCCS_summary <- March_SCCS_block_means %>%
  
  dplyr::group_by(SCCS) %>%
  
  dplyr::summarise(
    n = sum(!is.na(Block_mean)),
    
    Mean = mean(
      Block_mean,
      na.rm = TRUE
    ),
    
    SD = sd(
      Block_mean,
      na.rm = TRUE
    ),
    
    SE = SD / sqrt(n),
    
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    Mean_SE = paste0(
      round(Mean, 2),
      " ± ",
      round(SE, 2)
    )
  )


March_SCCS_summary

View(March_SCCS_summary)


# ============================================================
# 5. MARCH RAW COVER MEANS ± SE
# Averaged across SCCS treatments
# ============================================================

March_Cover_summary <- March_data %>%
  
  dplyr::group_by(Cover) %>%
  
  dplyr::summarise(
    n = sum(!is.na(response)),
    Mean = mean(response, na.rm = TRUE),
    SD = sd(response, na.rm = TRUE),
    SE = SD / sqrt(n),
    .groups = "drop"
  ) %>%
  
  dplyr::mutate(
    Mean_SE = paste0(
      round(Mean, 2),
      " ± ",
      round(SE, 2)
    )
  )


March_Cover_summary
View(March_Cover_summary)


# ============================================================
# 6. FIT ORIGINAL MARCH SPLIT-PLOT MODEL
# ============================================================

model_March_original <- lme(
  response ~ SCCS * Cover,
  random = ~1 | Block/SCCS,
  data = March_data,
  na.action = na.omit,
  method = "REML"
)


summary(model_March_original)


# ============================================================
# 7. CHECK ORIGINAL MARCH RESIDUAL NORMALITY
# ============================================================

March_Shapiro_original <- shapiro.test(
  residuals(
    model_March_original,
    type = "pearson"
  )
)

March_Shapiro_original


resid_panel(
  model_March_original,
  plots = c(
    "qq",
    "hist",
    "resid"
  ),
  type = "pearson",
  smoother = TRUE,
  qqbands = TRUE
)


# ============================================================
# 8. CHOOSE WHETHER TO USE BOX-COX FOR MARCH
#
# Keep FALSE when residuals are acceptable.
# Change to TRUE when Box-Cox transformation is needed.
# ============================================================

use_boxcox_March <- FALSE


# Start with original model as final model
final_model_March <- model_March_original

March_transformation <- "No transformation"


# ============================================================
# 9. OPTIONAL MARCH BOX-COX TRANSFORMATION
# Runs only when use_boxcox_March is TRUE
# ============================================================

if (use_boxcox_March == TRUE) {
  
  # Box-Cox requires positive values
  if (any(
    March_data$response <= 0,
    na.rm = TRUE
  )) {
    
    stop(
      paste(
        "March response contains zero or negative values.",
        "A standard Box-Cox transformation cannot be used."
      )
    )
  }
  
  
  # Temporary model for estimating lambda
  boxcox_lm_March <- lm(
    response ~
      SCCS * Cover +
      Block +
      Block:SCCS,
    data = March_data,
    na.action = na.omit
  )
  
  
  # Estimate and display Box-Cox lambda
  boxcox_profile_March <- MASS::boxcox(
    boxcox_lm_March,
    lambda = seq(
      -2,
      2,
      by = 0.05
    ),
    plotit = TRUE
  )
  
  
  lambda_March <-
    boxcox_profile_March$x[
      which.max(
        boxcox_profile_March$y
      )
    ]
  
  
  cat(
    "\nMarch Box-Cox lambda:",
    lambda_March,
    "\n"
  )
  
  
  # Apply transformation
  if (abs(lambda_March) < 0.05) {
    
    March_data$response_BoxCox <-
      log(March_data$response)
    
    March_transformation <-
      "Natural-log transformation because lambda was near zero"
    
  } else {
    
    March_data$response_BoxCox <-
      (
        March_data$response^lambda_March - 1
      ) / lambda_March
    
    March_transformation <- paste0(
      "Box-Cox transformation with lambda = ",
      round(lambda_March, 2)
    )
  }
  
  
  # Fit transformed split-plot model
  model_March_BoxCox <- lme(
    response_BoxCox ~ SCCS * Cover,
    random = ~1 | Block/SCCS,
    data = March_data,
    na.action = na.omit,
    method = "REML"
  )
  
  
  summary(model_March_BoxCox)
  
  
  # Check transformed residual normality
  March_Shapiro_BoxCox <- shapiro.test(
    residuals(
      model_March_BoxCox,
      type = "pearson"
    )
  )
  
  March_Shapiro_BoxCox
  
  
  resid_panel(
    model_March_BoxCox,
    plots = c(
      "qq",
      "hist",
      "resid"
    ),
    type = "pearson",
    smoother = TRUE,
    qqbands = TRUE
  )
  
  
  # Use transformed model for ANOVA and post-hoc tests
  final_model_March <- model_March_BoxCox
}


cat(
  "\nMarch final analysis:",
  March_transformation,
  "\n"
)


# ============================================================
# 10. FINAL MARCH ANOVA
# ============================================================

March_ANOVA <- anova(
  final_model_March,
  type = "marginal"
)


March_ANOVA

View(
  as.data.frame(
    March_ANOVA
  )
)


# ============================================================
# MARCH MANUAL POST-HOC TESTS
#
# Keep all sections FALSE initially.
#
# Change only the appropriate post-hoc section to TRUE after
# reviewing March_ANOVA.
# ============================================================


# ============================================================
# MARCH OPTION 1:
# SCCS SIGNIFICANT
# INTERACTION NOT SIGNIFICANT
# ============================================================

if (FALSE) {
  
  em_sccs_March <- emmeans(
    final_model_March,
    ~ SCCS
  )
  
  
  em_sccs_March
  
  
  March_SCCS_pairs <- pairs(
    em_sccs_March,
    adjust = "tukey"
  )
  
  March_SCCS_pairs
  
  
  cld_sccs_March <- multcomp::cld(
    em_sccs_March,
    Letters = letters,
    adjust = "tukey",
    sort = FALSE
  )
  
  cld_sccs_March
  
  
  March_SCCS_letters <- as.data.frame(
    cld_sccs_March
  ) %>%
    
    dplyr::transmute(
      SCCS,
      Letter = trimws(.group)
    )
  
  
  March_SCCS_final_table <-
    March_SCCS_summary %>%
    
    dplyr::left_join(
      March_SCCS_letters,
      by = "SCCS"
    ) %>%
    
    dplyr::mutate(
      Mean_SE_Letter = paste(
        Mean_SE,
        Letter
      )
    )
  
  
  March_SCCS_final_table
  
  View(
    March_SCCS_final_table
  )
}


# ============================================================
# MARCH OPTION 2:
# COVER SIGNIFICANT
# INTERACTION NOT SIGNIFICANT
# ============================================================

if (FALSE) {
  
  em_cover_March <- emmeans(
    final_model_March,
    ~ Cover
  )
  
  
  em_cover_March
  
  
  March_Cover_pairs <- pairs(
    em_cover_March,
    adjust = "none"
  )
  
  March_Cover_pairs
  
  
  cld_cover_March <- multcomp::cld(
    em_cover_March,
    Letters = letters,
    adjust = "none",
    sort = FALSE
  )
  
  cld_cover_March
  
  
  March_Cover_letters <- as.data.frame(
    cld_cover_March
  ) %>%
    
    dplyr::transmute(
      Cover,
      Letter = trimws(.group)
    )
  
  
  March_Cover_final_table <-
    March_Cover_summary %>%
    
    dplyr::left_join(
      March_Cover_letters,
      by = "Cover"
    ) %>%
    
    dplyr::mutate(
      Mean_SE_Letter = paste(
        Mean_SE,
        Letter
      )
    )
  
  
  March_Cover_final_table
  
  View(
    March_Cover_final_table
  )
}


# ============================================================
# MARCH OPTION 3:
# SCCS × COVER INTERACTION SIGNIFICANT
# ============================================================

if (FALSE) {
  
  # ----------------------------------------------------------
  # Compare SCCS treatments within each Cover level
  # ----------------------------------------------------------
  
  em_sccs_within_cover_March <- emmeans(
    final_model_March,
    ~ SCCS | Cover
  )
  
  
  em_sccs_within_cover_March
  
  
  March_SCCS_within_Cover_pairs <- pairs(
    em_sccs_within_cover_March,
    adjust = "tukey"
  )
  
  March_SCCS_within_Cover_pairs
  
  
  cld_sccs_within_cover_March <- multcomp::cld(
    em_sccs_within_cover_March,
    Letters = letters,
    adjust = "tukey",
    sort = FALSE
  )
  
  cld_sccs_within_cover_March
  
  
  March_interaction_letters <- as.data.frame(
    cld_sccs_within_cover_March
  ) %>%
    
    dplyr::transmute(
      SCCS,
      Cover,
      Letter = trimws(.group)
    )
  
  
  March_interaction_final_table <-
    March_raw_summary %>%
    
    dplyr::left_join(
      March_interaction_letters,
      by = c(
        "SCCS",
        "Cover"
      )
    ) %>%
    
    dplyr::mutate(
      Mean_SE_Letter = paste(
        Mean_SE,
        Letter
      )
    )
  
  
  March_interaction_final_table
  
  View(
    March_interaction_final_table
  )
  
  
  # ----------------------------------------------------------
  # Compare CC versus No-CC within each SCCS treatment
  # ----------------------------------------------------------
  
  em_cover_within_sccs_March <- emmeans(
    final_model_March,
    ~ Cover | SCCS
  )
  
  
  March_Cover_within_SCCS_pairs <- pairs(
    em_cover_within_sccs_March
  )
  
  
  March_Cover_within_SCCS_Holm <- summary(
    March_Cover_within_SCCS_pairs,
    by = NULL,
    adjust = "holm"
  )
  
  
  March_Cover_within_SCCS_Holm
  
  View(
    as.data.frame(
      March_Cover_within_SCCS_Holm
    )
  )
}