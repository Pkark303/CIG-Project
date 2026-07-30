# =====================================================================
# SCCS YEAR 2 SOIL DATA ANALYSIS
#
# Experimental design:
# Main-plot factor: SCCS treatment, 6 levels
# Split-plot factor: Cover crop, 2 levels
# Random factor: Block, 4 levels
# Sampling times: December and March, analyzed separately
#
# Response variables:
# POXC
# Mineralizable C
# ACE Protein
# BG
# NAG
# =====================================================================


# =====================================================================
# 1. INSTALL PACKAGES
# Run this section only once
# =====================================================================

packages_needed <- c(
  "readxl",
  "dplyr",
  "tidyr",
  "ggplot2",
  "lme4",
  "lmerTest",
  "emmeans",
  "writexl"
)

new_packages <- packages_needed[
  !(packages_needed %in% installed.packages()[, "Package"])
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}


# =====================================================================
# 2. LOAD PACKAGES
# =====================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(writexl)


# =====================================================================
# 3. IMPORT THE EXCEL FILE
# =====================================================================

# Option 1: Select the file manually
data_file <- file.choose()

data_file <- "C:/Users/pooon/OneDrive/Documents/CIG_Project/CIG SCCS Year 2 Soil Data.xlsx"

raw_data <- read_excel(
  path = data_file,
  sheet = "Sheet1"
)


# Remove extra spaces from the column names
names(raw_data) <- trimws(names(raw_data))

# Check original column names
print(names(raw_data))


# =====================================================================
# 4. CLEAN AND ORGANIZE THE DATA
# =====================================================================

dat <- raw_data %>%
  
  rename(
    time = Time,
    sccs = Treament,
    cover = `Cover crop`,
    block = Block,
    poxc = POXC,
    mineralizable_c = `Mineralizable C`,
    ace_protein = `ACE Protein`,
    bg = BG,
    nag = NAG
  ) %>%
  
  mutate(
    
    # Remove extra spaces from treatment names
    time = trimws(as.character(time)),
    sccs = trimws(as.character(sccs)),
    cover = trimws(as.character(cover)),
    
    # Standardize treatment names for the poster
    sccs = recode(
      sccs,
      "152 cm GS+CP" = "152 cm GS + CP",
      "152 cm GS + CP" = "152 cm GS + CP",
      "152 cm GS+FSB" = "152 cm GS + SB",
      "152 cm GS + FSB" = "152 cm GS + SB",
      "152 cm GS+SB" = "152 cm GS + SB"
    ),
    
    # Set the sampling-time order
    time = factor(
      time,
      levels = c(
        "December",
        "March"
      )
    ),
    
    # Set the SCCS treatment order
    sccs = factor(
      sccs,
      levels = c(
        "76 cm GS",
        "152 cm GS",
        "152 cm GS + CP",
        "152 cm GS + SB",
        "Monoculture CP",
        "Monoculture FSB"
      )
    ),
    
    # Set the cover-crop treatment order
    cover = factor(
      cover,
      levels = c(
        "CC",
        "No-CC"
      )
    ),
    
    block = factor(block)
  )


# Examine the cleaned data
str(dat)
head(dat)
summary(dat)


# =====================================================================
# 5. CHECK FOR TREATMENT-LABEL PROBLEMS
# =====================================================================

# These tables should not contain NA treatment levels
table(dat$time, useNA = "ifany")
table(dat$sccs, useNA = "ifany")
table(dat$cover, useNA = "ifany")
table(dat$block, useNA = "ifany")


# Stop the analysis if treatment labels became missing
if (any(is.na(dat$time))) {
  stop("Some sampling-time labels were not recognized.")
}

if (any(is.na(dat$sccs))) {
  stop("Some SCCS treatment labels were not recognized.")
}

if (any(is.na(dat$cover))) {
  stop("Some cover-crop labels were not recognized.")
}

if (any(is.na(dat$block))) {
  stop("Some block values are missing.")
}


# =====================================================================
# 6. CHECK THAT RESPONSE VARIABLES ARE NUMERIC
# =====================================================================

response_variables <- c(
  "poxc",
  "mineralizable_c",
  "ace_protein",
  "bg",
  "nag"
)

dat <- dat %>%
  mutate(
    across(
      all_of(response_variables),
      as.numeric
    )
  )


# Check missing observations
missing_values <- data.frame(
  Variable = response_variables,
  Number_missing = sapply(
    dat[response_variables],
    function(x) sum(is.na(x))
  )
)

print(missing_values)


# =====================================================================
# 7. VERIFY THE EXPERIMENTAL DESIGN
# =====================================================================

design_check <- dat %>%
  count(
    time,
    block,
    sccs,
    cover,
    name = "number_of_observations"
  )

print(design_check, n = Inf)


# Each Time × Block × SCCS × Cover combination should have one value
design_problems <- design_check %>%
  filter(number_of_observations != 1)

if (nrow(design_problems) == 0) {
  
  message(
    "The dataset has one observation for each ",
    "Time × Block × SCCS × Cover combination."
  )
  
} else {
  
  warning(
    "Some treatment combinations do not have exactly one observation."
  )
  
  print(design_problems)
}


# Check the number of observations
cat(
  "\nTotal number of observations:",
  nrow(dat),
  "\n"
)

cat(
  "Expected number for a complete design:",
  2 * 4 * 6 * 2,
  "\n"
)


# Cross-tabulation of the design
with(
  dat,
  table(
    time,
    block,
    sccs,
    cover
  )
)


# =====================================================================
# 8. TYPE III CONTRAST SETTINGS
# =====================================================================

# These contrast settings are required for appropriate Type III tests
options(
  contrasts = c(
    "contr.sum",
    "contr.poly"
  )
)


# =====================================================================
# 9. RESPONSE-VARIABLE LABELS
# =====================================================================

response_labels <- c(
  poxc = "POXC",
  mineralizable_c = "Mineralizable C",
  ace_protein = "ACE Protein",
  bg = "BG",
  nag = "NAG"
)


# =====================================================================
# 10. CREATE OUTPUT FOLDERS
# =====================================================================

output_directory <- "SCCS_ANOVA_Results"

dir.create(
  output_directory,
  showWarnings = FALSE
)

dir.create(
  file.path(
    output_directory,
    "Poster_Plots"
  ),
  showWarnings = FALSE
)

dir.create(
  file.path(
    output_directory,
    "Diagnostic_Plots"
  ),
  showWarnings = FALSE
)


# =====================================================================
# 11. CREATE EMPTY LISTS TO STORE RESULTS
# =====================================================================

models <- list()

anova_results <- list()

cell_emmeans_results <- list()

main_sccs_emmeans_results <- list()

main_cover_emmeans_results <- list()

pairwise_main_sccs_results <- list()

pairwise_main_cover_results <- list()

sccs_within_cover_results <- list()

cover_within_sccs_results <- list()

normality_results <- list()

model_information_results <- list()


# =====================================================================
# 12. RUN THE SPLIT-PLOT ANALYSES
# =====================================================================

for (sampling_time in levels(dat$time)) {
  
  # Select one sampling time
  dat_time <- dat %>%
    filter(time == sampling_time) %>%
    droplevels()
  
  for (response in response_variables) {
    
    model_name <- paste(
      sampling_time,
      response,
      sep = "_"
    )
    
    response_name <- response_labels[[response]]
    
    cat(
      "\n============================================================\n"
    )
    
    cat(
      "Sampling time:",
      sampling_time,
      "\n"
    )
    
    cat(
      "Response:",
      response_name,
      "\n"
    )
    
    cat(
      "============================================================\n"
    )
    
    
    # -----------------------------------------------------------------
    # Remove observations missing the response variable
    # -----------------------------------------------------------------
    
    model_data <- dat_time %>%
      filter(!is.na(.data[[response]])) %>%
      droplevels()
    
    
    # -----------------------------------------------------------------
    # SPLIT-PLOT MIXED MODEL
    #
    # Fixed effects:
    # SCCS
    # Cover crop
    # SCCS × Cover crop
    #
    # Random effects:
    # Block
    # Block × SCCS main-plot experimental unit
    #
    # (1 | block/sccs) expands to:
    # (1 | block) + (1 | block:sccs)
    # -----------------------------------------------------------------
    
    model_formula <- as.formula(
      paste0(
        response,
        " ~ sccs * cover + ",
        "(1 | block) + ",
        "(1 | block:sccs)"
      )
    )
    
    
    model <- lmer(
      formula = model_formula,
      data = model_data,
      REML = TRUE,
      na.action = na.exclude
    )
    
    
    # Save the model
    models[[model_name]] <- model
    
    
    # Print model summary
    print(summary(model))
    
    
    # =================================================================
    # 12A. TYPE III ANOVA
    # =================================================================
    
    anova_table <- as.data.frame(
      anova(
        model,
        type = 3,
        ddf = "Satterthwaite"
      )
    )
    
    anova_table$Effect <- rownames(anova_table)
    
    rownames(anova_table) <- NULL
    
    anova_table <- anova_table %>%
      select(
        Effect,
        everything()
      ) %>%
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    anova_results[[model_name]] <- anova_table
    
    print(anova_table)
    
    
    # =================================================================
    # 12B. EXTRACT RESIDUALS AND FITTED VALUES
    # =================================================================
    
    model_residuals <- residuals(model)
    
    model_fitted <- fitted(model)
    
    
    # =================================================================
    # 12C. SHAPIRO-WILK NORMALITY TEST
    #
    # Normality is tested on model residuals, not raw observations.
    # =================================================================
    
    if (
      length(model_residuals) >= 3 &&
      length(model_residuals) <= 5000
    ) {
      
      shapiro_result <- shapiro.test(model_residuals)
      
      shapiro_w <- unname(
        shapiro_result$statistic
      )
      
      shapiro_p <- shapiro_result$p.value
      
    } else {
      
      shapiro_w <- NA
      
      shapiro_p <- NA
    }
    
    
    # =================================================================
    # 12D. ADDITIONAL MODEL INFORMATION
    # =================================================================
    
    singular_model <- isSingular(
      model,
      tol = 1e-4
    )
    
    residual_mean <- mean(
      model_residuals,
      na.rm = TRUE
    )
    
    residual_sd <- sd(
      model_residuals,
      na.rm = TRUE
    )
    
    residual_skewness <- mean(
      (
        (
          model_residuals - residual_mean
        ) / residual_sd
      )^3,
      na.rm = TRUE
    )
    
    residual_kurtosis <- mean(
      (
        (
          model_residuals - residual_mean
        ) / residual_sd
      )^4,
      na.rm = TRUE
    ) - 3
    
    
    normality_results[[model_name]] <- data.frame(
      Sampling_time = sampling_time,
      Response = response_name,
      Number_of_residuals = length(model_residuals),
      Shapiro_W = shapiro_w,
      Shapiro_p_value = shapiro_p,
      Residual_skewness = residual_skewness,
      Residual_excess_kurtosis = residual_kurtosis,
      Normality_conclusion = ifelse(
        is.na(shapiro_p),
        "Test not conducted",
        ifelse(
          shapiro_p > 0.05,
          "No strong evidence of non-normality",
          "Possible departure from normality"
        )
      )
    )
    
    
    model_information_results[[model_name]] <- data.frame(
      Sampling_time = sampling_time,
      Response = response_name,
      Number_of_observations = nobs(model),
      Singular_model = singular_model,
      AIC = AIC(model),
      BIC = BIC(model),
      Log_likelihood = as.numeric(logLik(model))
    )
    
    
    # =================================================================
    # 12E. SAVE DIAGNOSTIC PLOTS
    #
    # Plot 1: Residuals versus fitted values
    # Plot 2: Normal Q-Q plot
    # Plot 3: Histogram of residuals
    # Plot 4: Residuals by SCCS × Cover treatment
    # =================================================================
    
    diagnostic_file <- file.path(
      output_directory,
      "Diagnostic_Plots",
      paste0(
        gsub(" ", "_", sampling_time),
        "_",
        response,
        "_diagnostics.png"
      )
    )
    
    png(
      filename = diagnostic_file,
      width = 2600,
      height = 2200,
      res = 250
    )
    
    par(
      mfrow = c(2, 2),
      mar = c(5, 5, 4, 2)
    )
    
    
    # Residuals versus fitted values
    plot(
      model_fitted,
      model_residuals,
      xlab = "Fitted values",
      ylab = "Residuals",
      main = paste(
        response_name,
        sampling_time,
        "\nResiduals versus fitted values"
      ),
      pch = 19
    )
    
    abline(
      h = 0,
      lty = 2,
      linewidth = 2
    )
    
    
    # Normal Q-Q plot
    qqnorm(
      model_residuals,
      main = paste(
        response_name,
        sampling_time,
        "\nNormal Q-Q plot"
      ),
      pch = 19
    )
    
    qqline(
      model_residuals,
      lty = 2,
      linewidth = 2
    )
    
    
    # Histogram of residuals
    hist(
      model_residuals,
      breaks = "FD",
      xlab = "Residuals",
      main = paste(
        response_name,
        sampling_time,
        "\nHistogram of residuals"
      )
    )
    
    
    # Residuals by treatment combination
    treatment_group <- interaction(
      model_data$sccs,
      model_data$cover,
      sep = "\n"
    )
    
    boxplot(
      model_residuals ~ treatment_group,
      las = 2,
      xlab = "",
      ylab = "Residuals",
      main = paste(
        response_name,
        sampling_time,
        "\nResiduals by treatment combination"
      ),
      cex.axis = 0.6
    )
    
    abline(
      h = 0,
      lty = 2,
      linewidth = 2
    )
    
    
    dev.off()
    
    
    # =================================================================
    # 12F. ESTIMATED MARGINAL MEANS:
    # SCCS × COVER CROP COMBINATIONS
    # =================================================================
    
    cell_emmeans <- emmeans(
      model,
      ~ sccs * cover
    )
    
    cell_emmeans_table <- as.data.frame(
      cell_emmeans
    ) %>%
      
      rename(
        SCCS = sccs,
        Cover = cover
      ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    cell_emmeans_results[[model_name]] <-
      cell_emmeans_table
    
    
    # =================================================================
    # 12G. SCCS MAIN-EFFECT ESTIMATED MARGINAL MEANS
    # =================================================================
    
    main_sccs_emmeans <- emmeans(
      model,
      ~ sccs
    )
    
    main_sccs_table <- as.data.frame(
      main_sccs_emmeans
    ) %>%
      
      rename(
        SCCS = sccs
      ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    main_sccs_emmeans_results[[model_name]] <-
      main_sccs_table
    
    
    # =================================================================
    # 12H. COVER-CROP MAIN-EFFECT ESTIMATED MARGINAL MEANS
    # =================================================================
    
    main_cover_emmeans <- emmeans(
      model,
      ~ cover
    )
    
    main_cover_table <- as.data.frame(
      main_cover_emmeans
    ) %>%
      
      rename(
        Cover = cover
      ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    main_cover_emmeans_results[[model_name]] <-
      main_cover_table
    
    
    # =================================================================
    # 12I. PAIRWISE SCCS MAIN-EFFECT COMPARISONS
    #
    # Use mainly when the SCCS × Cover interaction is not significant.
    # =================================================================
    
    pairwise_main_sccs <- pairs(
      main_sccs_emmeans,
      adjust = "tukey"
    )
    
    pairwise_main_sccs_table <- as.data.frame(
      pairwise_main_sccs
    ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    pairwise_main_sccs_results[[model_name]] <-
      pairwise_main_sccs_table
    
    
    # =================================================================
    # 12J. PAIRWISE COVER-CROP MAIN-EFFECT COMPARISON
    #
    # There are only two cover-crop levels.
    # =================================================================
    
    pairwise_main_cover <- pairs(
      main_cover_emmeans,
      adjust = "none"
    )
    
    pairwise_main_cover_table <- as.data.frame(
      pairwise_main_cover
    ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    pairwise_main_cover_results[[model_name]] <-
      pairwise_main_cover_table
    
    
    # =================================================================
    # 12K. SCCS COMPARISONS WITHIN EACH COVER-CROP TREATMENT
    #
    # Use these comparisons when the interaction is significant.
    # =================================================================
    
    sccs_within_cover <- emmeans(
      model,
      ~ sccs | cover
    )
    
    sccs_within_cover_pairs <- pairs(
      sccs_within_cover,
      adjust = "tukey"
    )
    
    sccs_within_cover_table <- as.data.frame(
      sccs_within_cover_pairs
    ) %>%
      
      rename(
        Cover = cover
      ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    sccs_within_cover_results[[model_name]] <-
      sccs_within_cover_table
    
    
    # =================================================================
    # 12L. COVER-CROP COMPARISONS WITHIN EACH SCCS TREATMENT
    #
    # Use these comparisons when the interaction is significant.
    # =================================================================
    
    cover_within_sccs <- emmeans(
      model,
      ~ cover | sccs
    )
    
    cover_within_sccs_pairs <- pairs(
      cover_within_sccs,
      adjust = "holm"
    )
    
    cover_within_sccs_table <- as.data.frame(
      cover_within_sccs_pairs
    ) %>%
      
      rename(
        SCCS = sccs
      ) %>%
      
      mutate(
        Sampling_time = sampling_time,
        Response = response_name,
        .before = 1
      )
    
    cover_within_sccs_results[[model_name]] <-
      cover_within_sccs_table
    
    
    # =================================================================
    # 12M. CREATE POSTER GRAPH
    #
    # Graph shows estimated marginal means ± SE.
    # =================================================================
    
    dodge_position <- position_dodge(
      width = 0.25
    )
    
    poster_plot <- ggplot(
      cell_emmeans_table,
      aes(
        x = SCCS,
        y = emmean,
        group = Cover,
        shape = Cover,
        linetype = Cover
      )
    ) +
      
      geom_line(
        position = dodge_position,
        linewidth = 0.8
      ) +
      
      geom_point(
        position = dodge_position,
        size = 3.5
      ) +
      
      geom_errorbar(
        aes(
          ymin = emmean - SE,
          ymax = emmean + SE
        ),
        position = dodge_position,
        width = 0.15,
        linewidth = 0.7
      ) +
      
      labs(
        title = paste0(
          response_name,
          " — ",
          sampling_time
        ),
        x = "Solar Corridor Cropping System treatment",
        y = paste0(
          response_name,
          " estimated marginal mean ± SE"
        ),
        shape = "Cover crop",
        linetype = "Cover crop"
      ) +
      
      theme_classic(
        base_size = 14
      ) +
      
      theme(
        plot.title = element_text(
          hjust = 0.5,
          face = "bold"
        ),
        axis.text.x = element_text(
          angle = 35,
          hjust = 1
        ),
        legend.position = "top"
      )
    
    
    ggsave(
      filename = file.path(
        output_directory,
        "Poster_Plots",
        paste0(
          gsub(" ", "_", sampling_time),
          "_",
          response,
          "_poster_plot.png"
        )
      ),
      plot = poster_plot,
      width = 11,
      height = 7,
      units = "in",
      dpi = 300
    )
  }
}


# =====================================================================
# 13. COMBINE ALL RESULTS
# =====================================================================

anova_all <- bind_rows(
  anova_results
)

cell_emmeans_all <- bind_rows(
  cell_emmeans_results
)

main_sccs_emmeans_all <- bind_rows(
  main_sccs_emmeans_results
)

main_cover_emmeans_all <- bind_rows(
  main_cover_emmeans_results
)

pairwise_main_sccs_all <- bind_rows(
  pairwise_main_sccs_results
)

pairwise_main_cover_all <- bind_rows(
  pairwise_main_cover_results
)

sccs_within_cover_all <- bind_rows(
  sccs_within_cover_results
)

cover_within_sccs_all <- bind_rows(
  cover_within_sccs_results
)

normality_all <- bind_rows(
  normality_results
)

model_information_all <- bind_rows(
  model_information_results
)


# =====================================================================
# 14. CREATE RAW SUMMARY STATISTICS FOR POSTER TABLES
# =====================================================================

poster_summary <- dat %>%
  
  pivot_longer(
    cols = all_of(response_variables),
    names_to = "response",
    values_to = "value"
  ) %>%
  
  group_by(
    time,
    sccs,
    cover,
    response
  ) %>%
  
  summarise(
    n = sum(!is.na(value)),
    
    Mean = mean(
      value,
      na.rm = TRUE
    ),
    
    SD = sd(
      value,
      na.rm = TRUE
    ),
    
    SE = SD / sqrt(n),
    
    Minimum = min(
      value,
      na.rm = TRUE
    ),
    
    Maximum = max(
      value,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    Response = unname(
      response_labels[response]
    )
  ) %>%
  
  rename(
    Sampling_time = time,
    SCCS = sccs,
    Cover = cover
  ) %>%
  
  select(
    Sampling_time,
    Response,
    SCCS,
    Cover,
    n,
    Mean,
    SD,
    SE,
    Minimum,
    Maximum
  )


# =====================================================================
# 15. CREATE A CLEAN ANOVA P-VALUE SUMMARY
# =====================================================================

anova_pvalue_summary <- anova_all %>%
  
  filter(
    Effect %in% c(
      "sccs",
      "cover",
      "sccs:cover"
    )
  ) %>%
  
  select(
    Sampling_time,
    Response,
    Effect,
    `F value`,
    `Pr(>F)`
  ) %>%
  
  mutate(
    Significance = case_when(
      `Pr(>F)` < 0.001 ~ "***",
      `Pr(>F)` < 0.01 ~ "**",
      `Pr(>F)` < 0.05 ~ "*",
      `Pr(>F)` < 0.10 ~ ".",
      TRUE ~ "NS"
    )
  )


# =====================================================================
# 16. PRINT NORMALITY RESULTS
# =====================================================================

# =====================================================================
# PRINT NORMALITY RESULTS
# =====================================================================

cat("\n\nNORMALITY TEST RESULTS\n")

print(
  tibble::as_tibble(normality_all),
  n = Inf
)

# =====================================================================
# PRINT NORMALITY RESULTS
# =====================================================================

cat("\n\nNORMALITY TEST RESULTS\n")

print(
  tibble::as_tibble(normality_all),
  n = Inf
)


# =====================================================================
# PRINT ANOVA P-VALUE SUMMARY
# =====================================================================

cat("\n\nANOVA P-VALUE SUMMARY\n")

print(
  tibble::as_tibble(anova_pvalue_summary),
  n = Inf
)

# =====================================================================
# 18. EXPORT ALL RESULTS TO EXCEL
# =====================================================================

output_excel_file <- file.path(
  output_directory,
  "SCCS_split_plot_ANOVA_results.xlsx"
)

write_xlsx(
  list(
    
    Cleaned_Data =
      dat,
    
    Design_Check =
      design_check,
    
    Missing_Values =
      missing_values,
    
    ANOVA =
      anova_all,
    
    ANOVA_Pvalue_Summary =
      anova_pvalue_summary,
    
    Residual_Normality =
      normality_all,
    
    Model_Information =
      model_information_all,
    
    Cell_EMMeans =
      cell_emmeans_all,
    
    SCCS_Main_EMMeans =
      main_sccs_emmeans_all,
    
    Cover_Main_EMMeans =
      main_cover_emmeans_all,
    
    Pairwise_Main_SCCS =
      pairwise_main_sccs_all,
    
    Pairwise_Main_Cover =
      pairwise_main_cover_all,
    
    SCCS_within_Cover =
      sccs_within_cover_all,
    
    Cover_within_SCCS =
      cover_within_sccs_all,
    
    Raw_Poster_Summary =
      poster_summary
  ),
  
  path = output_excel_file
)


# =====================================================================
# 19. SAVE ALL FITTED MODELS
# =====================================================================

saveRDS(
  models,
  file = file.path(
    output_directory,
    "SCCS_split_plot_models.rds"
  )
)


# =====================================================================
# 20. SAVE THE CLEANED DATA AS CSV
# =====================================================================

write.csv(
  dat,
  file = file.path(
    output_directory,
    "SCCS_cleaned_data.csv"
  ),
  row.names = FALSE
)


# =====================================================================
# 21. DISPLAY INDIVIDUAL MODEL RESULTS
# =====================================================================

# Examples:

# December POXC model
summary(
  models[["December_poxc"]]
)

anova(
  models[["December_poxc"]],
  type = 3,
  ddf = "Satterthwaite"
)

shapiro.test(
  residuals(
    models[["December_poxc"]]
  )
)


# March POXC model
summary(
  models[["March_poxc"]]
)

anova(
  models[["March_poxc"]],
  type = 3,
  ddf = "Satterthwaite"
)

shapiro.test(
  residuals(
    models[["March_poxc"]]
  )
)


# =====================================================================
# 22. FINAL MESSAGE
# =====================================================================

cat(
  "\nAnalysis completed successfully.\n",
  "\nResults were saved in:\n",
  normalizePath(output_directory),
  "\n\nExcel results file:\n",
  normalizePath(output_excel_file),
  "\n"
)

