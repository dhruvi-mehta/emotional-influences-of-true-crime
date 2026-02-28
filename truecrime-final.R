# ============================================================
# True Crime Media Consumption Analysis
# - Logistic regression (TV + Podcasts)
# - Chi-square tests
# - Spearman correlations (ordinal -> numeric copy)
# - Predicted probability visualizations
# - Correlation heatmap for numeric predictors
# ============================================================

# ---- 0) PACKAGES ----
required_packages <- c(
  "dplyr", "tidyverse", "corrplot", "car", "patchwork", "reshape2"
)

# Install missing packages (optional, but helps reproducibility)
installed <- rownames(installed.packages())
for (p in required_packages) {
  if (!p %in% installed) install.packages(p)
  library(p, character.only = TRUE)
}

# ---- 1) LOAD DATA ----
data <- read.csv("data/true_crime.csv")

# Inspect dataset
head(data)
str(data)

# Optional: define variables of interest (useful as a reference)
variables <- c(
  paranoid = "Q6_Fearful_Paranoid",
  curiosity = "Q17_Curiosity_Reasons",
  vigilance = "Q4F_Vigilance",
  excitement = "Q4G_Excitement",
  crimpsych = "Q4B_Understanding_Criminals",
  crimlaw = "Q4C_Understanding_Justice",
  consume = "Q1_Consume_True_Crime",
  frequency = "Q10_Frequency",
  socialconsumption = "Q13_Social_Consumption",
  tv = "Q9_Media_Formats_TV",
  podcast = "Q9_Media_Formats_Podcasts"
)

# ---- 2) CLEAN + TYPE CONVERSIONS ----
# Note: For logistic regression with binomial(link="logit"),
# outcomes should be binary (0/1 or a 2-level factor).
# Check levels below to confirm.
data <- data %>%
  mutate(
    Q6_Fearful_Paranoid = as.factor(Q6_Fearful_Paranoid),
    Q17_Curiosity_Reasons = as.factor(Q17_Curiosity_Reasons),
    Q9_Media_Formats_TV = as.factor(Q9_Media_Formats_TV),
    Q9_Media_Formats_Podcasts = as.factor(Q9_Media_Formats_Podcasts),
    Q10_Frequency = as.numeric(Q10_Frequency)
  )

# Clean dataset (only rows needed for the models)
data_clean <- data %>%
  drop_na(
    Q6_Fearful_Paranoid,
    Q17_Curiosity_Reasons,
    Q4F_Vigilance,
    Q4G_Excitement,
    Q9_Media_Formats_TV,
    Q9_Media_Formats_Podcasts
  )

# Confirm row count
nrow(data_clean)

# Quick sanity check: are outcomes binary?
table(data_clean$Q9_Media_Formats_TV)
table(data_clean$Q9_Media_Formats_Podcasts)

# ---- 3) LOGISTIC REGRESSION MODELS ----
tv_model <- glm(
  Q9_Media_Formats_TV ~ Q6_Fearful_Paranoid + Q17_Curiosity_Reasons + Q4F_Vigilance + Q4G_Excitement,
  family = binomial(link = "logit"),
  data = data_clean
)

podcast_model <- glm(
  Q9_Media_Formats_Podcasts ~ Q6_Fearful_Paranoid + Q17_Curiosity_Reasons + Q4F_Vigilance + Q4G_Excitement,
  family = binomial(link = "logit"),
  data = data_clean
)

# Model summaries
summary(tv_model)
summary(podcast_model)

# Odds ratios
exp(coef(tv_model))
exp(coef(podcast_model))

# ---- 4) CHI-SQUARE TESTS ----
fear_tv_table <- table(data_clean$Q6_Fearful_Paranoid, data_clean$Q9_Media_Formats_TV)
chi_test_tv <- chisq.test(fear_tv_table)
print(chi_test_tv)

curiosity_podcast_table <- table(data_clean$Q17_Curiosity_Reasons, data_clean$Q9_Media_Formats_Podcasts)
chi_test_podcast <- chisq.test(curiosity_podcast_table)
print(chi_test_podcast)

# ---- 5) SPEARMAN CORRELATIONS (KEEP CLEAN DATA INTACT) ----
# Create a separate dataset for ordinal->numeric conversions
data_spear <- data_clean %>%
  mutate(
    paranoid_num  = as.numeric(as.character(Q6_Fearful_Paranoid)),
    curiosity_num = as.numeric(as.character(Q17_Curiosity_Reasons)),
    tv_num        = as.numeric(as.character(Q9_Media_Formats_TV)),
    podcast_num   = as.numeric(as.character(Q9_Media_Formats_Podcasts))
  )

# Fear and TV
spearman_fear_tv <- cor.test(
  data_spear$paranoid_num,
  data_spear$tv_num,
  method = "spearman"
)
print(spearman_fear_tv)

# Fear and Podcast
spearman_fear_podcast <- cor.test(
  data_spear$paranoid_num,
  data_spear$podcast_num,
  method = "spearman"
)
print(spearman_fear_podcast)

# Curiosity and TV
spearman_curiosity_tv <- cor.test(
  data_spear$curiosity_num,
  data_spear$tv_num,
  method = "spearman"
)
print(spearman_curiosity_tv)

# Curiosity and Podcast
spearman_curiosity_podcast <- cor.test(
  data_spear$curiosity_num,
  data_spear$podcast_num,
  method = "spearman"
)
print(spearman_curiosity_podcast)

# Fear and Curiosity
spearman_fear_curiosity <- cor.test(
  data_spear$paranoid_num,
  data_spear$curiosity_num,
  method = "spearman"
)
print(spearman_fear_curiosity)

# Fear and Frequency
spearman_fear_frequency <- cor.test(
  data_spear$paranoid_num,
  data_clean$Q10_Frequency,
  method = "spearman"
)
print(spearman_fear_frequency)

# ---- 6) PREDICTED PROBABILITIES + VISUALIZATIONS ----
# Predicted probabilities (use the same data_clean used to fit the models)
data_clean$Predicted_Prob_TV <- predict(tv_model, newdata = data_clean, type = "response")
data_clean$Predicted_Prob_Podcast <- predict(podcast_model, newdata = data_clean, type = "response")

# Plot 1: TV probability by Fear, colored by Curiosity
ggplot(data_clean, aes(x = Q6_Fearful_Paranoid, y = Predicted_Prob_TV, color = Q17_Curiosity_Reasons)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of TV Consumption",
    x = "Fear Level (Paranoid)",
    y = "Predicted Probability",
    color = "Curiosity Level"
  ) +
  theme_minimal()

# Plot 2: Podcast probability by Fear, colored by Curiosity
ggplot(data_clean, aes(x = Q6_Fearful_Paranoid, y = Predicted_Prob_Podcast, color = Q17_Curiosity_Reasons)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of Podcast Consumption",
    x = "Fear Level (Paranoid)",
    y = "Predicted Probability",
    color = "Curiosity Level"
  ) +
  theme_minimal()

# Plot 3: TV probability by Curiosity, colored by Fear (minimal)
ggplot(data_clean, aes(x = Q17_Curiosity_Reasons, y = Predicted_Prob_TV, color = Q6_Fearful_Paranoid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of TV Consumption by Curiosity Levels",
    x = "Curiosity Level",
    y = "Predicted Probability",
    color = "Fear Level (Paranoid)"
  ) +
  theme_minimal()

# Plot 4: Podcast probability by Curiosity, colored by Fear (minimal)
ggplot(data_clean, aes(x = Q17_Curiosity_Reasons, y = Predicted_Prob_Podcast, color = Q6_Fearful_Paranoid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of Podcast Consumption by Curiosity Levels",
    x = "Curiosity Level",
    y = "Predicted Probability",
    color = "Fear Level (Paranoid)"
  ) +
  theme_minimal()

#7) OPTIONAL: NEON THEME VERSIONS (KEEPING YOUR STYLE)
neon_palette <- c(
  "1" = "#FF6EC7", # Neon pink
  "2" = "#FFD700", # Neon yellow
  "3" = "#32CD32", # Neon green
  "4" = "#00FFFF", # Neon cyan
  "5" = "#FF4500"  # Neon orange-red
)

neon_theme <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "#161616", color = NA),
    panel.background = element_rect(fill = "#161616", color = NA),
    legend.background = element_rect(fill = "#161616", color = NA),
    legend.text = element_text(color = "#FFFFFF"),
    legend.title = element_text(color = "#FFFFFF"),
    axis.text = element_text(color = "#FFFFFF"),
    axis.title = element_text(color = "#FFFFFF"),
    plot.title = element_text(color = "#FFFFFF")
  )

# Neon Plot A: TV by Curiosity, colored by Fear
ggplot(data_clean, aes(x = Q17_Curiosity_Reasons, y = Predicted_Prob_TV, color = Q6_Fearful_Paranoid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of TV Consumption by Curiosity Levels",
    x = "Curiosity Level",
    y = "Predicted Probability",
    color = "Fear Level (Paranoid)"
  ) +
  scale_color_manual(values = neon_palette) +
  neon_theme

# Neon Plot B: Podcast by Curiosity, colored by Fear
ggplot(data_clean, aes(x = Q17_Curiosity_Reasons, y = Predicted_Prob_Podcast, color = Q6_Fearful_Paranoid)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of Podcast Consumption by Curiosity Levels",
    x = "Curiosity Level",
    y = "Predicted Probability",
    color = "Fear Level (Paranoid)"
  ) +
  scale_color_manual(values = neon_palette) +
  neon_theme

# Neon Plot C: Podcast by Fear, colored by Curiosity
ggplot(data_clean, aes(x = Q6_Fearful_Paranoid, y = Predicted_Prob_Podcast, color = Q17_Curiosity_Reasons)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of Podcast Consumption by Fear Levels",
    x = "Fear Level",
    y = "Predicted Probability",
    color = "Curiosity Level"
  ) +
  scale_color_manual(values = neon_palette) +
  neon_theme

# Neon Plot D: TV by Fear, colored by Curiosity
ggplot(data_clean, aes(x = Q6_Fearful_Paranoid, y = Predicted_Prob_TV, color = Q17_Curiosity_Reasons)) +
  geom_point() +
  geom_smooth(method = "loess") +
  labs(
    title = "Predicted Probability of TV Consumption by Fear Levels",
    x = "Fear Level",
    y = "Predicted Probability",
    color = "Curiosity Level"
  ) +
  scale_color_manual(values = neon_palette) +
  neon_theme

#8) CORRELATION HEATMAP (NUMERIC PREDICTORS
numeric_vars <- data_clean %>%
  select(Q4F_Vigilance, Q4G_Excitement, Q10_Frequency) %>%
  mutate(across(everything(), as.numeric))

corr_matrix <- cor(numeric_vars, use = "complete.obs")
melted_corr <- reshape2::melt(corr_matrix)

ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Matrix (Numeric Predictors)", x = "Variables", y = "Variables") +
  theme_minimal()