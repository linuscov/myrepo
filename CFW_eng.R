


# ================================================================
# 1. DATA IMPORT AND PREPARATION
# ================================================================

# -------------------------------
# 1.1 Load Packages
# -------------------------------

# Define all required packages
required_packages <- c(
  "haven", "naniar", "dplyr", "purrr", "papeR", "labelled", "rstatix", 
  "lme4", "glmmTMB", "statmod", "psych", "lavaan", "semPlot", "tidyr", 
  "semTools", "univOutl", "ggplot2", "ggpubr", "kableExtra", "sandwich", 
  "lmtest", "MuMIn", "papaja", "gtsummary", "officer", "rempsyc", 
  "performance", "DHARMa", "corrr", "ggcorrplot", "lmerTest", "DHARMa"
)

# Install any that are missing, then load all
invisible(lapply(required_packages, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))


# -------------------------------
# 1.2 Load Raw Data
# -------------------------------

set.seed(1234)
setwd("C:/Users/Covic/Nextcloud/linus/CFW")

data <- read_dta("0_DFHH_DATA_allInterventions_TB_MOA_2waves.dta")
CFW <- data[data$Interv == 'CFW',] # take only CFW data 


# -------------------------------
# 1.3 Data Preparation
# -------------------------------

## Exclude all columns that are completly missing from data frame  
clean_missing_data <- function(df) {
  var_miss <- miss_var_summary(df)
  missing_col <- var_miss$variable[var_miss$n_miss == nrow(df)]
  df %>% select(!all_of(missing_col))
}

CFW <- clean_missing_data(CFW)


# 1.3.1 Calculate new variables 

# Age 
CFW$age = CFW$f22_8 + 16

# Total number of people in household 
CFW$hh_members <- rowSums(CFW[, c("KindG", "KindK", "Adult")], na.rm = TRUE)


# New intervention variable 
CFW <- CFW %>%
  mutate(Intervention = factor(case_when(IV_int1 == 1 ~ 1,
                                         IV_int2 == 1 ~ 2,
                                         IV == 0 ~0,
                                         TRUE ~ NA),
                               levels = c (0,1,2),
                               labels = c ("Kontroll", "Seminar", "Video")))


# Transformed waste variables 
CFW$wastepd_log <- log(CFW$wastepd + 0.0001)
CFW$wastepd_root <- CFW$wastepd^ (1/3)

CFW$edible_log <- log(CFW$edible + 0.0001)
CFW$edible_root <- CFW$edible ^ (1/3)


# Storage variable
CFW <- CFW %>%
  mutate(
    score_a9_11 = case_when(a9_11 == 9 ~ 1,TRUE ~ 0),
    score_a9_12 = case_when(a9_12 == 1 ~ 1,TRUE ~ 0),
    score_a9_13 = case_when(a9_13 == 3 ~ 1, a9_13 == 8 ~ 0.5, TRUE ~ 0),
    score_a9_14 = case_when(a9_14 == 9 ~ 1, a9_14 == 8 ~ 0.5, TRUE ~ 0),
    score_a9_15 = case_when(a9_15 == 4 ~ 1,TRUE ~ 0),
    score_a9_16 = case_when(a9_16 == 7 ~ 1, a9_16 == 1 ~ 1, TRUE ~ 0),
    score_a9_17 = case_when(a9_17 == 9 ~ 1,TRUE ~ 0),
    score_a9_18 = case_when(a9_18 == 9 ~ 1, a9_18 == 8 ~ 0.5, TRUE ~ 0),
    score_a9_19 = case_when(a9_19 == 2 ~ 1,TRUE ~ 0),
    score_a9_110 = case_when(a9_110 == 9 ~ 1,TRUE ~ 0),
    riping = case_when(a9_21 + a9_23 + a9_25 == 6 ~1,
                       a9_21 + a9_23 + a9_25 == 5 ~0.5,
                       TRUE ~ 0),
    air_permeability = a9_31 + a9_33 + a9_34 + a9_36 - 4,
    A_Storage = (score_a9_11 + score_a9_12 + score_a9_13 + score_a9_14 + score_a9_15 + score_a9_16 + score_a9_17 + score_a9_18 + score_a9_19 + score_a9_110 + riping + air_permeability)/15*4+1)


# Ability and Opportunity variables 
CFW <- CFW %>%
  mutate(
    A_Edibility = rowMeans(select(., a8_11, a8_21, a8_31, a8_41)),
    O_Availability = o10_1,
  )


# 1.3.2 Factorize 

# Convert to factor 
CFW <- CFW %>%
  mutate(across(c('IV_int1', 'IV_int2', 'IV', 'PCode', 'wave'), as.factor))

# Gender
CFW$f22_7 <- factor(CFW$f22_7,
                    levels = c(-1,1,2,3),
                    labels = c("Prefer not to say",
                               "Female",
                               "Male",
                              "No answer"))

# Intervention 
CFW$IV <- factor(CFW$IV,
                 levels = c(0,1),
                 labels = c("Control", "Treatment"))

# Education
CFW$f22_9 <- factor(CFW$f22_9,
                    levels = c(1,9,3,4,5,6,7,8,11,10),
                    labels = c("No school leaving certificate",
                               "Still in school",
                               "Basic secondary school certificate",
                               "Secondary school certificate or equivalent",
                               "Completed vocational training",
                               "Technical diploma or university of applied sciences entrance qualification",
                               "General university entrance qualification (Abitur)",
                               "University degree",
                               "Advanced vocational qualification (Master craftsperson, technician, business administrator)",
                               "Other qualification"
                    ))

# Income
CFW$f22_5 <- factor(CFW$f22_5,
                    levels = c(1,9,3,4,5,6,7,8,11,10),
                    labels = c("Less than €900",
                               "€900 to under €1300",
                               "€1300 to under €1500",
                               "€1500 to under €2000",
                               "€2000 to under €2600",
                               "€2600 to under €3200",
                               "€3200 to under €4500",
                               "€4500 to under €6000",
                               "€6000 and more",
                               "Prefer not to answer"))

# ?
CFW$f22_6 <- factor(CFW$f22_6,
                    levels = c(1,2),
                    labels = c("Yes",
                               "No"))

# Living situation 
CFW$f22_2 <- factor(CFW$f22_2,
                    levels = c(1,2,3,4,6,5),
                    labels = c("With family",
                               "In a shared apartment",
                               "Alone",
                               "Assisted living",
                               "With a partner or partners",
                               "Other"
                    ))


# Rename variables 
CFW <- CFW %>%
  mutate(across(c(f22_5, f22_6, f22_7, f22_9), factor)) %>%
  rename(
    income = f22_5,
    gender = f22_7,
    education = f22_9,
    cohabitation = f22_2
  )



# Take only cases with two time points 
twice <- CFW %>%
  group_by(PCode) %>%
  dplyr::filter(n() == 2) %>%
  ungroup()

CFW_compl <- CFW %>%
  semi_join(twice, by = "PCode")







# ================================================================
# 2. CFA and Reliability  
# ================================================================


# -------------------------------------
# 2.1 Preparation (Check missing data)
# -------------------------------------

moa_items <- c(
  "m3_2", "m3_1", "m3_6", "m3_3", "m3_4", "m3_5",             # M_Awareness
  "m2_1", "m2_2", "m2_5", "m2_3", "m2_4",                     # M_Attitudes
  "m4_2", "m4_1", "m4_3", "m4_4", "m4_5", "m4_6",             # M_SocialNorms 
  "m5_1", "m5_3", "m5_5", "m5_2", "m5_4",                     # M_Intention
  "a6_1", "a6_2", "a6_3", "a6_4",                             # A_Planning
  "a7_1", "a7_2", "a7_3",                                     # A_Cooking
  "o13_1", "o13_2", "o13_3",                                  # O_Events
  "o11_1", "o11_2",                                           # O_Accessibility 
  "o12_1", "o12_2"                                            # O_Equipment          
)

# Select those columns
cfw_moa <- CFW %>% select(all_of(moa_items))

# Check missing data 

# Total missing values
n_miss(cfw_moa)
prop_miss(cfw_moa)  

# Missing values per variable
var_miss <- as.data.frame(miss_var_summary(cfw_moa))
gg_miss_var(cfw_moa)

# Missing percentage per case
case_miss_perc <- as.data.frame(miss_case_summary(cfw_moa))

# Pattern of missing data 
vis_miss(cfw_moa)
gg_miss_upset(cfw_moa)




# --------------------------------------
# 2.2 Confirmatory Factor Analysis (CFA)
# --------------------------------------

# Model specification
model_cfa <- '
  # Motivation
  M_Awareness          =~ m3_1 + m3_2 + m3_3 + m3_4 + m3_5 + m3_6
  M_Attitude           =~ m2_1 + m2_2 + m2_3 + m2_4 + m2_5
  M_Injunctive_Norm    =~ m4_1 + m4_2 + m4_3
  M_Descriptive_Norm   =~ m4_4 + m4_5 + m4_6  
  M_Intention          =~ m5_1 + m5_3 + m5_4 + m5_5
  
  # Ability
  A_Planning           =~ a6_1 + a6_2 + a6_3 + a6_4
  A_Cooking            =~ a7_1 + a7_2 + a7_3
  
  # Opportunity
  O_Accessibility      =~ o11_1 + o11_2
  O_Equipment          =~ o12_1 + o12_2
  O_Events             =~ o13_1 + o13_2 + o13_3
  
  # Allow correlated residuals (based on theory or MI)
  m3_1 ~~ m3_2
  m5_4 ~~ o12_1
'

# Configural model (no equality constraints)
fit_configural <- sem(model_cfa, 
                      data = CFW,
                      group = "wave",
                      estimator = "MLR",
                      missing = "FIML",
                      meanstructure = TRUE)

# View modification indices
modificationIndices(fit_configural) %>%
  arrange(desc(mi))

# Metric invariance (equal loadings across groups)
fit_metric <- sem(model_cfa, 
                  data = CFW,
                  group = "wave",
                  group.equal = "loadings",
                  estimator = "MLR",
                  missing = "FIML",
                  meanstructure = TRUE)

# Scalar invariance (equal loadings and intercepts)
fit_scalar <- cfa(model_cfa, 
                  data = CFW,
                  group = "wave",
                  group.equal = c("loadings", "intercepts"),
                  estimator = "MLR",
                  missing = "FIML",
                  meanstructure = TRUE)

# Summarize models with fit indices and standardized loadings
summary(fit_configural, fit.measures = TRUE, standardized = TRUE)
summary(fit_metric, fit.measures = TRUE, standardized = TRUE)
summary(fit_scalar, fit.measures = TRUE, standardized = TRUE)

# Extract common fit indices
indices <- c("cfi", "tli", "rmsea", "srmr")

fitMeasures(fit_configural, fit.measures = indices)
fitMeasures(fit_metric, fit.measures = indices)
fitMeasures(fit_scalar, fit.measures = indices)

# Compare models via chi-square difference tests
anova(fit_configural, fit_metric, fit_scalar)

# Visualization of configural model 
semPaths(fit_configural, what = "std", layout = "tree", edge.label.cex = 0.8)

# Create table of standardized factor loadings
loadings_df <- standardizedSolution(fit_configural) %>%
  filter(op == "=~") %>%
  select(Factor = lhs, Item = rhs, Loading = est.std) %>%
  arrange(Factor)

# Extract standardized loadings from the configural model
stdsol <- standardizedSolution(fit_configural) %>%
  filter(op == "=~") %>%
  select(group, lhs, rhs, est.std) %>%
  rename(
    wave    = group,
    factor  = lhs,
    item    = rhs,
    loading = est.std
  )



# ------------------------
# 2.3 Reliability Analysis
# ------------------------

# Define factors and their items
factors <- list(
  M_Awareness       = c("m3_1","m3_2","m3_3","m3_4","m3_5", "m3_6"),
  M_Attitude        = c("m2_1","m2_2","m2_3","m2_4","m2_5"),
  M_Injunctive_Norm = c("m4_1","m4_2","m4_3"),
  M_Descriptive_Norm= c("m4_4","m4_5","m4_6"),
  M_Intention       = c("m5_1","m5_3","m5_4","m5_5"),
  A_Planning        = c("a6_1","a6_2","a6_3","a6_4"),
  A_Cooking         = c("a7_1","a7_2","a7_3"),
  O_Accessibility   = c("o11_1","o11_2"),
  O_Equipment       = c("o12_1","o12_2"),
  O_Events          = c("o13_1","o13_2","o13_3")
)


# Function to compute Cronbach's alpha, Composite Reliability (CR), and Average Variance Extracted (AVE)
get_reliabilities <- function(df, items, this_wave, this_factor) {
  lam <- stdsol %>%
    filter(wave == this_wave, factor == this_factor) %>%
    pull(loading)
  
  α   <- psych::alpha(df[, items], warnings = FALSE)$total$raw_alpha
  CR  <- sum(lam)^2 / (sum(lam)^2 + sum(1 - lam^2))
  AVE <- sum(lam^2) / length(lam)
  
  tibble(alpha = α, CR = CR, AVE = AVE)
}


# --------------------
# 2.4 Output 
# --------------------

# Build final reliability report table for all waves and factors
report <- map_dfr(unique(CFW$wave), function(w) {
  df_w <- filter(CFW, wave == w)
  
  map_dfr(names(factors), function(f) {
    its <- factors[[f]]
    rel <- get_reliabilities(
      df_w, its, w, 
      paste0("M_M_", f) %>% 
        sub("^M_M_", "", .) %>%  
        { # find exact lavaan factor name matching
          grep(f, stdsol$factor, ignore.case = TRUE, value = TRUE)[1]
        }
    )
    
    # Summary row with reliability metrics
    sum_row <- tibble(
      wave    = w,
      factor  = f,
      item    = NA_character_,
      loading = NA_real_,
      alpha   = rel$alpha,
      CR      = rel$CR,
      AVE     = rel$AVE
    )
    
    # Rows for individual items with loadings
    item_rows <- stdsol %>%
      filter(wave == w, factor %in% stdsol$factor[grep(f, stdsol$factor, ignore.case = TRUE)]) %>%
      select(item, loading) %>%
      mutate(
        wave   = w,
        factor = NA_character_,
        alpha  = NA_real_,
        CR     = NA_real_,
        AVE    = NA_real_
      ) %>%
      select(wave, factor, item, loading, alpha, CR, AVE)
    
    bind_rows(sum_row, item_rows)
  })
})

# Print reliability report
print(report)

# Export reliability report to CSV
write.csv(report, "cfa_report_wide.csv", row.names = FALSE)


# Calculating latent variables (when at least two items are not NA)
CFW <- CFW %>% 
  bind_cols(
    map_dfc(factors, ~ 
              pmap_dbl(CFW[.x], function(...) {
                vals <- c(...)
                if (sum(!is.na(vals)) >= 2) {
                  mean(vals, na.rm = TRUE)
                } else {
                  NA_real_
                }
              })
    ) %>% setNames(names(factors))
  )





# ================================================================
# 3. Descriptives  
# ================================================================


# Count how many times each person appears in each wave
wave_summary <- CFW %>%
  select(PCode, wave) %>%
  distinct() %>%
  group_by(PCode) %>%
  dplyr::summarise(waves = paste(sort(unique(wave)), collapse = ",")) %>%
  ungroup()

# Classify each person
wave_summary <- wave_summary %>%
  mutate(wave_participation = case_when(
    waves == "1" ~ "Wave 1 only",
    waves == "2" ~ "Wave 2 only",
    waves == "1,2" ~ "Both waves",
    TRUE ~ "Other"
  ))

# Count how many in each group
table(wave_summary$wave_participation)


# First: Get list of PCode values only in wave 2
wave2_only_ids <- wave_summary %>%
  filter(wave_participation == "Wave 2 only") %>%
  pull(PCode)

# Now: Filter your main dataset (CFW) for these individuals
CFW_wave2_only <- CFW %>%
  filter(PCode %in% wave2_only_ids)

# Optional: View the result
View(CFW_wave2_only)


# Interventions 
summary(CFW$Intervention)


# -----------------------------------------
# 3.1 Motivation–Ability–Opportunity (MOA)
# -----------------------------------------

# Vector of MOA variable names
moa_vars <- c(
  "M_Awareness", "M_Attitude", "M_Injunctive_Norm", "M_Descriptive_Norm", "M_Intention",
  "A_Planning", "A_Storage", "A_Edibility", "A_Cooking",
  "O_Events", "O_Availability", "O_Equipment", "O_Accessibility"
)

# Ensure all are treated as numeric
moa_descriptives <- data.frame(moa_vars)

for (t in 1:2) {
  moa_data <- CFW[CFW$wave == t, ] %>%
    select(all_of(moa_vars)) %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  moa_means <- round(colMeans(moa_data, na.rm = TRUE), 2)
  moa_sd <- round(apply(moa_data, 2, function(x) sd(x, na.rm = TRUE)), 2)
  moa_missing <- paste(colSums(is.na(moa_data)), "(", round(colSums(is.na(moa_data)) / nrow(moa_data), 2), "%)")
  
  moa_descriptives <- cbind(moa_descriptives, moa_means, moa_sd, moa_missing)
}
rownames(moa_descriptives) <- NULL

add_header_above(kbl(moa_descriptives), c(" " = 1, "Pre" = 3, "Post" = 3))


# Create APA-style table 
moa_table <- moa_descriptives %>%
  kbl(format = "html", caption = "Descriptive statistics for MOA constructs") %>%
  add_header_above(c(" " = 1, "Pre" = 3, "Post" = 3)) %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover"))

save_kable(moa_table, file = "moa_table.html")


# Reshape data into long format
moa_long <- CFW %>%
  select(all_of(moa_vars)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Plot histograms
ggplot(moa_long, aes(x = Value, fill = Variable)) +
  geom_histogram(bins = 30, alpha = 0.7, color = "white") +
  facet_wrap(~ Variable, scales = "free") +
  theme_minimal() +
  labs(title = "Histograms of MOA Variables", x = "Value", y = "Count") +
  theme(legend.position = "none")



# -----------------------------------------
# 3.2 Reasons for Food Waste
# -----------------------------------------

# Reshape the data into long format
reasons_data <- CFW %>%
  select(wave, starts_with("Reason_")) %>%
  mutate(row = row_number()) %>%
  pivot_longer(
    cols = starts_with("Reason_"),
    names_to = "reason",
    values_to = "rank"
  ) %>%
  filter(!is.na(rank))  # remove unranked


# Assign point values (Rank 1 = 3 pts, Rank 2 = 2 pts, Rank 3 = 1 pt)
reasons_scored <- reasons_data %>%
  mutate(score = case_when(
    rank == 1 ~ 3,
    rank == 2 ~ 2,
    rank == 3 ~ 1,
    TRUE ~ 0
  ))

# Sum total points per reason
reason_scores_by_wave <- reasons_scored %>%
  group_by(wave, reason) %>%
  dplyr::summarise(total_score = sum(score, na.rm = TRUE), .groups = "drop")

# Labels
labels <- c(
  "Uncertain if the product is still fresh/edible",
  "Products spoiled too quickly",
  "Best-before date exceeded",
  "No longer visually appealing",
  "Products forgotten",
  "Products were already spoiled/damaged when bought",
  "Cooked too much",
  "Did not consider shelf life when shopping",
  "Incorrect storage",
  "Bought too much / miscalculated",
  "No idea for further use"
)

# Assign labels
reason_scores_by_wave$reason <- rep(labels, 2)  # wave 1 and 2

# Normalize scores
reason_scores_norm <- reason_scores_by_wave %>%
  group_by(wave) %>%
  mutate(percentage = total_score / sum(total_score)) %>%
  ungroup()

# Pie Chart
ggplot(reason_scores_norm, aes(x = "", y = percentage, fill = reason)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  facet_wrap(~wave, labeller = as_labeller(c("1" = "Pre", "2" = "Post"))) +
  labs(
    title = "Reasons for Food Waste by Wave",
    fill = "Reason"
  ) +
  theme_void() +
  theme(
    legend.position = "right",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
    plot.title = element_text(margin = margin(b = 20))
  )




# -----------------------------------------
# 3.3 Demographics 
# -----------------------------------------

# Select demographic variables 
CFW_selected <- CFW %>%
  select(
    Gender = gender,
    Age = age,
    Education = education,
    Income = income,
    Household_Size = hh_members,
    Housing_Type = cohabitation
  ) %>%
  mutate(Household_Size = as.numeric(Household_Size))

CFW_selected <- CFW_selected %>%
  mutate(across(where(is.factor), droplevels))

descriptive_table <- CFW_selected %>%
  tbl_summary(
    type = list(Household_Size ~ "continuous"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_caption("**Table 1. Descriptive Statistics of the Sample (M, SD / n, %)**")

# Save as Word
descriptive_table %>%
  as_flex_table() %>%
  save_as_docx(path = "Descriptive_Table.docx")






# -----------------------------------------
# 3.4 Behavorial Advices 
# -----------------------------------------

# Variable selection
tip_vars <- paste0("Tip_", 1:5)

# Assign labels
labels <- c(
  Tip_1 = "Store fruits and vegetables separately",
  Tip_2 = "Store food with breathable packaging",
  Tip_3 = "Organize the fridge optimally",
  Tip_4 = "What does not belong in the fridge",
  Tip_5 = "Remove leafy greens"
)

# Bring wave 2 data into long format
df_long_wave2 <- CFW %>%
  filter(wave == 2) %>%
  select(all_of(tip_vars)) %>%
  pivot_longer(cols = everything(), names_to = "tip", values_to = "response") %>%
  mutate(
    response = case_when(
      response == 1 ~ "not yet",
      response == 2 ~ "partially",
      response == 3 ~ "already",
      response == -9 ~ "no answer",
      TRUE ~ NA_character_
    ),
    response = factor(response, levels = c("not yet", "partially", "already", "no answer")),
    tip_label = labels[tip]
  ) %>%
  drop_na(response)

# Plot
ggplot(df_long_wave2, aes(x = tip_label, fill = response)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Familiarity with Storage Tips",
    x = "Tip",
    y = "Number of Participants",
    fill = "Response"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# Define variable groups and labels
knowledge_vars <- paste0("FE19_0", 1:5)
application_vars <- paste0("FE20_0", 1:5)
helpfulness_vars <- paste0("FE21_0", 1:5)

tip_names <- c(
  "Store fruits and vegetables separately",
  "Store food in breathable packaging",
  "Organize the fridge optimally",
  "Items that do not belong in the fridge",
  "Remove leafy greens"
)

# --- Tip Knowledge ---

knowledge_long <- CFW %>%
  select(wave, all_of(knowledge_vars)) %>%
  pivot_longer(-wave, names_to = "tip", values_to = "knowledge") %>%
  mutate(
    tip = factor(tip, levels = knowledge_vars, labels = tip_names),
    knowledge = factor(knowledge, levels = c(1, 2, 3), 
                       labels = c("Not yet", "Partially", "Already"))
  ) %>%
  filter(!is.na(knowledge))

knowledge_plot <- ggplot(knowledge_long, aes(x = tip, fill = knowledge)) +
  geom_bar(position = "fill") +
  labs(title = "Knowledge of Tips", x = NULL, y = "Proportion") +
  scale_fill_brewer(palette = "YlGnBu", name = "Knowledge Level") +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.text = element_text(size = 11)
  )

ggsave("Tip_Knowledge.png", plot = knowledge_plot, width = 8, height = 5, dpi = 300)

# --- Tip Application ---

application_long <- CFW %>%
  select(wave, all_of(application_vars)) %>%
  pivot_longer(-wave, names_to = "tip", values_to = "application") %>%
  mutate(
    tip = factor(tip, levels = application_vars, labels = tip_names),
    application = factor(application, levels = c(1, 2), 
                         labels = c("Not selected", "Selected"))
  ) %>%
  filter(!is.na(application))

application_plot <- ggplot(application_long, aes(x = tip, fill = application)) +
  geom_bar(position = "fill") +
  labs(title = "Application of Tips", x = NULL, y = "Proportion") +
  scale_fill_brewer(palette = "YlGnBu", name = "Application") +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.text = element_text(size = 11)
  )

ggsave("Tip_Application.png", plot = application_plot, width = 8, height = 5, dpi = 300)

# --- Tip Helpfulness ---

helpfulness_long <- CFW %>%
  select(wave, all_of(helpfulness_vars)) %>%
  pivot_longer(-wave, names_to = "tip", values_to = "helpfulness") %>%
  mutate(
    tip = factor(tip, levels = hilfreich_vars, labels = tip_names),
    helpfulness = factor(helpfulness, levels = c(1, 2), 
                         labels = c("Not selected", "Selected"))
  ) %>%
  filter(!is.na(helpfulness))

helpfulness_plot <- ggplot(helpfulness_long, aes(x = tip, fill = helpfulness)) +
  geom_bar(position = "fill") +
  labs(title = "Perceived Helpfulness of Tips", x = NULL, y = "Proportion") +
  scale_fill_brewer(palette = "YlGnBu", name = "Helpfulness") +
  theme_minimal() +
  coord_flip() +
  theme(
    axis.text = element_text(size = 11)
  )

ggsave("Tip_Helpfulness.png", plot = helpfulness_plot, width = 8, height = 5, dpi = 300)





# -----------------------------------------
# 3.5 Correlation Table  
# -----------------------------------------
# Select variables
cor_data <- CFW %>%
  select(wastepd_root,
         M_Awareness,
         M_Attitude,
         M_Injunctive_norm,
         M_Descriptive_norm,
         M_Intention,
         O_Accessibility,
         O_Equipment,
         O_Events,
         O_Availability,
         A_Planning,
         A_Cooking,
         A_Edibility,
         A_Storage)

# Calculate Spearman correlations
cor_result <- corr.test(cor_data,
                        method = "spearman",
                        use = "pairwise.complete.obs",
                        adjust = "none")

# Print correlations
print(round(cor_result$r, 2))  # Correlation coefficients
print(round(cor_result$p, 4))  # p-values

# Optional visualization
ggcorrplot(cor_result$r,
           method = "circle",
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           tl.cex = 10,
           colors = c("red", "white", "blue"),
           title = "Spearman Correlations with wastepd_root")







# ================================================================
# 4. Linear Mixed Model
# ================================================================


# -----------------------------------------
# 4.1 Preparation
# -----------------------------------------

# Exclude outliers 
Q1 <- quantile(CFW$wastepd, .25)
Q3 <- quantile(CFW$wastepd, .75)
IQR <- IQR(CFW$wastepd)

# Subset data where values are within 1.5*IQR of Q1 and Q3
CFW <- CFW %>%
  mutate(wastepd_no_outliers = case_when(wastepd > (Q1 - 1.5 * IQR) & CFW$wastepd < (Q3 + 1.5 * IQR) ~ wastepd,
                                         TRUE ~ NA),
         wastepd_no_outliers_log = log(wastepd_no_outliers))

#######

# Check missing cases 
# List all variables used in the model
model_vars <- c(
  "wastepd_root", "wave", "Intervention",
  "A_Planning", "A_Storage", "A_Edibility", "A_Cooking",
  "O_Events", "O_Availability", "PCode"
)

# Subset the data to these variables
model_data <- CFW[, model_vars]

# Count number of rows with at least one NA
num_missing_rows <- sum(!complete.cases(model_data))

# Print result
cat("Number of rows with missing values in model variables:", num_missing_rows, "\n")


# Rescale variables 
CFW <- CFW %>%
  mutate(across(
    c(M_Intention, M_Attitude, M_Injunctive_Norm, M_Descriptive_Norm, M_Awareness, 
      A_Planning, A_Cooking, A_Edibility, A_Storage, 
      O_Accessibility, O_Equipment, O_Events, O_Availability),
    ~ scale(.)[, 1],
    .names = "{.col}_s"
  ))


# Check amount of complete cases for model 

# List all variables used in the model
vars_used <- c("wastepd_root", "wave", "IV", "M_Intention", 
               "A_Storage", "O_Events", "O_Availability", 
               "O_Equipment", "O_Accessibility", "PCode")

# Subset only the relevant columns
model_data <- na.omit(CFW[, vars_used])

# Count complete cases 
nrow(model_data)

# Count unique participants 
length(unique(model_data$PCode))


# -----------------------------------------
# 4.2 Main Model (waste as DV)
# -----------------------------------------


# 4.2.1 Calculate models 

# Baseline model
model_base <- glmmTMB(wastepd_root ~ wave * IV + (1 | PCode),
                      data = CFW,
                      ziformula = ~ 1,  # model for the zero-inflation part
                      family = gaussian
)

summary(model_base)
AIC(model_base)

# Intermediate model
model_intermediate <- glmmTMB(
  wastepd_root ~ wave * IV + M_Intention +
    A_Storage + 
    O_Events + O_Availability + O_Equipment + O_Accessibility +
    (1 | PCode),
  family = gaussian(link = "identity"),
  data = CFW,
  ziformula = ~ 1,
  na.action = na.omit,
  control = glmmTMBControl(
    optCtrl = list(iter.max = 10000, eval.max = 10000)
  )
)

summary(model_intermediate)
AIC(model_intermediate)

nrow(model.frame(model_intermediate))

# Full model
model_full <- glmmTMB(
  wastepd_root ~ wave * IV + 
    M_Intention +
    A_Planning + A_Storage + A_Edibility + A_Cooking +
    O_Events + O_Availability + O_Equipment + O_Accessibility +
    (1 | PCode),
  family = gaussian(link = "identity"),
  data = CFW,
  ziformula = ~ 1,
  na.action = na.omit
)

summary(model_full)


############

# Output 

coefs <- round((summary(model_intermediate)$coefficients$cond),3)
coefs



# 4.2.2 Check Assumptions 

# Simulate residuals
res <- simulateResiduals(model_intermediate)

# Plot diagnostics
plot(res)

testResiduals(res)

qqnorm(residuals(model_intermediate))

# Check random effects
ranef(model_intermediate)

# Check multicollinearity 
model_lm <- lm(wastepd_root ~ wave * IV + M_Intention +
                 A_Storage + 
                 O_Events + O_Availability + O_Equipment + O_Accessibility,
               data = CFW)
vif(model_lm)

# Test zero-inflation 
testZeroInflation(res)

# Test dispersion
testDispersion(res)

# Test for outliers 
testOutliers(res)

#######################################################################

# -----------------------------------------
# 4.2 LMMs for Ability variables 
# -----------------------------------------

library(lmerTest)

# Model for A_Planning
model_planning <- lmer(A_Planning ~ wave * Intervention + (1 | PCode), data = CFW)
summary(model_planning)

# Model for A_Storage
model_storage <- lmer(A_Storage ~ wave * IV + (1 | PCode), data = CFW)
summary(model_storage)

# Model for A_Edibility
model_edibility <- lmer(A_Edibility ~ wave * Intervention + (1 | PCode), data = CFW)
summary(model_edibility)

# Model for A_Cooking
model_cooking <- lmer(A_Cooking ~ wave * Intervention + (1 | PCode), data = CFW)
summary(model_cooking)

# Combine p-values
# Function to get p-values from lmerTest models
get_pvals <- function(model) {
  as.data.frame(summary(model)$coefficients)[, "Pr(>|t|)"]
}

# Extracting p-values for each model
pvals_planning <- get_pvals(model_planning)
pvals_storage <- get_pvals(model_storage)
pvals_edibility <- get_pvals(model_edibility)
pvals_cooking <- get_pvals(model_cooking)

# Combine all into one named list or data frame
pvals_all <- list(
  Planning = pvals_planning,
  Storage = pvals_storage,
  Edibility = pvals_edibility,
  Cooking = pvals_cooking
)

# Combine all p-values into one named vector
pvals_all_named <- c(pvals_planning, pvals_storage, pvals_edibility, pvals_cooking)

# Apply Benjamini-Hochberg correction 
p.adjust(pvals_all_named, method = "BH")


# Check Assumptions for Storage model

# Residuals and fitted values
res <- residuals(model_storage)
fitted_vals <- fitted(model_storage)

# Linearity and Homoscedasticity
plot(fitted_vals, res,
     main = "Residuals vs Fitted",
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, col = "red")

# 2. Normality of residuals
qqnorm(res)
qqline(res, col = "red")

hist(res, breaks = 20, main = "Histogram of Residuals", xlab = "Residuals")

shapiro.test(res)

# Simulate residuals to check distribution
simres <- simulateResiduals(fittedModel = model_storage)
plot(simres)  
summary(simres)


