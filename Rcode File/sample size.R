# Read the CSV file into a data frame
pilot_data <- read.csv("D:/Second Semester/Project Multivariate and Hierarchical Data/G10.pilot.data.csv")
pilot_data
pilot_data$climate_scenario <- 1

View(pilot_data)
summary(pilot_data)
# Replace 'PearQuality' with the actual column name in your CSV file
mean_value <- mean(pilot_data$quality_index, na.rm = TRUE)
# within variblity 
variance_value <- var(pilot_data$quality_index, na.rm = TRUE)
sd_value <- sd(pilot_data$quality_index, na.rm = TRUE)

# Print the results
cat("Mean:", mean_value, "\n")
cat("Variance:", variance_value, "\n")
cat("Standard Deviation:", sd_value, "\n")

# Assuming you have already read your pilot data into a data frame called 'pilot_data'
# and that the column containing the pear quality score is named "PearQuality".

# Fit a one-sample linear model (intercept-only model)
control_model <- lm(quality_index ~ climate_scenario, data = pilot_data)
#control_model <- lm(quality_index ~ 1, data = pilot_data)
summary(control_model)
qqnorm(resid(control_model), main = "Q-Q Plot of Model Residuals")
qqline(resid(control_model), col = "red", lwd = 2)



# Summarize the model
summary(control_model)
summary(pilot_data$quality_index)
boxplot(pilot_data$quality_index, main = "Quality Index")

# Count the number of pears per type
pear_counts <- table(pilot_data$species)
print(pear_counts)
# Perform an independent two-sample t-test:
t_test_result <- t.test(quality_index ~ species, data = pilot_data)
print(t_test_result)
# Create a boxplot comparing quality scores by pear type
boxplot(quality_index ~ species, data = pilot_data,
        main = "Quality Score by Pear Type",
        xlab = "Pear Type",
        ylab = "Quality Score",
        col = c("lightblue", "lightgreen"))




# in this simulation we compared only two groups the control and one of other 
# climate intervention . here i chosed one climate intervention and not all intervention.
# as the sample size that we get can apply on the all other scenarios .
# as they might have approximately same numbers or close number of pears
# with N= 190 we can see that the power is 85%


set.seed(444)
N <- 190
alpha <- 0.05
delta <- 10    # effect size
sdChoice <- 35.94
numberSimulation <- 5000
pval <- numeric(numberSimulation) # here we will store the p-values

for (i in 1:numberSimulation){
  controlGroup <- rnorm(N, mean = 51.08, sd = sdChoice)
  treatedGroup <- rnorm(N, mean = 51.08 + delta, sd = sdChoice)   # correctly named variable
  
  # Perform the one-sided t-test (testing if control < treated)
  pval[i] <- t.test(controlGroup, treatedGroup, alternative = "less",
                    mu = 0, paired = FALSE, var.equal = TRUE, conf.level = 1 - alpha)$p.value
}

head(pval)
power_estimate <- sum(pval < alpha) / numberSimulation
cat("Estimated power:", power_estimate, "\n")



# Set parameters
set.seed(333)
alpha <- 0.05  # Using overall alpha because ANOVA handles all comparisons at once
n_sims <- 5000
N <- 212  # Sample size per group (you can adjust this)
delta <- 10  # Expected improvement for scenarios
mean_control <- 51.08
sd_common <- 35.94

# Define group means
group_means <- c(mean_control,
                 mean_control + delta,
                 mean_control + delta,
                 mean_control + delta)

# Storage for p-values
p_values <- numeric(n_sims)

# Simulation loop
for (i in 1:n_sims) {
  all_scores <- numeric()
  all_groups <- numeric()
  
  for (g in 1:4) {
    scores <- rnorm(N, mean = group_means[g], sd = sd_common)
    all_scores <- c(all_scores, scores)
    all_groups <- c(all_groups, rep(g, N))
  }
  
  group_factor <- factor(all_groups, labels = c("Control", "Scenario2", "Scenario3", "Scenario4"))
  
  # Fit ANOVA
  model <- aov(all_scores ~ group_factor)
  p_values[i] <- summary(model)[[1]][["Pr(>F)"]][1]
}

# Estimate power
power_estimate <- mean(p_values < alpha)
cat("Estimated power for N =", N, "per group is:", power_estimate, "\n")
#------------------------------------------------------------------------------
set.seed(888)
n_sims <- 5000
N <- 160
alpha <- 0.017
delta <- 10
mean_control <- 51.08
sd <- 35.94

success_count <- 0

for (i in 1:n_sims) {
  control <- rnorm(N, mean = mean_control, sd = sd)
  scen2 <- rnorm(N, mean = mean_control + delta, sd = sd)
  scen3 <- rnorm(N, mean = mean_control + delta, sd = sd)
  scen4 <- rnorm(N, mean = mean_control + delta, sd = sd)
  
  p1 <- t.test(control, scen2, alternative = "less", var.equal = TRUE)$p.value
  p2 <- t.test(control, scen3, alternative = "less", var.equal = TRUE)$p.value
  p3 <- t.test(control, scen4, alternative = "less", var.equal = TRUE)$p.value
  
  if (p1 < alpha | p2 < alpha | p3 < alpha) {
    success_count <- success_count + 1
  }
}

power_estimate <- success_count / n_sims
cat("Estimated power across 3 t-tests (Bonferroni alpha = 0.017):", power_estimate, "\n")
