set.seed(12344)

# Parameters
alpha <- 0.05 / 4       # Bonferroni-corrected alpha
delta <- 10
# Expected improvement in quality score
sd <- 35.94             # Based on pilot control data
mean_control <- 51.08
n_sims <- 5000
N <- 160            # Candidate sample size per group
successes <- 0

for (i in 1:n_sims) {
  # Simulate 4 groups
  control <- rnorm(N, mean = mean_control, sd = sd)
  scenario2 <- rnorm(N, mean = mean_control + delta, sd = sd)
  scenario3 <- rnorm(N, mean = mean_control + delta, sd = sd)
  scenario4 <- rnorm(N, mean = mean_control + delta, sd = sd)
  
  # Perform one-sided t-tests (intervention > control)
  p1 <- t.test(scenario2, control, alternative = "greater", var.equal = TRUE)$p.value
  p2 <- t.test(scenario3, control, alternative = "greater", var.equal = TRUE)$p.value
  p3 <- t.test(scenario4, control, alternative = "greater", var.equal = TRUE)$p.value
  
  # Count success if at least one comparison is significant
  if (p1 < alpha | p2 < alpha | p3 < alpha) {
    successes <- successes + 1
  }
}

# Calculate estimated power
power_estimate <- successes / n_sims
cat("Estimated power (at least one scenario better than control) for N =", N, "is:", power_estimate, "\n")


library(ggplot2)

set.seed(122134)

# Define simulation parameters
alpha <- 0.05 / 3               # Bonferroni-corrected alpha
sd <- 35.94
mean_control <- 51.08
n_sims <- 1000                 # Reduce for faster plotting (5000 for final)

sample_sizes <- seq(10, 200, by = 8)   # Range of N values to try
effect_sizes <- c(10, 15, 20,25)            # Different deltas to simulate

# Empty data frame to store results
results <- data.frame()

# Loop over effect sizes and sample sizes
for (delta in effect_sizes) {
  for (N in sample_sizes) {
    successes <- 0
    
    for (i in 1:n_sims) {
      # Simulate 4 groups
      control <- rnorm(N, mean = mean_control, sd = sd)
      scenario2 <- rnorm(N, mean = mean_control + delta, sd = sd)
      scenario3 <- rnorm(N, mean = mean_control + delta, sd = sd)
      scenario4 <- rnorm(N, mean = mean_control + delta, sd = sd)
      
      # One-sided t-tests (scenario > control)
      p1 <- t.test(scenario2, control, alternative = "greater", var.equal = TRUE)$p.value
      p2 <- t.test(scenario3, control, alternative = "greater", var.equal = TRUE)$p.value
      p3 <- t.test(scenario4, control, alternative = "greater", var.equal = TRUE)$p.value
      
      if (p1 < alpha | p2 < alpha | p3 < alpha) {
        successes <- successes + 1
      }
    }
    
    power <- successes / n_sims
    results <- rbind(results, data.frame(N = N, delta = delta, power = power))
  }
}
library(dplyr)

results$alpha=0.017
results_sorted <- results %>%
  arrange(desc(N))
results_sorted


# Round power to nearest 0.01 and filter for desired levels


filtered_results <- results %>%
  filter(power >= 0.80 & power <= 0.90) %>%
  arrange(delta, N)

# View the filtered data
filtered_results


# Plot the results
ggplot(results, aes(x = N, y = power, color = factor(delta))) +
  geom_line(size = 1.2) +
  labs(title = "Power vs Sample Size for Different Effect Size",
       subtitle = "Each data point is based on simulation with Bonferroni-corrected alpha = 0.017",
       x = "Sample Size per Group",
       y = "Estimated Power",
       color = "Effect Size ") +
  theme_minimal() +
  geom_hline(yintercept = 0.85, linetype = "dashed", color = "gray40") +
  geom_hline(yintercept = 0.80, linetype = "dotted", color = "gray40") +
  geom_hline(yintercept = 0.90, linetype = "solid", color = "gray40") +
  theme(plot.title = element_text(face = "bold"))

