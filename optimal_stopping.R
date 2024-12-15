library(tidyverse)

# Parameters
n <- 100          # Number of candidates
p <- 0.37         # Proportion to observe (about 37% is optimal for large n)
simulations <- 1000  # Number of simulations

# Simulation Function
simulate_optimal_stopping <- function(n, p) {
  # Generate candidate rankings
  candidates <- tibble(
    id = 1:n,
    score = runif(n) # Random scores for candidates
  ) %>%
    arrange(desc(score)) %>%  # Sort by score (higher is better)
    mutate(rank = row_number()) # Assign ranks (1 is best)
  
  # Observation phase: decide threshold
  observed <- candidates %>%
    filter(id <= floor(p * n)) # Observe the first p*n candidates
  
  # Stopping phase: select the next best candidate after observation
  selected <- candidates %>%
    filter(id > floor(p * n)) %>%
    filter(score > max(observed$score)) %>%
    slice(1) # Select the first one that meets the condition
  
  # Return rank of the selected candidate (or NA if no candidate selected)
  selected_rank <- if (nrow(selected) > 0) selected$rank else NA
  return(selected_rank)
}

# Run simulations
results <- tibble(
  sim = 1:simulations,
  selected_rank = map_dbl(sim, ~ simulate_optimal_stopping(n, p))
)

# Analyze results
summary_results <- results %>%
  summarise(
    success_rate = mean(selected_rank == 1, na.rm = TRUE), # Success: Best candidate selected
    avg_rank = mean(selected_rank, na.rm = TRUE)          # Average rank of selected candidates
  )

# Print results
print(summary_results)

# Plot distribution of ranks
results %>%
  ggplot(aes(x = selected_rank)) +
  geom_histogram(bins = 20, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Ranks for Selected Candidates",
    x = "Rank of Selected Candidate",
    y = "Frequency"
  )