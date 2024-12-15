library(tidyverse)

generate_candidates = function(n) {
  # Generate random scores
  candidates = tibble(
    id = 1:n,                     # Candidate IDs
    score = runif(n)              # Random scores (higher is better)
  ) %>%
  arrange(desc(score)) %>%        # Sort by descending scores to assign ranks
  mutate(rank = row_number()) %>% # Assign ranks after sorting
  sample_frac(1) %>%              # Shuffle again to preserve random order
  mutate(sequence = row_number()) # Preserve random presentation order
  return(candidates)
}

observe_candidates = function(candidates, p) {
  num_observations = floor(p * nrow(candidates))
  observed = candidates %>%
    filter(sequence <= num_observations)
  return(observed)
}

select_candidate = function(candidates, observed) {
  # Determine the threshold (maximum score in observed data)
  threshold = max(observed$score)
  # Candidates after the observation phase
  selection_pool = anti_join(candidates, observed, by = "id")
  # Check if any candidate in the selection pool exceeds the threshold
  selected = selection_pool %>%
    filter(score > threshold) %>%
    slice_min(sequence, n = 1)
  # If no candidate is selected, fallback to the candidate with the largest sequence
  if (nrow(selected) == 0) {
    selected = selection_pool %>%
      slice_max(sequence, n = 1) # Select the candidate with the largest sequence
  }
  return(selected)
}

run_optimal_stopping = function(n, p) {
  candidates = generate_candidates(n)
  observed = observe_candidates(candidates, p)
  selected_candidate = select_candidate(candidates, observed)
  return(selected_candidate)
}

run_simulations = function(n, p, num_simulations) {
  selected_candidate_ranks = replicate(num_simulations, {
    selected_candidate = run_optimal_stopping(n, p)
    selected_candidate$rank
  })
  results = tibble(
    simulation = 1:num_simulations,
    rank = selected_candidate_ranks
  )
}

n = 100
num_simulations = 1000
results_p0.10 = run_simulations(n, 0.10, num_simulations) %>% mutate(p = ".10")
results_p0.37 = run_simulations(n, 0.37, num_simulations) %>% mutate(p = ".37")
results_p0.50 = run_simulations(n, 0.50, num_simulations) %>% mutate(p = ".50")
results_p0.90 = run_simulations(n, 0.90, num_simulations) %>% mutate(p = ".90")

results_all = bind_rows(results_p0.10,
                        results_p0.37,
                        results_p0.50,
                        results_p0.90) %>%
  mutate(p = as.factor(p))

# Plot grouped bar plot
results_all %>%
  count(p, rank) %>%  # Count occurrences of each rank per type
  ggplot(aes(x = rank, y = n, fill = p)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Frequency of Ranks by Simulation Type",
    x = "Rank of Selected Candidate",
    y = "Frequency",
    fill = "Simulation Type"
  ) +
  theme_minimal()

# Plot rank vs frequency
tmp = results_all %>% 
  group_by(p) %>%
  count(rank) %>%
  ungroup()
  
  
tmp %>% 
  ggplot(aes(x = rank, y = n, color = p)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Frequency of Ranks in Selected Candidates",
    x = "Rank of Selected Candidate",
    y = "Frequency"
  )



stats37 = results_df %>%
  summarise(
    success_rate = mean(rank == 1),    # Proportion of times the best candidate was selected
    avg_rank = mean(rank)             # Average rank of selected candidates
  )

n = 100
p = 0.50
num_simulations = 1000
results = replicate(num_simulations, {
  selected_candidate = run_optimal_stopping(n, p)
  selected_candidate$rank  
})
# Create a tibble of results
results_df = tibble(
  simulation = 1:simulations,
  rank = results
)
# Calculate statistics
stats = results_df %>%
  summarise(
    success_rate = mean(rank == 1),    # Proportion of times the best candidate was selected
    avg_rank = mean(rank)             # Average rank of selected candidates
  )



# Plot rank vs frequency
results_df %>%
  count(rank) %>%  # Count occurrences of each rank
  ggplot(aes(x = rank, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  labs(
    title = "Frequency of Ranks in Selected Candidates",
    x = "Rank of Selected Candidate",
    y = "Frequency"
  ) +
  theme_minimal()

