library(igraph)
library(socmod)

make_cooperation_model <- function(grid_height, grid_width, coop_benefit,
                                   coop_cost) {
  
  g <- make_lattice(dim = c(grid_height, grid_width), circular = FALSE)
  
  # Initialized like Smaldino _MSB_ Ch. 6
  abm <- 
    make_abm(learning_strategy = normal_game_strategy, 
             graph = g, 
             coop_benefit = coop_benefit, 
             coop_cost = coop_cost) %>% 
    
    initialize_agents(initial_prevalence = 0.5,
                      adaptive_behavior = "Cooperate",
                      legacy_behavior = "Defect")
  
  return (abm)
}


# Two individuals play game with one another; b = coop_benefit, c = coop_cost
play_game <- function(focal_agent, partner_agent, model) {
  # Default payoff: if both defect, this payoff_to_focal remains unchanged
  payoff_to_focal <- 0.0
  if (focal_agent$get_behavior() == "Cooperate") {
    # Both cooperate
    if (partner_agent$get_behavior() == "Cooperate") {
      return (model$coop_benefit - model$coop_cost)
    # Focal agent is betrayed
    } else {
      return (- model$coop_cost)
    }
  } else {
    # Focal agent defects, gets cooperative benefit
    if (partner_agent$get_behavior() == "Cooperate") {
      payoff_to_focal <-  model$coop_benefit
    } 
  }
}

play_game_with_neighbors <- function(focal_agent, model) {
  
  # Focal agent plays game with neighbors, accumulating payoffs
  total_payoff <- 
    sum(
      focal_agent$get_neighbors()$map(\(n) play_game(focal_agent, n, model))
    )
  
  focal_agent$set_next_fitness(total_payoff)
}


# For this normal game in this format we don't have individual partner selection 
# and interaction steps. Instead all is handled in the model_step, 
# i.e., coop_model_step defined below
normal_game_strategy <- make_learning_strategy(
  partner_selection = \(f, m) NULL,
  interaction = \(f, p, m) NULL,
  model_step = \(abm) coop_model_step,
  label = "Normal game strategy"
)


coop_model_step <- function(abm) {
  # Each agent plays game, receiving a total fitness from playing with each neighbor
  purrr::walk(
    abm$agents,
    \(agent) {
      play_game_with_neighbors(agent, abm)
      
      # Use success-biased learning
    }
  )
  
  
}