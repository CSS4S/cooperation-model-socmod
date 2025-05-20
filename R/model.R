library(igraph)
library(socmod)

make_cooperation_model <- function(grid_height = 11, grid_width = 11, 
                                   coop_benefit = 1.0, coop_cost = 0.25,
                                   disaster_cost = 0.0) {
  # Set up spatial grid
  g <- make_lattice(
    dimvector = c(grid_height, grid_width), periodic = FALSE
  )
  # Define payoff matrix: row 1 is for focal cooperator, row 2 focal defector
  payoff_matrix <- matrix(
    c(coop_benefit - coop_cost,  -1.0 * coop_cost, 
      coop_benefit,              -1.0 * disaster_cost),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      "Focal" = c("Cooperate", "Defect"),
      "Partner" = c("Cooperate", "Defect")
    )
  )
  
  # Initialized like Smaldino _MSB_ Ch. 6
  abm <- 
    make_abm(
      graph = g, 
      coop_benefit = coop_benefit, 
      coop_cost = coop_cost,
      payoff_matrix = payoff_matrix,
      learning_strategy = normal_game_strategy
    ) %>% 
    initialize_agents(initial_prevalence = 0.5,
                      adaptive_behavior = "Cooperate",
                      legacy_behavior = "Defect")
  
  return (abm)
}


# Two individuals play game with one another; b = coop_benefit, c = coop_cost
play_game <- function(focal_agent, partner_agent, model) {
  return (
    model$payoff_matrix[
      focal_agent$behavior_current,
      partner_agent$behavior_current
    ]
  )
}
#   # Default payoff: if both defect, this payoff_to_focal remains unchanged
#   payoff_to_focal <- 0.0
#   
#   if (focal_agent$get_behavior() == "Cooperate") {
#     # Both cooperate
#     if (partner_agent$get_behavior() == "Cooperate") {
#       payoff_to_focal <- model$coop_benefit - model$coop_cost
#     # Focal agent is betrayed
#     } else {
#       payoff_to_focal <- -1.0 * model$coop_cost
#     }
#   } else {
#     # Focal agent defects, gets cooperative benefit
#     if (partner_agent$get_behavior() == "Cooperate") {
#       payoff_to_focal <-  model$coop_benefit
#     } 
#   }
#   
#   return (payoff_to_focal)
# }

# Have the focal_agent play the coordination game with all neighbors
play_game_with_neighbors <- function(focal_agent, model) {
  # Focal agent plays game with neighbors, accumulating payoffs
  total_payoff <- 
    sum(unlist(
      # Use the Neighbors$map function to play with all neighbors
      focal_agent$get_neighbors()$map(
        \(n) play_game(focal_agent, n, model)
      )
    ))
  # Focal agent's fitness gets set to the total_payoff
  focal_agent$fitness_current <- total_payoff
}


coop_model_step <- function(abm) {
  # Each agent plays game, receiving a total fitness from playing with each neighbor
  purrr::walk(
    abm$agents,
    \(agent) {
      play_game_with_neighbors(agent, abm)
    }
  )
  
  # After each agent plays, iterate through again to do success-biased learning
  purrr::walk(
    abm$agents,
    \(agent) {
      # For this application we need to manually check if 
      neighbors <- agent$get_neighbors()
      tot_neighbor_fitnesses <- 
        sum(unlist(neighbors$map(\(n) n$get_fitness())))
      # If all neighbors fitnesses are zero
      if (tot_neighbor_fitnesses == 0.0) {
        teacher <- neighbors$sample()
      } else {
        teacher <- success_bias_select_teacher(agent, abm)
      }
      agent$set_next_behavior(teacher$get_behavior())
      
      # Agent-level fitness resets after each round (i.e. time step in this case)
      agent$set_next_fitness(0.0)
    }
  )
  
  # Use socmod-provided model step function for learning given next_behavior/fitness
  iterate_learning_model(abm)
}


# For this normal game in this format we don't have individual partner selection 
# and interaction steps. Instead all is handled in the model_step, 
# i.e., coop_model_step defined below
coop_game_strategy <- make_learning_strategy(
  partner_selection = \(f, m) NULL,
  interaction = \(f, p, m) NULL,
  model_step = coop_model_step,
  label = "Normal game strategy"
)