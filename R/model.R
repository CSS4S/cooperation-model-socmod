library(igraph)
library(socmod)


# Model generator for run_trials
cooperation_abm_gen <- function(params) {
  return (
    make_cooperation_model(
      params$grid_height, params$grid_width, params$coop_benefit, 
      params$coop_cost, params$disaster_cost
    )
  )
}


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
      learning_strategy = coop_game_strategy
    ) %>% 
    initialize_agents(initial_prevalence = 0.5,
                      adaptive_behavior = "Cooperate",
                      legacy_behavior = "Defect")
  
  # There's a bug in initialize_agents: fitness_next and behavior_next are not set,
  # which causes a problem in the model step in this model that would end up setting
  # some agents to do the "Legacy" behavior instead of 
  purrr::walk(abm$agents, \(a) a$set_next_behavior(a$behavior_current))
  
  return (abm)
}


# Two individuals play game with one another; b = coop_benefit, c = coop_cost
play_game <- function(focal_agent, partner_agent, model) {

  return (
    model$get_parameter("payoff_matrix")[
      focal_agent$behavior_current,
      partner_agent$behavior_current
    ]
  )
}


# Have the focal_agent play the coordination game with all neighbors
play_game_with_neighbors <- function(focal_agent, model) {
  # Focal agent plays game with neighbors, accumulating payoffs
  total_payoff <- 
    sum(purrr::map_vec(
      focal_agent$get_neighbors()$agents,
      \(neighbor) play_game(focal_agent, neighbor, model)
      )
    )
  # Focal agent's fitness gets set to the total_payoff
  focal_agent$fitness_current <- total_payoff
}


# Model step used to specify the LearningStrategy 
coop_model_step <- function(abm) {
  
  # Each agent plays game, receiving a total fitness from playing with each neighbor
  for (agent in abm$agents) {
    play_game_with_neighbors(agent, abm)
  }
  
  # After each agent plays, iterate through again to do success-biased learning,
  # but rescaling to deal with potentially negative fitness values
  purrr::walk(
    abm$agents,
    \(agent) {
      # Get focal agent's neighbors
      neighbors <- agent$get_neighbors()
      
      # Calculate unscaled weights
      weights <- purrr::map_vec(unname(neighbors$agents), \(n) n$fitness_current)
      # cat("weights:\n", weights, "\n")
      max_idx <- which.max(weights)
      # cat("max_idx:\n", max_idx, "\n")
      
      teacher <- neighbors$agents[[max_idx]]
      # Only use teacher's behavior if they have a higher
      if (teacher$fitness_current > agent$fitness_current) {
        agent$set_next_behavior(teacher$behavior_current)
      }
      
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