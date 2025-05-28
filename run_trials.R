source("R/analysis.R")


# Run all cooperation cost trials
run_all_coopcost_trials <- function(coop_costs = c(0.0, 0.1, 0.3),
                                    migration_rate = 0.0,
                                    n_trials_per_param = 3,
                                    L = 10,
                                    stop_step = 50,
                                    save_path = file.path("Data", "coopcost_outcomes.csv"),
                                    overwrite = F) {

  if (!file.exists(save_path) || overwrite) {
    outcomes <- coopcost_outcomes_summary(
      coop_costs = coop_costs,
      L = L,
      stop_step = stop_step,
      n_trials_per_param = n_trials_per_param,
    ) 

    readr::write_csv(outcomes, save_path)
  } else {
    
    outcomes <- readr::read_csv(save_path)
  }

  return (outcomes)
}


# Run all migration trials 
run_all_migration_trials <- function(migration_rates = seq(0.0, 0.175, 0.025),     
                                     coop_cost = 0.2,
                                     n_trials_per_param = 3, 
                                     L = 20,
                                     save_path = file.path("Data", "migration_outcomes.csv"),
                                     overwrite = F) {

  if (!file.exists(save_path) || overwrite) {
    outcomes <- migration_outcomes_summary(
      migration_rates = migration_rates,
      coop_cost = coop_cost,
      L = L,
      n_trials_per_param = n_trials_per_param,
      # Don't sync raw trial data in this version of the automated analysis
      overwrite = F
    ) 

    readr::write_csv(outcomes, save_path)
  } else {
    
    outcomes <- readr::read_csv(save_path)
  }

  return (outcomes)
}


run_all <- function(sync_dir = "Data/", sync_root = NULL) {
  
  .make_save_path <- function(sync_dir, sync_root, final_part) {
    return (file.path(sync_dir, sync_root, final_part))
  }
  
  run_all_coopcost_trials(.make_save_path("coopcost.csv"))
  run_all_migration_trials(.make_save_path("migration.csv")) 
}

