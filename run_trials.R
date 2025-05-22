source("R/analysis.R")

# Run all trials 
run_all_migration_trials <- function(overwrite = T, 
                                     migration_rates = seq(0.0, 0.175, 0.025),     
                                     n_trials_per_param = 3, 
                                     L = 21,
                                     .dev = TRUE) {
  if (.dev) {
    n_trials_per_param = 3, 
        
  }
  migration_outcomes_summary(
    migration_rates = migration_rates,
    n_trials_per_param = n_trials_per_param,
    L = L,
    # Never overwrite
    overwrite = F
  ) 
}


run_all <- function(sync_dir = "Data/", sync_root = NULL) {
  
  .make_save_path <- function(sync_dir, sync_root, final_part) {
    return (file.path(sync_dir, sync_root, final_part))
  }
  
  run_all_coopcost_trials(.make_save_path("coopcost.csv"))
  run_all_migration_trials(.make_save_)
}


# run_all()
