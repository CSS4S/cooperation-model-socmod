source("R/analysis.R")


#------------------- COOPERATION COST ANALYSES ------------

# coopcost_outcomes_full <- coopcost_summary_final_step_tbl()


# tbl_final_step <- coopcost_summary_final_step_tbl(
  # n_trials_per_param = 10, stop_step = 50, L=21, overwrite = F
# )


#------------------- MIGRATION ANALYSES -------------------

# mig_outcomes_full <- 
#   migration_outcomes_summary(
#     overwrite = T, 
#     migration_rates = seq(0.0, 0.2, 0.025),
#     n_trials_per_param = 20, 
#     L = 21
# )


# Run all trials 
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

