source("R/analysis.R")

#------------------- COOPERATION COST ANALYSES ------------

coopcost_outcomes_full <- coopcost_summary_final_step_tbl()

tbl_final_step <- coopcost_summary_final_step_tbl(n_trials_per_param = 10, stop_step = 50, L=21, overwrite = F)



#------------------- MIGRATION ANALYSES -------------------


mig_outcomes_full <- 
  migration_outcomes_summary(
    overwrite = T, 
    migration_rates = seq(0.0, 0.2, 0.025),
    n_trials_per_param = 20, 
    L = 21
)


# Run all trials 
run_all_migration_trials <- function(overwrite = T, 
                                     migration_rates = seq(0.0, 0.175, 0.025),     
                                     coop_costs = c(0.2),
                                     n_trials_per_param = 3, 
                                     L = 20,
                                     .dev = TRUE) {

  if (.dev) {
    n_trials_per_param <- 3, 
    migration_rates <- c(0.0, 0.05, 0.1, 0.175)
    L <- 10
    coop_costs <- c(0.2)
  }

  migration_outcomes_summary(
    migration_rates = migration_rates,
    coop_costs = coop_costs,
    L = L,
    n_trials_per_param = n_trials_per_param,
    # Never overwrite in this version of the automated analysis
    overwrite = F
  ) 
}


run_all <- function(sync_dir = "Data/", sync_root = NULL) {
  
  .make_save_path <- function(sync_dir, sync_root, final_part) {
    return (file.path(sync_dir, sync_root, final_part))
  }
  
  run_all_coopcost_trials(.make_save_path("coopcost.csv"))
  run_all_migration_trials(.make_save_path("migration.csv")) 
}


# run_all()
