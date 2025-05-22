library(ggplot2)

#------------- COOPERATION COST PREVALENCE SERIES -------------



p <- plot_final_step_prevalence(


#------------- EFFECT OF COOPERATION COST ON PREVALENCE AT t = T ----------


#------------- MIGRATION PREVALENCE SERIES -------------



make_migration_prevalence_plot <- function(save_path = 
                                            file.path("Figures", 
                                                      "migration_outcomes.png"),
                                           migration_rates = c(0.0, 0.075, 0.1125), 
                                           width = 8.25, height = 4.5) {
  p <- 
    plot_migration_prevalence(
      migration_rates = migration_rates,
      stop_step = 50, coop_cost = 0.2, L = 20, 
      n_trials_per_param = 3, overwrite = T
    )

  ggsave(save_path, p, width = width, height = height, dpi = 300)
}

#------------- EFFECT OF MIGRATION ON PREVALENCE AT t=T  -------------

plot_outcomes_over_migration <- function(data_path = file.path("Data", 
                                                               "migration_outcomes.csv"),
                                         save_path = file.path("Figures", 
                                                               "migration_outcomes.png"),
                                         width = 8.25, height = 4.5) {

  # Assume that mig_outcomes_full has been aggregated only to the Trial level
  mig_outcomes_full <- readr::read_csv(data_path)

  mig_outcomes_full$migration_rate <- factor(mig_outcomes_full$migration_rate,
                                             unique(mig_outcomes_full$migration_rate))

  # Create plot by plotting each 
  p <- 
    ggplot(mig_outcomes_full, aes(x = as.numeric(migration_rate), y = Prevalence)) +
    # ***NEXT*** with a test using three coop costs around the critical point:
    # ggplot(mig_outcomes_full, aes(x = as.numeric(migration_rate), y = Prevalence, color = coop_cost, linetype = coop_cost)) +
      geom_jitter(size = 2, width = 0.1, alpha = 0.7, color = "dodgerblue") +
      stat_summary(fun = mean, geom = "line", color = "dodgerblue", linewidth = 1.2) +
      scale_x_continuous(
        breaks = seq_along(levels(mig_outcomes_full$migration_rate)), 
        labels = levels(mig_outcomes_full$migration_rate)
      ) + 
      xlab("Migration rate") + 
      ylab(paste0("Cooperator prevalence at t = 50")) + 
      theme_classic(base_size = 14) 

  ggsave(save_path, p, width = width, height = height, dpi = 300)
}
