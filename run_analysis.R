library(ggplot2)

source("R/analysis.R")

#------------- COOPERATION COST PREVALENCE SERIES -------------
make_coopcost_prevalence_plot <- function(save_path = file.path("Figures", 
                                                                "coopcost_prevalence.png"),
                                          coop_costs = c(0.0, 0.1, 0.249, 0.25),
                                          L = 20,
                                          stop_step = 25,
                                          width = 8.25, height = 3.85,
                                          overwrite = F){

  p <- 
    plot_costs_prevalences(
      coop_costs = coop_costs, stop_step = stop_step, 
      overwrite = overwrite, L = L
    )
  
  ggsave(save_path, p, width = width, height = height, dpi = 300)
}


#------------- EFFECT OF COOPERATION COST ON PREVALENCE AT t = T ----------
plot_outcomes_over_coopcost <- function (data_path = file.path("Data", 
                                                               "coopcost_outcomes.csv"),
                                         save_path = file.path("Figures", 
                                                               "coopcost_outcomes.png"),
                                         width = 8.25, height = 4.5,
                                         breaks = c(0.0, 0.1, 0.2, 0.25, 0.3)) {
  
  # Assume that outcomes aggregated only to trial level
  outcomes <- readr::read_csv(data_path)

  # outcomes$coop_cost <- factor(outcomes$coop_cost, unique(outcomes$coop_cost))
  outcomes$coop_cost <- as.numeric(outcomes$coop_cost)

  p <- 
    ggplot(outcomes, aes(x = as.numeric(coop_cost), y = Prevalence)) +
    # ***NEXT*** with a test using three coop costs around the critical point:
    # ggplot(mig_outcomes_full, aes(x = as.numeric(migration_rate), y = Prevalence, color = coop_cost, linetype = coop_cost)) +
      geom_jitter(size = 2, width = 0.005, alpha = 0.7, color = "dodgerblue") +
      stat_summary(fun = mean, geom = "line", color = "dodgerblue", linewidth = 1.2) +
      scale_x_continuous(
    name = TeX("Cooperation cost, $\\gamma$"),
    breaks = breaks, labels = scales::number_format(accuracy = 0.001)

  ) +
      # scale_x_continuous(
      #   breaks = seq_along(levels(outcomes$coop_cost)), 
      #   # breaks = seq_along(levels(outcomes$coop_cost)), 
      #   labels = levels(outcomes$coop_cost)
      # ) + 
      xlab(TeX("Cooperation cost, $\\gamma$")) + 
      ylab(paste0("Cooperator prevalence at t = 50")) + 
      theme_classic(base_size = 14) 

  ggsave(save_path, p, width = width, height = height, dpi = 300)
}


#------------- MIGRATION PREVALENCE SERIES -------------
make_migration_prevalence_plot <- function(save_path = 
                                            file.path("Figures", 
                                                      "migration_prevalence.png"),
                                           stop_step = 25,
                                           migration_rates = c(0.0, 0.075, 0.1125), 
                                           width = 8.25, height = 4.5) {
  p <- 
    plot_migration_prevalence(
      migration_rates = migration_rates,
      stop_step = stop_step, coop_cost = 0.2, L = 20, 
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


# Plot-making helpers
make_coopcost_plots <- function() {
  make_coopcost_prevalence_plot()
  plot_outcomes_over_coopcost()
}

make_migration_plots <- function() {
  make_migration_prevalence_plot()
  plot_outcomes_over_migration()
}

make_all_plots <- function() {
  make_migration_plots()
  make_coopcost_plots()
}
