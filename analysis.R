library(socmod)
library(ggplot2)
library(dplyr)
library(purrr)
library(latex2exp)


source("R/model.R")

plot_costs_prevalences <- function(trials = NULL, coop_costs = c(0.235, 0.265), 
                                   L = 21, step_final = 50, overwrite = T) {
  
  if (is.null(trials)) {
    trials <- 
      run_trials(
        cooperation_abm_gen, 
        n_trials_per_param = 3, 
        grid_height = L, 
        grid_width = L, 
        coop_benefit = 1.0, 
        coop_cost = coop_costs, 
        disaster_cost = 0.0, 
        stop = step_final, 
        .progress = TRUE, 
        syncfile = "three-trials-two-costs.RData", 
        overwrite = overwrite
      )
  }

  prevalence_summary <- 
    summarise_prevalence(
      trials, 
      input_parameters = "coop_cost",
      tracked_behaviors = c("Cooperate"),
      across_trials = FALSE
    ) %>%
    dplyr::mutate(
      coop_cost = factor(coop_cost, unique(coop_cost))
    )
  
  p <- 
    ggplot(
      prevalence_summary,
      aes(x=Step, y=Prevalence,
          color=coop_cost,
          linetype=coop_cost,
          group = trial_id)
    ) +
    geom_line(linewidth=1.4, alpha = 0.875) + 
    guides(linetype=guide_legend(title=TeX("Cooperation cost, $\\gamma$"))) +
    scale_color_manual(values = unname(SOCMOD_PALETTE), name = TeX("Cooperation cost, $\\gamma$")) +
    ylab("Cooperator prevalence") +
    theme_classic(base_size = 16)

  return (p)
}


costs_outcomes_visualization <- function(trials = NULL, stop_step = 40,
                                         coop_costs = seq(0.175, 0.325, 0.05),
                                         n_trials_per_param = 10,
                                         overwrite = F) {
  if (is.null(trials)) {
    trials <- 
      run_trials(
        cooperation_abm_gen, 
        n_trials_per_param = 3, 
        grid_height = 21, 
        grid_width = 21, 
        coop_benefit = 1.0, 
        coop_cost = coop_costs, 
        disaster_cost = 0.0, 
        stop = stop_step, 
        .progress = TRUE, 
        syncfile = "costs-experiment.RData", 
        overwrite = overwrite
      )
  }
  
  prevalence_summary_final_step <- 
    summarise_prevalence(
      trials, 
      input_parameters = "coop_cost",
      tracked_behaviors = c("Cooperate"),
      across_trials = FALSE
    ) %>%
    mutate(
      coop_cost = factor(coop_cost, unique(coop_cost))
    ) %>%
    filter(Step == stop_step) %>%
    group_by(coop_cost) %>%
    summarise()
  
  
}
  
costs_migration_visualization <- function() {}


prevalence_summary_final_step_tbl <- function(trials = NULL, stop_step = 40,
                                          coop_costs = seq(0.175, 0.325, 0.05),
                                          n_trials_per_param = 5,
                                          L = 21,
                                          overwrite = F) {
  if (is.null(trials)) {
    trials <- 
      run_trials(
        cooperation_abm_gen, 
        n_trials_per_param = n_trials_per_param, 
        grid_height = L, 
        grid_width = L, 
        coop_benefit = 1.0, 
        coop_cost = coop_costs, 
        disaster_cost = 0.0, 
        stop = stop_step, 
        .progress = TRUE, 
        syncfile = "costs-experiment.RData", 
        overwrite = overwrite
      )
  }
  
  return (
    summarise_prevalence(
      trials, 
      input_parameters = "coop_cost",
      tracked_behaviors = c("Cooperate"),
      across_trials = FALSE
    ) %>%
    mutate(
      coop_cost = factor(coop_cost, unique(coop_cost))
    ) %>%
    group_by(trial_id) %>%
    filter(Step == max(Step))
  )
}


plot_final_step_prevalence <- function(tbl_final_step) {
  
  final_tstep <- tbl_final_step$Step[1]
  # tbl_final_step <- group_by(tbl_final_step, coop_cost)
  p <- ggplot(tbl_final_step, aes(x = as.numeric(coop_cost), y = Prevalence)) +
    geom_jitter(size = 2, width = 0.02, alpha = 0.7, color = "dodgerblue") +
    stat_summary(fun = mean, geom = "line", color = "dodgerblue", size = 1.2) +
    scale_x_continuous(
      breaks = seq_along(levels(tbl_final_step$coop_cost)),
      labels = levels(tbl_final_step$coop_cost)
    ) +
    xlab("Cooperation cost") +
    ylab(paste0("Cooperator prevalence at t = ", final_tstep)) +
    theme_classic()
}