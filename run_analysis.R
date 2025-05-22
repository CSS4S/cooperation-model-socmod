


ggplot(mig_outcomes_full, aes(x = as.numeric(migration_rate), y = Prevalence)) +
  geom_jitter(size = 2, width = 0.06, alpha = 0.7, color = "dodgerblue") +
  stat_summary(fun = mean, geom = "line", color = "dodgerblue", size = 1.2) +
  scale_x_continuous(breaks = seq_along(levels(mig_outcomes_full$migration_rate)), 
                     labels = levels(mig_outcomes_full$migration_rate)) + 
  xlab("Migration rate") + 
  ylab(paste0("Cooperator prevalence at t = 50")) + 
  theme_classic(base_size = 14) 
