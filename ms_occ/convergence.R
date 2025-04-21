## Visualize

library(jagsUI)
library(coda)
library(tidyverse)
library(ggplot2)
library(MCMCvis)
library(bayesplot)

## Read in

jags_out <- readRDS('ms_occ/outputs/target_sp_no_guilds.RDS')

str(jags_out)
names(jags_out)

names(jags_out$model)
names(jags_out$BUGSoutput)

## Convergence?

summary_df <- as.data.frame(jags_out$BUGSoutput$summary)
summary_df$parameter <- rownames(summary_df)

summary_df <- summary_df %>%
  mutate(group = case_when(
    grepl("^P\\[", parameter) ~ "latent_state",
    grepl("^beta\\[", parameter) ~ "regression_coeff",
    TRUE ~ "other"
  ))


ggplot(summary_df, aes(x = reorder(parameter, Rhat), y = Rhat, color = group)) +
  geom_point() +
  geom_hline(yintercept = 1.1, linetype = "dashed", color = "red") +
  coord_flip() +
  labs(title = "Rhat by parameter group", x = "Parameter", y = "Rhat")

ggplot(summary_df, aes(x = reorder(parameter, n.eff), y = n.eff)) +
  geom_point() +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "blue") +
  coord_flip() +
  labs(title = "Effective sample size per parameter", x = "Parameter", y = "n.eff")


summary_df %>%
  filter(Rhat > 1.1) %>%
  arrange(desc(Rhat))

summary_df %>%
  arrange(n.eff)

#visualize
MCMCplot(jags_out$BUGSoutput$summary, main = '"p" parameters', ref_ovl = TRUE)

#or:
mcmc_chains <- as.mcmc(jags_out$BUGSoutput)

plot(mcmc_chains['P[1]'])

#or
# Extract array for bayesplot: iterations x chains x parameters
mcmc_array <- as.array(mcmc_chains)

# Trace plot for first few parameters
mcmc_trace(mcmc_array$sims.array, pars = c("P[1]", "P[2]", "P[3]"))

# Density overlay
mcmc_dens_overlay(mcmc_array$sims.array, pars = c("P[1]", "P[2]", "P[3]"))



