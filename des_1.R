############################################################
# NHS REFERRAL QUEUE MODELLING FRAMEWORK
# ----------------------------------------------------------
# Combines:
# 1. Analytical calibration (queueing theory)
# 2. Discrete Event Simulation (simmer)
# 3. Monte Carlo uncertainty analysis
# 4. Policy scenario testing
#
# Author: Chris Mainey (adapted)
# Purpose: Model RTT (18 weeks) + diagnostics (6 weeks)
############################################################


###########################
# 1. LOAD LIBRARIES
###########################

library(simmer)
library(dplyr)
library(ggplot2)

set.seed(123)


###########################
# 2. GLOBAL PARAMETERS
###########################

# Time horizon (days)
SIM_TIME <- 365

# Demand
lambda_week <- 100        # referrals per week
lambda_day  <- lambda_week / 7

# Pathway split
p_diag <- 0.4             # proportion needing diagnostics

# Capacity (baseline)
diag_capacity  <- 30      # diagnostic slots/day
treat_capacity <- 35      # treatment slots/day

# Service time distributions (days)
diag_time  <- function() rgamma(1, shape=2, rate=1/2)  # mean ~4 days
treat_time <- function() rgamma(1, shape=2, rate=1/3)  # mean ~6 days


###########################
# 3. ANALYTICAL CALIBRATION
###########################
# Simple M/M/c-style sanity check

mu_diag  <- 1 / 4   # service rate (per day)
mu_treat <- 1 / 6

rho_diag  <- (lambda_day * p_diag) / (diag_capacity * mu_diag)
rho_treat <- lambda_day / (treat_capacity * mu_treat)

cat("=== ANALYTICAL CHECK ===\n")
cat("Diagnostic utilisation:", rho_diag, "\n")
cat("Treatment utilisation:", rho_treat, "\n")

# Kingman approximation (waiting time)
kingman_wait <- function(rho, mu, cv2 = 1) {
    (rho / (1 - rho)) * (cv2) * (1 / mu)
}

Wq_diag_est  <- kingman_wait(rho_diag, mu_diag)
Wq_treat_est <- kingman_wait(rho_treat, mu_treat)

cat("Estimated diagnostic wait (days):", Wq_diag_est, "\n")
cat("Estimated treatment wait (days):", Wq_treat_est, "\n\n")


###########################
# 4. DES MODEL FUNCTION
###########################

run_simulation <- function(diag_capacity, treat_capacity, lambda_day, p_diag) {

    env <- simmer("RTT_model")

    # Resources
    env %>%
        add_resource("diagnostics", capacity=diag_capacity, queue_size=Inf) %>%
        add_resource("treatment",   capacity=treat_capacity, queue_size=Inf)

    # Patient pathway
    patient <- trajectory() %>%

        # Record start time
        set_attribute("start_time", function() now(env)) %>%

        # Branch: diagnostic vs non-diagnostic
        branch(
            function() rbinom(1,1,p_diag),
            continue = c(TRUE, TRUE),

            # --- NO DIAGNOSTICS ---
            trajectory() %>%
                seize("treatment") %>%
                timeout(treat_time) %>%
                release("treatment"),

            # --- WITH DIAGNOSTICS ---
            trajectory() %>%
                seize("diagnostics") %>%
                timeout(diag_time) %>%
                release("diagnostics") %>%

                set_attribute("diag_end_time", function() now(env)) %>%

                seize("treatment") %>%
                timeout(treat_time) %>%
                release("treatment")
        ) %>%

        # Record end time
        set_attribute("end_time", function() now(env))

    # Arrival process
    env %>%
        add_generator("patient", patient, function() rexp(1, lambda_day))

    # Run simulation
    env %>% run(until = SIM_TIME)

    # Extract results
    arrivals <- get_mon_arrivals(env)

    # Convert to weeks
    arrivals$rtt_weeks <- arrivals$end_time / 7

    # Diagnostic timing (if exists)
    if ("diag_end_time" %in% names(arrivals)) {
        arrivals$diag_weeks <- arrivals$diag_end_time / 7
    } else {
        arrivals$diag_weeks <- NA
    }

    # Metrics
    results <- data.frame(
        mean_rtt = mean(arrivals$rtt_weeks),
        p18      = mean(arrivals$rtt_weeks <= 18),
        p6_diag  = mean(arrivals$diag_weeks <= 6, na.rm = TRUE)
    )

    return(results)
}


###########################
# 5. MONTE CARLO RUNS
###########################

run_monte_carlo <- function(n_runs, diag_capacity, treat_capacity) {

    sims <- replicate(
        n_runs,
        run_simulation(diag_capacity, treat_capacity, lambda_day, p_diag),
        simplify = FALSE
    )

    sims <- bind_rows(sims)

    summary <- sims %>%
        summarise(
            mean_rtt = mean(mean_rtt),
            p18_mean = mean(p18),
            p18_sd   = sd(p18),
            p6_diag  = mean(p6_diag)
        )

    return(list(raw = sims, summary = summary))
}


cat("Running Monte Carlo baseline...\n")
baseline <- run_monte_carlo(50, diag_capacity, treat_capacity)
print(baseline$summary)


###########################
# 6. POLICY SCENARIO TESTING
###########################

# Scenario grid
scenarios <- expand.grid(
    diag_capacity  = c(25, 30, 35, 40),
    treat_capacity = c(30, 35, 40)
)

run_policy <- function(diag_capacity, treat_capacity) {

    res <- run_monte_carlo(30, diag_capacity, treat_capacity)$summary

    data.frame(
        diag_capacity  = diag_capacity,
        treat_capacity = treat_capacity,
        mean_rtt       = res$mean_rtt,
        p18            = res$p18_mean,
        p6_diag        = res$p6_diag
    )
}

cat("Running policy scenarios...\n")

policy_results <- do.call(rbind, apply(scenarios, 1, function(x) {
    run_policy(x[1], x[2])
}))

print(policy_results)


###########################
# 7. VISUALISATION
###########################

# RTT performance vs diagnostic capacity
ggplot(policy_results, aes(x=diag_capacity, y=p18, colour=factor(treat_capacity))) +
    geom_line() +
    geom_point() +
    labs(
        title = "RTT 18-week Performance",
        x = "Diagnostic Capacity",
        y = "% within 18 weeks",
        colour = "Treatment Capacity"
    ) +
    theme_minimal()


# Diagnostic performance
ggplot(policy_results, aes(x=diag_capacity, y=p6_diag, colour=factor(treat_capacity))) +
    geom_line() +
    geom_point() +
    labs(
        title = "Diagnostic 6-week Performance",
        x = "Diagnostic Capacity",
        y = "% within 6 weeks",
        colour = "Treatment Capacity"
    ) +
    theme_minimal()


###########################
# 8. KEY OUTPUT INTERPRETATION
###########################

cat("\n=== INTERPRETATION GUIDE ===\n")
cat("- p18 close to 0.92+ indicates RTT compliance\n")
cat("- p6_diag close to 0.95 indicates diagnostic compliance\n")
cat("- Look for bottleneck shifts:\n")
cat("  Increasing diagnostics alone may worsen RTT if treatment is constrained\n")
cat("- Use results to inform capacity planning and pathway redesign\n")
