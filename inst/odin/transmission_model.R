# ------------------------------------------------------------------------------
# INITIAL COMPARTMENT VALUES
# ------------------------------------------------------------------------------

# Set the initial state for all compartments (S, E, I, R, Is, Rc) using input parameters or zeros.
initial(S[, , ])  <- S0[i, j, k]           # Susceptible: initialised from input
initial(E[, , ])  <- 0                     # Exposed: no one exposed at t = 0
initial(I[, , ])  <- I0[i, j, k]           # Infectious: initialised from input
initial(R[, , ])  <- Rpop0[i, j, k]        # Recovered (non-severe): from prior immunity
initial(Is[, , ]) <- 0                     # Infectious (severe): starts at 0
initial(Rc[, , ]) <- 0                     # Recovered with complications: starts at 0

# ------------------------------------------------------------------------------
# COMPARTMENT UPDATES
# ------------------------------------------------------------------------------
# Update the state of each compartment at every timestep, accounting for infection, aging, vaccination, waning, migration, and death.
# S: Susceptible
update(S[, , ]) <- max(S[i, j, k] + waning_R[i, j, k] + waning_Rc[i, j, k] + aging_into_S[i, j, k] - aging_out_of_S[i, j, k] - lambda_S[i, j, k] - S_death[i, j, k] + migration_S[i, j, k] * pos_neg_migration + vaccinating_into_S[i, j, k] - vaccinating_out_of_S[i, j, k] + waning_to_S_long[i, j, k] + waning_to_S_unvaccinated[i, j, k] - waning_from_S_short[i, j, k] - waning_from_S_long[i, j, k] - seeded_actual[i, j, k] + waning_to_S_short[i, j, k], 0)

# E: Exposed
update(E[, , ]) <- max(E[i, j, k] + lambda_S[i, j, k] - incubated[i, j, k] + aging_into_E[i, j, k] - aging_out_of_E[i, j, k] - E_death[i, j, k] + migration_E[i, j, k] * pos_neg_migration + vaccinating_into_E[i, j, k] - vaccinating_out_of_E[i, j, k] + waning_to_E_long[i, j, k] + waning_to_E_unvaccinated[i, j, k] - waning_from_E_short[i, j, k] - waning_from_E_long[i, j, k], 0)

# I: Infectious (non-severe)
update(I[, , ]) <- max(I[i, j, k] + into_I[i, j, k] + aging_into_I[i, j, k] - aging_out_of_I[i, j, k] - recovered_I_to_R[i, j, k] - I_death[i, j, k] + seeded_actual[i, j, k] + migration_I[i, j, k] * pos_neg_migration + vaccinating_into_I[i, j, k] - vaccinating_out_of_I[i, j, k] + waning_to_I_long[i, j, k] + waning_to_I_unvaccinated[i, j, k] - waning_from_I_short[i, j, k] - waning_from_I_long[i, j, k], 0)

# R: Recovered (non-severe)
update(R[, , ]) <- max(R[i, j, k] + recovered_I_to_R[i, j, k] + recovered_Is_to_R[i, j, k] - waning_R[i, j, k] + aging_into_R[i, j, k] - aging_out_of_R[i, j, k] - R_death[i, j, k] + migration_R[i, j, k] * pos_neg_migration + vaccinating_into_R[i, j, k] - vaccinating_out_of_R[i, j, k] + waning_to_R_long[i, j, k] + waning_to_R_unvaccinated[i, j, k] - waning_from_R_short[i, j, k] - waning_from_R_long[i, j, k], 0)

# Is: Infectious (severe)
update(Is[, , ]) <- max(Is[i, j, k] + into_Is[i, j, k] - recovered_from_Is[i, j, k] + aging_into_Is[i, j, k] - aging_out_of_Is[i, j, k] - Is_death[i, j, k] + migration_Is[i, j, k] * pos_neg_migration + vaccinating_into_Is[i, j, k] - vaccinating_out_of_Is[i, j, k] + waning_to_Is_long[i, j, k] + waning_to_Is_unvaccinated[i, j, k] - waning_from_Is_short[i, j, k] - waning_from_Is_long[i, j, k], 0)

# Rc: Recovered with complications
update(Rc[, , ]) <- max(Rc[i, j, k] + recovered_Is_to_Rc[i, j, k] - waning_Rc[i, j, k] + aging_into_Rc[i, j, k] - aging_out_of_Rc[i, j, k] - Rc_death[i, j, k] + migration_Rc[i, j, k] * pos_neg_migration + vaccinating_into_Rc[i, j, k] - vaccinating_out_of_Rc[i, j, k] + waning_to_Rc_long[i, j, k] + waning_to_Rc_unvaccinated[i, j, k] - waning_from_Rc_short[i, j, k] - waning_from_Rc_long[i, j, k], 0)

# ------------------------------------------------------------------------------
# SEROSURVEY OBSERVATION MODEL
# ------------------------------------------------------------------------------
# Define the likelihood function for observed seroprevalence using a Poisson distribution, based on model-derived seropositive proportions.
# Observed seropositivity by age group (e.g. from survey data)
serosurvey[] <- data()

# Likelihood: observed seropositives ~ Poisson(expected)
serosurvey[] ~ Poisson(seropositive[i])

# ------------------------------------------------------------------------------
# ENTERING AND EXITING COMPARTMENTS
# ------------------------------------------------------------------------------
# Define transition flows into and out of compartments: infection, progression, recovery, seeding, background and disease-specific mortality.
# -- Available population for transitions (post-waning + migration) --

S_available[, , ]  <- S_after_waning[i, j, k]  + migration_S[i, j, k] * pos_neg_migration
E_available[, , ]  <- E_after_waning[i, j, k]  + migration_E[i, j, k] * pos_neg_migration
I_available[, , ]  <- I_after_waning[i, j, k]  + migration_I[i, j, k] * pos_neg_migration
R_available[, , ]  <- R_after_waning[i, j, k]  + migration_R[i, j, k] * pos_neg_migration
Rc_available[, , ] <- Rc_after_waning[i, j, k] + migration_Rc[i, j, k] * pos_neg_migration
Is_available[, , ] <- Is_after_waning[i, j, k] + migration_Is[i, j, k] * pos_neg_migration

# -- Infections and progression --

# Force of infection applied to susceptible individuals
lambda_S[, , ] <- if (S_available[i, j, k] <= 0) 0 else
  Binomial(S_available[i, j, k], max(min(lambda[i, j, k], 1), 0))

# Exposed individuals incubate into infection
incubated[, , ] <- if (E_available[i, j, k] <= 0) 0 else
  Binomial(E_available[i, j, k], max(min(incubation_rate, 1), 0))

# I → R (recovery from non-severe)
recovered_I_to_R[, , ] <- if (I_available[i, j, k] <= 0) 0 else
  Binomial(I_available[i, j, k], max(min(recovery_rate, 1), 0))

# Is → R + Rc (recovery from severe)
recovered_from_Is[, , ] <- if (Is_available[i, j, k] <= 0) 0 else
  Binomial(Is_available[i, j, k], max(min(severe_recovery_rate, 1), 0))

# -- Deaths from background and disease --
S_death[, , ] <- if (S_available[i, j, k] <= 0) 0 else
  Binomial(S_available[i, j, k], max(min(background_death[i, k], 1), 0))

E_death[, , ] <- if (E_available[i, j, k] <= 0) 0 else
  Binomial(E_available[i, j, k], max(min(background_death[i, k], 1), 0))

I_death[, , ] <- if (I_available[i, j, k] <= 0) 0 else
  Binomial(I_available[i, j, k], max(min(background_death[i, k], 1), 0) + max(cfr_normal[i, j, k], 0))

R_death[, , ] <- if (R_available[i, j, k] <= 0) 0 else
  Binomial(R_available[i, j, k], max(min(background_death[i, k], 1), 0))

Is_death[, , ] <- if (Is_available[i, j, k] <= 0) 0 else
  Binomial(Is_available[i, j, k], max(min(background_death[i, k], 1), 0) + max(cfr_severe[i, j, k], 0))

Rc_death[, , ] <- if (Rc_available[i, j, k] <= 0) 0 else
  Binomial(Rc_available[i, j, k], max(min(background_death[i, k], 1), 0))

# -- Waning of natural immunity --
waning_R[, , ]  <- if (R_after_aging[i, j, k] <= 0) 0 else
  Binomial(R_after_aging[i, j, k], max(min(natural_immunity_waning, 1), 0))

waning_Rc[, , ] <- if (Rc_after_aging[i, j, k] <= 0) 0 else
  Binomial(Rc_after_aging[i, j, k], max(min(natural_immunity_waning, 1), 0))

# -- Split incubated into mild vs severe --
into_I[, , ] <- if (incubated[i, j, k] <= 0) 0 else
  Binomial(incubated[i, j, k], max(min(1 - prop_severe[i, j, k], 1), 0))

into_Is[, , ] <- if (incubated[i, j, k] - into_I[i, j, k] <= 0) 0 else
  incubated[i, j, k] - into_I[i, j, k]

# -- Split recovery from severe infection into Rc vs R --
recovered_Is_to_R[, , ] <- if (recovered_from_Is[i, j, k] <= 0) 0 else
  Binomial(recovered_from_Is[i, j, k], max(min(1 - prop_complications[i, j, k], 1), 0))

recovered_Is_to_Rc[, , ] <- if(recovered_Is_to_R[i, j, k] >= recovered_from_Is[i, j, k]) 0 else recovered_from_Is[i, j, k] - recovered_Is_to_R[i, j, k]

# ------------------------------------------------------------------------------
# STEP 1: AGING — Individuals transition from one age group to the next
# ------------------------------------------------------------------------------
#Compute aging flows and post-aging compartment states for all modelled compartments.

# Susceptible (S)
aging_into_S[1, 1, ]              <- sum(Births)  # Newborns enter first age & vacc group
aging_into_S[2:n_age, , ]         <- if(S[i - 1, j, k] <= 0 || aging_rate[i - 1] <= 0) 0 else floor(S[i - 1, j, k] * max(min(aging_rate[i - 1], 1), 0))
aging_out_of_S[1, , ] <- if(S[1, j, k] - aging_into_S[1, 1, k] <= 0 || aging_rate[1] <= 0) 0 else floor(max(S[1, j, k] - aging_into_S[1, 1, k], 0) * aging_rate[1])
aging_out_of_S[2:n_age, , ] <- if(S[i, j, k] <= 0 || aging_rate[i] <= 0) 0 else floor(S[i, j, k] * max(min(aging_rate[i], 1), 0))
S_after_aging[, , ]               <- max(S[i, j, k] + aging_into_S[i, j, k] - aging_out_of_S[i, j, k], 0)

# Exposed (E)
aging_into_E[2:n_age, , ]         <- if(E[i - 1, j, k] <= 0 || aging_rate[i - 1] <= 0) 0 else floor(E[i - 1, j, k] * max(min(aging_rate[i - 1], 1), 0))
aging_out_of_E[1, , ] <- if(E[1, j, k] - aging_into_E[1, 1, k] <= 0 || aging_rate[1] <= 0) 0 else floor(max(E[1, j, k] - aging_into_E[1, 1, k], 0) * aging_rate[1])
aging_out_of_E[2:n_age, , ] <- if(E[i, j, k] <= 0 || aging_rate[i] <= 0) 0 else floor(E[i, j, k] * max(min(aging_rate[i], 1), 0))
E_after_aging[, , ]               <- max(E[i, j, k] + aging_into_E[i, j, k] - aging_out_of_E[i, j, k], 0)

# Infectious (I)
aging_into_I[2:n_age, , ]         <- if(I[i - 1, j, k] <= 0 || aging_rate[i - 1] <= 0) 0 else floor(I[i - 1, j, k] * max(min(aging_rate[i - 1], 1), 0))
aging_out_of_I[1, , ] <- if(I[1, j, k] - aging_into_I[1, 1, k] <= 0 || aging_rate[1] <= 0) 0 else floor(max(I[1, j, k] - aging_into_I[1, 1, k], 0) * aging_rate[1])
aging_out_of_I[2:n_age, , ] <- if(I[i, j, k] <= 0 || aging_rate[i] <= 0) 0 else floor(I[i, j, k] * max(min(aging_rate[i], 1), 0))
I_after_aging[, , ]               <- max(I[i, j, k] + aging_into_I[i, j, k] - aging_out_of_I[i, j, k], 0)

# Recovered (R)
aging_into_R[2:n_age, , ]         <- if(R[i - 1, j, k] <= 0 || aging_rate[i - 1] <= 0) 0 else floor(R[i - 1, j, k] * max(min(aging_rate[i - 1], 1), 0))
aging_out_of_R[1, , ] <- if(R[1, j, k] - aging_into_R[1, 1, k] <= 0 || aging_rate[1] <= 0) 0 else floor(max(R[1, j, k] - aging_into_R[1, 1, k], 0) * aging_rate[1])
aging_out_of_R[2:n_age, , ] <- if(R[i, j, k] <= 0 || aging_rate[i] <= 0) 0 else floor(R[i, j, k] * max(min(aging_rate[i], 1), 0))
R_after_aging[, , ]               <- max(R[i, j, k] + aging_into_R[i, j, k] - aging_out_of_R[i, j, k], 0)

# Severe Infectious (Is)
aging_into_Is[2:n_age, , ]         <- if(Is[i - 1, j, k] <= 0 || aging_rate[i - 1] <= 0) 0 else floor(Is[i - 1, j, k] * max(min(aging_rate[i - 1], 1), 0))
aging_out_of_Is[1, , ] <- if(Is[1, j, k] - aging_into_Is[1, 1, k] <= 0 || aging_rate[1] <= 0) 0 else floor(max(Is[1, j, k] - aging_into_Is[1, 1, k], 0) * aging_rate[1])
aging_out_of_Is[2:n_age, , ] <- if(Is[i, j, k] <= 0 || aging_rate[i] <= 0) 0 else floor(Is[i, j, k] * max(min(aging_rate[i], 1), 0))
Is_after_aging[, , ]               <- max(Is[i, j, k] + aging_into_Is[i, j, k] - aging_out_of_Is[i, j, k], 0)

# Recovered with Complications (Rc)
aging_into_Rc[2:n_age, , ]         <- if(Rc[i - 1, j, k] <= 0 || aging_rate[i - 1] <= 0) 0 else floor(Rc[i - 1, j, k] * max(min(aging_rate[i - 1], 1), 0))
aging_out_of_Rc[1, , ] <- if(Rc[1, j, k] - aging_into_Rc[1, 1, k] <= 0 || aging_rate[1] <= 0) 0 else floor(max(Rc[1, j, k] - aging_into_Rc[1, 1, k], 0) * aging_rate[1])
aging_out_of_Rc[2:n_age, , ] <- if(Rc[i, j, k] <= 0 || aging_rate[i] <= 0) 0 else floor(Rc[i, j, k] * max(min(aging_rate[i], 1), 0))
Rc_after_aging[, , ]               <- max(Rc[i, j, k] + aging_into_Rc[i, j, k] - aging_out_of_Rc[i, j, k], 0)

# ------------------------------------------------------------------------------
# STEP 2: VACCINATION — Apply vaccination after aging
# ------------------------------------------------------------------------------
#Apply stochastic or deterministic vaccination transitions between vaccination strata for each compartment.

# Susceptible (S)
vaccinating_out_of_S[, , ] <- if (
  n_vacc == 1 || j >= n_vacc - 1 || S_after_aging[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0
) 0 else if (stochastic_vaccination == 1) Binomial(S_after_aging[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0)) else
  floor(S_after_aging[i, j, k] * max(min(vaccination_prop[i, j, k], 1), 0))

S_after_vaccination[, , ] <- S_after_aging[i, j, k] + vaccinating_into_S[i, j, k] - vaccinating_out_of_S[i, j, k]

# Exposed (E)
vaccinating_out_of_E[, , ] <- if (
  n_vacc == 1 || j >= n_vacc - 1 || E_after_aging[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0
) 0 else if (stochastic_vaccination == 1) Binomial(E_after_aging[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0)) else
  floor(E_after_aging[i, j, k] * max(min(vaccination_prop[i, j, k], 1), 0))

E_after_vaccination[, , ] <- E_after_aging[i, j, k] + vaccinating_into_E[i, j, k] - vaccinating_out_of_E[i, j, k]

# Infectious (I)
vaccinating_out_of_I[, , ] <- if (
  n_vacc == 1 || j >= n_vacc - 1 || I_after_aging[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0
) 0 else if (stochastic_vaccination == 1) Binomial(I_after_aging[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0)) else
  floor(I_after_aging[i, j, k] * max(min(vaccination_prop[i, j, k], 1), 0))

I_after_vaccination[, , ] <- I_after_aging[i, j, k] + vaccinating_into_I[i, j, k] - vaccinating_out_of_I[i, j, k]

# Recovered (R)
vaccinating_out_of_R[, , ] <- if (
  n_vacc == 1 || j >= n_vacc - 1 || R_after_aging[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0
) 0 else if (stochastic_vaccination == 1) Binomial(R_after_aging[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0)) else
  floor(R_after_aging[i, j, k] * max(min(vaccination_prop[i, j, k], 1), 0))

R_after_vaccination[, , ] <- R_after_aging[i, j, k] + vaccinating_into_R[i, j, k] - vaccinating_out_of_R[i, j, k]

# Severe Infectious (Is)
vaccinating_out_of_Is[, , ] <- if (
  n_vacc == 1 || j >= n_vacc - 1 || Is_after_aging[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0
) 0 else if (stochastic_vaccination == 1) Binomial(Is_after_aging[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0)) else
  floor(Is_after_aging[i, j, k] * max(min(vaccination_prop[i, j, k], 1), 0))

Is_after_vaccination[, , ] <- Is_after_aging[i, j, k] + vaccinating_into_Is[i, j, k] - vaccinating_out_of_Is[i, j, k]

# Recovered with Complications (Rc)
vaccinating_out_of_Rc[, , ] <- if (
  n_vacc == 1 || j >= n_vacc - 1 || Rc_after_aging[i, j, k] <= 0 || vaccination_prop[i, j, k] <= 0
) 0 else if (stochastic_vaccination == 1) Binomial(Rc_after_aging[i, j, k], max(min(vaccination_prop[i, j, k], 1), 0)) else
  floor(Rc_after_aging[i, j, k] * max(min(vaccination_prop[i, j, k], 1), 0))

Rc_after_vaccination[, , ] <- Rc_after_aging[i, j, k] + vaccinating_into_Rc[i, j, k] - vaccinating_out_of_Rc[i, j, k]

# --- Vaccination transitions INTO higher protection compartments ---

# Susceptible (S)
vaccinating_into_S[, 1:2, ]     <- 0
vaccinating_into_S[, 4, ] <- 0
vaccinating_into_S[, 3, ]       <- if(vaccinating_out_of_S[i, 1, k] <= 0) 0 else vaccinating_out_of_S[i, 1, k]
vaccinating_into_S[, 5:n_vacc, ] <- if (j >= 5 && j %% 2 == 1) vaccinating_out_of_S[i, j - 2, k] + vaccinating_out_of_S[i, j - 3, k] else 0

# Exposed (E)
vaccinating_into_E[, 1:2, ]     <- 0
vaccinating_into_E[, 4, ] <- 0
vaccinating_into_E[, 3, ]       <- if(vaccinating_out_of_E[i, 1, k] <= 0) 0 else vaccinating_out_of_E[i, 1, k]
vaccinating_into_E[, 5:n_vacc, ] <- if (j >= 5 && j %% 2 == 1) vaccinating_out_of_E[i, j - 2, k] + vaccinating_out_of_E[i, j - 3, k] else 0

# Infectious (I)
vaccinating_into_I[, 1:2, ]     <- 0
vaccinating_into_I[, 4, ] <- 0
vaccinating_into_I[, 3, ]       <- if(vaccinating_out_of_I[i, 1, k] <= 0) 0 else vaccinating_out_of_I[i, 1, k]
vaccinating_into_I[, 5:n_vacc, ] <- if (j >= 5 && j %% 2 == 1) vaccinating_out_of_I[i, j - 2, k] + vaccinating_out_of_I[i, j - 3, k] else 0

# Recovered (R)
vaccinating_into_R[, 1:2, ]     <- 0
vaccinating_into_R[, 4, ] <- 0
vaccinating_into_R[, 3, ]       <- if(vaccinating_out_of_R[i, 1, k] <= 0) 0 else vaccinating_out_of_R[i, 1, k]
vaccinating_into_R[, 5:n_vacc, ] <- if (j >= 5 && j %% 2 == 1) vaccinating_out_of_R[i, j - 2, k] + vaccinating_out_of_R[i, j - 3, k] else 0

# Severe Infectious (Is)
vaccinating_into_Is[, 1:2, ]     <- 0
vaccinating_into_Is[, 4, ] <- 0
vaccinating_into_Is[, 3, ]       <- if(vaccinating_out_of_Is[i, 1, k] <= 0) 0 else vaccinating_out_of_Is[i, 1, k]
vaccinating_into_Is[, 5:n_vacc, ] <- if (j >= 5 && j %% 2 == 1) vaccinating_out_of_Is[i, j - 2, k] + vaccinating_out_of_Is[i, j - 3, k] else 0

# Recovered with Complications (Rc)
vaccinating_into_Rc[, 1:2, ]     <- 0
vaccinating_into_Rc[, 4, ] <- 0
vaccinating_into_Rc[, 3, ]       <- if(vaccinating_out_of_Rc[i, 1, k] <= 0) 0 else vaccinating_out_of_Rc[i, 1, k]
vaccinating_into_Rc[, 5:n_vacc, ] <- if (j >= 5 && j %% 2 == 1) vaccinating_out_of_Rc[i, j - 2, k] + vaccinating_out_of_Rc[i, j - 3, k] else 0

# ------------------------------------------------------------------------------
# STEP 3: WANING — Apply waning to the results after vaccination
# ------------------------------------------------------------------------------
# Apply biphasic waning transitions (short → long → unvaccinated) to all compartments, modifying vaccination strata.

# Susceptible (S)
waning_from_S_short[, , ] <- if (j %% 2 == 1 && j > 1 && S_after_vaccination[i, j, k] > 0)
  Binomial(S_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_S_long[, 1:(n_vacc - 1), ] <- if (j %% 2 == 0 && j > 1) waning_from_S_short[i, j + 1, k] else 0
waning_from_S_long[, , ] <- if (j %% 2 == 0 && j > 1 && j <= n_vacc && S_after_vaccination[i, j, k] > 0)
  Binomial(S_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_S_unvaccinated[, , ] <- if (j == 1 && n_vacc >= 2) waning_from_S_long[i, 2, k] else 0
waning_to_S_short[, 1:(n_vacc - 1), ] <- if (j %% 2 == 1 && j > 1 && j + 1 <= n_vacc)
  waning_from_S_long[i, j + 1, k] else 0
S_after_waning[, , ] <- S_after_vaccination[i, j, k] +
  waning_to_S_long[i, j, k] + waning_to_S_unvaccinated[i, j, k] +
  waning_to_S_short[i, j, k] -
  waning_from_S_short[i, j, k] - waning_from_S_long[i, j, k]

# Exposed (E)
waning_from_E_short[, , ] <- if (j %% 2 == 1 && j > 1 && E_after_vaccination[i, j, k] > 0)
  Binomial(E_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_E_long[, 1:(n_vacc - 1), ] <- if (j %% 2 == 0 && j > 1) waning_from_E_short[i, j + 1, k] else 0
waning_from_E_long[, , ] <- if (j %% 2 == 0 && j > 1 && E_after_vaccination[i, j, k] > 0)
  Binomial(E_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_E_unvaccinated[, , ] <- if (j == 1) sum(waning_from_E_long[i, 2:n_vacc, k]) else 0
E_after_waning[, , ] <- E_after_vaccination[i, j, k] +
  waning_to_E_long[i, j, k] + waning_to_E_unvaccinated[i, j, k] -
  waning_from_E_short[i, j, k] - waning_from_E_long[i, j, k]

# Infectious (I)
waning_from_I_short[, , ] <- if (j %% 2 == 1 && j > 1 && I_after_vaccination[i, j, k] > 0)
  Binomial(I_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_I_long[, 1:(n_vacc - 1), ] <- if (j %% 2 == 0 && j > 1) waning_from_I_short[i, j + 1, k] else 0
waning_from_I_long[, , ] <- if (j %% 2 == 0 && j > 1 && I_after_vaccination[i, j, k] > 0)
  Binomial(I_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_I_unvaccinated[, , ] <- if (j == 1) sum(waning_from_I_long[i, 2:n_vacc, k]) else 0
I_after_waning[, , ] <- I_after_vaccination[i, j, k] +
  waning_to_I_long[i, j, k] + waning_to_I_unvaccinated[i, j, k] -
  waning_from_I_short[i, j, k] - waning_from_I_long[i, j, k]

# Recovered (R)
waning_from_R_short[, , ] <- if (j %% 2 == 1 && j > 1 && R_after_vaccination[i, j, k] > 0)
  Binomial(R_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_R_long[, 1:(n_vacc - 1), ] <- if (j %% 2 == 0 && j > 1) waning_from_R_short[i, j + 1, k] else 0
waning_from_R_long[, , ] <- if (j %% 2 == 0 && j > 1 && R_after_vaccination[i, j, k] > 0)
  Binomial(R_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_R_unvaccinated[, , ] <- if (j == 1) sum(waning_from_R_long[i, 2:n_vacc, k]) else 0
R_after_waning[, , ] <- R_after_vaccination[i, j, k] +
  waning_to_R_long[i, j, k] + waning_to_R_unvaccinated[i, j, k] -
  waning_from_R_short[i, j, k] - waning_from_R_long[i, j, k]

# Severe Infectious (Is)
waning_from_Is_short[, , ] <- if (j %% 2 == 1 && j > 1 && Is_after_vaccination[i, j, k] > 0)
  Binomial(Is_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_Is_long[, 1:(n_vacc - 1), ] <- if (j %% 2 == 0 && j > 1) waning_from_Is_short[i, j + 1, k] else 0
waning_from_Is_long[, , ] <- if (j %% 2 == 0 && j > 1 && Is_after_vaccination[i, j, k] > 0)
  Binomial(Is_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_Is_unvaccinated[, , ] <- if (j == 1) sum(waning_from_Is_long[i, 2:n_vacc, k]) else 0
Is_after_waning[, , ] <- Is_after_vaccination[i, j, k] +
  waning_to_Is_long[i, j, k] + waning_to_Is_unvaccinated[i, j, k] -
  waning_from_Is_short[i, j, k] - waning_from_Is_long[i, j, k]

# Recovered with Complications (Rc)
waning_from_Rc_short[, , ] <- if (j %% 2 == 1 && j > 1 && Rc_after_vaccination[i, j, k] > 0)
  Binomial(Rc_after_vaccination[i, j, k], max(min(short_term_waning[j], 1), 0)) else 0
waning_to_Rc_long[, 1:(n_vacc - 1), ] <- if (j %% 2 == 0 && j > 1) waning_from_Rc_short[i, j + 1, k] else 0
waning_from_Rc_long[, , ] <- if (j %% 2 == 0 && j > 1 && Rc_after_vaccination[i, j, k] > 0)
  Binomial(Rc_after_vaccination[i, j, k], max(min(long_term_waning[j], 1), 0)) else 0
waning_to_Rc_unvaccinated[, , ] <- if (j == 1) sum(waning_from_Rc_long[i, 2:n_vacc, k]) else 0
Rc_after_waning[, , ] <- Rc_after_vaccination[i, j, k] +
  waning_to_Rc_long[i, j, k] + waning_to_Rc_unvaccinated[i, j, k] -
  waning_from_Rc_short[i, j, k] - waning_from_Rc_long[i, j, k]

# ------------------------------------------------------------------------------
# STEP 4: MIGRATION — Adjust population by movement between risk groups
# ------------------------------------------------------------------------------

# Apply population migration based on user-defined volumes and stratified weights across compartments.
# Interpolated migration inputs
migration <- interpolate(tt_migration, migration_in_number, "constant")               # migration volume per stratum
migration_distribution <- interpolate(tt_migration, migration_distribution_values, "constant")  # compartment split weights

# Determine migration direction and magnitude
pos_neg_migration <- if (sum(migration) < 0) -1 else 1
migration_adjusted[, , ] <- migration[i, j, k] * pos_neg_migration

# Susceptible (S)
migration_prop_S <- if(N <= 0) 0 else sum(S)/N
migration_occuring_S <- if (migration_distribution[1] <= 0 || sum(S) <= 0) 0 else
  Binomial(sum(migration_adjusted), max(min(migration_prop_S, 1), 0))
migration_S[, , ] <- if (migration_occuring_S <= 0 || sum(S) <= 0) 0 else
  Binomial(migration_occuring_S, S[i, j, k]/sum(S))

# Exposed (E)
migration_prop_E <- if(N <= 0) 0 else sum(E)/N
migration_occuring_E <- if (migration_distribution[1] <= 0 || sum(E) <= 0) 0 else
  Binomial(sum(migration_adjusted), max(min(migration_prop_E, 1), 0))
migration_E[, , ] <- if (migration_occuring_E <= 0 || sum(E) <= 0) 0 else
  Binomial(migration_occuring_E, E[i, j, k]/sum(E))

# Infectious (I)
migration_prop_I <- if(N <= 0) 0 else sum(I)/N
migration_occuring_I <- if (migration_distribution[1] <= 0 || sum(I) <= 0) 0 else
  Binomial(sum(migration_adjusted), max(min(migration_prop_I, 1), 0))
migration_I[, , ] <- if (migration_occuring_I <= 0 || sum(I) <= 0) 0 else
  Binomial(migration_occuring_I, I[i, j, k]/sum(I))

# Recovered (R)
migration_prop_R <- if(N <= 0) 0 else sum(R)/N
migration_occuring_R <- if (migration_distribution[1] <= 0 || sum(R) <= 0) 0 else
  Binomial(sum(migration_adjusted), max(min(migration_prop_R, 1), 0))
migration_R[, , ] <- if (migration_occuring_R <= 0 || sum(R) <= 0) 0 else
  Binomial(migration_occuring_R, R[i, j, k]/sum(R))

# Severe Infectious (Is)
migration_prop_Is <- if(N <= 0) 0 else sum(Is)/N
migration_occuring_Is <- if (migration_distribution[1] <= 0 || sum(Is) <= 0) 0 else
  Binomial(sum(migration_adjusted), max(min(migration_prop_Is, 1), 0))
migration_Is[, , ] <- if (migration_occuring_Is <= 0 || sum(Is) <= 0) 0 else
  Binomial(migration_occuring_Is, Is[i, j, k]/sum(Is))

# Recovered with Complications (Rc)
migration_prop_Rc <- if(N <= 0) 0 else sum(Rc)/N
migration_occuring_Rc <- if (migration_distribution[1] <= 0 || sum(Rc) <= 0) 0 else
  Binomial(sum(migration_adjusted), max(min(migration_prop_Rc, 1), 0))
migration_Rc[, , ] <- if (migration_occuring_Rc <= 0) 0 else
  Binomial(migration_occuring_Rc, Rc[i, j, k]/sum(Rc))

# ------------------------------------------------------------------------------
# USER PARAMETERS — All parameters passed into the model
# ------------------------------------------------------------------------------
# Declare all user-defined input parameters, grouped by theme (e.g. structure, disease progression, transmission, vaccination, demography, etc).

# -- 1. MODEL STRUCTURE AND DIMENSIONS --
n_age  <- parameter(type = "integer")   # Number of age groups
n_vacc <- parameter(type = "integer")   # Number of vaccination strata
n_risk <- parameter(type = "integer")   # Number of risk groups

# -- 2. INITIAL CONDITIONS --
S0     <- parameter(type = "integer")    # Initial susceptible population [dim1 = n_age x n_vacc x n_risk]
I0     <- parameter(type = "integer")    # Initial infectious population [same dims]
Rpop0  <- parameter(type = "integer")    # Initial recovered population [same dims]

# -- 3. DISEASE NATURAL HISTORY PARAMETERS --
incubation_rate         <- parameter()  # Rate of progression from E to I
recovery_rate           <- parameter()  # Recovery rate from I (non-severe)
natural_immunity_waning <- parameter()  # Waning rate for natural immunity (R, Rc)

prop_severe             <- parameter()  # Proportion of infections that are severe (I → Is)
severe_recovery_rate    <- parameter()  # Recovery rate from severe infection (Is)
cfr_normal              <- parameter()  # Case fatality rate for non-severe cases (I)
cfr_severe              <- parameter()  # Case fatality rate for severe cases (Is)
prop_complications      <- parameter()  # Proportion of severe recoveries leading to complications (Is → Rc)

# -- 4. TRANSMISSION AND CONTACT STRUCTURE --
R0               <- parameter()  # Basic reproduction number
tt_R0            <- parameter()  # Time points for changes in R0
no_R0_changes    <- parameter()  # Number of R0 change points

contact_matrix   <- parameter()  # Age-to-age contact matrix (n_age x n_age)

seeded           <- parameter()  # Initial seeding pattern (n_age x n_vacc x n_risk x no_seeded_changes)
tt_seeded        <- parameter()  # Time points for changes in seeding
no_seeded_changes <- parameter() # Number of seeding change points

# -- 5. VACCINATION --
tt_vaccination_coverage       <- parameter()  # Time points for changing vaccination
no_vacc_changes               <- parameter()  # Number of vaccination change events
vaccination_coverage          <- parameter()  # Coverage values over time (dim4 = no_vacc_changes)
age_vaccination_beta_modifier <- parameter()  # Age & vacc modifier to beta (reduces transmission)

stochastic_vaccination <- parameter(0)  # Whether vaccination is stochastic (1) or deterministic (0)

# -- 6. DEMOGRAPHY (BIRTHS AND DEATHS) --
crude_death        <- parameter()       # Crude death rate (n_age x n_risk x no_death_changes)
no_death_changes   <- parameter()       # Number of death rate change events
tt_death_changes   <- parameter()       # Time points for death rate changes

crude_birth        <- parameter()       # Crude birth rate (n_age x no_birth_changes)
no_birth_changes   <- parameter()       # Number of birth rate change events
tt_birth_changes   <- parameter()       # Time points for birth rate changes

repro_low          <- parameter()       # Lower age index for reproductive population
repro_high         <- parameter()       # Upper age index for reproductive population

simp_birth_death   <- parameter(1)      # If 1, births match deaths to stabilize pop

stochastic_birth   <- parameter(0)      # If 1, births are drawn from a binomial

aging_rate <- parameter()               # Aging rate

# -- 7. MATERNAL PROTECTION --
age_maternal_protection_ends <- parameter()  # Age below which maternal protection applies
protection_weight_vacc       <- parameter()  # Proportion protected from vaccinated mothers
protection_weight_rec        <- parameter()  # Proportion protected from naturally immune mothers

# -- 8. MIGRATION --
no_migration_changes      <- parameter()  # Number of migration change points
tt_migration              <- parameter()  # Time points for migration changes
migration_in_number       <- parameter()  # Net migration per stratum (n_age x n_vacc x n_risk x time)
migration_distribution_values <- parameter()  # Relative weight of compartments (S, E, I, R, Is, Rc)

# -- 9. FITTING PARAMETERS --
reporting_rate <- parameter(1)  # Reporting rate (used in seeding)
R0_modifier    <- parameter(1)  # Scalar to modify R0 over time
repro_weight   <- parameter()   # Weighting of age-specific contributions to reproductive population

# -- 10. WANING PARAMETERS --
short_term_waning <- parameter()  # Rate from short-term → long-term protection (per vacc group)
long_term_waning  <- parameter()  # Rate from long-term → unvaccinated (per vacc group)


# ------------------------------------------------------------------------------
# CALCULATED PARAMETERS — Derived from user inputs or interpolated
# ------------------------------------------------------------------------------
# Compute all intermediate quantities used for transmission, demography, immunity, and vaccination dynamics.

# 1. POPULATION TOTALS

# Total population at current timestep
N <- sum(S) + sum(E) + sum(I) + sum(R) + sum(Is) + sum(Rc)

# Population by age and risk group (used for background death and births)
Npop_age_risk[, ] <- sum(S[i, , j]) + sum(E[i, , j]) + sum(I[i, , j]) + sum(R[i, , j]) +
  sum(Is[i, , j]) + sum(Rc[i, , j])

# Population by age (used for force of infection and seropositivity)
Npop_age[] <- sum(S_available[i, , ]) + sum(E_available[i, , ]) + sum(I_available[i, , ]) +
  sum(R_available[i, , ]) + sum(Is_available[i, , ]) + sum(Rc_available[i, , ])

# 2. DEATH RATES

# Interpolate death rate changes over time
death_int <- interpolate(tt_death_changes, crude_death, "constant")

# Background death rate: fixed or time-varying
background_death[, ] <- if (simp_birth_death == 1)
  max(min(crude_death[i, j, 1], 1), 0) else
    max(min(death_int[i, j], 1), 0)

# Binomial draw for background deaths per age-risk group
Npop_background_death[, ] <- if (Npop_age_risk[i, j] <= 0) 0 else
  Binomial(Npop_age_risk[i, j], max(min(background_death[i, j], 1), 0))

# 3. BIRTHS

# Interpolate reproductive weight over time
repro_weight_now <- interpolate(tt_migration, repro_weight, "constant")

# Reproductive population (weighted sum across compartments)
reproductive_population[] <- if (i >= repro_low && i <= repro_high)
  floor((sum(S[i, , ]) + sum(E[i, , ]) + sum(I[i, , ]) +
           sum(R[i, , ]) + sum(Is[i, , ]) + sum(Rc[i, , ])) * repro_weight_now[i]) else 0

# Birth rate is based on current background death rate
birth_rate[] <- if (reproductive_population[i] <= 0) 0 else
  sum(Npop_background_death[i, ]) / reproductive_population[i]

# Interpolate time-varying crude birth rate
birth_int <- interpolate(tt_birth_changes, crude_birth, "constant")

# Compute births
Births[] <- if (reproductive_population[i] <= 0) 0 else if (simp_birth_death == 1)
  Binomial(reproductive_population[i], max(min(birth_rate[i] / 2, 1), 0)) else if (stochastic_birth == 1) Binomial(reproductive_population[i], max(min(birth_int[i] / 2, 1), 0)) else floor(reproductive_population[i] * max(min(birth_int[i] / 2, 1), 0))

# 4. MATERNAL IMMUNITY CONTRIBUTIONS

# Vaccinated mothers in reproductive age range (from vacc strata ≥ 2)
vaccinated_mums[] <- if (n_vacc <= 1) 0 else
  sum(S[repro_low:repro_high, 2:n_vacc, i]) +
  sum(E[repro_low:repro_high, 2:n_vacc, i]) +
  sum(I[repro_low:repro_high, 2:n_vacc, i]) +
  sum(R[repro_low:repro_high, 2:n_vacc, i]) +
  sum(Is[repro_low:repro_high, 2:n_vacc, i]) +
  sum(Rc[repro_low:repro_high, 2:n_vacc, i])

# Naturally immune mothers
antibody_mums[] <- sum(I[repro_low:repro_high, , i]) +
  sum(R[repro_low:repro_high, , i]) +
  sum(Is[repro_low:repro_high, , i]) +
  sum(Rc[repro_low:repro_high, , i])

# Proportion of newborns with maternal antibodies
prop_maternal_vaccinated[] <- if (reproductive_population[i] <= 0) 0 else vaccinated_mums[i] / reproductive_population[i]
prop_maternal_natural[]    <- if (reproductive_population[i] <= 0) 0 else antibody_mums[i] / reproductive_population[i]

# 5. VACCINATION COVERAGE

# Interpolated vaccination coverage (age-, vacc-, risk- specific)
vaccination_prop <- interpolate(tt_vaccination_coverage, vaccination_coverage, "constant")

# Optional output: track total coverage
update(vac_prop) <- sum(vaccination_prop)
initial(vac_prop) <- 0

# 6. TRANSMISSION PARAMETERS

# Effective infectious period, accounting for severity mix and mortality
infectious_period[, , ] <- if (
  (severe_recovery_rate + cfr_severe[i, j, k] + background_death[i, k]) <= 0 ||
  (recovery_rate + cfr_normal[i, j, k] + background_death[i, k]) <= 0
) 0 else
  (1 - prop_severe[i, j, k]) / (recovery_rate + cfr_normal[i, j, k] + background_death[i, k]) +
  prop_severe[i, j, k] / (severe_recovery_rate + cfr_severe[i, j, k] + background_death[i, k])

# Interpolated time-varying R0
t_R0 <- interpolate(tt_R0, R0, "constant")

# Base transmission rate
beta[, , ] <- if (infectious_period[i, j, k] <= 0) 0 else
  (R0_modifier * t_R0) / infectious_period[i, j, k]

# Adjusted beta based on age, vaccination, and maternal protection
beta_updated[, , ] <- if (i <= age_maternal_protection_ends)
  beta[i, j, k] *
  (1 - age_vaccination_beta_modifier[i, j, k]) *
  (1 - (protection_weight_vacc * prop_maternal_vaccinated[k] +
          protection_weight_rec  * prop_maternal_natural[k])) else
            beta[i, j, k] * (1 - age_vaccination_beta_modifier[i, j, k])

# 7. FORCE OF INFECTION

# Weighted sum of infectious individuals contributing to transmission
inf_weighted[, , ] <- beta_updated[i, j, k] * (I_available[i, j, k] + Is_available[i, j, k])
infectious_source[] <- sum(inf_weighted[i, , ])  # By age only

# Age-specific FOI: contact matrix × infectious source
lambda_contact[, ] <- contact_matrix[i, j] * infectious_source[j]

# Raw FOI per age group
lambda_raw[] <- if (Npop_age[i] <= 0) 0 else sum(lambda_contact[i, ]) / Npop_age[i]

# Final FOI per age-vacc-risk stratum
lambda[, , ] <- if (N <= 0) 0 else
  max(0, lambda_raw[i]) * (1 - age_vaccination_beta_modifier[i, j, k])

# 8. NEXT GENERATION MATRIX AND Reff

# Full NGM matrix elements: S × β × duration × contact
ngm_unfolded[, , , ] <- S_available[i, k, l] *
  beta_updated[i, k, l] *
  infectious_period[i, k, l] *
  contact_matrix[i, j]

# Age-aggregated NGM contribution (next-generation per age group)
ngm[] <- if (Npop_age[i] <= 0) 0 else sum(ngm_unfolded[i, , , ]) / Npop_age[i]

# Mean Reff across all age groups
update(Reff) <- if (n_age <= 0) 0 else sum(ngm) / n_age
initial(Reff) <- R0[1]

# 9. SEEDING

# Interpolate seeding pattern over time
t_seeded <- interpolate(tt_seeded, seeded, "constant")

# Capped seeding into S → I: no more than susceptible
seeded_actual[, , ] <- if (S[i, j, k] < t_seeded[i, j, k])
  floor(S[i, j, k] * reporting_rate) else
    floor(t_seeded[i, j, k] * reporting_rate)

# Track total seedings
update(seeded_actual_sum) <- sum(seeded_actual)
initial(seeded_actual_sum) <- 0

# ------------------------------------------------------------------------------
# DIMENSIONS — Explicitly define array shapes for all variables
# ------------------------------------------------------------------------------

# -- 2. INITIAL POPULATION ARRAYS --
dim(S0)     <- c(n_age, n_vacc, n_risk)
dim(I0)     <- c(n_age, n_vacc, n_risk)
dim(Rpop0)  <- c(n_age, n_vacc, n_risk)

# -- 3. COMPARTMENT STATES (MAIN TRACKED STATES) --
dim(S)      <- c(n_age, n_vacc, n_risk)
dim(E)      <- c(n_age, n_vacc, n_risk)
dim(I)      <- c(n_age, n_vacc, n_risk)
dim(R)      <- c(n_age, n_vacc, n_risk)
dim(Is)     <- c(n_age, n_vacc, n_risk)
dim(Rc)     <- c(n_age, n_vacc, n_risk)

# -- 4. AGEING AND TRANSITION STATES --

dim(aging_rate) <- n_age

dim(S_after_aging)     <- c(n_age, n_vacc, n_risk)
dim(E_after_aging)     <- c(n_age, n_vacc, n_risk)
dim(I_after_aging)     <- c(n_age, n_vacc, n_risk)
dim(R_after_aging)     <- c(n_age, n_vacc, n_risk)
dim(Is_after_aging)    <- c(n_age, n_vacc, n_risk)
dim(Rc_after_aging)    <- c(n_age, n_vacc, n_risk)

dim(aging_into_S) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_S) <- c(n_age, n_vacc, n_risk)
dim(aging_into_E) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_E) <- c(n_age, n_vacc, n_risk)
dim(aging_into_I) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_I) <- c(n_age, n_vacc, n_risk)
dim(aging_into_R) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_R) <- c(n_age, n_vacc, n_risk)
dim(aging_into_Is) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_Is) <- c(n_age, n_vacc, n_risk)
dim(aging_into_Rc) <- c(n_age, n_vacc, n_risk)
dim(aging_out_of_Rc) <- c(n_age, n_vacc, n_risk)

dim(S_after_vaccination)  <- c(n_age, n_vacc, n_risk)
dim(E_after_vaccination)  <- c(n_age, n_vacc, n_risk)
dim(I_after_vaccination)  <- c(n_age, n_vacc, n_risk)
dim(R_after_vaccination)  <- c(n_age, n_vacc, n_risk)
dim(Is_after_vaccination) <- c(n_age, n_vacc, n_risk)
dim(Rc_after_vaccination) <- c(n_age, n_vacc, n_risk)

dim(S_after_waning)     <- c(n_age, n_vacc, n_risk)
dim(E_after_waning)     <- c(n_age, n_vacc, n_risk)
dim(I_after_waning)     <- c(n_age, n_vacc, n_risk)
dim(R_after_waning)     <- c(n_age, n_vacc, n_risk)
dim(Is_after_waning)    <- c(n_age, n_vacc, n_risk)
dim(Rc_after_waning)    <- c(n_age, n_vacc, n_risk)

# -- 5. VACCINATION TRANSITION ARRAYS --
dim(tt_vaccination_coverage) <- no_vacc_changes
dim(vaccination_coverage) <- c(n_age, n_vacc, n_risk, no_vacc_changes)
dim(vaccination_prop) <- c(n_age, n_vacc, n_risk)

dim(vaccinating_into_S)  <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_S) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_into_E)  <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_E) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_into_I)  <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_I) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_into_R)  <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_R) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_into_Is)  <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_Is) <- c(n_age, n_vacc, n_risk)
dim(vaccinating_into_Rc)  <- c(n_age, n_vacc, n_risk)
dim(vaccinating_out_of_Rc) <- c(n_age, n_vacc, n_risk)

# -- 6. WANING TRANSITIONS --
dim(waning_to_S_short) <- c(n_age, n_vacc, n_risk)

dim(waning_R) <- c(n_age, n_vacc, n_risk)
dim(waning_Rc) <- c(n_age, n_vacc, n_risk)

dim(waning_from_S_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_S_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_S_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_S_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_E_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_E_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_E_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_E_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_I_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_I_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_I_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_I_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_R_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_R_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_R_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_R_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_Is_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Is_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_Is_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Is_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(waning_from_Rc_short) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Rc_long) <- c(n_age, n_vacc, n_risk)
dim(waning_from_Rc_long) <- c(n_age, n_vacc, n_risk)
dim(waning_to_Rc_unvaccinated) <- c(n_age, n_vacc, n_risk)

dim(short_term_waning) <- n_vacc
dim(long_term_waning)  <- n_vacc

# -- 7. MIGRATION VARIABLES --
dim(tt_migration)              <- no_migration_changes
dim(migration_distribution)    <- 6
dim(migration_distribution_values) <- c(6, no_migration_changes)
dim(migration_in_number)       <- c(n_age, n_vacc, n_risk, no_migration_changes)
dim(migration_adjusted)        <- c(n_age, n_vacc, n_risk)
dim(migration)                 <- c(n_age, n_vacc, n_risk)

dim(migration_S)   <- c(n_age, n_vacc, n_risk)
dim(migration_E)   <- c(n_age, n_vacc, n_risk)
dim(migration_I)   <- c(n_age, n_vacc, n_risk)
dim(migration_R)   <- c(n_age, n_vacc, n_risk)
dim(migration_Is)  <- c(n_age, n_vacc, n_risk)
dim(migration_Rc)  <- c(n_age, n_vacc, n_risk)

# -- 8. BIRTH & DEATH CALCULATIONS --
dim(Births)                 <- n_age
dim(reproductive_population) <- n_age
dim(birth_rate)            <- n_risk
dim(crude_birth)           <- c(n_age, no_birth_changes)
dim(crude_death)           <- c(n_age, n_risk, no_death_changes)
dim(tt_birth_changes)      <- no_birth_changes
dim(tt_death_changes)      <- no_death_changes
dim(background_death)      <- c(n_age, n_risk)
dim(Npop_background_death) <- c(n_age, n_risk)
dim(birth_int)             <- n_age
dim(death_int)             <- c(n_age, n_risk)

dim(S_death)  <- c(n_age, n_vacc, n_risk)
dim(E_death)  <- c(n_age, n_vacc, n_risk)
dim(I_death)  <- c(n_age, n_vacc, n_risk)
dim(R_death)  <- c(n_age, n_vacc, n_risk)
dim(Is_death) <- c(n_age, n_vacc, n_risk)
dim(Rc_death) <- c(n_age, n_vacc, n_risk)

# -- 9. TRANSMISSION CALCULATIONS --
dim(inf_weighted)     <- c(n_age, n_vacc, n_risk)
dim(infectious_source) <- n_age
dim(lambda_contact)   <- c(n_age, n_age)
dim(lambda_raw)       <- n_age
dim(ngm_unfolded)      <- c(n_age, n_age, n_vacc, n_risk)
dim(ngm)               <- n_age
dim(lambda_S) <- c(n_age, n_vacc, n_risk)
dim(beta_updated) <- c(n_age, n_vacc, n_risk)
dim(lambda) <- c(n_age, n_vacc, n_risk)
dim(infectious_period) <- c(n_age, n_vacc, n_risk)
dim(beta) <- c(n_age, n_vacc, n_risk)
dim(tt_R0) <- no_R0_changes
dim(R0) <- no_R0_changes
dim(cfr_normal) <- c(n_age, n_vacc, n_risk)
dim(cfr_severe) <- c(n_age, n_vacc, n_risk)
dim(prop_severe) <- c(n_age, n_vacc, n_risk)
dim(prop_complications) <- c(n_age, n_vacc, n_risk)
dim(age_vaccination_beta_modifier) <- c(n_age, n_vacc, n_risk)
dim(contact_matrix) <- c(n_age, n_age)

# -- 10. SEEDING & FITTING OUTPUTS --
dim(seeded_actual)    <- c(n_age, n_vacc, n_risk)
dim(seropositive)     <- n_age
dim(serosurvey)       <- n_age
dim(incubated) <- c(n_age, n_vacc, n_risk)
dim(recovered_I_to_R) <- c(n_age, n_vacc, n_risk)
dim(recovered_from_Is) <- c(n_age, n_vacc, n_risk)
dim(into_I) <- c(n_age, n_vacc, n_risk)
dim(into_Is) <- c(n_age, n_vacc, n_risk)
dim(recovered_Is_to_R) <- c(n_age, n_vacc, n_risk)
dim(recovered_Is_to_Rc) <- c(n_age, n_vacc, n_risk)
dim(Npop_age_risk) <- c(n_age, n_risk)
dim(tt_seeded) <- no_seeded_changes
dim(seeded) <- c(n_age, n_vacc, n_risk, no_seeded_changes)
dim(t_seeded) <- c(n_age, n_vacc, n_risk)

# -- 11. REPRODUCTIVE AGE STRUCTURE --
dim(repro_weight)     <- c(n_age, no_migration_changes)
dim(repro_weight_now) <- n_age
dim(vaccinated_mums) <- n_age
dim(antibody_mums) <- n_age
dim(prop_maternal_vaccinated) <- n_age
dim(prop_maternal_natural) <- n_age

# -- 12. TRANSITION POOLS FOR DERIVED COMPARTMENTS --
dim(S_available)   <- c(n_age, n_vacc, n_risk)
dim(E_available)   <- c(n_age, n_vacc, n_risk)
dim(I_available)   <- c(n_age, n_vacc, n_risk)
dim(R_available)   <- c(n_age, n_vacc, n_risk)
dim(Is_available)  <- c(n_age, n_vacc, n_risk)
dim(Rc_available)  <- c(n_age, n_vacc, n_risk)

dim(Npop_age) <- n_age

# ------------------------------------------------------------------------------
# ADDITIONAL OUTPUTS — For diagnostics, summaries, and sanity checks
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 1. TOTAL POPULATION TRACKING
# ------------------------------------------------------------------------------

# Current total population (recalculated at each step)
update(total_pop) <- N

# Initial total population (from starting state)
initial(total_pop) <- sum(S0) + sum(I0) + sum(Rpop0)

# ------------------------------------------------------------------------------
# 2. SEROLOGICAL OUTPUT
# ------------------------------------------------------------------------------

# Age-specific proportion seropositive (excluding unvaccinated)
# Includes:
#   - vaccinated susceptibles (j > 1)
#   - all infectious (I, Is)
#   - all recovered (R, Rc)
update(seropositive[]) <- if(Npop_age[i] <= 0) 0 else (
  sum(S[i, 2:n_vacc, ]) +
    sum(I[i, , ]) +
    sum(Is[i, , ]) +
    sum(R[i, , ]) +
    sum(Rc[i, , ])
) / Npop_age[i]

initial(seropositive[]) <- 0

# ------------------------------------------------------------------------------
# 3. BIRTHS AND DEATHS
# ------------------------------------------------------------------------------

update(total_births) <- sum(Births)
initial(total_births) <- 0

update(total_deaths) <- sum(S_death) + sum(E_death) + sum(I_death) +
  sum(R_death) + sum(Is_death) + sum(Rc_death)
initial(total_deaths) <- 0

# ------------------------------------------------------------------------------
# 4. VACCINATION FLOWS
# ------------------------------------------------------------------------------

# Total number of S individuals vaccinated (used for debugging)
update(S_vaccinated) <- sum(vaccinating_out_of_S)
initial(S_vaccinated) <- 0

# ------------------------------------------------------------------------------
# 5. CONSISTENCY CHECKS
# ------------------------------------------------------------------------------

# Net population change per timestep
update(net_pop_change) <- total_births - total_deaths
initial(net_pop_change) <- 0

# ------------------------------------------------------------------------------
# 6. CASES
# ------------------------------------------------------------------------------

# -- New infections (used for output) --
update(new_case[, , ]) <- incubated[i, j, k] + seeded_actual[i, j, k]
initial(new_case[, , ]) <- I0[i, j, k]
dim(new_case) <- c(n_age, n_vacc, n_risk)

browser(phase = "update")
