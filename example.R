# Load necessary libraries
library(brms)
library(readxl)
library(dplyr)

# Load data
df <- read_excel("AUDITEE.xlsx")

# Rename columns to simpler names
df <- df %>%
  rename(
    FRQ = `AUDIT REPORT DELAY`,
    ARB = `LEVERAGE DEBT TO EQUITY(X)`,
    SWT = `AUDIT SWITCH (1,0)`,
    TEN = `AUDIT TENURE`,
    COMP = `AUDIT FEES`,
    SIZ = `LOG OF TOTAL ASSET (FIRM SIZE)`,
    INDEP = `BOARD INDEPENDENCE (%)`
  )

# Verify column names
print(names(df))  # Debugging: Ensure new column names are applied

# Fit the Bayesian regression model using Student-T likelihood
fit <- brm(
  formula = FRQ ~ ARB + SWT + TEN + COMP + SIZ + INDEP,
  data = df,
  family = student(),  # Correct Student-T distribution
  prior = c(
    set_prior("normal(0, 5)", class = "b"),        # Priors for coefficients
    set_prior("normal(0, 10)", class = "Intercept"), # Prior for intercept
    set_prior("cauchy(0, 5)", class = "sigma")     # Prior for sigma
  ),
  iter = 4000,   
  chains = 4,    
  cores = 4,    
  control = list(adapt_delta = 0.95)  
)

# Summary and diagnostic checks
summary(fit)
plot(fit)
pp_check(fit)  # Posterior predictive check



library(brms)

# Bayesian regression with weakly informative priors
bayesian_model <- brm(FRQ ~ ARB + SWT + TEN + LEV + COMP + SIZ + INDEP, 
                      data = banking_data,
                      family = student(),  # Handles non-normality
                      prior = c(set_prior("normal(0,5)", class = "b"),  # Prior for coefficients
                                set_prior("normal(0,10)", class = "Intercept")),  # Prior for Intercept
                      iter = 4000, chains = 4, cores = 4, control = list(adapt_delta = 0.95))

summary(bayesian_model)
plot(bayesian_model)  # Check posterior distributions


bayesian_qr <- brm(bf(FRQ ~ ARB + SWT + TEN + LEV + COMP + SIZ + INDEP, quantile = 0.5), 
                   data = banking_data, family = asymmetric(),
                   iter = 4000, chains = 4, cores = 4)
summary(bayesian_qr)


bayesian_panel_model <- brm(FRQ ~ ARB + SWT + TEN + LEV + COMP + SIZ + INDEP + (1 | Bank), 
                            data = banking_data, 
                            family = student(),
                            iter = 4000, chains = 4, cores = 4)
summary(bayesian_panel_model)
