#brightness
bt<-lmer(B2~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = throat, REML = FALSE)
summary(bt) #sex (estimate = 1.31 for males; p-value =0.019) is significant
confint(bt) #sex (0.26,2.24)
rsq(bt) #0.52
hist(resid(bt))
isSingular(bt, tol = 1e-7) #model is not singular
rePCA(bt)
vif(bt)

################################################################################

#FWHM
ft<-lmer(FWHM~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = throat, REML = FALSE)
summary(ft) # sex (estimate = -41.74 for males; p-value = 0.048) 
confint(ft) #sex (-77.96,-5.52)
rsq(ft) #0.25
hist(resid(ft))
isSingular(ft, tol = 1e-7) #model is not singular
rePCA(ft)
vif(ft)

################################################################################

#UV Chroma
#st<-lmer(I(asin(sqrt(S1U)))~sex+year+julian+bodycond + urbanization*pvol.tars + (1|nestid),data = throat)
st<-lmer(S1U~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = throat, REML = FALSE)
summary(st)
confint (st)
rsq(st) #0.44
hist(resid(st))
isSingular(st, tol = 1e-7) #model is not singular
rePCA(st)
vif(st)
################################################################################

#Peak wavelength
ht<-lmer(H1~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = throat, REML = FALSE)
summary(ht) #year (estimate = 30.97; p-value = 0.076)
confint(ht) #year (0.82,61.13)
rsq(ht) #0.22
hist(resid(ht))
isSingular(ht, tol = 1e-7) #model is not singular
rePCA(ht)
vif(ht) 


#Brightness
be <- lmer(B2~sex+year+julian+bodycond+urbanization+pvol.tars+
             (1|nestid), data = belly, REML = FALSE)
summary(be) #urb.score*body condition (estimate = 0.16; p-value = 0.010), sex (estimate = -0.55; p-value = 0.078)
confint(be)
rsq(be) #0.75
hist(resid(be))
isSingular(be, tol = 1e-7) #model is singular
rePCA(be)
vif(be)
################################################################################

#FWHM
fe<-lmer(FWHM~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = belly, REML = FALSE)
summary(fe) #no significance
confint(fe, method = "boot", nsim = 1000)
rsq(fe) #0.27
hist(resid(fe))
isSingular(fe, tol = 1e-7) #model is not singular
rePCA(fe)
vif(fe)

################################################################################

#UV Chroma
#se<-lmer(S1U~sex+julian+year+bodycond + urbanization*pvol.tars + (1|nestid), data = belly)
se<-lmer(S1U~sex+year+julian+pvol.tars+urbanization*bodycond+
           (1|nestid), data = belly, REML = FALSE)
summary(se) #Sex (estimate = 0.028 for males; p-value = 0.00098)
confint(se) #sex (0.014,0.041)
rsq(se) #0.52
hist(resid(se))
isSingular(se, tol = 1e-7) #model is not singular
rePCA(se)
vif(se) 

################################################################################

#Peak wavelength
#s<- list(1/300)
he<-lmer(H1~sex+year+julian+pvol.tars+urbanization+bodycond+
           (1|nestid), data = belly, REML = FALSE)
summary(he) #Sex (estimate = -17.16 for males; p-value = 0.063)
confint(he)
rsq(he) #0.42
hist(resid(he))
isSingular(he, tol = 1e-7) #model is not singular
rePCA(he)
vif(he) #all <5

#Brightness
ba <- lmer(B2~sex+year+julian+bodycond+urbanization+pvol.tars+
             (1|nestid), data = back, REML = FALSE)
summary(ba) #no significant effect on brightness
confint(ba)
rsq(ba) #0.16
hist(resid(ba))
isSingular(ba, tol = 1e-7)
rePCA(ba)
vif(ba) #all <5
################################################################################

#FWHM
fa<-lmer(FWHM~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = back, REML = FALSE)
summary(fa) #body condition (estimate = -12.80; p-value = 0.0073)
confint(fa) #body condition: (-20.52,-4.44)
rsq(fa) #0.72
hist(resid(fa))
isSingular(fa, tol = 1e-7) #model is not singular
rePCA(fa)
vif(fa) 

################################################################################

#UV Chroma
#sa<-lmer(I(asin(sqrt(S1U)))~sex+julian+year+bodycond+ urbanization*pvol.tars+(1|nestid),data = back)
sa<-lmer(S1U~sex+year+julian+bodycond+urbanization+pvol.tars+
           (1|nestid), data = back, REML = FALSE)
summary(sa) 
confint(sa)
rsq(sa) #0.24
hist(resid(sa))
isSingular(sa, tol = 1e-7) #model is not singular
rePCA(sa)
vif(sa) #multicollinearity not present

################################################################################

#Peak wavelength
#s<- list(1/300)
ha<-lmer(H1~sex+year+julian+pvol.tars+urbanization*bodycond+
           (1|nestid),data = back, REML = FALSE)
summary(ha) 
confint(ha)
rsq(ha) #0.41
hist(resid(ha))
isSingular(ha, tol = 1e-7) #IT IS SINGULAR, but just because the random effect is 0
rePCA(ha)
vif(ha) 


# Define a function to format numbers based on scientific practices
format_number <- function(x) {
  if (abs(x) >= 0.01 && abs(x) < 100) {
    formatC(x, digits = 2, format = "f")
  } else {
    formatC(x, digits = 2, format = "e")
  }
}

# Define a function to extract and format information from summary and confint
extract_model_info <- function(model, model_name) {
  # Get model summary
  model_summary <- summary(model)
  
  # Extract fixed effects
  fixed_effects <- model_summary$coefficients
  estimates <- fixed_effects[, "Estimate"]
  std_errors <- fixed_effects[, "Std. Error"]
  t_values <- fixed_effects[, "t value"]
  p_values <- fixed_effects[, "Pr(>|t|)"]
  
  # Format estimates and standard errors
  formatted_estimates <- paste0(sapply(estimates, format_number),
                                " +/- ",
                                sapply(std_errors, format_number))
  
  # Get confidence intervals for fixed effects only
  conf_intervals <- confint(model, parm = "beta_", method = "profile")
  
  # Format confidence intervals
  formatted_intervals <- apply(conf_intervals, 1, function(x) {
    paste0(" ", sapply(x[1], format_number),
           " - ",
           sapply(x[2], format_number))
  })
  
  # Combine everything into a data frame
  summary_data <- data.frame(
    Model = model_name,
    Fixed_Effects = formatted_estimates,
    t_value = formatC(t_values, digits = 2, format = "f"),
    p_value = formatC(p_values, digits = 3, format = "g"),
    Confidence_Interval = formatted_intervals,
    Term = rownames(fixed_effects)
  )
  
  return(summary_data)
}

## save model results to table
# Extract and format summaries for all models
models <- list(bt = bt, ft = ft, st = st, ht = ht, be = be, fe = fe, se = se, he = he, ba = ba, fa = fa, sa = sa, ha = ha)
all_summaries <- lapply(names(models), function(name) extract_model_info(models[[name]], name))

# Combine summaries into one data frame
combined_summaries <- bind_rows(all_summaries)

# Move 'Term' column to the first position
combined_summaries <- combined_summaries %>%
  select(Term, Model, everything())

# Write to CSV file with UTF-8 encoding
library(here)
write.csv(combined_summaries, here("results", "model_summaries.csv"), row.names = FALSE)