# Purpose:     Script for the Supplementary Result of Climate Anomalies & Ambient Air Pollution and Miscarriage.
# Manuscript DOI: 10.1097/EE9.0000000000000420.  
# Author: 			Prince M. Amegbor
# Date Created:	April 9th, 2025 by Prince M. Amegbor
# *******************************************************************************************************************************/
rm(list = ls())
gc(reset=TRUE)

# Load libraries
library(INLA)
library(sf)          
library(tidyverse)   
library(openxlsx)    
library(dplyr)
library(purrr)
library(labelled)



###Load manuscript data  
GDHS_2 <- readRDS("~/GMHS_ENVS_2017.rds" )  

###priors - Set prior on precision
prec.prior1 <- list(theta =list(prior="pc.prec",param=c(0.5,0.01))) 

prec.prior2 <- list(prec = list(prior = "loggamma", param = c(0.01, 0.01)))

prec.prior3 <- list(prec = list(prior = "pc.prec",
                                param = c(1, 0.01)))


# ---------------------------------------------------------------------------------------------------------------
# 1.	Appendix A – Results Bayesian Non-Linear Hierarchical Models
# ---------------------------------------------------------------------------------------------------------------

# Non-linear model for Tmp_z
Mod_in1a <- inla(pregMC ~ f(inla.group(Tmp_z), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for Tmx_z
Mod_in1b <- inla(pregMC ~ f(inla.group(Tmx_z), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))


# Non-linear model for Preci_z
Mod_in1d <- inla(pregMC ~ f(inla.group(Preci_z), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for PM
Mod_in2a <- inla(pregMC ~ f(inla.group(PM), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for NO
Mod_in2b <- inla(pregMC ~ f(inla.group(NO), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for CO
Mod_in2c <- inla(pregMC ~ f(inla.group(CO), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for SO
Mod_in2d <- inla(pregMC ~ f(inla.group(SO), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for SU
Mod_in2e <- inla(pregMC ~ f(inla.group(SU), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for Ozone
Mod_in2f <- inla(pregMC ~ f(inla.group(Ozone), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for OC
Mod_in2g <- inla(pregMC ~ f(inla.group(OC), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for BC
Mod_in2h <- inla(pregMC ~ f(inla.group(BC), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for EVI
Mod_in2i <- inla(pregMC ~ f(inla.group(EVI), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))



# Function to extract, rename, exponentiate from random effects
tidy_inla_random <- function(model, var) {
  out <- as.data.frame(model$summary.random[[paste0("inla.group(", var, ")")]])
  out <- out %>%
    rename(lci = `0.025quant`, uci = `0.975quant`) %>%
    mutate(across(c(mean, lci, uci), exp))
  return(out)
}

# Define models and their target variables
rand_specs <- list(
  Mod1_Temp  = list(model = Mod_in1a, var = "Tmp_z"),
  Mod1_Tmx   = list(model = Mod_in1b, var = "Tmx_z"),
  Mod1_Preci = list(model = Mod_in1c, var = "Preci_z"),
  Mod2_PM    = list(model = Mod_in2a, var = "PM"),
  Mod2_NO    = list(model = Mod_in2b, var = "NO"),
  Mod2_CO    = list(model = Mod_in2c, var = "CO"),
  Mod2_SO    = list(model = Mod_in2d, var = "SO"),
  Mod2_SU    = list(model = Mod_in2e, var = "SU"),
  Mod2_Ozone = list(model = Mod_in2f, var = "Ozone"),
  Mod2_OC    = list(model = Mod_in2g, var = "OC"),
  Mod2_BC    = list(model = Mod_in2h, var = "BC"),
  Mod2_EVI   = list(model = Mod_in2i, var = "EVI")
)

# Apply extractor to each specification
rand_summaries <- imap(rand_specs, ~ tidy_inla_random(.x$model, .x$var))

# Export to Excel (each element = one sheet)
write.xlsx(rand_summaries,file = "PregMC_GAM_mini.xlsx",overwrite = TRUE) ## save results for minimally adjusted model

##### Adjusted Non-linear models
Mod_in1a <- inla(pregMC ~ f(inla.group(Tmp_z), model="rw2", scale.model=TRUE) +
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for Tmx_z
Mod_in1b <- inla(pregMC ~ f(inla.group(Tmx_z), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for Preci_z
Mod_in1c <- inla(pregMC ~ f(inla.group(Preci_z), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for PM
Mod_in2a <- inla(pregMC ~ f(inla.group(PM), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for NO
Mod_in2b <- inla(pregMC ~ f(inla.group(NO), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) +  
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for CO
Mod_in2c <- inla(pregMC ~ f(inla.group(CO), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) +  
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for SO
Mod_in2d <- inla(pregMC ~ f(inla.group(SO), model="rw2", scale.model=TRUE) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) +  
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for SU
Mod_in2e <- inla(pregMC ~ f(inla.group(SU), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for Ozone
Mod_in2f <- inla(pregMC ~ f(inla.group(Ozone), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for OC
Mod_in2g <- inla(pregMC ~ f(inla.group(OC), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for BC
Mod_in2h <- inla(pregMC ~ f(inla.group(BC), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))

# Non-linear model for EVI
Mod_in2i <- inla(pregMC ~ f(inla.group(EVI), model="rw2", scale.model=TRUE) + 
                   factor(qhwlthi) + factor(ph_cook_clean) +  
                   factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + 
                   f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1) + 
                   f(Reg_ID, model = "iid", hyper = prec.prior3) + 
                   f(DIST_ID, model = "iid", hyper = prec.prior3),
                 data=GDHS_2, family = "binomial", Ntrials = 1,
                 control.family = list(link = "cloglog"), verbose=TRUE,
                 control.compute = list(dic=TRUE, waic=TRUE))




# Define models and their target variables
rand_specs <- list(
  Mod1_Temp  = list(model = Mod_in1a, var = "Tmp_z"),
  Mod1_Tmx   = list(model = Mod_in1b, var = "Tmx_z"),
  Mod1_Preci = list(model = Mod_in1c, var = "Preci_z"),
  Mod2_PM    = list(model = Mod_in2a, var = "PM"),
  Mod2_NO    = list(model = Mod_in2b, var = "NO"),
  Mod2_CO    = list(model = Mod_in2c, var = "CO"),
  Mod2_SO    = list(model = Mod_in2d, var = "SO"),
  Mod2_SU    = list(model = Mod_in2e, var = "SU"),
  Mod2_Ozone = list(model = Mod_in2f, var = "Ozone"),
  Mod2_OC    = list(model = Mod_in2g, var = "OC"),
  Mod2_BC    = list(model = Mod_in2h, var = "BC"),
  Mod2_EVI   = list(model = Mod_in2i, var = "EVI")
)

# Apply extractor to each specification
rand_summaries <- imap(rand_specs, ~ tidy_inla_random(.x$model, .x$var))

# Export to Excel (each element = one sheet)
write.xlsx(rand_summaries,file = "PregMC_GAM_adj.xlsx",overwrite = TRUE) ## save results for minimally adjusted model


#------------------------
# Plotting Figure A1 
#------------------------
## Import 
my_sheet_names <- excel_sheets("PregMC_GAM__mini.xlsx")
my_sheets <- lapply(my_sheet_names, function(x) read_excel("PregMC_GAM__mini.xlsx", 
                                                           sheet = x))
names(my_sheets) <- my_sheet_names
list2env(my_sheets, envir=.GlobalEnv)

#####In-utero GAM Stunted Models
#Temperature
T1 <- ggplot(data=Mod1_Temp,aes(x=ID, y=mean, ymin=lci, ymax=uci)) + 
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  xlab(as.expression(expression( paste("Mean Temperature anomaly (", sd, ")") ))) + 
  ylab("Estimated effect (HR)") + labs(title="Model 1")

T2 <- ggplot(data=Mod1_Tmx,aes(x=ID, y=mean, ymin=lci, ymax=uci)) + 
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  xlab(as.expression(expression( paste("Maximum Temperature anomaly (", sd, ")") ))) + 
  ylab("Estimated effect (HR)") + labs(title="Model 1")

P1 <- ggplot(data=Mod1_Preci,aes(x=ID, y=mean, ymin=lci, ymax=uci)) + 
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  xlab(as.expression(expression( paste("Precipitation anomaly (", sd, ")") ))) + 
  ylab("Estimated effect (HR)") + labs(title="Model 1")


PM1 <- ggplot(data = Mod2_PM, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("PM2.5 (", mu, "g/m³)"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")


NO1 <- ggplot(data = Mod2_NO, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Nitrogen Dioxide (", log, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

CO1 <- ggplot(data = Mod2_CO, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Carbon Monoxide (", ppbv, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")


SO1 <- ggplot(data = Mod2_SO, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("SO (", mu, "g/m³)"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")


Ozone1 <- ggplot(data = Mod2_Ozone, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Ozone (", DU, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")


EVI1 <- ggplot(data = Mod2_EVI, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Enhanced Vegetation Index (", EVI, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")


tiff("Figure_A1.tiff", units="in", width=15, height=18, res=300)
ggarrange(T1,T2,P1,PM1,NO1,CO1,SO1,Ozone1,EVI1,
          ncol = 3, nrow = 3)
dev.off()


#######
## Import 
my_sheet_names <- excel_sheets("PregMC_GAM_adj.xlsx")
my_sheets <- lapply(my_sheet_names, function(x) read_excel("PregMC_GAM_adj.xlsx", 
                                                           sheet = x))
names(my_sheets) <- my_sheet_names
list2env(my_sheets, envir=.GlobalEnv)

#####In-utero GAM Stunted Models
#Temperature
T1 <- ggplot(data=Mod1_Temp,aes(x=ID, y=mean, ymin=lci, ymax=uci)) + 
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  xlab(as.expression(expression( paste("Mean Temperature anomaly (", sd, ")") ))) + 
  ylab("Estimated effect (HR)") + labs(title="Model 1")

T2 <- ggplot(data=Mod1_Tmx,aes(x=ID, y=mean, ymin=lci, ymax=uci)) + 
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  xlab(as.expression(expression( paste("Maximum Temperature anomaly (", sd, ")") ))) + 
  ylab("Estimated effect (HR)") + labs(title="Model 1")

P1 <- ggplot(data=Mod1_Preci,aes(x=ID, y=mean, ymin=lci, ymax=uci)) + 
  geom_line() + 
  geom_ribbon(alpha=0.1) + 
  xlab(as.expression(expression( paste("Precipitation anomaly (", sd, ")") ))) + 
  ylab("Estimated effect (HR)") + labs(title="Model 1")

PM1 <- ggplot(data = Mod2_PM, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("PM2.5 (", mu, "g/m³)"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

NO1 <- ggplot(data = Mod2_NO, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Nitrogen Dioxide (", log, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

CO1 <- ggplot(data = Mod2_CO, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Carbon Monoxide (", ppbv, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

SO1 <- ggplot(data = Mod2_SO, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("SO (", mu, "g/m³)"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

Ozone1 <- ggplot(data = Mod2_Ozone, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Ozone (", DU, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

EVI1 <- ggplot(data = Mod2_EVI, aes(x = ID, y = mean, ymin = lci, ymax = uci)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.1) + 
  xlab(expression(paste("Enhanced Vegetation Index (", EVI, ")"))) + 
  ylab("Estimated effect (HR)") + 
  labs(title = "Model 1")

tiff("Figure_A2.tiff", units="in", width=15, height=18, res=300)
ggarrange(T1,T2,P1,PM1,NO1,CO1,SO1,Ozone1,EVI1,
          ncol = 3, nrow = 3)
dev.off()


# ----------------------------------------------------------------------------------------------
# 2.	Appendix B – Results of Region-Specific Effect on Spontaneous Miscarriage 
# ----------------------------------------------------------------------------------------------
CL_1 <- inla(pregMC ~ 1 + f(Reg_ID, model="iid") + 
               f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1),
             data=GDHS_2,family = "binomial", Ntrials = 1,control.family = list(link = "cloglog"),
             verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

CL_2 <- inla(pregMC ~ 1 + Tmp_z + Tmx_z + Preci_z + f(Reg_ID, model="iid") +
               f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1),
             data=GDHS_2,family = "binomial", Ntrials = 1,control.family = list(link = "cloglog"),
             verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


CL_3 <- inla(pregMC ~ 1 + PM + NO + CO + SO +SU + Ozone + f(Reg_ID, model="iid") + 
               f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1),
             data=GDHS_2,family = "binomial", Ntrials = 1,control.family = list(link = "cloglog"),
             verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


CL_4 <- inla(pregMC ~ 1 + Tmp_z + Tmx_z + Preci_z +
               PM + NO + CO + SO +SU + Ozone + EVI + f(Reg_ID, model="iid") + 
               f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1),
             data=GDHS_2,family = "binomial", Ntrials = 1,control.family = list(link = "cloglog"),
             verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

CL_5 <- inla(pregMC ~ 1 + Tmp_z + Tmx_z + Preci_z +
               PM + NO + CO + SO +SU + Ozone + EVI + factor(qhwlthi) + factor(ph_cook_clean) +  
               factor(ph_wtr_improve) + factor(ph_sani_improve) + factor(EDU) + factor(qtype) + f(Reg_ID, model="iid") + 
               f(inla.group(wt), model="rw1", scale.model=TRUE, hyper = prec.prior1),
             data=GDHS_2,family = "binomial", Ntrials = 1,control.family = list(link = "cloglog"),
             verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


tidy_random <- function(model, df, id_col = "Reg_ID", label_col = "qregion") {
  model$summary.random[[id_col]] %>%
    rename(lci = `0.025quant`, uci = `0.975quant`) %>%
    mutate(
      !!label_col := df[[label_col]][match(ID, df[[id_col]])],
      across(c(mean, lci, uci), exp)
    )
}


Mod_1 <- tidy_random(CL_1)
Mod_2 <- tidy_random(CL_2)
Mod_3 <- tidy_random(CL_3)
Mod_4 <- tidy_random(CL_4)
Mod_5 <- tidy_random(CL_5)

# ------------------------------------------------
# Figure B1
# ------------------------------------------------
require(ggplot2)
require(gridExtra)
library(tidyverse)
library(ggpubr)
library(ggh4x)


RG_1 <- ggplot(Mod_1, aes(x = mean, y = qregion)) +
  geom_point() +  # Plot points for the mean estimates
  geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.2) +  # Add horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Add a vertical line at 0
  labs(title = "Model 1",
       x = "Random Effect (Posterior Mean - HR (exp(β)))",
       y = "Regions") +
  scale_x_log10() + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

RG_2 <- ggplot(Mod_2, aes(x = mean, y = qregion)) +
  geom_point() +  # Plot points for the mean estimates
  geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.2) +  # Add horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Add a vertical line at 0
  labs(title = "Model 2",
       x = "Random Effect (Posterior Mean - HR (exp(β)))",
       y = "Regions") +
  scale_x_log10() + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

RG_3 <- ggplot(Mod_3, aes(x = mean, y = qregion)) +
  geom_point() +  # Plot points for the mean estimates
  geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.2) +  # Add horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Add a vertical line at 0
  labs(title = "Model 3",
       x = "Random Effect (Posterior Mean - HR (exp(β)))",
       y = "Regions") +
  scale_x_log10() + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

RG_4 <- ggplot(Mod_4, aes(x = mean, y = qregion)) +
  geom_point() +  # Plot points for the mean estimates
  geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.2) +  # Add horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Add a vertical line at 0
  labs(title = "Model 4",
       x = "Random Effect (Posterior Mean - HR (exp(β)))",
       y = "Regions") +
  scale_x_log10() + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title

RG_5 <- ggplot(Mod_5, aes(x = mean, y = qregion)) +
  geom_point() +  # Plot points for the mean estimates
  geom_errorbarh(aes(xmin = lci, xmax = uci), height = 0.2) +  # Add horizontal error bars
  geom_vline(xintercept = 1, linetype = "dashed", color = "red") +  # Add a vertical line at 0
  labs(title = "Model 5",
       x = "Random Effect (Posterior Mean - HR  (exp(β)))",
       y = "Regions") +
  scale_x_log10() + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))  # Center the title


# Adjust plot margins and sizes
RG_1 <-RG_1 + theme(plot.margin = margin(10, 10, 10, 10))
RG_2 <-RG_2 + theme(plot.margin = margin(10, 10, 10, 10))
RG_3 <-RG_3 + theme(plot.margin = margin(10, 10, 10, 10))
RG_4 <-RG_4 + theme(plot.margin = margin(10, 10, 10, 10))
RG_5 <-RG_5 + theme(plot.margin = margin(10, 10, 10, 10))

# Save as TIFF
tiff("Figure_B1.tiff", units="in", width=9, height=12, res=300)
ggarrange(RG_1, RG_2, RG_3, RG_4,RG_5,
          ncol = 2, nrow = 3,
          common.legend = TRUE, legend = "bottom")
dev.off()


# ---------------------------------------------------------------------------------------------------
# Appendix C –  Effect of Preconception Exposure on Spontaneous Miscarriage by Rural-Urban Residency
# ---------------------------------------------------------------------------------------------------
TMP<- inla(pregMC ~ 1 + f(inla.group(Tmp_z), model="rw2", group=qtype),
           data=GDHS_2, family = "binomial", Ntrials = 1, 
           control.family = list(link = "cloglog"), 
           verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


TMX<- inla(pregMC ~ 1 + f(inla.group(Tmx_z), model="rw2", group=qtype),
           data=GDHS_2, family = "binomial", Ntrials = 1, 
           control.family = list(link = "cloglog"), 
           verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


PRE<- inla(pregMC ~ 1 + f(inla.group(Preci_z), model="rw2", group=qtype),
           data=GDHS_2, family = "binomial", Ntrials = 1, 
           control.family = list(link = "cloglog"), 
           verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


PM<- inla(pregMC ~ 1 + f(inla.group(PM), model="rw2", group=qtype),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

NO<- inla(pregMC ~ 1 + f(inla.group(NO), model="rw2", group=qtype),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

CO<- inla(pregMC ~ 1 + f(inla.group(CO), model="rw2", group=qtype),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

SO<- inla(pregMC ~ 1 + f(inla.group(SO), model="rw2", group=qtype),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

O3<- inla(pregMC ~ 1 + f(inla.group(Ozone), model="rw2", group=qtype),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

EV<- inla(pregMC ~ 1 + f(inla.group(EVI), model="rw2", group=qtype),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


#--- Tidy summary.fitted
tidy_inla <- function(model) {
  model$summary.fitted[, c(1, 2, 3, 5)] %>%
    rename(lci = `0.025quant`, uci = `0.975quant`) %>%
    mutate(across(c(mean, lci, uci), exp))
}

#--- Plot function (call it when you want a plot)
plot_inla <- function(data, mod_summary, var_name, title_text, file_name) {
  tiff(file_name, units="in", width=10, height=8, res=300)
  ggplot(data = data.frame(
    Place = data$qtype,
    xval = inla.group(data[[var_name]]),
    HR = mod_summary$mean,
    lci = mod_summary$lci,
    uci = mod_summary$uci
  ),
  aes(x = xval, y = HR, col = Place)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lci, ymax = uci, fill = Place), alpha = 0.2) +
    ggtitle(title_text) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  dev.off()
}

##Extract and Plot
#--- Temperature Models
Mod_TMP <- tidy_inla(TMP)
plot_inla(GDHS_2, Mod_TMP, "Tmp_z", "Mean Temperature Anomaly", "Figure C1.tiff")

Mod_TMX <- tidy_inla(TMX)
plot_inla(GDHS_2, Mod_TMX, "Tmx_z", "Maximum Temperature Anomaly", "Figure C2.tiff")

Mod_PRE <- tidy_inla(PRE)
plot_inla(GDHS_2, Mod_PRE, "Preci_z", "Precipitation Anomaly", "Figure C3.tiff")

#--- Air Pollution Models
Mod_PM <- tidy_inla(PM)
plot_inla(GDHS_2, Mod_PM, "PM", "PM2.5", "Figure C4.tiff")


Mod_NO <- tidy_inla(NO)
plot_inla(GDHS_2, Mod_NO, "NO", "Nitrogen Dioxide", "Figure C5.tiff")

Mod_CO <- tidy_inla(CO)
plot_inla(GDHS_2, Mod_CO, "CO", "Carbon Monoxide", "Figure C6.tiff")

Mod_SO <- tidy_inla(SO)
plot_inla(GDHS_2, Mod_SO, "SO", "Sulphur Dioxide", "Figure C7.tiff")

Mod_O3 <- tidy_inla(O3)
plot_inla(GDHS_2, Mod_O3, "Ozone", "Ozone", "Figure C8.tiff")

#--- Vegetation
Mod_EV <- tidy_inla(EV)
plot_inla(GDHS_2, Mod_EV, "EVI", "Enhanced Vegetation Index", "Figure C9.tiff")



# ------------------------------------------------------------------------------------------------------------------------
# 6.	Appendix D –  Effect of Preconception Exposure on Spontaneous Miscarriage by Vegetation (Enhanced Vegetation Index)
# ------------------------------------------------------------------------------------------------------------------------
# Calculate the 25th and 75th percentiles
evi_percentiles <- quantile(GDHS_2$EVI, probs = c(0.25, 0.75), na.rm = TRUE)

# Create the new categorical variable based on the percentiles
GDHS_2$EVI_cat <- cut(GDHS_2$EVI,
                      breaks = c(-Inf, evi_percentiles[1], evi_percentiles[2], Inf),
                      labels = c("low", "medium", "high"),
                      right = FALSE)  # right = FALSE to make sure the range is < 25th and > 75th percentile

table(GDHS_2$EVI_cat)

GDHS_2$EVI_cat2 <- as.numeric(GDHS_2$EVI_cat)


##Models
TMP <- inla(pregMC ~ 1 +  f(inla.group(Tmp_z), model="rw2", group=EVI_cat2),
            data=GDHS_2, family = "binomial", Ntrials = 1,
            control.family = list(link = "cloglog"), verbose=TRUE,
            control.compute = list(dic=TRUE, waic=TRUE))


TMX<- inla(pregMC ~ 1 + f(inla.group(Tmx_z), model="rw2", group=EVI_cat2),
           data=GDHS_2, family = "binomial", Ntrials = 1, 
           control.family = list(link = "cloglog"), 
           verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


PRE<- inla(pregMC ~ 1 + f(inla.group(Preci_z), model="rw2", group=EVI_cat2),
           data=GDHS_2, family = "binomial", Ntrials = 1, 
           control.family = list(link = "cloglog"), 
           verbose=T, control.compute=list(dic=TRUE, waic=TRUE))


PM<- inla(pregMC ~ 1 + f(inla.group(PM), model="rw2", group=EVI_cat2),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

NO<- inla(pregMC ~ 1 + f(inla.group(NO), model="rw2", group=EVI_cat2),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

CO<- inla(pregMC ~ 1 + f(inla.group(CO), model="rw2", group=EVI_cat2),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

SO<- inla(pregMC ~ 1 + f(inla.group(SO), model="rw2", group=EVI_cat2),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))

O3<- inla(pregMC ~ 1 + f(inla.group(Ozone), model="rw2", group=EVI_cat2),
          data=GDHS_2, family = "binomial", Ntrials = 1, 
          control.family = list(link = "cloglog"), 
          verbose=T, control.compute=list(dic=TRUE, waic=TRUE))



# Function to extract, rename, and exponentiate
tidy_inla <- function(mod) {
  df <- mod$summary.fitted[, c(1, 2, 3, 5)]  # select relevant columns
  df <- df %>% rename(lci = `0.025quant`, uci = `0.975quant`)
  df[c("mean", "lci", "uci")] <- exp(df[c("mean", "lci", "uci")])
  return(df)
}


# Apply to each model individually
Mod_TMP <- tidy_inla(TMP)
Mod_TMX <- tidy_inla(TMX)
Mod_PRE <- tidy_inla(PRE)
Mod_PM  <- tidy_inla(PM)
Mod_CO  <- tidy_inla(CO)
Mod_NO  <- tidy_inla(NO)
Mod_SO  <- tidy_inla(SO)
Mod_O3  <- tidy_inla(O3)


plot_inla_EVI <- function(data, mod_summary, var_name, title_text, file_name, evi_var = "EVI_cat") {
  tiff(file_name, units = "in", width = 10, height = 8, res = 300)
  
  ggplot(
    data = data.frame(
      EVI = data[[evi_var]],
      xval = inla.group(data[[var_name]]),
      HR = mod_summary$mean,
      lci = mod_summary$lci,
      uci = mod_summary$uci
    ),
    aes(x = xval, y = HR, col = EVI)
  ) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = lci, ymax = uci, fill = EVI), alpha = 0.2) +
    ggtitle(title_text) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
  
  dev.off()
}

#--- Apply function to each model

plot_inla_EVI(GDHS_2, Mod_TMP, "Tmp_z", "Mean Temperature Anomaly", "Figure_D1.tiff")
plot_inla_EVI(GDHS_2, Mod_TMX, "Tmx_z", "Maximum Temperature Anomaly", "Figure_D2.tiff")
plot_inla_EVI(GDHS_2, Mod_PRE, "Preci_z", "Precipitation Anomaly", "Figure_D3.tiff")
plot_inla_EVI(GDHS_2, Mod_PM, "PM", "PM2.5", "Figure_D4.tiff")
plot_inla_EVI(GDHS_2, Mod_CO, "CO", "Carbon Monoxide", "Figure_D5.tiff")
plot_inla_EVI(GDHS_2, Mod_NO, "NO", "Nitrogen Dioxide", "Figure_D6.tiff")
plot_inla_EVI(GDHS_2, Mod_SO, "SO", "Sulphur Dioxide", "Figure_D7.tiff")
plot_inla_EVI(GDHS_2, Mod_O3, "Ozone", "Ozone", "Figure_D8.tiff")
plot_inla_EVI(GDHS_2, Mod_EV, "EVI", "Enhanced Vegetation Index", "Figure_D9.tiff")

























