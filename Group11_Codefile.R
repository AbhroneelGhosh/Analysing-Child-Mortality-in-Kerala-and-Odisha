# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ECO_STAT PROJECT     #######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOADING LIBRARIES AND DATA             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Importing libraries

library(tidyverse)
library(Hmisc)
library(reshape2)
library(DHS.rates)
library(survival)
library(survminer)
library(glmnet)
library(ggplot2)
library(ggfortify)
library(gridExtra)
library(haven)

library(rineq)
# library(concentration)
# library(IC2)
library(gglorenz)

# Importing data

df_nfhs <- read_dta("NFHS5_Birthfile.dta")
df_nfhs_2015 <- read_dta("IABR74FL.DTA")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA PREPROCESSING           ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Identifying states

table(df_nfhs$v024)
df_kerala <- df_nfhs[df_nfhs$v024 == 32,]
df_odisha <- df_nfhs[df_nfhs$v024 == 21,]

df_kerala_2015 <- df_nfhs_2015[df_nfhs_2015$v024 == 17,]
df_odisha_2015 <- df_nfhs_2015[df_nfhs_2015$v024 == 26,]

dim(df_kerala)
dim(df_odisha)

table(df_kerala$sdist)
table(df_odisha$sdist)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# IMRS USING CHMORT           ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# table(df_kerala$b7)
# 
# df_kerala <- df_kerala %>%
#   mutate(infant_mortality = factor(case_when(b7 < 12 ~ 1,
#                                              TRUE ~ 0)))
# 
# df_odisha <- df_odisha %>%
#   mutate(infant_mortality = factor(case_when(b7 < 12 ~ 1,
#                                              TRUE ~ 0)))

# IMR

# table(df_kerala['infant_mortality'])[2]/nrow(df_kerala)*1000
# table(df_odisha['infant_mortality'])[2]/nrow(df_odisha)*1000
# 
# table(df_kerala['infant_mortality'])
# 
# df_kerala$b6[df_kerala$infant_mortality == 1]

chmort(df_kerala, JK = 'Yes')
chmort(df_odisha, JK = 'Yes')

# Factoring by Urban/Rural

chmort(df_kerala[df_kerala$v025 == 1, ])
chmort(df_kerala[df_kerala$v025 == 2, ])

# Factor by No Education/Primary/Secondary/Higher

chmort(df_kerala[df_kerala$v106 == 0, ])
chmort(df_kerala[df_kerala$v106 == 1, ])
chmort(df_kerala[df_kerala$v106 == 2, ])
chmort(df_kerala[df_kerala$v106 == 3, ])

# Factor by Poorest/Poor/Middle/Richer/Richest

chmort(df_kerala[df_kerala$v190a == 1, ])
chmort(df_kerala[df_kerala$v190a == 2, ])
chmort(df_kerala[df_kerala$v190a == 3, ])
chmort(df_kerala[df_kerala$v190a == 4, ])
chmort(df_kerala[df_kerala$v190a == 5, ])

# Factor by Male/Female

chmort(df_kerala[df_kerala$b4 == 1, ])
chmort(df_kerala[df_kerala$b4 == 2, ])

# Factoring by Urban/Rural

chmort(df_odisha[df_odisha$v025 == 1, ])
chmort(df_odisha[df_odisha$v025 == 2, ])

# Factor by No Education/Primary/Secondary/Higher

chmort(df_odisha[df_odisha$v106 == 0, ])
chmort(df_odisha[df_odisha$v106 == 1, ])
chmort(df_odisha[df_odisha$v106 == 2, ])
chmort(df_odisha[df_odisha$v106 == 3, ])

# Factor by Poorest/Poor/Middle/Richer/Richest

chmort(df_odisha[df_odisha$v190a == 1, ])
chmort(df_odisha[df_odisha$v190a == 2, ])
chmort(df_odisha[df_odisha$v190a == 3, ])
chmort(df_odisha[df_odisha$v190a == 4, ])
chmort(df_odisha[df_odisha$v190a == 5, ])

# Factor by Male/Female

chmort(df_odisha[df_odisha$b4 == 1, ])
chmort(df_odisha[df_odisha$b4 == 2, ])


# 2015 IMRs

chmort(df_kerala_2015, JK = 'Yes')
chmort(df_odisha_2015, JK = 'Yes')


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PROCESSING FOR SURV           ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data Processing

# Age of living Child in months

df_kerala$b8A <- (df_kerala$v008 - df_kerala$b3)
df_odisha$b8A <- (df_odisha$v008 - df_odisha$b3)

df_kerala_2015$b8A <- (df_kerala_2015$v008 - df_kerala_2015$b3)
df_odisha_2015$b8A <- (df_odisha_2015$v008 - df_odisha_2015$b3)

# Creating status/time columns

df_kerala <- df_kerala %>%
  mutate(time = case_when(b5 == 0 ~ b7,
                          b5 == 1 ~ b8A),
         status_imr = case_when(b7 < 12 ~ 1,
                                TRUE ~ 0),
         status_u5 = case_when(b7 < 60 ~ 1,
                               TRUE ~ 0),
         status = case_when(b5 == 0 ~ 1,
                            b5 == 1 ~ 0),
         v161a = case_when(v161 %in% c(1,2,3) ~ 1,
                           TRUE ~ 0))

df_kerala_2015 <- df_kerala_2015 %>%
  mutate(time = case_when(b5 == 0 ~ b7,
                          b5 == 1 ~ b8A),
         status_imr = case_when(b7 < 12 ~ 1,
                                TRUE ~ 0),
         status_u5 = case_when(b7 < 60 ~ 1,
                               TRUE ~ 0),
         status = case_when(b5 == 0 ~ 1,
                            b5 == 1 ~ 0),
         v161a = case_when(v161 %in% c(1,2,3) ~ 1,
                           TRUE ~ 0))

colnames(df_kerala)

df_odisha <- df_odisha %>%
  mutate(time = case_when(b5 == 0 ~ b7,
                          b5 == 1 ~ b8A),
         status_imr = case_when(b7 < 12 ~ 1,
                                TRUE ~ 0),
         status_u5 = case_when(b7 < 60 ~ 1,
                               TRUE ~ 0),
         status = case_when(b5 == 0 ~ 1,
                            b5 == 1 ~ 0),
         v161a = case_when(v161 %in% c(1,2,3) ~ 1,
                           TRUE ~ 0))

df_odisha_2015 <- df_odisha_2015 %>%
  mutate(time = case_when(b5 == 0 ~ b7,
                          b5 == 1 ~ b8A),
         status_imr = case_when(b7 < 12 ~ 1,
                                TRUE ~ 0),
         status_u5 = case_when(b7 < 60 ~ 1,
                               TRUE ~ 0),
         status = case_when(b5 == 0 ~ 1,
                            b5 == 1 ~ 0),
         v161a = case_when(v161 %in% c(1,2,3) ~ 1,
                           TRUE ~ 0))

# Converting columns to factors

df_kerala$v025 = factor(df_kerala$v025)
df_odisha$v025 = factor(df_odisha$v025)

df_kerala$v106 = factor(df_kerala$v106)
df_odisha$v106 = factor(df_odisha$v106)

df_kerala$v190 = factor(df_kerala$v190)
df_odisha$v190 = factor(df_odisha$v190)

df_kerala$v160 = factor(df_kerala$v160)
df_odisha$v160 = factor(df_odisha$v160)

df_kerala$v161a = factor(df_kerala$v161a)
df_odisha$v161a = factor(df_odisha$v161a)

df_kerala$b4 = factor(df_kerala$b4)
df_odisha$b4 = factor(df_odisha$b4)

# sum(is.na(df_kerala$status_u5))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SURVIVAL ANALYSIS           ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

predictor_vars <- c('v025', 'v106', 'v190', 'v161a','b4')

# Cox Regression

df_ker_surv <- df_kerala[,c(predictor_vars, 'time', 'status_imr', 'status_u5', 'status')]
df_ker_surv_2015 <- df_kerala_2015[,c(predictor_vars, 'time', 'status_imr', 'status_u5', 'status')]

df_imrk_2015 <- df_kerala_2015[,c(predictor_vars, 'time', 'status_imr')]
df_u5k_2015 <- df_kerala_2015[,c(predictor_vars, 'time', 'status_u5')]

surv_Object <- Surv(df_ker_surv$time, df_ker_surv$status)
comb_fit <- survfit(formula = surv_Object~v025, data = df_ker_surv)
summary(comb_fit)
# plot(comb_fit, xlab = "Time", ylab = "Survival Probability", main = "Kaplan-Meier Survival Curve")

ggsurvplot(comb_fit, data = df_ker_surv, conf.int = TRUE, pval = TRUE, fun = "event",
                    legend.labs = c("Male", "Female"),
                    #legend.labs = c("Urban", "Rural")
                    legend.title = "Sex of the baby",
                    ylim = c(0,0.05), xlim = c(0, 60),
                    break.x.by = 5,
                    xlab = "Time (months from birth)", ylab = "Cumulative Prob of Mortality",
                    title = "KM Mortality Curve - Kerala NFHS 5")


surv_Object <- Surv(df_ker_surv_2015$time, df_ker_surv_2015$status)
comb_fit <- survfit(formula = surv_Object~b4, data = df_ker_surv_2015)
summary(comb_fit)

ggsurvplot(comb_fit, data = df_ker_surv_2015, conf.int = TRUE, pval = TRUE, fun = "event",
           legend.labs = c("Male", "Female"), legend.title = "Sex of the baby",
           ylim = c(0,0.05), xlim = c(0, 350),
           xlab = "Time (months from birth)", title = "KM Mortality Curve")


df_odi_surv <- df_odisha[,c(predictor_vars, 'time', 'status_imr', 'status_u5', 'status')]
df_odi_surv_2015 <- df_odisha[,c(predictor_vars, 'time', 'status_imr', 'status_u5', 'status')]

surv_Object <- Surv(df_odi_surv$time, df_odi_surv$status)
comb_fit <- survfit(formula = surv_Object~b4, data = df_odi_surv)
summary(comb_fit)

ggsurvplot(comb_fit, data = df_odi_surv, conf.int = TRUE, pval = FALSE, fun = "event",
           legend.labs = c("Male", "Female"), legend.title = "Sex of the baby",
           xlab = "Time (months from birth)", ylab = "Cumulative Prob of Mortality",
           ylim = c(0,0.2), xlim = c(0, 60),
           break.x.by = 5,
           title = "KM Mortality Curve - Odisha NFHS 5")


surv_Object <- Surv(df_odi_surv_2015$time, df_odi_surv_2015$status_u5)
comb_fit <- survfit(formula = surv_Object~b4, data = df_odi_surv_2015)
summary(comb_fit)

ggsurvplot(comb_fit, data = df_odi_surv_2015, conf.int = TRUE, pval = TRUE, fun = "event",
           legend.labs = c("Female", "Male"), legend.title = "Sex of the baby",
           ylim = c(0,0.1), xlim = c(0, 30),
           xlab = "Time (months from birth)", title = "KM Mortality Curve")


# Cox Regression

df_ker_surv$v005 = df_kerala$v005/100000
df_ker_surv_2015$v005 = df_kerala$v005/100000

# model <- coxph(Surv(time, status_imr) ~ ., data = df_imrk, weights = v005)
# model_temp <- tidy(model, conf.int = TRUE)
# coeffs_imrk <- model_temp$estimate
# names(coeffs_imrk) <- model_temp$term

model <- glm(status_imr~., family = binomial(link = "logit"), data = df_ker_surv[c(predictor_vars, "status_imr", "v005")], weights = v005)
logit_ker <- tidy(model, conf.int = TRUE)
logit_ker


df_odi_surv$v005 = df_odisha$v005/100000
df_odi_surv_2015$v005 = df_odisha$v005/100000

# model <- coxph(Surv(time, status_imr) ~ ., data = df_imro, weights = v005)
# model_temp <- tidy(model, conf.int = TRUE)
# coeffs_imro <- model_temp$estimate
# names(coeffs_imro) <- model_temp$term

model <- glm(status_imr~., family = binomial(link = "logit"), data = df_odi_surv[c(predictor_vars, "status_imr", "v005")], weights = v005)
logit_odi <- tidy(model, conf.int = TRUE)
logit_odi


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REGRESSION ON HMIS        ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

file1k <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/Kerala_IMR_2016.csv")
file1o <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/Odisha_IMR_2016.csv")

file2k <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/hmis-item-2016-17-mn-ker-upto-Jan.csv")
file2o <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/hmis-item-2016-17-mn-od-upto-Mar.csv")

file2k_new <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/hmis_kerala_2016-17_upto_March1.csv")
file2o_new <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/hmis_odisha_2016-17_upto_March1.csv")

file2k_new_2019 <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/hmis_kerala_2019-20_upto_March.csv")
file2o_new_2019 <- read.csv("C:/Users/home/Desktop/Abhroneel/Sem 5 Projects/Eco Stat/hmis_odisha_2019-20_upto_March.csv")


predictor_vars <- c(3, 15, 16, 31, 32, 118, 119, 120, 126, 132, 153, 167)
predictor_vars_new <- c(7, 10, 22, 23, 38, 39, 46, 125, 126, 127, 131, 139, 160, 174)
predictor_labs <- c("Total number pregnant women registered for ANC",                    #Include
                    "No of pregnant women registered under JSY",                        #Include
                    "Total no.of deliveries conducted at home and attended by SBA",
                    "No. of newborns visited within 24 hrs of delivery (dels conducted at home)",
                    "Total no. of C-sections performed at public facilities",
                    "Total no. of C-sections performed at private facilities",
                    "Number of Newborns having weight less than 2.5 kg",
                    "No. of infants 0-11 months old recd Hepatitis B1 immunization",
                    "No. of infants 0-11 months old recd Hepatitis B2 immunization",
                    "No. of infants 0-11 months old recd Hepatitis B3 immunization",
                    "Total no. of children 9-11 months old fully immunized",             #Include
                    "Total no. of children 12-23 months old fully immunized",            #Include
                    "No. of cases of Malaria reported in children < 5 yrs",              #Include
                    "Total no. of times ambulance was used in transporting patients")

# Fully immunized - BCG+DPT123 / pentavalent123+opv123+measles

final_predictors <- predictor_vars_new[c(1,2,11,12)]
final_predictors_labs <- predictor_labs[c(1,2,11,12)]

IMR_kerala <- file1k[, c(4)]
U5_kerala <- file1k[, c(6)]
IMR_odisha <- file1o[, c(4)]
U5_odisha <- file1o[, c(6)]

kerala_linreg_coeff <- c()
kerala_linreg_pval <- c()
kerala_linreg_conflow <- c()
kerala_linreg_confhigh <- c()

odisha_linreg_coeff <- c()
odisha_linreg_pval <- c()
odisha_linreg_conflow <- c()
odisha_linreg_confhigh <- c()

for (i in 1:length(final_predictors)) {
  selected_rows <- c(final_predictors[i])
  X_selected = file2k_new[selected_rows, 6:19]
  model = lm(U5_kerala~. , data = data.frame(unlist(as.numeric(X_selected)), unlist(df_wealthk$v191)))
  model_temp <- tidy(model, conf.int = TRUE)
  kerala_linreg_coeff <- c(kerala_linreg_coeff, as.numeric(model_temp[2,2]))
  kerala_linreg_pval <- c(kerala_linreg_pval, as.numeric(model_temp[2,5]))
  kerala_linreg_conflow <- c(kerala_linreg_conflow, as.numeric(model_temp[2,6]))
  kerala_linreg_confhigh <- c(kerala_linreg_confhigh, as.numeric(model_temp[2,7]))
}

for (i in 1:length(final_predictors)) {
  selected_rows <- c(final_predictors[i])
  X_selected = file2o_new[selected_rows, 6:35]
  model = lm(U5_odisha~. , data = data.frame(unlist(as.numeric(X_selected)), unlist(df_wealtho$v191)))
  model_temp <- tidy(model, conf.int = TRUE)
  odisha_linreg_coeff <- c(odisha_linreg_coeff, as.numeric(model_temp[2,2]))
  odisha_linreg_pval <- c(odisha_linreg_pval, as.numeric(model_temp[2,5]))
  odisha_linreg_conflow <- c(odisha_linreg_conflow, as.numeric(model_temp[2,6]))
  odisha_linreg_confhigh <- c(odisha_linreg_confhigh, as.numeric(model_temp[2,7]))
}

length(odisha_linreg_coeff)
length(kerala_linreg_coeff)

names(kerala_linreg_coeff) <- final_predictors_labs
names(kerala_linreg_confhigh) <- final_predictors_labs
names(kerala_linreg_conflow) <- final_predictors_labs

names(odisha_linreg_coeff) <- final_predictors_labs
names(odisha_linreg_confhigh) <- final_predictors_labs
names(odisha_linreg_conflow) <- final_predictors_labs

kerala_linreg_coeff <- 1000*kerala_linreg_coeff
kerala_linreg_confhigh <- 1000*kerala_linreg_confhigh
kerala_linreg_conflow <- 1000*kerala_linreg_conflow

odisha_linreg_coeff <- 1000*odisha_linreg_coeff
odisha_linreg_confhigh <- 1000*odisha_linreg_confhigh
odisha_linreg_conflow <- 1000*odisha_linreg_conflow


df_coeffs_U5 <- data.frame(coeffs = c(kerala_linreg_coeff, odisha_linreg_coeff),
                        CI_start = c(kerala_linreg_conflow, odisha_linreg_conflow),
                        CI_end = c(kerala_linreg_confhigh, odisha_linreg_confhigh),
                        #pred = rep(1:length(kerala_linreg_coeff), 2),
                        pred = rep(final_predictors_labs, 2),
                        state = c(rep('kerala', length(kerala_linreg_coeff)), rep('odisha', length(odisha_linreg_coeff))))

plot1 <- df_coeffs_U5 %>% 
  ggplot( aes(x = pred, y = coeffs, colour = state, group = state)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0) +
  coord_flip()+
  labs(title = "Confidence intervals for LR Coefficients", subtitle = 'U5 Mortality Rates on HMIS Indicators district-wise 2016') +
  labs(x = 'Predictors') + 
  labs(y = 'Coefficients') +
  #theme_dark() +
  scale_y_continuous(limits = c(-0.75, 0.75), )

plot1

# For 2019

kerala_linreg_coeff <- c()
kerala_linreg_pval <- c()
kerala_linreg_conflow <- c()
kerala_linreg_confhigh <- c()

odisha_linreg_coeff <- c()
odisha_linreg_pval <- c()
odisha_linreg_conflow <- c()
odisha_linreg_confhigh <- c()

for (i in 1:length(final_predictors)) {
  selected_rows <- c(final_predictors[i]+1)
  X_selected = file2k_new_2019[selected_rows, seq(from = 10, by = 5, length.out = 14)]
  model = lm(U5_kerala~. , data = data.frame(unlist(as.numeric(X_selected)), unlist(df_wealthk_2019$v191)))
  model_temp <- tidy(model, conf.int = TRUE)
  kerala_linreg_coeff <- c(kerala_linreg_coeff, as.numeric(model_temp[2,2]))
  kerala_linreg_pval <- c(kerala_linreg_pval, as.numeric(model_temp[2,5]))
  kerala_linreg_conflow <- c(kerala_linreg_conflow, as.numeric(model_temp[2,6]))
  kerala_linreg_confhigh <- c(kerala_linreg_confhigh, as.numeric(model_temp[2,7]))
}

for (i in 1:length(final_predictors)) {
  selected_rows <- c(final_predictors[i]+1)
  X_selected = file2o_new[selected_rows, 6:35]
  model = lm(U5_odisha~. , data = data.frame(unlist(as.numeric(X_selected)), unlist(df_wealtho_2019$v191)))
  model_temp <- tidy(model, conf.int = TRUE)
  odisha_linreg_coeff <- c(odisha_linreg_coeff, as.numeric(model_temp[2,2]))
  odisha_linreg_pval <- c(odisha_linreg_pval, as.numeric(model_temp[2,5]))
  odisha_linreg_conflow <- c(odisha_linreg_conflow, as.numeric(model_temp[2,6]))
  odisha_linreg_confhigh <- c(odisha_linreg_confhigh, as.numeric(model_temp[2,7]))
}

length(odisha_linreg_coeff)
length(kerala_linreg_coeff)

names(kerala_linreg_coeff) <- final_predictors_labs
names(kerala_linreg_confhigh) <- final_predictors_labs
names(kerala_linreg_conflow) <- final_predictors_labs

names(odisha_linreg_coeff) <- final_predictors_labs
names(odisha_linreg_confhigh) <- final_predictors_labs
names(odisha_linreg_conflow) <- final_predictors_labs

kerala_linreg_coeff <- 1000*kerala_linreg_coeff
kerala_linreg_confhigh <- 1000*kerala_linreg_confhigh
kerala_linreg_conflow <- 1000*kerala_linreg_conflow

odisha_linreg_coeff <- 1000*odisha_linreg_coeff
odisha_linreg_confhigh <- 1000*odisha_linreg_confhigh
odisha_linreg_conflow <- 1000*odisha_linreg_conflow


df_coeffs_U5_2019 <- data.frame(coeffs = c(kerala_linreg_coeff, odisha_linreg_coeff),
                           CI_start = c(kerala_linreg_conflow, odisha_linreg_conflow),
                           CI_end = c(kerala_linreg_confhigh, odisha_linreg_confhigh),
                           #pred = rep(1:length(kerala_linreg_coeff), 2),
                           pred = rep(final_predictors_labs, 2),
                           state = c(rep('kerala', length(kerala_linreg_coeff)), rep('odisha', length(odisha_linreg_coeff))))

plot2 <- df_coeffs_U5_2019 %>% 
  ggplot( aes(x = pred, y = coeffs, colour = state, group = state)) +
  geom_point(position = position_dodge(width = 0.75))+
  geom_errorbar(aes(ymin = CI_start, ymax = CI_end), position = position_dodge(width = 0.75)) +
  geom_hline(yintercept = 0) +
  coord_flip()+
  labs(title = "Confidence intervals for LR Coefficients", subtitle = 'U5 Mortality Rates on HMIS Indicators district-wise 2019') +
  labs(x = 'Predictors') + 
  labs(y = 'Coefficients') +
  #theme_dark() +
  scale_y_continuous(limits = c(-0.75, 0.75), )

plot2

# Individual Bivariate plots

i = 2
selected_rows <- c(predictor_vars_new[i])

#Kerala
X_selected = file2k_new[selected_rows, 6:19]
model_1 = lm(U5_kerala~. , data = data.frame(unlist(df_wealthk$v191)))
res <- model_1$residuals
model_2 <- lm(res~. , data = data.frame(unlist(as.numeric(X_selected))))

df_lr <- data.frame(unlist(res), unlist(as.numeric(X_selected)))
names(df_lr) <- c("X", "Y")
df_lr %>% 
  ggplot(aes(x = Y, y = X)) +
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  geom_text(x = 41000, y = 10, label = "Slope: ") +
  geom_text(x = 46000, y = 10, label = round(1000*as.numeric(model_2$coefficients[2]), 2)) +
  labs(title = "Bivariate Plot", subtitle = 'Kerala') +
  labs(x = final_predictors_labs[i]) + 
  labs(y = 'U5 Mortality Rate Residuals')

plot(res~as.numeric(X_selected))
abline(model_2)

#Odisha
X_selected = file2o_new[selected_rows, 6:35]
model_1 = lm(U5_odisha~. , data = data.frame(unlist(df_wealtho$v191)))
res <- model_1$residuals
model_2 <- lm(res~. , data = data.frame(unlist(as.numeric(X_selected))))

df_lr <- data.frame(unlist(res), unlist(as.numeric(X_selected)))
names(df_lr) <- c("X", "Y")
df_lr %>% 
  ggplot(aes(x = Y, y = X)) +
  geom_point() +
  stat_smooth(method = "lm") +
  geom_text(x = 41000, y = 10, label = "Slope: ") +
  geom_text(x = 46000, y = 10, label = round(1000*as.numeric(model_2$coefficients[2]), 2)) +
  labs(title = "Bivariate Plot", subtitle = 'Odisha') +
  labs(x = final_predictors_labs[i]) + 
  labs(y = 'U5 Mortality Rate Residuals')

plot(res~as.numeric(X_selected))
abline(model_2)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# INEQUALITY        ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(file1k, aes(Infant.Mortality.Rate.per.1000.live.births.)) +
  stat_lorenz() +
  geom_abline(linetype = 'dashed') +
  annotate_ineq(file1k$Infant.Mortality.Rate.per.1000.live.births.) +
  labs(title = "Lorenz Curve", subtitle = 'Kerala 2016') +
  labs(x = "cumulative households by IMR") + 
  labs(y = 'cumulative share by IMR')

ggplot(file1o, aes(Infant.Mortality.Rate.per.1000.live.births.)) +
  stat_lorenz() +
  geom_abline(linetype = 'dashed') +
  annotate_ineq(file1o$Infant.Mortality.Rate.per.1000.live.births.) +
  labs(title = "Lorenz Curve", subtitle = 'Odisha 2016') +
  labs(x = "cumulative households by IMR") + 
  labs(y = 'cumulative share by IMR')

Gini(file1k$Neo.Natal.Mortality.Rate.per.1000.live.births.)
Gini(file1o$Neo.Natal.Mortality.Rate.per.1000.live.births.)

Gini(file1k$Infant.Mortality.Rate.per.1000.live.births.)
Gini(file1o$Infant.Mortality.Rate.per.1000.live.births.)

Gini(file1k$Under.5.Mortality.Rate.per.1000.live.births.)
Gini(file1o$Under.5.Mortality.Rate.per.1000.live.births.)

# Concentration Index

ci_kerala <- ci(scale(as.numeric(df_kerala_2015$v191)), df_kerala_2015$status_u5, df_kerala_2015$v005, type = 'CIw')
summary(ci_kerala)
plot(ci_kerala)
var(as.numeric(df_kerala$v191))

plot(ci_kerala$fractional_rank~ci_kerala$ineqvar)

ci_odisha <- ci(scale(as.numeric(df_odisha$v191)), df_odisha$status_u5, df_odisha$v005, type = 'CIw')
summary(ci_odisha)
plot(ci_odisha)
var(as.numeric(df_odisha$v191))

# Quadrant plot for 2015-16

df_kerala_reg$Districts.of.Kerala

korder <- c(598, 595, 596, 589, 588, 600, 597, 591, 592, 593, 599, 601, 594, 590)

df_wealthk <- df_kerala_2015 %>%
  group_by(sdistri) %>%
  summarise(across(c(v190, v191), mean)) %>%
  arrange(match(sdistri, korder))

df_wealthk

df_wealthk_2019 <- df_kerala %>%
  group_by(sdist) %>%
  summarise(across(c(v191), mean)) %>%
  arrange(match(sdist, korder))

df_odisha_reg$Districts.of.Odisha

oorder <- c(384, 393, 377, 370, 391, 378, 381, 373, 383, 389, 388, 380, 382, 371, 395, 
            390, 379, 375, 386, 398, 399, 376, 397, 385, 394, 387, 396, 372, 392, 374)
length(oorder)

df_wealtho <- df_odisha_2015 %>%
  group_by(sdistri) %>%
  summarise(across(c(v190, v191), ~weighted.mean(.x, w = v005))) %>%
  arrange(match(sdistri, oorder))

df_wealtho

df_wealtho_2019 <- df_odisha %>%
  group_by(sdist) %>%
  summarise(across(c(v191), ~weighted.mean(.x, w = v005))) %>%
  arrange(match(sdist, oorder))

kerala_districts <- c(
  "Alappuzha", "Ernakulam", "Idukki", "Kannur", "Kasaragod", "Kollam", 
  "Kottayam", "Kozhikode", "Malappuram", "Palakkad", "Pathanamthitta", 
  "Thrissur", "Thiruvananthapuram", "Wayanad"
)
length(kerala_districts)

kerala_districts_pop_2016 <- c(
  "Alappuzha" = 2125894, "Ernakulam" = 3186953, "Idukki" = 1094041, "Kannur" = 2552097, 
  "Kasaragod" = 1377226, "Kollam" = 2611853, "Kottayam" = 1968174, "Kozhikode" = 3027507, 
  "Malappuram" = 4167840, "Palakkad" = 2803846, "Pathanamthitta" = 1243283, 
  "Thrissur" = 3146927, "Thiruvananthapuram" = 3307201, "Wayanad" = 819558
)

df_quadrant <- df_wealthk
df_quadrant$IMR <- IMR_kerala
#df_quadrant$v191 <- scale(df_quadrant$v191)
df_quadrant$dist <- kerala_districts
df_quadrant$population <- kerala_districts_pop_2016
df_quadrant %>%
  ggplot(aes(y = IMR, x = v191, z = population, label = dist)) +
  geom_point(aes(size = population), shape = 21, colour = "red", fill = "white") +
  geom_text(hjust = 0, nudge_x = 1000, nudge_y = 0.05, vjust = 0) +
  geom_hline(yintercept = mean(df_quadrant$IMR)) +
  geom_vline(xintercept = mean(df_quadrant$v191))
  
df_quadrant %>%
  ggplot(aes(y = IMR, x = v190, z = population, label = dist)) +
  geom_point(aes(size = population), shape = 21, colour = "red", fill = "white", stroke = 1) +
  geom_text(hjust = 0, nudge_x = 0.02, nudge_y = 0.05, vjust = 0) +
  geom_hline(yintercept = mean(df_quadrant$IMR)) +
  geom_vline(xintercept = mean(df_quadrant$v190)) +
  labs(title = "IMR Inequality along Wealth Index", subtitle = 'Kerala 2016') +
  labs(x = "Wealth Index") + 
  labs(y = 'Infant Mortality Rate')


odisha_districts <- c(
  "Angul", "Balangir", "Baleswar", "Bargarh", "Baud", "Bhadrak", "Cuttack", 
  "Deogarh", "Dhenkanal", "Gajapati", "Ganjam", "Jagatsinghpur", 
  "Jajpur", "Jharsuguda", "Kalahandi", "Kandhamal", "Kendrapara", 
  "Kendujhar", "Khordha", "Koraput", "Malkangiri", "Mayurbhanj", 
  "Nabarangpur", "Nayagarh", "Nuapada", "Puri", "Rayagada", "Sambalpur", 
  "Subarnapur", "Sundargarh"
)
length(odisha_districts)

odisha_districts_pop_2016 <- c(
  "Angul" = 1370424, "Balangir" = 1504092, 
  "Baleswar" = 1658400, "Bargarh" = 1444528, "Baud" = 441162, "Bhadrak" = 1514775, "Cuttack" = 2861187, "Deogarh" = 286205, "Dhenkanal" = 934173, 
  "Gajapati" = 682551, "Ganjam" = 3520892, "Jagatsinghpur" = 1696747, "Jajpur" = 2113040, 
  "Jharsuguda" = 623161, "Kalahandi" = 1561914, "Kandhamal" = 729754, "Kendrapara" = 1391480, 
  "Kendujhar" = 1912532, "Khordha" = 2179074, "Koraput" = 1372638, "Malkangiri" = 629144, 
  "Mayurbhanj" = 2547113, "Nabarangpur" = 1220588, "Nayagarh" = 989277, "Nuapada" = 623231, 
  "Puri" = 1849666, "Rayagada" = 717514, "Sambalpur" = 1128235, "Subarnapur" = 561613, 
  "Sundargarh" = 2117739
)

df_quadrant <- df_wealtho
df_quadrant$IMR <- IMR_odisha
#df_quadrant$v191 <- scale(df_quadrant$v191)
df_quadrant$dist <- odisha_districts
df_quadrant$population <- odisha_districts_pop_2016
df_quadrant %>%
  ggplot(aes(y = IMR, x = v191, z = population, label = dist)) +
  geom_point(aes(size = population), shape = 21, colour = "blue", fill = "white", stroke = 1) +
  geom_text(hjust = 0, nudge_x = 1000, nudge_y = 0.05, vjust = 0) +
  geom_hline(yintercept = mean(df_quadrant$IMR)) +
  geom_vline(xintercept = mean(df_quadrant$v191))

df_quadrant %>%
  ggplot(aes(y = IMR, x = v190, z = population, label = dist)) +
  geom_point(aes(size = population), shape = 21, colour = "blue", fill = "white", stroke = 1) +
  geom_text(hjust = 0, nudge_x = 0.02, nudge_y = 0.05, vjust = 0) +
  geom_hline(yintercept = mean(df_quadrant$IMR)) +
  geom_vline(xintercept = mean(df_quadrant$v190)) +
  labs(title = "IMR Inequality along Wealth Index", subtitle = 'Odisha 2016') +
  labs(x = "Wealth Index") + 
  labs(y = 'Infant Mortality Rate')
