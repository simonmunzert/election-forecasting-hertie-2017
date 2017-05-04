# ************************************************
### election forecasting
### simon munzert
### modeling
# ************************************************

source("packages.r")
source("functions.r")



### import data ------------------------
ger_df_long <- read_dta("./data/ger_model_df.dta")

# prepare election ids
election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

### run models on full sample -----------
model_out <- lm(swing ~ chancellor_party + voteshare_l1 + polls_200_230, data = ger_df_long)
model_out <- lm(voteshare ~ chancellor_party + voteshare_l1 + polls_200_230, data = ger_df_long)
summary(model_out)

# evaluate fit
model_out_fit <- augment(model_out)
model_out_fit$party <- ger_df_long$party[as.numeric(model_out_fit$.rownames)]
model_out_fit$year <- ger_df_long$year[as.numeric(model_out_fit$.rownames)]
mean(abs(model_out_fit$.resid))
group_by(model_out_fit, party) %>% summarize(mae = mean(abs(.resid)))

plot(model_out_fit$.fitted, model_out_fit$voteshare, cex = .5, pch = 20)
text(model_out_fit$.fitted, model_out_fit$voteshare, paste0(model_out_fit$party, str_sub(as.character(model_out_fit$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)



### run multiple models -----------------

# define set of independent variables and dependent variable
d <- select(ger_df_long, swing, swing_l1, voteshare_l1, voteshare_l1_3, polls_200_230, chancellor_party, major, gov, parl) 
dep_var <- 'swing'
indep_vars <- setdiff(names(d), dep_var)

# run all possible models
lms <- Reduce(append, lapply(seq_along(indep_vars),
             function(num_vars) {
               Reduce(append, apply(combn(length(indep_vars), num_vars), 2, function(vars) {
                 formula_string <- paste(c(dep_var, paste(indep_vars[vars], collapse = "+")), collapse = '~')
                 structure(list(lm(as.formula(formula_string), data = d)), .Names = formula_string)
               }))
              }
))
lms[[1]] %>% summary
length(lms)

# get overview of model performance
sum_tab <- data.frame(model_name = names(lms), 
                      num_vars = sapply(lms, function(x) { x %>% .$coefficients %>% length}) - 1,
                      r_squared = sapply(lms, function(x) { summary(x) %>% .$r.squared}),
                      adj_r_squared = sapply(lms, function(x) { summary(x) %>% .$adj.r.squared})
)
sum_tab$ratio <- sum_tab$r_squared / sum_tab$num_vars
View(sum_tab)

# create multiple predictions for 2017 
i = 2017
lms_best <- lms[sum_tab$r_squared > .35]
lms_best_predictions <- sapply(lms_best, predict.lm, newdata = filter(ger_df_long, year == i)) %>% t() %>% as.data.frame() 
lms_best_predictions <- apply(lms_best_predictions, 1, add, filter(ger_df_long, year == i)$voteshare_l1) %>% t() %>% as.data.frame

# summarize predictions
names(lms_best_predictions) <- filter(ger_df_long, year == i)$party
summary(lms_best_predictions)

# check if predictions are (more or less) logically consistent
lms_best_predictions$vote_sums <- rowSums(lms_best_predictions)
summary(lms_best_predictions$vote_sums)
hist(lms_best_predictions$vote_sums)



### out-of-sample checks --------------------------------------------------

# prepare formula
vars <- c("voteshare_l1",  "chancellor_party", "polls_200_230")
fmla <- as.formula(paste("voteshare ~ ", paste(vars, collapse= "+")))

# run out-of-sample predictions
model_out <- list()
model_pred <- list()
for(i in seq_along(election_years)) {
  insample <- filter(ger_df_long, year != election_years[i])
  outsample <- filter(ger_df_long, year == election_years[i])
  model_out[[i]] <- lm(fmla, data = insample)
  model_pred[[i]] <- augment(model_out[[i]], newdata = outsample, type.predict = "response")
}

# evaluate fit
model_pred_df <- do.call(rbind, model_pred)
mean(abs(model_pred_df$voteshare - model_pred_df$.fitted), na.rm = TRUE)
group_by(model_pred_df, party) %>% summarize(mae = mean(abs(voteshare - .fitted), na.rm = TRUE))
plot(model_pred_df$.fitted, model_pred_df$voteshare, cex = .5, pch = 20)
text(model_pred_df$.fitted, model_pred_df$voteshare, paste0(model_pred_df$party, str_sub(as.character(model_pred_df$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)




### predictions for 2005, 2009, 2013, 2017 elections ---------------------

voteshares_pred_list <- list()
elections <- c(2005, 2009, 2013, 2017)
for (i in 1:4) {
  model_out <- lm(voteshare ~ chancellor_party + voteshare_l1 + polls_200_230, data = filter(ger_df_long, year < elections[i]))
  summary(model_out)
  # predicted vote shares
  voteshares_pred_df <- data.frame(party = filter(ger_df_long, year == elections[i])$party,
                                   voteshare_true = filter(ger_df_long, year == elections[i])$voteshare,
                                   voteshare_pred = predict(model_out, filter(ger_df_long, year == elections[i])),
                                   se_fit = predict(model_out, filter(ger_df_long, year == elections[i]),  se.fit = TRUE, interval = "prediction")$se.fit,
                                   lwr_conf = predict(model_out, filter(ger_df_long, year == elections[i]), se.fit = TRUE, interval = "confidence")$fit[,"lwr"],
                                   upr_conf = predict(model_out, filter(ger_df_long, year == elections[i]), se.fit = TRUE, interval = "confidence")$fit[,"upr"],
                                   lwr_pred = predict(model_out, filter(ger_df_long, year == elections[i]), se.fit = TRUE, interval = "prediction")$fit[,"lwr"],
                                   upr_pred = predict(model_out, filter(ger_df_long, year == elections[i]), se.fit = TRUE, interval = "prediction")$fit[,"upr"]
  )
  voteshares_pred_df$se_pred <- sqrt(voteshares_pred_df$se_fit^2+var(model_out$residuals)) # see http://stats.stackexchange.com/questions/154247/what-are-the-formulae-used-in-r-by-predict-lm-when-interval-a-none-b-pred
  cat("\n", "Election ", elections[i], "\n")
  print(voteshares_pred_df)
  voteshares_pred_list[[i]] <- voteshares_pred_df
}
save(voteshares_pred_list, file = "structural_forecasts_lm.RData")



### arriving at event probabilities / simulate predictions ---------------------

model_out <- lm(voteshare ~ chancellor_party + voteshare_l1 + polls_200_230, data = ger_df_long)
voteshare_pred <- predict(model_out, filter(ger_df_long, year == 2017), se.fit = TRUE, interval = "prediction")

voteshare_pred_sim <- replicate(1000, rnorm(rep(1, length(voteshare_pred$fit[,"fit"])), mean = voteshare_pred$fit[,"fit"], sd = sqrt(voteshare_pred$se.fit^2+var(model_out$residuals)))) %>% t() %>% as.data.frame

names(voteshare_pred_sim) <- filter(ger_df_long, year == 2017)$party %>% as.character
plot(density(voteshare_pred_sim$cdu), xlim = c(20, 50))
lines(density(voteshare_pred_sim$spd), col = "red")

prop.table(table(voteshare_pred_sim$cdu > voteshare_pred_sim$spd))
prop.table(table(voteshare_pred_sim$fdp < 5))
prop.table(table(voteshare_pred_sim$gru < 5))


