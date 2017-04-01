### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

source("packages.r")
source("functions.r")

### import data --------------------------------------------

load("../data/structural/ger_model_df.RData")
election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

### run models on full sample ---------------------------------------------
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
abline(v=0)
abline(h=0)



### run all possible models -----------------------------------------
ger_df_long$chancellor_partyXunemp_swing <- ger_df_long$chancellor_party*ger_df_long$unemp_swing
ger_df_long$chancellor_partyXvoteshare_l1 <- ger_df_long$chancellor_party*ger_df_long$voteshare_l1

d <- select(ger_df_long, swing, chancellor_party, voteshare_l1, voteshare_l1_3, polls_200_230, ltw_swing_mean_100_full, major, gov, parl, unemp_swing, swing_l1, chancellor_partyXunemp_swing, chancellor_partyXvoteshare_l1)
dep_var <- 'swing'
indep_vars <- setdiff(names(d), dep_var)

lms <- Reduce(append, lapply(seq_along(indep_vars),
                             function(num_vars) {
                               Reduce(append, apply(combn(length(indep_vars), num_vars), 2, function(vars) {
                                 formula_string <- paste(c(dep_var, paste(indep_vars[vars], collapse = "+")), collapse = '~')
                                 structure(list(lm(as.formula(formula_string), data = d)), .Names = formula_string)
                               }))
                             }
))
length(lms)

sum_tab <- data.frame(model_name = names(lms), 
                      num_vars = sapply(lms, function(x) { x %>% .$coefficients %>% length}) - 1,
                      #df = sapply(lms, function(x) { summary(x) %>% .$df[2,]}),
                      r_squared = sapply(lms, function(x) { summary(x) %>% .$r.squared}),
                      adj_r_squared = sapply(lms, function(x) { summary(x) %>% .$adj.r.squared})
)
sum_tab$ratio <- sum_tab$r_squared / sum_tab$num_vars

i = 2017
lms_best <- lms[sum_tab$r_squared > .35]
lms_best_predictions <- sapply(lms_best, predict.lm, newdata = filter(ger_df_long, year == i)) %>% t() %>% as.data.frame() 
lms_best_predictions <- apply(lms_best_predictions, 1, add, filter(ger_df_long, year == i)$voteshare_l1) %>% t() %>% as.data.frame
names(lms_best_predictions) <- filter(ger_df_long, year == i)$party
summary(lms_best_predictions)
lms_best_predictions$vote_sums <- rowSums(lms_best_predictions)




### out-of-sample checks --------------------------------------------------

# prepare formula
vars <- c("voteshare_l1", "major", "chancellor_party", "gov", "parl", "ltw_swing_mean_100_full", "polls_200_230")
fmla <- as.formula(paste("swing ~ ", paste(vars, collapse= "+")))

# run out-of-sample predictions
model_out <- list()
model_pred <- list()
for(i in seq_along(election_year)) {
  insample <- filter(ger_df_long, year != election_year[i])
  outsample <- filter(ger_df_long, year == election_year[i])
  model_out[[i]] <- lm(fmla, data = insample)
  model_pred[[i]] <- augment(model_out[[i]], newdata = outsample, type.predict = "response")
}

# evaluate fit
model_pred_df <- do.call(rbind, model_pred)
mean(abs(model_pred_df$swing - model_pred_df$.fitted), na.rm = TRUE)
group_by(model_pred_df, party) %>% summarize(mae = mean(abs(swing - .fitted), na.rm = TRUE))
plot(model_pred_df$.fitted, model_pred_df$swing, cex = .5, pch = 20)
text(model_pred_df$.fitted, model_pred_df$swing, paste0(model_pred_df$party, str_sub(as.character(model_pred_df$year), -2, -1)), pos = 3, offset = .15, cex = .6)
grid()
abline(0, 1)




### predictions for 2005, 2009, 2013, 2017 elections ---------------------

#sink("sink-predictions.txt")
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
#sink()
save(voteshares_pred_list, file = "structural_forecasts_lm.RData")



