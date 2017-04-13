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

d <- select(ger_df_long, voteshare, voteshare_l1, voteshare_l1_3, polls_200_230, chancellor_party, major, gov, parl) 
dep_var <- 'voteshare'
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


swing~ltw_swing_mean_200_full+major+voteshare_l1Xelection+polls_200_230yXelection
swing~ltw_swing_mean_200_full+chancellor_party+voteshare_l1Xelection+polls_200_230yXelection
swing~swing_l1+voteshare_l1Xelection+polls_200_230yXelection
swing~swing_l1+voteshare_l1Xelection+polls_200_230yXelection
swing~ltw_swing_mean_200_full+voteshare_l1Xelection+polls_200_230yXelection


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









### VON STIMMANTEILEN ZU SIEGESWAHRSCHEINLICHKEITEN
### ----------------------------------------------------------

?pnorm

est.mean <- prediction$fit[1]
est.se <- prediction$se.fit[1]
est.lower <- prediction$fit[2]
est.upper <- prediction$fit[3]

# Grafische Darstellung
# Normalverteilungskurve auf Basis von y_hat und sd_hat
x <- seq(40,60, length=500)
y <- dnorm(x,mean=est.mean,sd=est.se)
plot(x,y,type="l",lwd=2)
abline(v=50, lty=2)

# Fl?che, die der Wahrscheinlichkeit f?r y >= 50 entspricht
x <- seq(50,100,length=500)
y <- dnorm(x,mean=est.mean,sd=est.se)
polygon(c(50,x,100),c(0,y,0),col="gray")

# Berechnung
pnorm(50, mean=prediction$fit[1], sd=prediction$se.fit[1], lower.tail = FALSE)
pnorm(50, mean=prediction$fit[1], sd=prediction$se.fit[1], lower.tail = TRUE)
est.mean + qt(.975, 12) * est.se
est.mean - qt(.975, 12) * est.se

# ?berlappung von Kurven: Wie wahrscheinlich ist es, dass Kandidat 1 mehr Stimmen bekommt als Kandidat 2?
x <- seq(40,60, length=500)
kand.1 <- function(x) dnorm(x, mean=36, sd=1)
kand.2 <- function(x) dnorm(x, mean=33, sd=1)
curve(kand.1, from = 30, to = 40, n=500, type="l",lwd=2)
curve(kand.2, from = 30, to = 40, n=500, type="l",lwd=2, col="red", add=T)
text(37, .4, "Kand. 1")
text(34, .4, "Kand. 2", col="red")

# Differenz zweier unabh?ngiger normalverteilter Variablen ist ebenfalls normalverteilt, und zwar
diff.mean <- 36 - 33
diff.se <- sqrt(1^2 + 1^2)
pnorm(0, mean=diff.mean, sd=diff.se, lower.tail = TRUE)






# Datensatz neu laden
(us.df <- read.dta("usa_presidential.dta", convert.underscore = TRUE))

# Daten vorbereiten
(y <- us.df$incvoteshare[-1])
(x <-subset(us.df[-1,], select=-c(year,incvoteshare)))
(outcome    <- c("incvoteshare"))
(predictors <- names(x))
dataset    <- us.df
new <- data.frame("gdpgrowth.q1"=c(-1.5,2.5), "gdpgrowth.q2"=c(-2,1), "incapproval" = c(-20,20), "inc2terms" = c(1,0), "incdem" = c(1,0))

# Liste von Modellen erstellen
list.of.models <- lapply(seq_along((predictors)), function(n) {
  left.hand.side  <- outcome
  right.hand.side <- apply(X = combn(predictors, n), MARGIN = 2, paste, collapse = " + ")
  paste(left.hand.side, right.hand.side, sep = "  ~  ")
})

# Liste in Vektor umwandeln
vector.of.models <- unlist(list.of.models)

# Lineares Modell sch?tzen f?r alle Modelle; interessierende Gr??en extrahieren
list.of.fits <- lapply(vector.of.models, function(x) {
  
  formula    <- as.formula(x)
  fit        <- lm(formula, data = dataset)
  adj.r2 <- summary(fit)$adj.r.squared
  result.AIC <- extractAIC(fit)
  prediction.01 <- predict(fit, new)[1]
  prediction.02 <- predict(fit, new)[2]
  
  data.frame(num.predictors = result.AIC[1],
             adjr2          = summary(fit)$adj.r.squared[1],
             model          = x,
             ypred.01		  = prediction.01,
             ypred.02		  = prediction.02)
})


# Ergebnisse in ein data.frame schreiben
result <- do.call(rbind, list.of.fits)

# ?berblick ?ber Vorhersagen
result[order(result$adjr2),]
hist(result$ypred.01)
hist(result$ypred.02)

# Spezifikationsunsicherheit
summary(result$ypred.01)
sd(result$ypred.01)

