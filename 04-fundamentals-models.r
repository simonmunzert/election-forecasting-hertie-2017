### ----------------------------------------------------------
### election forecasting
### simon munzert
### ----------------------------------------------------------

# get packages
p_needed <- c("haven", "plyr", "dplyr", "magrittr","broom")
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)



# chancellor model
# stim = - 5.93 + 0.75*part + 0.38*kan - 1.52*amt
#
# stim: vote share of governing parties
# part: long-term party support (rolling average, last three elections)
# kan: chancellor support (average from polls, excluding undecided respondents, one and two months before election)
# amt: term a government is running


# forecasting funtion
gn.model <- function(part = 0, kan = 0, amt = 0, model = "2005") {
if (model=="2005") { 
     stim <-  -5.93 + 0.75*part + 0.38*kan - 1.52*amt } # formula from Gschwend/Norpoth 2005
else stim <-  -5.6  + 0.75*part + 0.38*kan - 1.5*amt    # formula from Norpoth/Gschwend 2010
return(stim)
} 
gn.model()
gn.model(part = 45, kan = 60, amt = 3)


# import historical data
ger_df <- read_dta("data/ger_nat.dta")
names(ger_df)
head(ger_df)

# inspect relationships between variables
plot(ger_df$gov_share_l3, ger_df$gov_share, pch = 20, ylim = c(30, 60))
abline(lm(gov_share ~ gov_share_l3, data = ger_df))
text(ger_df$gov_share_l3, ger_df$gov_share, labels = ger_df$year, pos = 3)

plot(ger_df$chancellor_polls, ger_df$gov_share, pch = 20, ylim = c(30, 65))
abline(lm(gov_share ~ chancellor_polls, data = ger_df))
text(ger_df$chancellor_polls, ger_df$gov_share, labels = ger_df$year, pos = 3)

plot(ger_df$term, ger_df$gov_share, pch = 20, ylim = c(30, 65))
abline(lm(gov_share ~ term, data = ger_df))
text(ger_df$term, ger_df$gov_share, labels = ger_df$year, pos = 2)

# run model
model_out <- lm(gov_share ~ gov_share_l3 + chancellor_polls + term, data = ger_df)
#model_out <- lm(gov_share ~ gov_share_l3 + chancellor_polls + term + chancellor, data = ger_df)
#model_out <- lm(gov_share ~ gov_share_l3 + chancellor_polls + term + chancellor*unemp, data = ger_df)
#model_out <- lm(gov_share ~ gov_share_l3 + chancellor_polls + term, data = filter(ger_df, year < 2013))
summary(model_out)

# evaluate fit
model_out_fit <- augment(model_out)
model_out_fit$year <- ger_df$year[2:18]
plot(model_out_fit$year, model_out_fit$gov_share, type = "o", xaxt = "n")
axis(1, model_out_fit$year, model_out_fit$year)
points(model_out_fit$year, model_out_fit$.fitted, col = "red", pch = 20)

# 2013 forecast, with and without prediction intervals
augment(model_out, newdata = filter(ger_df, year == 2013))
predict(model_out, filter(ger_df, year == 2013), se.fit = TRUE, interval = "confidence")
predict(model_out, filter(ger_df, year == 2013), se.fit = TRUE, interval = "prediction")

# true value: CDU/CSU 41.5, FDP 4.8 --> 46.3
