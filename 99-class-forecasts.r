### election forecasting
### simon munzert


# get packages
library(haven)
library(readr)
library(plyr)
library(dplyr)
library(magrittr)
library(xtable)
library(stringr)


## Liechtenstein parliamentary election Feb 5, 2017 ----------

# import data
forecasts <- read_csv2("data/class-forecasts.csv")

# select cases
forecasts_sub <- filter(forecasts, country == "Liechtenstein")

# add actual results
forecasts_sub$party1_true <- 35.2
forecasts_sub$party2_true <- 33.7
forecasts_sub$party3_true <- 18.4
forecasts_sub$party4_true <- 12.6
forecasts_sub <- mutate(forecasts_sub, sum_check_true = party1_true + party2_true + party3_true + party4_true)


# encode party names
party_names <- c("FBP", "VU", "DU", "FL")
# FBP = "Fortschrittliche Bürgerpartei"
# VU = "Vaterländische Union"
# DU = "Die Unabhängigen"
# FL = "Freie Liste"

  
# compute benchmarks
forecasts_sub <- forecasts_sub %>% 
                 rowwise() %>%
                 mutate(mae = mean(c(abs(party1_true - party1),
                                    abs(party2_true - party2), 
                                    abs(party3_true - party3), 
                                    abs(party4_true - party4))))

forecasts_sub <- forecasts_sub %>% 
                  rowwise() %>% 
                  mutate(rmse = sqrt(mean(c((party1_true - party1)^2,
                                            (party2_true - party2)^2,
                                            (party3_true - party3)^2,
                                            (party4_true - party4)^2))))



# generate latex table of results
forecasts_sub_res_table <- select(forecasts_sub, party1_true, party2_true, party3_true, party4_true) %>% slice(1)
forecasts_sub_res_table <- rename(forecasts_sub_res_table, FBP = party1_true, VU = party2_true, DU = party3_true, FL = party4_true)
print(xtable(forecasts_sub_res_table), include.rownames = FALSE, size = "small",  file = "class-forecasts/results-liechtenstein.tex")


# generate latex table of forecasting performance
forecasts_sub$rank <- rank(forecasts_sub$mae, ties.method = "first")
forecasts_sub_table <- select(forecasts_sub, rank, respondent, mae, rmse, party1, party2, party3, party4) %>% arrange(rank) 
forecasts_sub_table <- rename(forecasts_sub_table, FBP = party1, VU = party2, DU = party3, FL = party4)
print(xtable(forecasts_sub_table), include.rownames = FALSE, size = "small",  file = "class-forecasts/forecasts-liechtenstein.tex")


# generate table of top forecasters' strategies
forecasts_sub_notes <- select(forecasts_sub, rank, note) %>% arrange(rank) %>% slice(1:3)
print(xtable(forecasts_sub_notes, align = c("r","r", "p{9cm}")), include.rownames = FALSE, size = "scriptsize", file = "class-forecasts/forecasts-liechtenstein-notes.tex")


# overview of forecasts
pdf(file="class-forecasts/forecasts-liechtenstein.pdf", height=5, width=7, family="URWTimes")
 par(oma=c(0,0,0,0) + .7)
 par(mar=c(2.5, 2.5, 2.5, .5))
par(mfrow=c(2, 2))
plot(table(forecasts_sub$party1), xaxt = "n", main = party_names[1], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub$party1_true, col = rgb(1, 0, 0, .5), lwd = 2)
abline(v = mean(forecasts_sub$party1), lty = 3, lwd = 2)

plot(table(forecasts_sub$party2), xaxt = "n", main = party_names[2], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub$party2_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party2), lty = 3, lwd = 2)

plot(table(forecasts_sub$party3),  xaxt = "n", main = party_names[3], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub$party3_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party3), lty = 3, lwd = 2)

plot(table(forecasts_sub$party4),  xaxt = "n", main = party_names[4], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub$party4_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party4), lty = 3, lwd = 2)
dev.off()




## German parliamentary election Sep 24, 2017 ----------

# import data
forecasts <- read_csv2("data/class-forecasts.csv")

# select cases
forecasts_sub <- filter(forecasts, country == "Germany")

# encode party names
party_names <- c("CDU/CSU", "SPD", "Die Linke", "B'90/Die Gruenen", "FDP", "AfD", "Others")

# mean forecasts
mean(forecasts_sub$party1)
mean(forecasts_sub$party2)
mean(forecasts_sub$party3)
mean(forecasts_sub$party4)
mean(forecasts_sub$party5)
mean(forecasts_sub$party6)
mean(forecasts_sub$party7)
mean(forecasts_sub$turnout)
table(forecasts_sub$coalition)


# overview of forecasts
pdf(file="class-forecasts/forecasts-germany.pdf", height=7, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2.5, 2.5, 2.5, .5))
par(mfrow=c(3, 3))
plot(table(forecasts_sub$party1), xaxt = "n", main = party_names[1], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party1_true, col = rgb(1, 0, 0, .5), lwd = 2)
abline(v = mean(forecasts_sub$party1), lty = 3, lwd = 2)
plot(table(forecasts_sub$party2), xaxt = "n", main = party_names[2], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party2_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party2), lty = 3, lwd = 2)
plot(table(forecasts_sub$party3),  xaxt = "n", main = party_names[3], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party3_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party3), lty = 3, lwd = 2)
plot(table(forecasts_sub$party4),  xaxt = "n", main = party_names[4], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party4_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party4), lty = 3, lwd = 2)
plot(table(forecasts_sub$party5),  xaxt = "n", main = party_names[5], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party5_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party5), lty = 3, lwd = 2)
plot(table(forecasts_sub$party6),  xaxt = "n", main = party_names[6], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party6_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party6), lty = 3, lwd = 2)
plot(table(forecasts_sub$party7),  xaxt = "n", main = party_names[7], yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party7_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$party7), lty = 3, lwd = 2)
plot(table(forecasts_sub$turnout),  xaxt = "n", main = "Turnout", yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
#abline(v = forecasts_sub$party7_true, col = "red", lwd = 2)
abline(v = mean(forecasts_sub$turnout), lty = 3, lwd = 2)
dev.off()





## Turkmenistan presidential election Feb 5, 2017 ----------

# import data
forecasts <- read_csv2("data/class-forecasts.csv")

# select cases
forecasts_sub <- filter(forecasts, country == "Turkmenistan")

# add actual results
forecasts_sub$party1_true <- 1.02
forecasts_sub$party2_true <- 0.21
forecasts_sub$party3_true <- 0.36
forecasts_sub$party4_true <- 97.69
forecasts_sub$party5_true <- 0.15
forecasts_sub$party6_true <- 0.17
forecasts_sub$party7_true <- 0.25
forecasts_sub$party8_true <- 0.09
forecasts_sub$party9_true <- 0.06
forecasts_sub$turnout_true <- 97.27

forecasts_sub <- mutate(forecasts_sub, sum_check_true = party1_true + party2_true + party3_true + party4_true + party5_true + party6_true + party7_true + party8_true + party9_true)

# encode party names
party_names <- c("M. Annanpesov",	
                 "J. Annayev",	
                 "B. Atalyev",
                 "G. Berdimuhamedow (I)",	
                 "R. Durdyyev",	
                 "M. Gurbanov",	
                 "S. Jelilov",	
                 "S. Nurnepesov",	
                 "D. Orazov")


# compute benchmarks
forecasts_sub <- forecasts_sub %>% 
  rowwise() %>%
  mutate(mae = mean(c(abs(party1_true - party1),
                      abs(party2_true - party2), 
                      abs(party3_true - party3), 
                      abs(party4_true - party4),
                      abs(party5_true - party5),
                      abs(party6_true - party6),
                      abs(party7_true - party7),
                      abs(party8_true - party8),
                      abs(party9_true - party9))))

forecasts_sub <- forecasts_sub %>% 
  rowwise() %>% 
  mutate(rmse = sqrt(mean(c((party1_true - party1)^2,
                            (party2_true - party2)^2,
                            (party3_true - party3)^2,
                            (party4_true - party4)^2,
                            (party5_true - party5)^2,
                            (party6_true - party6)^2,
                            (party7_true - party7)^2,
                            (party8_true - party8)^2,
                            (party9_true - party9)^2))))



# generate latex table of results
forecasts_sub_res_table <- select(forecasts_sub, party1_true, party2_true, party3_true, party4_true, party5_true, party6_true, party7_true, party8_true, party9_true) %>% slice(1)
xtab <- xtable(forecasts_sub_res_table)
names(xtab) <- party_names
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/results-turkmenistan.tex")


# generate latex table of forecasting performance
forecasts_sub$rank <- rank(forecasts_sub$mae, ties.method = "first")
forecasts_sub_table <- select(forecasts_sub, rank, respondent, mae, rmse) %>% arrange(rank) 
xtab <- xtable(forecasts_sub_table)
#names(xtab)[5:ncol(xtab)] <- party_names
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/forecasts-turkmenistan.tex")


# generate table of top forecasters' strategies
forecasts_sub_notes <- select(forecasts_sub, rank, note) %>% arrange(rank) %>% slice(1:8)
print(xtable(forecasts_sub_notes, align = c("r","r", "p{9cm}")), include.rownames = FALSE, size = "scriptsize", file = "class-forecasts/forecasts-turkmenistan-notes.tex")


# overview of forecasts
pdf(file="class-forecasts/forecasts-turkmenistan.pdf", height=5, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2.5, 2.5, 2.5, .5))
par(mfrow=c(3, 4))

for (i in 1:length(party_names)) {
plot(table(forecasts_sub[,paste0("party",i)]), xaxt = "n", main = party_names[i], yaxt = "n", ylab = "", xlab = "Vote share in %", xlim = range(forecasts_sub[,paste0("party",i)], forecasts_sub[,paste0("party",i, "_true")][1,1]))
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub[,paste0("party",i, "_true")][1,1], col = "red", lwd = 2)
abline(v = colMeans(forecasts_sub[,paste0("party",i)]), lty = 3, lwd = 2)
}

plot(table(forecasts_sub[,"turnout"]), xaxt = "n", main = "Turnout", yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub[,"turnout_true"][1,1], col = "red", lwd = 2)
abline(v = colMeans(forecasts_sub[,"turnout"]), lty = 3, lwd = 2)
dev.off()



## Ecuadorian presidential election + referendum, Feb 19, 2017 ----------

# import data
forecasts <- read_csv2("data/class-forecasts.csv")

# select cases
forecasts_sub <- filter(forecasts, country == "Ecuador")

# select cases
forecasts_sub2 <- filter(forecasts, country == "Ecuador, referendum")
forecasts_sub$ref_true <- 54.99
forecasts_sub$ref <- forecasts_sub2$party1

  
# add actual results
forecasts_sub$party1_true <- 38.9
forecasts_sub$party2_true <- 28.5
forecasts_sub$party3_true <- 16.3
forecasts_sub$party4_true <- 6.81
forecasts_sub$party5_true <- 4.75
forecasts_sub$party6_true <- 4.74
forecasts_sub$turnout_true <- 81.73

forecasts_sub <- mutate(forecasts_sub, sum_check_true = party1_true + party2_true + party3_true + party4_true + party5_true + party6_true)

# encode party names
party_names <- c("Lenin Moreno",
                 "Guillermo Lasso",
                 "Cynthia Viteri",
                 "Paco Moncayo",
                 "Dalo Bucaram",
                 "Others")


# compute benchmarks
forecasts_sub <- forecasts_sub %>% 
  rowwise() %>%
  mutate(mae = mean(c(abs(party1_true - party1),
                      abs(party2_true - party2), 
                      abs(party3_true - party3), 
                      abs(party4_true - party4),
                      abs(party5_true - party5),
                      abs(party6_true - party6),
                      abs(turnout_true - turnout),
                      abs(ref_true - ref))))

forecasts_sub <- forecasts_sub %>% 
  rowwise() %>% 
  mutate(rmse = sqrt(mean(c((party1_true - party1)^2,
                            (party2_true - party2)^2,
                            (party3_true - party3)^2,
                            (party4_true - party4)^2,
                            (party5_true - party5)^2,
                            (party6_true - party6)^2,
                            (turnout_true - turnout)^2,
                            (ref_true - ref)^2))))



# generate latex table of results
forecasts_sub_res_table <- select(forecasts_sub, party1_true, party2_true, party3_true, party4_true, party5_true, party6_true) %>% slice(1)
xtab <- xtable(forecasts_sub_res_table)
names(xtab) <- party_names
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/results-ecuador.tex")


# generate latex table of forecasting performance
forecasts_sub$rank <- rank(forecasts_sub$mae, ties.method = "first")
forecasts_sub_table <- select(forecasts_sub, rank, respondent, mae, rmse, time) %>% arrange(rank) 
xtab <- xtable(forecasts_sub_table)
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/forecasts-ecuador.tex")


# generate table of top forecasters' strategies
forecasts_sub_notes <- select(forecasts_sub, rank, note) %>% arrange(rank) %>% slice(c(1, 2, 6, 7))
print(xtable(forecasts_sub_notes, align = c("r","r", "p{9cm}")), include.rownames = FALSE, size = "scriptsize", file = "class-forecasts/forecasts-ecuador-notes.tex")


# overview of forecasts
pdf(file="class-forecasts/forecasts-ecuador.pdf", height=5, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2.5, 2.5, 2.5, .5))
par(mfrow=c(3, 3))

for (i in 1:length(party_names)) {
  plot(table(forecasts_sub[,paste0("party",i)]), xaxt = "n", main = party_names[i], yaxt = "n", ylab = "", xlab = "Vote share in %", xlim = range(forecasts_sub[,paste0("party",i)], forecasts_sub[,paste0("party",i, "_true")][1,1]))
  axis(1, seq(0, 100, 2), seq(0, 100, 2))
  axis(2, seq(0, 10, 1), seq(0, 10, 1))
  abline(v = forecasts_sub[,paste0("party",i, "_true")][1,1], col = "red", lwd = 2)
  abline(v = colMeans(forecasts_sub[,paste0("party",i)]), lty = 3, lwd = 2)
}

plot(table(forecasts_sub[,"turnout"]), xaxt = "n", main = "Turnout", yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub[,"turnout_true"][1,1], col = "red", lwd = 2)
abline(v = colMeans(forecasts_sub[,"turnout"]), lty = 3, lwd = 2)

plot(table(forecasts_sub[,"ref"]), xaxt = "n", main = "Referendum", yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub[,"ref_true"][1,1], col = "red", lwd = 2)
abline(v = colMeans(forecasts_sub[,"ref"]), lty = 3, lwd = 2)

dev.off()




## Northern Ireland Assembly election, March 2, 2017 ----------

# import data
forecasts <- read_csv2("data/class-forecasts.csv")

# select cases
forecasts_sub <- filter(forecasts, country == "Northern Ireland")


# add actual results
forecasts_sub$party1_true <- 28.1
forecasts_sub$party2_true <- 27.9
forecasts_sub$party3_true <- 12.9
forecasts_sub$party4_true <- 11.9
forecasts_sub$party5_true <- 9.1
forecasts_sub$party6_true <- 2.3
forecasts_sub$party7_true <- 2.6
forecasts_sub$party8_true <- 1.8
forecasts_sub$turnout_true <- 64.78

forecasts_sub <- mutate(forecasts_sub, sum_check_true = party1_true + party2_true + party3_true + party4_true + party5_true + party6_true + party7_true + party8_true)

# encode party names
party_names <- c("DUP",
                 "Sinn Fein",
                 "UUP",
                 "SDLP",
                 "Alliance",
                 "Green",
                 "People Before Profit",
                 "TUV")


# compute benchmarks
forecasts_sub <- forecasts_sub %>% 
  rowwise() %>%
  mutate(mae = mean(c(abs(party1_true - party1),
                      abs(party2_true - party2), 
                      abs(party3_true - party3), 
                      abs(party4_true - party4),
                      abs(party5_true - party5),
                      abs(party6_true - party6),
                      abs(party7_true - party7),
                      abs(party8_true - party8),
                      abs(turnout_true - turnout))))

forecasts_sub <- forecasts_sub %>% 
  rowwise() %>% 
  mutate(rmse = sqrt(mean(c((party1_true - party1)^2,
                            (party2_true - party2)^2,
                            (party3_true - party3)^2,
                            (party4_true - party4)^2,
                            (party5_true - party5)^2,
                            (party6_true - party6)^2,
                            (party7_true - party7)^2,
                            (party8_true - party8)^2,
                            (turnout_true - turnout)^2))))



# generate latex table of results
forecasts_sub_res_table <- select(forecasts_sub, party1_true, party2_true, party3_true, party4_true, party5_true, party6_true, party7_true, party8_true) %>% slice(1)
xtab <- xtable(forecasts_sub_res_table)
names(xtab) <- party_names
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/results-northernireland.tex")


# generate latex table of forecasting performance
forecasts_sub$rank <- rank(forecasts_sub$mae, ties.method = "first")
forecasts_sub_table <- select(forecasts_sub, rank, respondent, mae, rmse, time) %>% arrange(rank) 
xtab <- xtable(forecasts_sub_table)
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/forecasts-northernireland.tex")


# generate table of top forecasters' strategies
forecasts_sub_notes <- select(forecasts_sub, rank, note) %>% arrange(rank) %>% slice(c(1, 2, 6, 7))
print(xtable(forecasts_sub_notes, align = c("r","r", "p{9cm}")), include.rownames = FALSE, size = "scriptsize", file = "class-forecasts/forecasts-northernireland-notes.tex")


# overview of forecasts
pdf(file="class-forecasts/forecasts-northernireland.pdf", height=5, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2.5, 2.5, 2.5, .5))
par(mfrow=c(3, 3))

for (i in 1:length(party_names)) {
  plot(table(forecasts_sub[,paste0("party",i)]), xaxt = "n", main = party_names[i], yaxt = "n", ylab = "", xlab = "Vote share in %", xlim = range(forecasts_sub[,paste0("party",i)], forecasts_sub[,paste0("party",i, "_true")][1,1]))
  axis(1, seq(0, 100, 2), seq(0, 100, 2))
  axis(2, seq(0, 10, 1), seq(0, 10, 1))
  abline(v = forecasts_sub[,paste0("party",i, "_true")][1,1], col = "red", lwd = 2)
  abline(v = colMeans(forecasts_sub[,paste0("party",i)]), lty = 3, lwd = 2)
}

plot(table(forecasts_sub[,"turnout"]), xaxt = "n", main = "Turnout", yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub[,"turnout_true"][1,1], col = "red", lwd = 2)
abline(v = colMeans(forecasts_sub[,"turnout"]), lty = 3, lwd = 2)

dev.off()



## Netherlands general election, March 15, 2017 ----------


# import data
forecasts <- read_csv2("data/class-forecasts.csv")

# select cases
forecasts_sub <- filter(forecasts, country == "Netherlands")


# add actual results
forecasts_sub$party1_true <- 21.3
forecasts_sub$party2_true <- 5.7
forecasts_sub$party3_true <- 13.1
forecasts_sub$party4_true <- 9.1
forecasts_sub$party5_true <- 12.4
forecasts_sub$party6_true <- 12.1
forecasts_sub$turnout_true <- 77.7

forecasts_sub <- mutate(forecasts_sub, sum_check_true = party1_true + party2_true + party3_true + party4_true + party5_true + party6_true)

# encode party names
party_names <- c("VVD",
                 "PdvA",
                 "PVV",
                 "SP",
                 "CDA",
                 "D66")


# compute benchmarks
forecasts_sub <- forecasts_sub %>% 
  rowwise() %>%
  mutate(mae = mean(c(abs(party1_true - party1),
                      abs(party2_true - party2), 
                      abs(party3_true - party3), 
                      abs(party4_true - party4),
                      abs(party5_true - party5),
                      abs(party6_true - party6),
                      abs(turnout_true - turnout))))

forecasts_sub <- forecasts_sub %>% 
  rowwise() %>% 
  mutate(rmse = sqrt(mean(c((party1_true - party1)^2,
                            (party2_true - party2)^2,
                            (party3_true - party3)^2,
                            (party4_true - party4)^2,
                            (party5_true - party5)^2,
                            (party6_true - party6)^2,
                            (turnout_true - turnout)^2))))



# generate latex table of results
forecasts_sub_res_table <- select(forecasts_sub, party1_true, party2_true, party3_true, party4_true, party5_true, party6_true) %>% slice(1)
xtab <- xtable(forecasts_sub_res_table)
names(xtab) <- party_names
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/results-netherlands.tex")


# generate latex table of forecasting performance
forecasts_sub$rank <- rank(forecasts_sub$mae, ties.method = "first")
forecasts_sub_table <- select(forecasts_sub, rank, respondent, mae, rmse, time) %>% arrange(rank) 
xtab <- xtable(forecasts_sub_table)
print(xtab, include.rownames = FALSE, size = "small",  file = "class-forecasts/forecasts-netherlands.tex")


# generate table of top forecasters' strategies
forecasts_sub_notes <- select(forecasts_sub, rank, note) %>% arrange(rank) %>% slice(c(1, 2, 6, 7))
print(xtable(forecasts_sub_notes, align = c("r","r", "p{9cm}")), include.rownames = FALSE, size = "scriptsize", file = "class-forecasts/forecasts-netherlands-notes.tex")


# overview of forecasts
pdf(file="class-forecasts/forecasts-netherlands.pdf", height=5, width=7, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(2.5, 2.5, 2.5, .5))
par(mfrow=c(3, 3))

for (i in 1:length(party_names)) {
  plot(table(forecasts_sub[,paste0("party",i)]), xaxt = "n", main = party_names[i], yaxt = "n", ylab = "", xlab = "Vote share in %", xlim = range(forecasts_sub[,paste0("party",i)], forecasts_sub[,paste0("party",i, "_true")][1,1]))
  axis(1, seq(0, 100, 2), seq(0, 100, 2))
  axis(2, seq(0, 10, 1), seq(0, 10, 1))
  abline(v = forecasts_sub[,paste0("party",i, "_true")][1,1], col = "red", lwd = 2)
  abline(v = colMeans(forecasts_sub[,paste0("party",i)]), lty = 3, lwd = 2)
}

plot(table(forecasts_sub[,"turnout"]), xaxt = "n", main = "Turnout", yaxt = "n", ylab = "", xlab = "Vote share in %")
axis(1, seq(0, 100, 2), seq(0, 100, 2))
axis(2, seq(0, 10, 1), seq(0, 10, 1))
abline(v = forecasts_sub[,"turnout_true"][1,1], col = "red", lwd = 2)
abline(v = colMeans(forecasts_sub[,"turnout"]), lty = 3, lwd = 2)

dev.off()

