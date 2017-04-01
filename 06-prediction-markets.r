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



# correction for favorite longshot bias
# Leigh et al. 2007 suggest the following transformation:
# Pr = cum_norm(1.64*inv_cum_norm(price))
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), xlab = "raw prediction", ylab = "debiased prediction")
abline(0, 1, lty = 2)
curve(pnorm(1.64*qnorm(x)), add = TRUE)
grid()


pdf(file="favorite-longshot.pdf", height=5, width=5, family="URWTimes")
par(oma=c(0,0,0,0) + .7)
par(mar=c(4, 4, 0, 0))
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), xlab = "market-implied probability", ylab = "true probability")
abline(0, 1, lty = 2)
#curve(pnorm(1.64*qnorm(x)), add = TRUE)
grid()
dev.off()



# Condorcet's jury theorem
plot(NA, NA, ylim = c(0, 1), xlim = c(0, 1), xlab = "individual probability", ylab = "group probability")
abline(0, 1, lty = 2)
N <- 50 # number of jurors
m <- N/2 + 1 # number of jurors required for majority
curve(pbinom(m, size = N, prob = x, lower.tail = FALSE), add = TRUE)
grid()