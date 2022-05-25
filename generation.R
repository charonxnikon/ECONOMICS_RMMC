set.seed(42)
good_pat1 <- 0.3*rnorm(60, 325, 50) + 0.3*rnorm(60, 275, 50) +
    + 0.3*rnorm(60, 375, 50) + 0.1*rnorm(60, 325, 100) 
good_pat1 <- round(good_pat1)
print(good_pat1)
div30 <- function(x)
{
    return(x/30)
}
covid_stat_day <- c(625, 690, 692, 671, 620, 690, 695, 642, 695, 698, 670, 650, 696, 730, 750, 730, 805, 825, 860, 915, 980, 970, 1050, 1560, 1792, 2016, 2217, 2300, 2308, 2424, 2704, 2884, 3327, 3537, 4082, 3229, 3323, 3701, 4105, 4501, 4395, 4618, 4573, 3942, 5049, 4648, 4610, 5376, 4999, 4389, 4413, 5478, 4453, 4455, 5224, 4312, 3670, 4906)


print(sapply(1:60, FUN = div30)*covid_stat_day/40)
#*rnorm(30, 30, 20)

noise <- sapply(1:30, FUN = div30)*covid_stat_day/40

good_pat1 <- good_pat1 + round(noise)
good_pat1 <- round(good_pat1*0.85)
print(runif(60, 0.05, 0.20))
all_pat1 <- round(good_pat1*(1 + runif(60, 0.10, 0.20)))
print(good_pat1)
print(all_pat1)
print(good_pat1/all_pat1)
coeff <- good_pat1/all_pat1
mean(coeff)
df <- data.frame(good_pat = good_pat1, all_pat = all_pat1, coeff = coeff)
write.table(x = df, file = "generation.csv", quote = FALSE)
