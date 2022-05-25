data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/budget/stats_all.txt")
data$V1 <- as.Date(data$V1, "%d.%m.%Y")
data
plot(data$V1, data$V2,
        main = "Stats Budget-all in Moscow 2020", xlab = "Date", ylab = "Count in billions rubles",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = FALSE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
axis(2,c(100,150,195,200,250,300,350))
plot(data$V1, data$V3,
        main = "Stats Budget by month in Moscow 2020", xlab = "Date", ylab = "Count in billions rubles",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)



plot(data$V8, data$V2,
        main = "Stats Budget-all in Moscow 2020", xlab = "Date", ylab = "Count in billions rubles",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)

plot(data$V8, data$V3,
        main = "Stats Budget by month in Moscow 2020", xlab = "Date", ylab = "Count in billions rubles",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)
