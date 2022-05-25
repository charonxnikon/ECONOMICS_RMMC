data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/budget/temp 2/stats.txt")
data$V1 <- as.Date(data$V1, "%d.%m.%Y")
data$V2 <- as.numeric(data$V2)
data$V3 <- as.numeric(data$V3)
data
plot(data$V1, data$V2,
        xlab = "Date", ylab = "Count in billions rubles",
        type = "l", lwd = 0.5, col = "blue",
        axes = FALSE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
axis(2,c(100,150,195,200,250,300,350,390,400,450))
plot(data$V1, data$V3,
        xlab = "Date", ylab = "Count in billions rubles",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)

plot(data$V4, data$V2,
        xlab = "Date", ylab = "Count in billions rubles",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)

plot(data$V4, data$V3,
        xlab = "Date", ylab = "Count in billions rubles",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)
