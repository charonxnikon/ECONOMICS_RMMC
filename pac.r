data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/pacients/new/pac.txt")
data
data$V4 <- as.Date(data$V4, "%d.%m.%Y")
plot(data$V4, data$V8,
        xlab = "Date", ylab = "Count notes",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)

plot(data$V9, data$V8,
        xlab = "Date", ylab = "Count notes",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
