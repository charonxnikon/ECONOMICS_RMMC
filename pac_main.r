data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/pacients/pac_2019_main.txt")
BD <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/pacients/pac_2020_main.txt")
data
BD
plot(data$V2, data$V3,
        main = "Stats EMIAC in Moscow 2019", xlab = "Date", ylab = "Count notes",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
plot(BD$V2, BD$V3,
        main = "Stats EMIAC in Moscow 2020", xlab = "Date", ylab = "Count notes",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)
