data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/moscow_covid/covid_moscow.txt")
names(data)
data$V1 <- as.Date(data$V1, "%d.%m.%Y")
data
plot(data$V1, data$V3,
	main = "Stats Covid-19 in Moscow", xlab = "Date", ylab = "Count new incidence",
	col.main = "red",type = "l", lwd = 0.5, col = "blue", 
	axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
plot(data$V1, data$V2,
        main = "Stats Covid-19 in Moscow", xlab = "Date", ylab = "Count main",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)
