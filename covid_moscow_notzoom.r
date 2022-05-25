data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/moscow_covid/covid_moscow_notzoom.txt")
names(data)
data
plot(data$V1, data$V4,
	main = "Stats Covid-19 in Moscow", xlab = "Date", ylab = "Count new incidence",
	col.main = "red",type = "l", lwd = 0.5, col = "blue", 
	axes = TRUE, frame.plot = TRUE)
plot(data$V1, data$V2,
        main = "Stats Covid-19 in Moscow", xlab = "Date", ylab = "Count main",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, frame.plot = TRUE)
