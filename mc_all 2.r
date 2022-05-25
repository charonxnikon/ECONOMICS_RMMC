data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/moscow_covid/all data/covid_moscow.txt")
names(data)
data$V1 <- as.Date(data$V1, "%d.%m.%Y")
data
plot(data$V1, data$V3,
        xlab = "Date", ylab = "Count new incidence",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
e.y = c(0:6); my.at <- e.y*10^5
plot(data$V1, data$V2,
        xlab = "Date", ylab = "",
        type = "l", lwd = 0.5, col = "blue",
        axes = FALSE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)
axis(2,at = my.at,col.axis = "black",las = 1,
labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^5))))
axis(1, c(0, 10, 20, 30, 40, 50, 60, 70))


plot(data$V5, data$V3,
        main = "Stats Covid-19 in Moscow", xlab = "Date", ylab = "Count new incidence",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)
plot(data$V5, data$V2,
        main = "Stats Covid-19 in Moscow", xlab = "Date", ylab = "Count main",
        col.main = "red",type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V2), lty = "dashed"),frame.plot = TRUE)
