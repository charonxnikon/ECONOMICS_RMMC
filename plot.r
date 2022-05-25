data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 1/final_predict/predict.txt")
data
plot(data$V1, data$V7,
        xlab = "Date", ylab = "Count good pac",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)

require(ggplot2)

df <- data.frame(data$V1,data$V7,data$V13)

ggplot(df, aes(data$V1)) +
  geom_line(aes(y=data$V7), colour="blue") +
  geom_line(aes(y=data$V13), colour="green")

plot(data$V1, data$V13,
        xlab = "Date", ylab = "Count good pac",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, frame.plot = TRUE)

