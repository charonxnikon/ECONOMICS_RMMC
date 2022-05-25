data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/pacients/pac_all/statx.txt")
names(data)
data$V5 <- as.Date(data$V5, "%d.%m.%Y")
data

plot(data$V4, data$V3,
        xlab = "Date", ylab = "Count",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)

plot(data$V5, data$V3,
        xlab = "Date", ylab = "Count",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, frame.plot = TRUE)

plot(data$V4, data$V1,
        xlab = "Date", ylab = "Count",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, panel.first = lines(stats::lowess(data$V1,data$V3),lty = "dashed"),frame.plot = TRUE)

plot(data$V5, data$V1,
        xlab = "Date", ylab = "Count",
        type = "l", lwd = 0.5, col = "blue",
        axes = TRUE, frame.plot = TRUE)

require(ggplot2)

df <- data.frame(data$V4,data$V3,data$V1)

ggplot(df, aes(data$V4)) +
  geom_line(aes(y=data$V3), colour="blue") +
  geom_line(aes(y=data$V1), colour="green")

df <- data.frame(data$V5,data$V3,data$V1)

ggplot(df, aes(data$V5)) +
  geom_line(aes(y=data$V3), colour="blue") +
  geom_line(aes(y=data$V1), colour="green")

