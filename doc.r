library(dplyr)

data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 2/pacients/pacients_doctor.txt")
data <- mutate_each(data, "factor", V2,V3)
data
glimpse(data)
cdplot(data$V7, data$V2, col = c("green", "blue","yellow",
	 "brown", "purple","lawngreen"),
	ylab = "Date", xlab = "Count Pacients", main = "Doctors EMIAC STATS")

cdplot(data$V7, data$V3, col = c("green", "blue","yellow",
         "brown", "purple","lawngreen", "black", "orange"),
        ylab = "Date", xlab = "Count Pacients", main = "MONTH EMIAC STATS")

