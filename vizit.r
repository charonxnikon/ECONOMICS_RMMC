
data <- read.table("/Users/Nikon/Desktop/Econ_stats/part 3/web/vizit_web.txt", header = TRUE, fill = TRUE)
data
e.y = c(0,2,4,6,8); my.at <- e.y*10^6
hist(data$x,
	 axes = FALSE, xlab = "Count vizits")
axis(1,at = my.at,col.axis = "black",las = 1,
labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^6))))
axis(2, c(0, 10, 20, 30, 40, 50, 60, 70))
e.y = c(0:8); my.at <- e.y*10^5
hist(data$y,
         axes = FALSE, xlab = "")
axis(1,at = my.at,col.axis = "black",las = 1,
labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^5))))
axis(2, c(0, 10, 20, 30, 40, 50, 60, 70))
hist(data$z,
         axes = FALSE, xlab = "Count vizits")
axis(1,at = my.at,col.axis = "black",las = 1,
labels = as.expression(lapply(e.y,function(E) bquote(.(E) %*% 10.^5))))
axis(2, c(0, 10, 20, 30, 40, 50, 60, 70))
