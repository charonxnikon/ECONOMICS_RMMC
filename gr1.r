x <- c(0,1,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20)
y1 <- 2/3*x 

require(ggplot2)


plot(y1, x, type = "l", lwd = 0.5, col = "blue", axes = TRUE)

y2 <- 2*x

plot(y2, x, type = "l", lwd = 0.5, col = "blue", axes = TRUE)

df <- data.frame(x,y1,y2)

ggplot(df, aes(x)) +                    
  geom_line(aes(y=y1), colour="blue") +
  geom_line(aes(y=y2), colour="green")

t1 <- c(0,1,3,4,5,6,7,8,9,10,11,12,13,15,16,17,18,19,20)
t2 <- t1+t1/6
t2

plot(t1, x, type = "l", lwd = 0.5, col = "blue", axes = TRUE)
plot(t2, x, type = "l", lwd = 0.5, col = "blue", axes = TRUE)

df <- data.frame(x,t1,t2)

ggplot(df, aes(x)) +
  geom_line(aes(y=t1), colour="blue") +
  geom_line(aes(y=t2), colour="green")


p1 <- y1/t1
p2 <- y2/t2

plot(p1, x, type = "l", lwd = 0.5, col = "blue", axes = TRUE)
plot(p2, x, type = "l", lwd = 0.5, col = "blue", axes = TRUE)


df <- data.frame(x,p1,p2)

ggplot(df, aes(x)) +
  geom_line(aes(y=p1), colour="blue") +
  geom_line(aes(y=p2), colour="green")
