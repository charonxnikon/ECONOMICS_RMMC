library(gdata)
library(DAAG)
library(nortest)
library(mixtools)

cnv <- function(x)
{
    return (as.numeric(as.vector(unlist(x))))
}

set.seed(10)

gen <- read.csv("generation.csv", header = TRUE, sep = " ")
head(gen)
coeff <- gen$coeff
ps <- read.xls("pat_stat.xlsx", sheet = 1, header = TRUE)
head(ps)
therap <- ps[ps$doctor == "Терапевт", ] 
otorin <- ps[ps$doctor == "Оториноларинголог", ] 
ophtal <- ps[ps$doctor == "Офтальмолог", ] 
surgeon <- ps[ps$doctor == "Хирург", ] 
urolog <- ps[ps$doctor == "Уролог", ] 
print(as.numeric(as.vector(unlist(therap$day0_1))))

therap$sum <- cnv(therap$day0_1) + cnv(therap$day2_3) + cnv(therap$more4)
otorin$sum <- cnv(otorin$day0_5) + cnv(otorin$day6_7) + cnv(otorin$more8)
ophtal$sum <- cnv(ophtal$day0_5) + cnv(ophtal$day6_7) + cnv(ophtal$more8)
surgeon$sum <- cnv(surgeon$day0_5) + cnv(surgeon$day6_7) +
                cnv(surgeon$more8)
urolog$sum <- cnv(urolog$day0_5) + cnv(urolog$day6_7) + cnv(urolog$more8)
##########

length(therap$sum)

write.table(therap[1:185,-c(1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15)],
                 file = "therap_predict.csv", row.names = FALSE)

write.table(otorin[1:185,-c(1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16)],
                file = "otorin_predict.csv", row.names = FALSE)

write.table(ophtal[1:185,-c(1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16)],
                file = "ophtal_predict.csv", row.names = FALSE)

write.table(surgeon[1:185,-c(1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16)],
                 file = "surgeon_predict.csv", row.names = FALSE)


write.table(urolog[1:185,-c(1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16)],
                 file = "urolog_predict.csv", row.names = FALSE)




therap = therap[185:244, ]
otorin = otorin[185:244, ]
ophtal = ophtal[185:244, ]
surgeon = surgeon[185:244, ]
urolog = urolog[185:244, ]
print("NBBBBBBBBBBBBBBBBBBBBBBBBBB!!!!!!!!!!!!!!!!!!")
print(length(therap$sum))
print(length(coeff))
therap$good_pat <- round(therap$sum*coeff)
otorin$good_pat <- round(otorin$sum*coeff)
ophtal$good_pat <- round(ophtal$sum*coeff)
surgeon$good_pat <- round(surgeon$sum*coeff)
urolog$good_pat <- round(urolog$sum*coeff)
print(round(therap$good_pat))

shapiro.test(therap$sum)
shapiro.test(therap$good_pat)


make_therap_est <- function(therap)
{
    lambda <- c(sum(therap$day0_1), sum(therap$day2_3), sum(therap$more4))/
                                            sum(therap$sum)
    print(lambda)                                        

    mix_res <- normalmixEM(therap$good_pat, lambda = lambda, 
    mu = c(mean(therap$day0_1), mean(therap$day2_3), mean(therap$more4)),
    sigma = c(sd(therap$day0_1), sd(therap$day2_3), sd(therap$more4)))

    print("RESULT")
    print(mix_res[c("lambda", "mu", "sigma")])
    #print(c(mean(therap$day0_1), mean(therap$day2_3), mean(therap$more4)))
    #print(c(sd(therap$day0_1), sd(therap$day2_3), sd(therap$more4)))

    print("HELLPDSL:LFJK:SDLKFJ:SDLFKS") 
    print(names(mix_res))
    print(mix_res)
    print("END")
    plot(mix_res, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
         main2="Mixtures normal distr", xlab2="people")

    #hist(mix_res$x, density = TRUE, breaks = 20, col = "deepskyblue")
    #lines(mix_res$mu)
    
    mean <- as.numeric(unlist(mix_res["mu"]))
    sigma <- as.numeric(unlist(mix_res["sigma"]))
    lambda <- as.numeric(unlist(mix_res["lambda"]))
    

    return (c(mean, sigma, lambda))
}

make_other_est<- function(therap)
{
    lambda <- c(sum(therap$day0_5), sum(therap$day6_7), sum(therap$more8))/
                                            sum(therap$sum)
    print("LAMBDA")
    print(lambda)                                        

    mix_res <- normalmixEM(therap$good_pat, lambda = lambda, 
    mu = c(mean(therap$day0_5), mean(therap$day6_7), mean(therap$more8)),
    sigma = c(sd(therap$day0_5), sd(therap$day6_7), sd(therap$more8)),
           epsilon = 1e-09)


    #mix_res[c("lambda", "mu", "sigma")]
    #print(c(mean(therap$day0_5), mean(therap$day6_7), mean(therap$more8)))
    #print(c(sd(therap$day0_5), sd(therap$day6_7), sd(therap$more8)))

    plot(mix_res, density=TRUE, cex.axis=1.4, cex.lab=1.4, cex.main=1.8,
         main2="Mixtures normal distr", xlab2="people")
    mean <- as.numeric(unlist(mix_res["mu"]))
    sigma <- as.numeric(unlist(mix_res["sigma"]))
    lambda <- as.numeric(unlist(mix_res["lambda"]))

    
    return (c(mean, sigma, lambda))
}



save_result <- function(therap, name, type = "other")
{

    #X11()
    par(mfrow = c(2, 1))
    if (type == "other")
    {
        s1 <- make_other_est(therap[1:30,])
        s2 <- make_other_est(therap[10:40, ])

        s3 <- make_other_est(therap[20:50, ])
        s4 <- make_other_est(therap[30:60, ])
    }
    if (type == "therap")
    {
        s1 <- make_therap_est(therap[1:30,])
        s2 <- make_therap_est(therap[10:40, ])

        s3 <- make_therap_est(therap[20:50, ])
        s4 <- make_therap_est(therap[30:60, ])
    }


    d1 <- s1
    d2 <- 0.5*s1 + 0.5*s2
    d3 <- (s1 + s2 + s3)/3
    d4 <- (s2 + s3 + s4)/3
    d5 <- (s3 + s4)/2
    d6 <- s4

    dfinal <- c(d1, d2, d3, d4, d5, d6)
    write.table(dfinal, name)
    print((as.numeric(unlist(s1))))
    
    #Sys.sleep(10)
    
    print("HEEEEEEELLOOOOO")

    return (c(d1[1:3]*d1[7:9], d2[1:3]*d2[7:9], d3[1:3]*d3[7:9], 
        d4[1:3]*d4[7:9], d5[1:3]*d5[7:9],  d6[1:3]*d6[7:9]))
}

therap_means <- save_result(therap, "therapist.csv", type = "therap")
otorin_means <- save_result(otorin, "otoring.csv")
ophtal_means <- save_result(ophtal, "ophtal.csv")
surgeon_means <- save_result(surgeon, "surgeon.csv")
urolog_means <- save_result(urolog, "urolog.csv")

write_therap <- function(therap)
{

    th_pr_day01 <- c()
    th_pr_day23 <- c()
    th_pr_more4 <- c()


    for (i in seq(from = 1, to = 18, by = 3))
    {
        th_pr_day01 <- append(th_pr_day01, rep(therap_means[i + 0], 
                                                    each = 10))
        th_pr_day23 <- append(th_pr_day23, rep(therap_means[i + 1], 
                                                    each = 10))
        th_pr_more4 <- append(th_pr_more4, rep(therap_means[i + 2], 
                                                    each = 10))
    }

    therap$pr_day01 <- th_pr_day01
    therap$pr_day23 <- th_pr_day23
    therap$pr_more4 <- th_pr_more4

    therap <- therap[, -c(1, 2, 3, 4, 8, 9, 10, 11, 12, 13, 14, 15)]

    write.table(therap, "therap_train2.csv", row.names = FALSE)
}

#mean6 <- function(x)
#{
#    m1 <- mean(x[1:10])
#    m2 <- mean(x[11:20])
#    m3 <- mean(x[21:30])
#    m4 <- mean(x[31:40])
#    m5 <- mean(x[41:50])
#    m6 <- mean(x[51:60])

#    return c(m1, m2, m3, m4, m5, m6)
#}


    
write_other <- function(therap, therap_means, name)
{

    th_pr_day05 <- c()
    th_pr_day67 <- c()
    th_pr_more8 <- c()


    for (i in seq(from = 1, to = 18, by = 3))
    {
        th_pr_day05 <- append(th_pr_day05, rep(therap_means[i + 0], 
                                                    each = 10))
        th_pr_day67 <- append(th_pr_day67, rep(therap_means[i + 1], 
                                                    each = 10))
        th_pr_more8 <- append(th_pr_more8, rep(therap_means[i + 2], 
                                                    each = 10))
    }

    therap$pr_day05 <- th_pr_day05
    therap$pr_day67 <- th_pr_day67
    therap$pr_more8 <- th_pr_more8

    therap <- therap[, -c(1, 2, 3, 4, 5, 6, 7, 11, 12, 13, 14, 15, 16)]

    write.table(therap, name, row.names = FALSE)
}

write_therap(therap)
write_other(otorin, otorin_means, "otorin_train2.csv")
write_other(ophtal, ophtal_means, "ophtal_train2.csv")
write_other(surgeon, surgeon_means, "surgeon_train2.csv")
write_other(urolog, urolog_means, "urolog_train2.csv")



###########################NOW WE MAKE RANDOM FOREST#######################

#make_therap_est(therap[1:20,])
#make_therap_est(therap[21:40, ])
#make_therap_est(therap[41:60,])

















