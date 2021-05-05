library(Boruta)
df_icu <- read.csv("https://dfzljdn9uc3pi.cloudfront.net/2020/10337/1/ICUMICE2754.csv")
df_expired <- read.csv("https://dfzljdn9uc3pi.cloudfront.net/2020/10337/1/DeadMICE.csv")

boruta <- Boruta(ICU.or.not~., data = df_icu)



X11()
plot(boruta, horizontal = T, ylab="", las = 2, size = 1, cex.axis=0.6, main = "ICU")



boruta_death <- Boruta(death~., data = df_expired)

X11()
plot(boruta_death, horizontal = T, ylab="", las = 2, size = 1, cex.axis=0.6, main = "Mortality")


