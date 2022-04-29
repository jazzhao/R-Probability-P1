getwd()
wdir <- "C:/Users/final/Downloads/ams 315 project 1/PART B"
setwd(wdir)
PartB<-read.csv('737724_PartB.CSV',header = TRUE)

#creating scatterplot
full<-lm(y~x,data=PartB)
full
summary(full)

library(knitr)
kable(anova(full), caption='ANOVA Table')

plot(PartB$y~PartB$x, main='Scatter : y ~ x', xlab='x', ylab='y', pch=20)
abline(full, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')
#---------------------------------------------------------------------------------------------
#transforming data
library(LambertW)
data_trans<-Gaussianize(PartB)
full2<-lm(y.X~x.X,data=data_trans)
full2
summary(full2)

plot(data_trans$y.X~data_trans$x.X, main='Scatter : y.X ~ x.X', xlab='x', ylab='y', pch=20)
abline(full2, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')

#------------------------------------------------------------------------------------------------------------------
#binning the data
x_1<-data_trans$x.X
z=cut(x_1,55)
data_trans$bins<-z

library(dplyr)
ID<-c(data_trans$ID.X)
x_1<-c(data_trans$x.X)
y_1<-c(data_trans$y.X)
bins<-c(data_trans$bins)

df <- tibble(x=x_1,y=y_1) %>%
  mutate(bins = cut(x, breaks=55)) %>%
  group_by(bins) %>%
  summarize(mean_bin = mean(x))

df1 <- data.frame(ID,x_1,y_1,bins)
df3<-merge(x=df,y=df1,by="bins",all.x=TRUE)

#------------------------------------------------------------------------------------------------------------------------
#lack of fit test

library(remotes)

library(alr3)

fit_b <- lm(y_1 ~ mean_bin, data = df3)
pureErrorAnova(fit_b)

