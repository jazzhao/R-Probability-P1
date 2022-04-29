getwd()
wdir <- "C:/Users/final/Downloads/ams 315 project 1/part a"
setwd(wdir)
IV<-read.csv('737724_IV.csv',header = TRUE)
DV<-read.csv('737724_DV.csv',header = TRUE)

#merging
total<-merge(IV,DV,by="ID")

any(is.na(total[,2]) == TRUE)#confirms that there are NA values in column 2, which is IV
any(is.nan(total[,2]) == TRUE)#confirms that there are no values that is not a number in IV column
any(is.na(total[,3]) == TRUE)#confirms that there are NA values in column 3, which is DV
any(is.nan(total[,3]) == TRUE)#confirms that there are no values that is not a number in DV column

library(mice)

#inspecting missing datas
md.pattern(total)
#there are complete 566 complete data sets
#IV is missing in 64 cases
#DV is missing in 60 cases
#IV and DV is missing in 28 cases

#imputing missing data
my_imp=mice(total,m=5,method=c("","pmm","pmm"))
my_imp$imp$IV 
summary(total$IV)
total2=complete(my_imp,3) #complete data
md.pattern(total2) #inspecting to make sure the data is complete

#simple regression for data
M <- lm(DV ~ IV, data=total2)
M
summary(M)

library(knitr)
kable(anova(M), caption='ANOVA Table')

plot(total2$DV ~ total2$IV, main='Scatter : DV ~ IV', xlab='IV', ylab='DV', pch=20)
abline(M, col='red', lty=3, lwd=2)
legend('topleft', legend='Estimated Regression Line', lty=3, lwd=2, col='red')

# Then we can calculate the confidence interval of the slope.
# 95% CI of slope:
confint(M, level = 0.95)

# 99% CI of slope:
confint(M, level = 0.99)


