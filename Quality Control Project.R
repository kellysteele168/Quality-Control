install.packages('readxl')
install.packages('reshape2')
install.packages('writexl')

library(readxl)
library(reshape2)
library(writexl)

Amtrak<-file.choose('Amtrak,xlxs')
data<-read_xlsx(Amtrak,sheet='Sheet1')
x<-data

plot(x,main='Deviation from Schedule', xlab='x',ylab='deviation (mins)')
 
#testing for normality
qqnorm(x$time)
shapiro.test(x$time)


#making statistical control chart
xbar<-mean(x$time)
xsd<-sd(x$time)
CL<-xbar
UCL<-xbar+3*xsd
LCL<-xbar-3*xsd
plot(x)
abline(h=CL)
abline(h=UCL)
abline(h=LCL)

x[which(x[,2]>250),]
new<-x[-c(306),]

newbar<-mean(new$time)
newsd<-sd(new$time)
CL<-newbar
UCL<-newbar+3*newsd
LCL<-newbar-3*newsd
plot(new)
abline(h=CL)
abline(h=UCL)
abline(h=LCL)

new[which(new[,2]>80),]
new1<-new[-c(230),]

new1bar<-mean(new1$time)
new1sd<-sd(new1$time)
CL<-new1bar
UCL<-new1bar+3*new1sd
LCL<-new1bar-3*new1sd
plot(new1)
abline(h=CL)
abline(h=UCL)
abline(h=LCL)

#control chart on policy
CL<-0
UCL<-30
plot(x)
abline(h=CL)
abline(h=UCL)

#Calculate x_bar_bar and R_bar using colMeans on each resprective row.
x_bar_bar<-round(colMeans((Ex6Data6_1)[7]),4)
print(x_bar_bar)
R_bar<-round(colMeans((Ex6Data6_1)[8]),5)
print(R_bar)




