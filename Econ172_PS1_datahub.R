
setwd("Econ172")
# Eventually need to update this to have PS1, PS2 etc

install.packages("stargazer")

library(haven)
library(stargazer)

#Problem 2
#2b
worms <- read.csv("Econ-172_PSet1-S17-data.csv")
View(worms)

worms2 <- read.csv("Econ172/Econ-172_PSet1-Spring2018-data.csv")
View(worms2)



m1.1=lm(female ~ group1, data=worms)
m1.2=lm(yob ~ group1, data=worms)
m1.3=lm(sap ~ group1, data=worms)

m1.4=lm(female ~ group2, data=worms[worms$group1==0,])
m1.5=lm(yob ~ group2, data=worms[worms$group1==0,])
m1.6=lm(sap ~ group2, data=worms[worms$group1==0,])

m1.7=lm(female ~ group2, data=subset(worms,group1==0))

stargazer(m1.1,m1.2,m1.3,m1.4,m1.5,m1.6,out="Table 1.html",type="html",
          title="Table 1: Covariate Analysis",align=TRUE,
          omit.stat=c("LL","ser","f","adj.rsq"),no.space=TRUE)

#2c
m2.1=lm(part98 ~ group1, data=worms)
m2.2=lm(part98 ~ group2, data=worms[worms$group1==0,])

stargazer(m2.1,m2.2,out="Table 2.html",type="html",
          title="Table 2: School Participation",align=TRUE,
          omit.stat=c("LL","ser","f","adj.rsq"),no.space=TRUE)

#2d
m3.1=lm(part98 ~ group1 + female + yob + sap, data=worms)
m3.2=lm(part98 ~ group2 + female + yob + sap,data=worms[worms$group1==0,])

stargazer(m3.1,m3.2,out="Table 3.html",type="html",
          title="Table 3: School Participation with controls",align=TRUE,
          omit.stat=c("LL","ser","f","adj.rsq"),no.space=TRUE)

#2e

#png(file="Plot 1.png")
plot(worms$female, worms$part98,main="All Schools",
    xlab="Share of Female Students",
    ylab="Average 1998 School Participation",
    col="red")
abline(lm(part98 ~ female,data=worms),col="blue")
#dev.off()

#png(file="Plot 2.png")
plot(worms$female[worms$group1==1], worms$part98[worms$group1==1],
     main="Group 1 Schools",
     xlab="Share of Female Students",
     ylab="Average 1998 School Participation",
     col="red")
abline(lm(part98 ~ female,data=worms[worms$group1==1,]),col="blue")
dev.off()

#2f
m4.1=lm(part98 ~ group1 + female + yob,data=worms[worms$sap==1,])
m4.2=lm(part98 ~ group1 + female + yob,data=worms[worms$sap==0,])

worms$group1xsap <- worms$group1 * worms$sap
m4.3=lm(part98 ~ group1 + group1xsap + sap + female + yob,data=worms)

stargazer(m4.1,m4.2,m4.3,out="Table 4.html",type="html",
          title="Table 4: School Participation by SAP",align=TRUE,
          omit.stat=c("LL","ser","f","adj.rsq"),no.space=TRUE)

savehistory(file="mylog.Rhistory")
