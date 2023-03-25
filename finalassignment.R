library("plm")
library("lmtest")
library("htest")

#Setting the working directory, pay attention to the backslashes.

setwd("//dustaff/home/nme/Documents/Advanced Econometrics/Assignment 5")

#Read the data

myData = read.csv("xm729pmi.csv")

#Declaring the logprod as Y-variable:
Y = cbind(myData[,5])

#Declaring the others as independent variables:
X = cbind(myData[,3], myData[,4])

#Set the data as panel data:
pdata = pdata.frame(myData, index = c("Time", "ID"))

print (summary(pdata))

pooling = plm(Y ~ X, data = pdata, model = "pooling")
print (summary(pooling))

#Fixed effect or within estimator
fixed = plm(Y ~ X, data = pdata, model = "within")
print (summary(fixed))


print(pFtest(fixed, pooling))

firstdiff = plm(Y ~ X, data = pdata, model = "fd")
print (summary(firstdiff))


between = plm(Y ~ X, data = pdata, model = "between")
random = plm(Y ~ X, data = pdata, model = "random")

print(summary(random))

#Breusch-Pegan test for heteroskedasticity
print(bptest(Y ~ X, data = pdata, studentize = FALSE))

sink("out2.txt")
h = plm(Y ~ X, data = pdata, model = "within",effect = "twoways")
print(summary(fixef(h, effect = "time")))
sink()
sink("out.txt")
unobserved = pwtest(Y ~ X, data = pdata)
print(unobserved)
sink()
#locally robust test
robustness = pbsytest(Y ~ X, data = pdata, test = "j")

#robust covariance matrix estimation
sink('out.txt')
robustRE = plm(Y ~ X, data = pdata, model = "random")
coeftest(robustRE, vcov = pvcovHC)
sink()

sink("out.txt")
a = (pggls(Y ~ X , data = pdata, model = "pooling"))
print (summary(a))
b = (pggls(Y ~ X , data = pdata, model = "random"))
print(summary(b))
sink()