##Panel Data Analysis
#install.packages("plm")

#import the dataset
library(plm)
data("Grunfeld", package="plm")
#data("EmplUK", package="plm")
#data("Produc", package="plm")
#data("Wages", package="plm")
Grunfeld$firm

#writint dataste to specific path for csv fiel
write.csv(Grunfeld,"D:\\SCMA 632\\R\\Project_6\\grun1.csv")

#Pooled OLS
grun.pols <- plm(inv~value+capital, data = Grunfeld, model = "pooling")
summary(grun.pols)


#Between Model
grun.btwn <- plm(inv~value+capital, data = Grunfeld, model = "between")
summary(grun.btwn)

#First Difference
grun.fd <- plm(inv~value+capital, data = Grunfeld, model = "fd")
summary(grun.fd)


#Fixed Effects Model
#firms are entities
grun.fe <- plm(inv~value+capital, data = Grunfeld, index=c("firm", "year"),model = "within")
summary(grun.fe)

fixef(grun.fe) 

#fixed effect model -one way
#install.packages('foregin')
library(foreign)
fixed.dum.1w <-lm(inv~value+capital + factor(firm)-1, data=Grunfeld)
summary(fixed.dum.1w)

#fixed effect model -two way
library(foreign)
fixed.dum.2w <-lm(inv~value+capital + factor(firm)+factor(year) - 1, data=Grunfeld)
summary(fixed.dum.2w)


#Random Effects Model
grun.re <- plm(inv~value+capital, data = Grunfeld, model = "random")
summary(grun.re)

#Hausman Test
phtest(grun.re, grun.fe)
#Tests for the statistical significance of the difference 
#between the coefficient estimates obtained by FE and by RE, 
#under then null hypothesis that the RE estimates are efficient and 
#consistent, and FE estimates are inefficient.




