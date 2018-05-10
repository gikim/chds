# Final Project
# Statistics 135
# Grace Kim
# gikim@post.harvard.edu

# Install and upload the packages
library(MASS)
library(dynlm)
library(tseries)
library(stats)
library(fSeries)
library(scatterplot3d)
library(lattice)
library(car)
library(sem)
library(boot)
library(zoo)

# Data Analysis: Time Forecasting Model

# Importing the dataset
retail <- read.delim("F:/Stat 135/R/retailsales_2.txt", header=T)
retail1 <- read.delim("F:/Stat 135/R/retailsales.txt", header=T)
retail2 <- subset(retail,select = c(-time,-psales,-psales1,-psales2,-psales3,-psales4,-dpi1,-dpi2,-cpi1,-cpi2,-sales1,-sales2,-sales3,-sales4))
names(retail)        
names(retail1)
names(retail2)
sapply(retail, mode) 
is(retail$sales)
is(retail$dpi)
summary(retail$sales)
summary(retail$dpi)
summary(retail$cpi)
summary(retail$r90)
summary(retail$r1)
summary(retail$r10)
summary(retail$rbaac)

stats <- rbind(c(summary(retail$sales), summary(retail$dpi), summary(retail$cpi), summary(retail$r90), summary(retail$r1), summary(retail$r10), summary(retail$rbaac)))
stats

# Scatterplots of the data. 
pairs(retail2,
      gap=0,
      diag.panel = function (x, ...) {
        par(new = TRUE)
        hist(x,
             col = "light blue",
             probability = TRUE,
             axes = FALSE,
             main = "")
        lines(density(x),
              col = "red",
              lwd = 3)
        rug(x)       
      })

# A time series graph of retail sales.
plot(retail$time, retail$sales, xlab="TIME", ylab="Retail Sales", main="Retail Sales (1992-2007)", col = "brown", pch=15)
plot(retail$sales, xlab="TIME", ylab="Retail Sales", main="Retail Sales (1992-2007)", col = "brown", pch=15)
plot(retail$dpi, xlab="TIME", ylab="DPI", main="Disposable Personal Income (1992-2007)", col = "blue", pch=15)
plot(retail$cpi, xlab="TIME", ylab="CPI", main="Consumer Price Index (1992-2007)", col = "green4", pch=15)
plot(retail$r10, xlab="TIME", ylab="r10", main="10-Year Treasury Rate (1992-2007)", col = "turquoise2", pch=15)

# Correlation of Sales with CPI and DPI
plot(retail$sales, retail$dpi, mar = c(6,6,6,6), main = "Retail Sales vs. Disposable Personal Income (1992-2007)", xlab = "Retail Sales", ylab = "Disposable Personal Income", col = "blue")
cor(retail$sales, retail$dpi)
plot(retail$sales, retail$cpi, main = "Retail Sales vs. CPI (1992-2007)", xlab = "Retail Sales", ylab = "CPI", col = "red")
cor(retail$sales, retail$cpi)

# Other potential forecast models
# Retail Sales with DPI
summary(lm(retail$sales~retail$dpi+retail$cpi))
summary(lm(retail$sales~retail$sales1, data = retail))
# First lag of retail sales is highly significant. Now using other predictors.
summary(lm(retail$sales~retail$sales1+retail$sales2+retail$sales3+retail$sales4, data = retail))
# 3rd and 4th lags are not significant at the 5% significance level
summary(lm(retail$sales~retail$sales1+retail$sales2+retail$dpi1+retail$dpi2+retail$cpi1+retail$cpi2, data=retail))
# None of the predictors except for the first and second lag are signficant.
summary(lm(retail$sales~retail$sales1+retail$sales2+retail$r90+retail$r1+retail$r10+retail$rbaac, data=retail))

# Our final model passes the Granger Causality Test, with the F-statistic on the two lags of retail sales highly significant.
# The Bayes Information Criteria (not calculated here) shows us to use two lags
summary(lm(retail$sales~retail$sales1+retail$sales2, data = retail))
lm.lag2 <- lm(retail$sales~retail$sales1+retail$sales2, data = retail)
plot(lm.lag2, col = "skyblue")
plot(retail$sales1, retail$sales, col = "turquoise", xlab = "Retail Sales(t-1)", ylab = "Retail Sales(t)", main="Retail Sales vs. First Lag")
abline(lm(retail$sales~retail$sales1, data = retail), col="red")
plot(retail$sales2, retail$sales, col = "gray", xlab = "Retail Sales(t-2)", ylab = "Retail Sales(t)", main="Retail Sales vs. Second Lag")
abline(lm(retail$sales~retail$sales2, data = retail), col="blue")
summary(retail$sales1)
summary(retail$sales2)
anova(lm.lag2)

options(na.action=na.exclude)      # excludes (x,y) where x or y is missing
plot(retail$sales1, retail$sales, xlab="Retail Sales Lag 1", ylab="Retail Sales", main = "Retail Sales vs. First Lag", col = "brown")
abline(lm(retail$sales~retail$sales1), col="red")
sales.fit <- fitted(lm(retail$sales~retail$sales1)) # y-values expected for observed x-values, given the linear fit
sales.res <- resid(lm(retail$sales~retail$sales1)) # vertical distances between obs & fit
segments(retail$sales1,sales.fit,

+retail$sales1,retail$sales)   # (x1,y1,x2,y2)

qqnorm(sales.res)   # graphical test of residual normality


# Alternatively plotting the fitted values
plot(retail$sales1, retail$sales, xlab="Retail Sales Lag 1", ylab="Retail Sales", main = "Retail Sales vs. First Lag", col = "brown")
abline(lm(retail$sales~retail$sales1), col="red")
x2 <- retail$sales1
y2 <- FIT
points(x2, y2, pch=1, col="blue")

dim(retail)
print(retail)
attach(retail)
par(mfrow=c(1,1))

par





# Make a graph with the actual and predicted values

fitted(lm(retail$sales~retail$sales1+retail$sales2, data = retail)) 

# Predicted variables: y-values we expect for observed x-values, given the linear fit

resid(lm(retail$sales~retail$sales1+retail$sales2, data = retail))

options(na.action=na.exclude)      # excludes (x,y) where x or y is missing


FIT <- fitted(lm(retail$sales~retail$sales1+retail$sales2, data = retail))
FIT
FIT <- as.matrix(FIT)
FIT
FIT <- ts(FIT)
FIT

# Forecasted Percent Change in Retail Sales
psales <- (FIT[190,]-FIT[189,])/FIT[189,]
psales

# Our forecast was: 0.004159697 = .4%
# On December 13, 2007, the Census Bureau released the advance estimates of U.S. retail and food services sales for November,
# adjusted for seasonal variation and holiday and trading-day differences, but not for price changes, were $385.8 billion,
# an increase of 1.2 percent (±0.7%) from the previous month and 6.3 percent (±0.8%) above November 2006.

RES <- resid(lm(retail$sales~retail$sales1+retail$sales2, data = retail))
# Data Analysis for Oil Daily Prices. Testing the "Uncertainty Hypothesis":
# whether economic uncertainty affects today's oil prices
# relative to tomorrow's, using the GARCH(1,1) model.
oil <- read.delim("F:/Stat 135/R/oildaily.txt", header=T)
names(oil)
summary(oil$spot)
summary(oil$fut1)

summary(oil$fut2)

summary(oil$fut3)

summary(oil$nyse)



oil1 <- subset(oil,select = c(-doy, -tday))

names(oil1)

# Scatterplots of the data. 

pairs(oil1,

      gap=0,

      diag.panel = function (x, ...) {

        par(new = TRUE)

        hist(x,

             col = "light blue",

             probability = TRUE,

             axes = FALSE,

             main = "")

        lines(density(x),

              col = "red",

              lwd = 3)

        rug(x)       

      })



hist(oil$spot, col = "peachpuff", xlab = "Oil Spot Prices")

hist(oil$nyse, col = "turquoise3", xlab = "Closing Value of NYSE Index")

hist(oil$fut1, col = "violet", xlab = "1 Month Futures Contract")

hist(oil$fut2, col = "rosybrown", xlab = "2 Month Futures Contract")

hist(oil$fut3, col = "cyan1", xlab = "3 Month Futures Contract")



library(tseries)

archoutput <- garch(oil$spot,order=c(1,1))

garchoutput <- garch(oil$spot,order=c(1,1))

fitted(garchoutput)

sds <- fitted(garchoutput)

fitted(garchoutput)[1,]

fitted(garchoutput)[2,]



sd <- sds[,1]

sd

sdm <- sds[,2]

sdm

plot(sd, col = "tan", main = "Standard Deviation Bands of the Oil Spot Prices", xlab = "Trading Day", ylab = "SD of the Oil Spot Prices")

plot(sdm, col = "green4", main = "Standard Deviation Bands of the Oil Spot Prices", xlab = "Trading Day", ylab = "SD of the Oil Spot Prices")



archoutput2 <- garch(oil$nyse,order=c(1,1))

garchoutput2 <- garch(oil$nyse,order=c(1,1))

fitted(garchoutput2)

sds2 <- fitted(garchoutput2)

fitted(garchoutput2)[1,]

fitted(garchoutput2)[2,]



sd2 <- sds2[,1]

sd2

sdm2 <- sds2[,2]

sdm2

plot(sd2, col = "tan", main = "Standard Deviation Bands of NYSE Index", xlab = "Trading Day", ylab = "SD of NYSE Index")

plot(sdm2, col = "green4", main = "Standard Deviation Bands of NYSE Index", xlab = "Trading Day", ylab = "SD of NYSE Index")



plot(oil$tday, sd2, main = "NYSE and Oil Price SD bands (scaled)", xlab = "Trading Day", ylab = "NYSE, Oil Spot Price SDs", col = "purple")

abline(lm(sd2~oil$tday), col="yellow")

sd.130 <- sd*130

x2 <- oil$tday

y2 <- sd.130

points(x2, y2, col="blue")

# Fairly similar, but NYSE closing values seems to be more volatile than Oil Daily Prices



basis <- oil$fut2/oil$spot

basis

basis2 <- log(basis)

basis2



lm.basis2 <- lm(basis2~sd2, data = oil)

summary(lm.basis2)

# basis2 is well estimated by the standard deviation bands of

# the NYSE closing values.

print(lm.basis2$coefficients)



basis2.500 <- basis2*500

plot(oil$tday, basis2.500, pch = 3, main = "Basis2, NYSE and Oil Price SD bands (scaled)", xlab = "Trading Day", ylab = "Basis2, NYSE, Oil Spot Price", col = "purple")

abline(lm(basis2~oil$tday), col="yellow")

x2 <- oil$tday

y2 <- sd

points(x2, y2, col="blue")

sd2.200 <- sd2/200

x3 <- oil$tday

y3 <- sd2.200

points(x3, y3, col="red")

# Graph of basis2, sd, sd2



# Graph of basis2 and predicted value of basis2

fitted(lm(basis2~sd2, data = oil)) 

# Predicted variables: y-values we expect for observed x-values, given linear fit

resid(lm(basis2~sd2, data = oil))

options(na.action=na.exclude)      # excludes (x,y) where x or y is missing



basis2.fit <- fitted(lm(basis2~sd2, data = oil)) 

basis2.fit

basis2

basis2.res

basis2.res <- resid(lm(basis2~sd2, data = oil))

plot(oil$tday, basis2, main = "2-month futures Basis & Predicted Value", xlab = "Trading Day", ylab = "Basis2 & Predicted", col = "brown")

abline(lm(basis2~oil$tday), col="red")

x4 <- oil$tday

y4 <- basis2.fit

points(x4, y4, col="tan")



# As seen from the graph, our model does not predict very well the values for basis2, and there

# is little evidence that the Uncertainty Hypothesis holds.



# R Graphics



# 3D normal distribution

mu1<-0 # setting the expected value of x1

mu2<-0 # setting the expected value of x2

s11<-10 # setting the variance of x1

s12<-15 # setting the covariance between x1 and x2
s22<-10 # setting the variance of x2
rho<-0.5 # setting the correlation coefficient between x1 and x2
x1<-seq(-10,10,length=41) # generating the vector series x1
x2<-x1 # copying x1 to x2

#
f<-function(x1,x2)
{
term1<-1/(2*pi*sqrt(s11*s22*(1-rho^2)))
term2<--1/(2*(1-rho^2))
term3<-(x1-mu1)^2/s11
term4<-(x2-mu2)^2/s22
term5<--2*rho*((x1-mu1)*(x2-mu2))/(sqrt(s11)*sqrt(s22))
term1*exp(term2*(term3+term4-term5))
} # setting up the function of the multivariate normal density

#
z<-outer(x1,x2,f) # calculating the density values

#

persp(x1, x2, z,

main="Three dimensional Normal Distribution",

sub=expression(italic(f)~(bold(x))==frac(1,2~pi~sqrt(sigma[11]~

sigma[22]~(1-rho^2)))~phantom(0)^bold(.)~exp~bgroup("{",

list(-frac(1,2(1-rho^2)),

bgroup("[", frac((x[1]~-~mu[1])^2, sigma[11])~-~2~rho~frac(x[1]~-~mu[1],

sqrt(sigma[11]))~ frac(x[2]~-~mu[2],sqrt(sigma[22]))~+~

frac((x[2]~-~mu[2])^2, sigma[22]),"]")),"}")),

col="lightgreen",

theta=30, phi=20,

r=50,

d=0.1,

expand=0.5,

ltheta=90, lphi=180,

shade=0.75,

ticktype="detailed",

nticks=5) # produces the 3-D plot

#

mtext(expression(list(mu[1]==0,mu[2]==0,sigma[11]==10,sigma[22]==10,sigma[12

]==15,rho==0.5)), side=3) # adding a text line to the graph





# 3D Scatterplot

data(iris)

cloud(Sepal.Length ~ Petal.Length * Petal.Width, data = iris,

groups = Species, screen = list(z = 20, x = -70),

perspective = FALSE,

key = list(title = "Iris Data", x = .15, y=.85, corner = c(0,1),

border = TRUE,

points = Rows(trellis.par.get("superpose.symbol"), 1:3),

text = list(levels(iris$Species))))



data(trees)

s3d <- scatterplot3d(trees, type="h", highlight.3d=TRUE,

angle=55, scale.y=0.7, pch=16, main="scatterplot3d - 5")

# Now adding some points to the "scatterplot3d"

s3d$points3d(seq(10,20,2), seq(85,60,-5), seq(60,10,-10),

col="blue", type="h", pch=16)

# Now adding a regression plane to the "scatterplot3d"

attach(trees)

my.lm <- lm(Volume ~ Girth + Height)

s3d$plane3d(my.lm)



# 3D Wireframe

x <- seq(-pi, pi, len = 20)

y <- seq(-pi, pi, len = 20)

g <- expand.grid(x = x, y = y)

g$z <- sin(sqrt(g$x^2 + g$y^2))

wireframe(z ~ x * y, g, drape = TRUE,

aspect = c(3,1), colorkey = TRUE)





# Simulations



# The Movies: (infinite number of servers; no delay)
# A new movie opens, but tickets must be bought in advance.

#The movie is on limited release and runs for two weeks. People buy tickets according to a Poisson process at rate 1200 (per day). For each buyer the separation S between their purchase time and their museum visit is Unif[0,3] (in days). What is the expected cumulative number of visitors T days after opening, where T=1,2,3,..., 14.

N <- 14         # number of days in the two opening weeks
A <- 1200       # mean rate of purchases per day
M <- 1000      # number of iterations (weeks)

museum.week <- function(o)
  {
    K <- rpois(1,N*A)      # number of buyers in week o
    P <- matrix(0,K,2)
    P[,1] <- sort(runif(K,0,N))
    P[,2] <- runif(K,0,3)  # separation S between purchase and visit
    P
  }                        # end definition of museum.week

museum.week()
visit.list <- function(o)
  {
    P <- museum.week()
    V <- P[,1]+P[,2]      # P[,1]+P[,2] is of indeterminate length K  
    L <- sapply(1:14, function(T) length(V[V<T]))
    L
  }

visit.list()
X <- t(sapply(1:M, function(o) visit.list()))
apply(X,2,mean)  
