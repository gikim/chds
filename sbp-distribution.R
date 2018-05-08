# Rcode
* Probability distributions for SBP, with varying means

* Graph 1

mean <- c(124.4)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
hx <- dnorm(x,mean_SBP,sd)

plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

for (i in 1){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}

abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 0), col=colors)


* Graph 2

mean <- c(124.4, 123.4)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "Mean=123.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
hx <- dnorm(x,mean_SBP,sd)

plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}

abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 0), col=colors)


* Graph 3

mean <- c(124.4, 122.4)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "Mean=122.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
hx <- dnorm(x,mean_SBP,sd)

plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}

abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 0), col=colors)


* Graph 4

mean <- c(124.4, 121.4)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "Mean=121.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
hx <- dnorm(x,mean_SBP,sd)

plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}

abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 0), col=colors)



* Shaded Graphs

* Graph 2_Shaded


mean <- c(124.4, 123.4, 110)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "Mean=123.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}
plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

lb=mean[3]
ub=150
i <- x >= lb & x <= ub
hx <- dnorm(x,mean[1],sd)
hx_2 <- dnorm(x,mean[2],sd)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="white", border="white")


lb=100
ub=124.9
i <- x >= lb & x <= ub
hx <- dnorm(x,mean[1],sd)
hx_2 <- dnorm(x,mean[2],sd)
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="white", border="white")


lines(x, hx, lwd=2, col="black")
lines(x, hx_2, lwd=2, col="red")
abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 0), col=colors)
axis(1, col="black")




* Graph 3_Shaded

mean <- c(124.4, 122.4, 110)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "Mean=122.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}
plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

lb=mean[3]
ub=150
i <- x >= lb & x <= ub
hx <- dnorm(x,mean[1],sd)
hx_2 <- dnorm(x,mean[2],sd)
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="white", border="white")

lb=100
ub=123.4
i <- x >= lb & x <= ub
hx <- dnorm(x,mean[1],sd)
hx_2 <- dnorm(x,mean[2],sd)
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="white", border="white")

lines(x, hx, lwd=2, col="black")
lines(x, hx_2, lwd=2, col="red")
abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 0), col=colors)
axis(1, col="black")




* Graph 4_Shaded

mean <- c(124.4, 121.4, 110)
sd=20
colors <- c("black", "red")
labels <- c("Mean=124.4", "Mean=121.4", "SD=20 mmHg")

x <- seq(100,150,length=100)
for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}
plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

lb=mean[3]
ub=150
i <- x >= lb & x <= ub 
hx <- dnorm(x,mean[1],sd) 
hx_2 <- dnorm(x,mean[2],sd) 
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="white", border="white")

lb=100
ub=122.9
i <- x >= lb & x <= ub
hx <- dnorm(x,mean[1],sd)
hx_2 <- dnorm(x,mean[2],sd)
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="white", border="white")

lines(x, hx, lwd=2, col="black")
lines(x, hx_2, lwd=2, col="red")
abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 0), col=colors)
axis(1, col="black")




* Graph 4_Shaded, with 140 mmHg line attached 

mean <- c(124.4, 121.4, 110)
sd=20
colors <- c("black", "red", "blue")
labels <- c("Mean=124.4", "Mean=121.4", "140 mmHg", "SD=20 mmHg")

x <- seq(100,150,length=100)
for (i in 1:2){
hx <- dnorm(x,mean[i],sd)
lines(x, hx, lwd=2, col=colors[i])
}
plot(x, hx, type="l", col="black", ylim=c(.01,.02), lty=2, xlab="Systolic Blood Pressure (mmHg)", ylab="Probability", axes=TRUE)

lb=mean[3]
ub=150
i <- x >= lb & x <= ub 
hx <- dnorm(x,mean[1],sd) 
hx_2 <- dnorm(x,mean[2],sd) 
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="white", border="white")

lb=100
ub=122.9
i <- x >= lb & x <= ub
hx <- dnorm(x,mean[1],sd)
hx_2 <- dnorm(x,mean[2],sd)
polygon(c(lb,x[i],ub), c(0,hx_2[i],0), col="grey", border="white")
polygon(c(lb,x[i],ub), c(0,hx[i],0), col="white", border="white")

lines(x, hx, lwd=2, col="black")
lines(x, hx_2, lwd=2, col="red")
abline(v=140, col="blue")
legend("topleft", inset=0, title="Distributions", labels, lwd=2, lty=c(1, 1, 1, 0), col=colors)
axis(1, col="black")




References
http://www.statmethods.net/advgraphs/probability.html
http://www.cyclismo.org/tutorial/R/probability.html
http://www.stat.wisc.edu/~deepayan/SIBS2005/demos/base-graphics.html
http://www.stat.psu.edu/~dhunter/R/html/graphics/html/polygon.html
http://www.r-project.org/misc/acpclust.R

