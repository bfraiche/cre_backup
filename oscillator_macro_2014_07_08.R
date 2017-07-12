library (deSolve)

df1 <-function(t,y,mu)(list(c(y[2],mu*y[1]^3-y[1]+0.005*cos(t))))

yini<-c(y1=0,y2=0)

df2 <-ode(y=yini,func=df1, times=0:100,parms=0.1667)

df2

write.table(df2, file = "my_wave.csv", sep = ",")

#plot(df2,type="l",which="y1",ylab="",xlab="", main="", col='#5B9BD5', lwd=4)

#abline(h=0,lty=5, col='red', lwd=4)
