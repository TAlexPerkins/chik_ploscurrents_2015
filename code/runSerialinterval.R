# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code estimates how weekly data should be weighted
# to reflect the serial interval distribution.


# estimates of the serial interval distribution were derived from...
#
# LOCAL AND REGIONAL SPREAD OF CHIKUNGUNYA FEVER IN THE AMERICAS
# S Cauchemez, M Ledrans, C Poletto, P Quenel, H de Valk, V Colizza, P Y Boelle
# Eurosurveillance 2014
#
# ...who report that the distribution has a mean of 23, standard devition of 6,
# and in a plot they show it looks somewhat like a gamma

# by the method of moments, the parameters of the gamma are
param.1 = (23/6)^2
param.2 = 23/(6^2)

# as one can see, most of the mass falls in weeks 3-5, with the amount in each
weights.serialinterval =
  sapply(1:5,function(ww)
    integrate(function(t){
      pgamma(t+7,param.1,param.2)-pgamma(t,param.1,param.2)},
      (ww-1)*7,ww*7)$value/7)
weights.serialinterval = weights.serialinterval / sum(weights.serialinterval)
#   diff(c(0,pgamma(7*(1:5),param.1,param.2))) /
#   sum(diff(c(0,pgamma(7*(1:5),param.1,param.2))))


# plot to demonstrate serial interval concept
pdf('../output/serialinterval.pdf',width=6.5,height=4)

par(oma=c(2,0,0,0),mar=c(2,4,1,4.5))

plot(
  -100,-100,xlim=c(-35,21),ylim=c(0,.07),
  xaxt='n',xlab='',ylab='Density',las=1)
abline(v=(-5:3)*7)

# segments(
#   seq(-35,-7,7)+7*1/6,
#   rep(0,8),
#   seq(-35,-7,7)+7*1/6,
#   rev(weights.serialinterval),
#   lwd=17,lend=1,col=rgb(1,0,0,.15))
# segments(
#   seq(-28,0,7)+7*3/6,
#   rep(0,8),
#   seq(-28,0,7)+7*3/6,
#   rev(weights.serialinterval),
#   lwd=17,lend=1,col=rgb(0,1,0,.15))
# segments(
#   seq(-21,7,7)+7*5/6,
#   rep(0,8),
#   seq(-21,7,7)+7*5/6,
#   rev(weights.serialinterval),
#   lwd=17,lend=1,col=rgb(0,0,1,.15))

polygon(
  c(seq(-35,0,.001),0,-35),
  c(dgamma(seq(35,0,-.001),param.1,param.2),0,0),
  col=rgb(1,0,0,.2),border=NA)
polygon(
  c(seq(-28,7,.001),7,-28),
  c(dgamma(seq(35,0,-.001),param.1,param.2),0,0),
  col=rgb(0,1,0,.2),border=NA)
polygon(
  c(seq(-21,14,.001),14,-21),
  c(dgamma(seq(35,0,-.001),param.1,param.2),0,0),
  col=rgb(0,0,1,.2),border=NA)

cases = c(.01,.014,.02,.026,.033,.042,.055,.07) # * .45 / .07

segments(
  seq(-28,21,7)-7*1/3,
  rep(0,8),
  seq(-28,21,7)-7*1/3,
  cases,
  lwd=7,lend=1,col=rgb(0,0,0,1))
segments(
  7-7*2/3,0,7-7*2/3,sum(weights.serialinterval * cases[5:1]),
  lwd=7,lend=1,col=rgb(1,0,0,1))
segments(
  14-7*2/3,0,14-7*2/3,sum(weights.serialinterval * cases[6:2]),
  lwd=7,lend=1,col=rgb(0,1,0,1))
segments(
  21-7*2/3,0,21-7*2/3,sum(weights.serialinterval * cases[7:3]),
  lwd=7,lend=1,col=rgb(0,0,1,1))

axis(1,at=seq(-35,21,7))
axis(4,at=seq(0,.07,.01),labels=as.character(seq(000,700,100)),las=1)
mtext('Infectious people',4,line=3.25)

mtext(expression(I["i,-5"]),3,at=-28-7*1/3,cex=.7)
mtext(expression(I["i,-4"]),3,at=-21-7*1/3,cex=.7)
mtext(expression(I["i,-3"]),3,at=-14-7*1/3,cex=.7)
mtext(expression(I["i,-2"]),3,at=-7-7*1/3,cex=.7)
mtext(expression(I["i,-1"]),3,at=0-7*1/3,cex=.7)
mtext(expression(I["i,0"]),3,at=7-7*1/3,cex=.7)
mtext(expression(I["i,1"]),3,at=14-7*1/3,cex=.7)
mtext(expression(I["i,2"]),3,at=21-7*1/3,cex=.7)
mtext(expression("I'"["i,0"]),3,at=7-7*2/3,cex=.7,col=rgb(1,0,0,1))
mtext(expression("I'"["i,1"]),3,at=14-7*2/3,cex=.7,col=rgb(0,1,0,1))
mtext(expression("I'"["i,2"]),3,at=21-7*2/3,cex=.7,col=rgb(0,0,1,1))

# plot(
#   -100,-100,xlim=c(-35,21),ylim=c(0,.3),
#   xaxt='n',xlab='',ylab='Density',las=1)
# abline(v=(-5:3)*7)
# 
# polygon(
#   c(seq(-14,0,.001),0,-14),
#   c(dgamma(seq(14,0,-.001),5.9*3,3),0,0),
#   col=rgb(1,0,0,.2),border=NA)
# polygon(
#   c(seq(-7,7,.001),7,-7),
#   c(dgamma(seq(14,0,-.001),5.9*3,3),0,0),
#   col=rgb(0,1,0,.2),border=NA)
# polygon(
#   c(seq(0,14,.001),14,0),
#   c(dgamma(seq(14,0,-.001),5.9*3,3),0,0),
#   col=rgb(0,0,1,.2),border=NA)
# 
# cases = cases / .07 * 700
# susc = 100000 - cumsum(cases)
# susc = (susc - 96000) / 4000 * .3
# 
# segments(
#   seq(-28,21,7)-7*2/3,
#   rep(0,8),
#   seq(-28,21,7)-7*2/3,
#   susc,
#   lwd=7,lend=1,col=rgb(0,0,0,.75))
# segments(
#   7-7*1/3,0,7-7*1/3,sum(c(.21,.79) * susc[4:5]),
#   lwd=7,lend=1,col=rgb(1,0,0,.75))
# segments(
#   14-7*1/3,0,14-7*1/3,sum(c(.21,.79) * susc[5:6]),
#   lwd=7,lend=1,col=rgb(0,1,0,.75))
# segments(
#   21-7*1/3,0,21-7*1/3,sum(c(.21,.79) * susc[6:7]),
#   lwd=7,lend=1,col=rgb(0,0,1,.75))
# 
axis(1,at=seq(-35,21,7))
# axis(4,at=seq(0,.3,.3/4),labels=as.character(seq(96000,100000,1000)),las=1)
# mtext('Susceptible people',4,line=3.75)
mtext('Time (days)',1,line=2.5)
# 
# mtext(expression(S["i,-5"]),3,at=-28-7*2/3,cex=.7)
# mtext(expression(S["i,-4"]),3,at=-21-7*2/3,cex=.7)
# mtext(expression(S["i,-3"]),3,at=-14-7*2/3,cex=.7)
# mtext(expression(S["i,-2"]),3,at=-7-7*2/3,cex=.7)
# mtext(expression(S["i,-1"]),3,at=0-7*2/3,cex=.7)
# mtext(expression(S["i,0"]),3,at=7-7*2/3,cex=.7)
# mtext(expression(S["i,1"]),3,at=14-7*2/3,cex=.7)
# mtext(expression(S["i,2"]),3,at=21-7*2/3,cex=.7)
# mtext(expression("S'"["i,0"]),3,at=7-7*1/3,cex=.7,col=rgb(1,0,0,.75))
# mtext(expression("S'"["i,1"]),3,at=14-7*1/3,cex=.7,col=rgb(0,1,0,.75))
# mtext(expression("S'"["i,2"]),3,at=21-7*1/3,cex=.7,col=rgb(0,0,1,.75))

dev.off()
