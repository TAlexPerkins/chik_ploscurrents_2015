# make figure showing the relationship between predicted and observed incidence
pdf('../output/fitted_lm_best.pdf',width=4.5,height=4.5)
  par(oma=rep(0,4),mar=c(4.5,4.5,.5,.5))
  plot(
    fitted(lm.best) + data.lm$log.pop,
    data.lm$log.I.new + data.lm$log.pop,
    cex = .3,
    xlab = 'ln(predicted cases)',
    ylab = 'ln(observed cases)',
    las = 1, pch = 19)
  text(
    -.1,11,pos=4,
    labels = expression(R^2 == 0.726))
  abline(0,1)
dev.off()



# define function that normalizes across weeks
normalize.weeks = function(m){
  for(cc in 1:ncol(m))
    m[,cc] = (log(m[,cc]) - min(log(m[,cc]))) / (max(log(m[,cc])) - min(log(m[,cc])))
  return(m)
}

# create time lagged temperature and precipitation matrices for whole year
temp.mean.seas = t(
  weights.serialinterval[1] * t(tMN)[c(52,1:51),] +
  weights.serialinterval[2] * t(tMN)[c(51:52,1:50),] +
  weights.serialinterval[3] * t(tMN)[c(50:52,1:49),] +
  weights.serialinterval[4] * t(tMN)[c(49:52,1:48),] +
  weights.serialinterval[5] * t(tMN)[c(48:52,1:47),])
row.names(temp.mean.seas) = row.names(tMN)
precip.mean.seas = t(
  weights.serialinterval[1] * t(pMN)[c(52,1:51),] +
  weights.serialinterval[2] * t(pMN)[c(51:52,1:50),] +
  weights.serialinterval[3] * t(pMN)[c(50:52,1:49),] +
  weights.serialinterval[4] * t(pMN)[c(49:52,1:48),] +
  weights.serialinterval[5] * t(pMN)[c(48:52,1:47),])
row.names(precip.mean.seas) = row.names(pMN)

# matrices that store beta and normalized beta by country and week
beta = matrix(0,nrow(temp.mean.seas),ncol(temp.mean.seas))
row.names(beta) = row.names(temp.mean.seas)
beta.norm = matrix(0,nrow(temp.mean.seas),ncol(temp.mean.seas))
row.names(beta.norm) = row.names(temp.mean.seas)
for(cc in row.names(tMN)){
  beta[cc,] = calc.beta(
    temp.mean.seas[cc,],
    precip.mean.seas[cc,])
  beta.norm[cc,] = beta[cc,] / sum(beta[cc,])
}

# calculate R0 based on beta
R0.seas =
  weights.serialinterval[1] * beta[,c(2:52,1)] +
  weights.serialinterval[2] * beta[,c(3:52,1:2)] +
  weights.serialinterval[3] * beta[,c(4:52,1:3)] +
  weights.serialinterval[4] * beta[,c(5:52,1:4)] +
  weights.serialinterval[5] * beta[,c(6:52,1:5)]
row.names(R0.seas) = row.names(beta)


month.labels = function(cex.in,axis.in){
  mtext('Jan',axis.in,at=(.5+0)/12*52+.5,cex=cex.in)
  mtext('Feb',axis.in,at=(.5+1)/12*52+.5,cex=cex.in)
  mtext('Mar',axis.in,at=(.5+2)/12*52+.5,cex=cex.in)
  mtext('Apr',axis.in,at=(.5+3)/12*52+.5,cex=cex.in)
  mtext('May',axis.in,at=(.5+4)/12*52+.5,cex=cex.in)
  mtext('Jun',axis.in,at=(.5+5)/12*52+.5,cex=cex.in)
  mtext('Jul',axis.in,at=(.5+6)/12*52+.5,cex=cex.in)
  mtext('Aug',axis.in,at=(.5+7)/12*52+.5,cex=cex.in)
  mtext('Sep',axis.in,at=(.5+8)/12*52+.5,cex=cex.in)
  mtext('Oct',axis.in,at=(.5+9)/12*52+.5,cex=cex.in)
  mtext('Nov',axis.in,at=(.5+10)/12*52+.5,cex=cex.in)
  mtext('Dec',axis.in,at=(.5+11)/12*52+.5,cex=cex.in)  
}



# # make figure showing seasonal patterns of beta by country by latitude by week
# pdf('../output/beta_seasonality.pdf',width=6.5,height=6.5)
#   par(oma=rep(0,4),mar=c(3,3,1,1))
#   image(
#     x =1:ncol(beta.norm),
#     y = 1:nrow(beta.norm),
#     z = normalize.weeks(t(beta[order(longlats[,2]),])),
#     xlab = '', ylab = '', xaxt = 'n', yaxt = 'n')
#   for(mm in 1:12){
#     abline(v = mm / 12 * 52 + .5)
#   }
#   month.labels(cex.in=.6,axis.in=3)
#   axis(1,at=seq(2,52,2),las=1,cex.axis=.6)
#   mtext('week',1,cex=.6,line=2)
#   axis(2,at=1:nrow(longlats),labels=row.names(longlats)[order(longlats[,2])],cex.axis=.6,las=1,lwd=.5)
# dev.off()



# make figure showing seasonal patterns of beta by country by latitude by week
if(!require(fields)){install.packages('fields');library(fields)}
pdf('../output/beta_seasonality_abs.pdf',width=6.5,height=6.5)
par(oma=rep(0,4),mar=c(3,2.2,1,.5))
image.plot(
  x =1:ncol(beta.norm),
  y = 1:nrow(beta.norm),
  z = t(R0.seas[order(longlats[,2]),]),
  xlab = '', ylab = '', xaxt = 'n', yaxt = 'n',
  lwd = 0, legend.mar = 4, legend.shrink = 1, legend.width = 1)
for(mm in 1:12){
  abline(v = mm / 12 * 52 + .5)
}
month.labels(cex.in=.6,axis.in=3)
axis(1,at=seq(2,52,2),las=1,cex.axis=.6)
mtext('week',1,cex=.6,line=2)
mtext(expression(R[0]),4,las=1,line=2.75)
axis(2,at=1:nrow(longlats),labels=row.names(longlats)[order(longlats[,2])],cex.axis=.6,las=1,lwd=.5)
dev.off()



# # map of average beta by country
# if(!require(maps)){install.packages('maps');library(maps)}
# if(!require(mapdata)){install.packages('mapdata');library(mapdata)}
# plot.west.hemi = function(cols){
#   map('worldHires','Aruba',fill=T,col=rgb(betas['ABW'],0,1-betas['ABW'],.8),,xlim=c(-170,-35),ylim=c(-60,90))
#   map('worldHires','Anguilla',fill=T,col=rgb(betas['AIA'],0,1-betas['AIA'],.8),add=T)
#   map('worldHires','Argentina',fill=T,col=rgb(betas['ARG'],0,1-betas['ARG'],.8),add=T)
#   map('worldHires','Antigua',fill=T,col=rgb(betas['ATG'],0,1-betas['ATG'],.8),add=T)
#   map('worldHires','Bahamas',fill=T,col=rgb(betas['BHS'],0,1-betas['BHS'],.8),add=T)
#   map('worldHires','Saint-Barthelemy',fill=T,col=rgb(betas['BLM'],0,1-betas['BLM'],.8),add=T)
#   map('worldHires','Belize',fill=T,col=rgb(betas['BLZ'],0,1-betas['BLZ'],.8),add=T)
#   map('worldHires','Bolivia',fill=T,col=rgb(betas['BOL'],0,1-betas['BOL'],.8),add=T)
#   map('worldHires','Brazil',fill=T,col=rgb(betas['BRA'],0,1-betas['BRA'],.8),add=T)
#   map('worldHires','Barbados',fill=T,col=rgb(betas['BRB'],0,1-betas['BRB'],.8),add=T)
#   map('worldHires','Canada',fill=T,col=rgb(betas['CAN'],0,1-betas['CAN'],.8),add=T)
#   map('worldHires','Chile',fill=T,col=rgb(betas['CHL'],0,1-betas['CHL'],.8),add=T)
#   map('worldHires','Colombia',fill=T,col=rgb(betas['COL'],0,1-betas['COL'],.8),add=T)
#   map('worldHires','Costa Rica',fill=T,col=rgb(betas['CRI'],0,1-betas['CRI'],.8),add=T)
#   map('worldHires','Cuba',fill=T,col=rgb(betas['CUB'],0,1-betas['CUB'],.8),add=T)
#   map('worldHires','Curacao',fill=T,col=rgb(betas['CUW'],0,1-betas['CUW'],.8),add=T)
#   map('worldHires','Cayman Islands',fill=T,col=rgb(betas['CYM'],0,1-betas['CYM'],.8),add=T)
#   map('worldHires','Costa Rica',fill=T,col=rgb(betas['CRI'],0,1-betas['CRI'],.8),add=T)
#   map('worldHires','Dominican Republic',fill=T,col=rgb(betas['DOM'],0,1-betas['DOM'],.8),add=T)
#   map('worldHires','Ecuador',fill=T,col=rgb(betas['ECU'],0,1-betas['ECU'],.8),add=T)
#   map('worldHires','Guadeloupe',fill=T,col=rgb(betas['GLP'],0,1-betas['GLP'],.8),add=T)
#   map('worldHires','Grenada',fill=T,col=rgb(betas['GRD'],0,1-betas['GRD'],.8),add=T)
#   map('worldHires','Guatemala',fill=T,col=rgb(betas['GTM'],0,1-betas['GTM'],.8),add=T)
#   map('worldHires','French Guiana',fill=T,col=rgb(betas['GUF'],0,1-betas['GUF'],.8),add=T)
#   map('worldHires','Guyana',fill=T,col=rgb(betas['GUY'],0,1-betas['GUY'],.8),add=T)
#   map('worldHires','Honduras',fill=T,col=rgb(betas['HND'],0,1-betas['HND'],.8),add=T)
#   map('worldHires','Haiti',fill=T,col=rgb(betas['HTI'],0,1-betas['HTI'],.8),add=T)
#   map('worldHires','Jamaica',fill=T,col=rgb(betas['JAM'],0,1-betas['JAM'],.8),add=T)
#   map('worldHires','Saint Lucia',fill=T,col=rgb(betas['LCA'],0,1-betas['LCA'],.8),add=T)
#   map('worldHires','Mexico',fill=T,col=rgb(betas['MEX'],0,1-betas['MEX'],.8),add=T)
#   map('worldHires','Montserrat',fill=T,col=rgb(betas['MSR'],0,1-betas['MSR'],.8),add=T)
#   map('worldHires','Martinique',fill=T,col=rgb(betas['MTQ'],0,1-betas['MTQ'],.8),add=T)
#   map('worldHires','Nicaragua',fill=T,col=rgb(betas['NIC'],0,1-betas['NIC'],.8),add=T)
#   map('worldHires','Panama',fill=T,col=rgb(betas['PAN'],0,1-betas['PAN'],.8),add=T)
#   map('worldHires','Peru',fill=T,col=rgb(betas['PER'],0,1-betas['PER'],.8),add=T)
#   map('worldHires','Puerto Rico',fill=T,col=rgb(betas['PRI'],0,1-betas['PRI'],.8),add=T)
#   map('worldHires','Paraguay',fill=T,col=rgb(betas['PRY'],0,1-betas['PRY'],.8),add=T)
#   map('worldHires','El Salvador',fill=T,col=rgb(betas['SLV'],0,1-betas['SLV'],.8),add=T)
#   map('worldHires','Suriname',fill=T,col=rgb(betas['SUR'],0,1-betas['SUR'],.8),add=T)
#   map('worldHires','Turks and Caicos',fill=T,col=rgb(betas['TCA'],0,1-betas['TCA'],.8),add=T)
#   map('worldHires','Trinidad',fill=T,col=rgb(betas['TTO'],0,1-betas['TTO'],.8),add=T)
#   map('worldHires','USA',fill=T,col=rgb(betas['USA'],0,1-betas['USA'],.8),add=T)
#   map('worldHires','Costa Rica',fill=T,col=rgb(betas['CRI'],0,1-betas['CRI'],.8),add=T)
#   map('worldHires','Saint Vincent',fill=T,col=rgb(betas['VCT'],0,1-betas['VCT'],.8),add=T)
#   map('worldHires','Venezuela',fill=T,col=rgb(betas['VEN'],0,1-betas['VEN'],.8),add=T)
#   map('worldHires','Virgin Islands',fill=T,col=rgb(betas['VIR'],0,1-betas['VIR'],.8),add=T)
#   map('worldHires','Uruguay',fill=T,col=rgb(betas['URY'],0,1-betas['URY'],.8),add=T)
# }
# 
# betas = (rowMeans(beta) - min(rowMeans(beta))) / max(rowMeans(beta))
# betas = order(order(rowMeans(beta))) / nrow(beta)
# names(betas) = row.names(beta)
# 
# pdf('../output/westernhemi_betarank.pdf',width=6.5,height=8)
#   plot.west.hemi(betas)
# dev.off()



# image of temp and precip on beta
jpeg('../output/beta_temp_precip_hires.jpeg',width=6.5,height=5.75,units='in',res=1000)
# pdf('../output/beta_temp_precip.pdf',width=6.5,height=5.75)
par(mar=c(3.5,3.5,1,.5),oma=rep(0,4))
image.plot(
  x=seq(0,450,length.out=200),
  y=seq(15,35,length.out=200),
  z=matrix(
    calc.beta(
      rep(seq(15,35,length.out=200),each=200),
      rep(seq(0,450,length.out=200),times=200)),200,200),
  xlab='',
  ylab='',
  las=1,
  lwd = 0, legend.mar = 4, legend.shrink = 1, legend.width = .5)
points(data.lm$precip.mean,data.lm$temp.mean,cex=.15)
points(data.lm$precip.mean,data.lm$temp.mean,pch=19,cex=.1,col='white')
mtext('Precipitation (mm per month)',1,line=2.5)
mtext('Temperature (degrees C)',2,line=2.5)
mtext(expression(beta),4,las=1,line=2.7)
dev.off()




# plot of mean R0 and range over the year by country by latitude of capital
max.R0 = sapply(1:nrow(R0.seas),function(cc)max(beta[cc,]))
min.R0 = sapply(1:nrow(R0.seas),function(cc)min(beta[cc,]))
mean.R0 = rowMeans(R0.seas)

pdf('../output/beta_country.pdf',width=6.5,height=3.25)
  par(oma=rep(0,4),mar=c(2.5,3.5,1,1))
  plot(mean.R0[order(longlats[,2])],ylim=c(0,8.5),pch=19,xaxt='n',xlab='',ylab='',las=1)
  segments(
    1:length(min.R0),min.R0[order(longlats[,2])],
    1:length(max.R0),max.R0[order(longlats[,2])])
  abline(h=1,lty=2)
  axis(1,at=1:nrow(longlats),labels=row.names(longlats)[order(longlats[,2])],cex.axis=.6,las=2,lwd=.5)
  mtext(expression(R[0]),2,las=1,line=2)
dev.off()








pdf('../output/partial_residuals.pdf',width=6.5,height=6.5)

layout(matrix(1:4,2,2))
par(mar = c(4.5,5,1,.5))

plot(
  data.lm$temp.mean,
  data.lm$log.I.new -
    data.lm$log.S.old +
    data.lm$log.pop -
    coef(lm.best)['(Intercept)'] -
    coef(lm.best)['precip.mean'] * data.lm$precip.mean -
    coef(lm.best)['precip.mean.2'] * data.lm$precip.mean ^ 2 -
    coef(lm.best)['log.I.old.num'] * data.lm$log.I.old.num,
  xlab = 'Temperature (degrees C)',
  ylab = 'Partial residuals',
  pch = 19, cex = .3, las = 1)
lines(
  seq(0,35,.0001),
  coef(lm.best)['temp.mean'] * seq(0,35,.0001) +
    coef(lm.best)['temp.mean.2'] * seq(0,35,.0001) ^ 2,
  lwd = 2,
  col=2)

plot(
  data.lm$log.I.old.num,
  data.lm$log.I.new -
    data.lm$log.S.old +
    data.lm$log.pop -
    coef(lm.best)['(Intercept)'] -
    coef(lm.best)['temp.mean'] * data.lm$temp.mean -
    coef(lm.best)['temp.mean.2'] * data.lm$temp.mean ^ 2 -
    coef(lm.best)['precip.mean'] * data.lm$precip.mean -
    coef(lm.best)['precip.mean.2'] * data.lm$precip.mean ^ 2,
  xlab = expression(ln("I'"["i,t"])),
  ylab = 'Partial residuals',
  pch = 19, cex = .3, las = 1)
lines(
  seq(-2,15,.0001),
  coef(lm.best)['log.I.old.num'] * seq(-2,15,.0001),
  lwd = 2,
  col=2)

plot(
  data.lm$precip.mean,
  data.lm$log.I.new -
    data.lm$log.S.old +
    data.lm$log.pop -
    coef(lm.best)['(Intercept)'] -
    coef(lm.best)['temp.mean'] * data.lm$temp.mean -
    coef(lm.best)['temp.mean.2'] * data.lm$temp.mean ^ 2 -
    coef(lm.best)['log.I.old.num'] * data.lm$log.I.old.num,
  xlab = 'Precipitation (mm per month)',
  ylab = 'Partial residuals',
  pch = 19, cex = .3, las = 1)
lines(
  seq(0,500,.001),
  coef(lm.best)['precip.mean'] * seq(0,500,.001) +
    coef(lm.best)['precip.mean.2'] * seq(0,500,.001) ^ 2,
  lwd = 2,
  col=2)

hist(
  data.lm$log.I.new -
    data.lm$log.S.old +
    data.lm$log.pop -
    coef(lm.best)['temp.mean'] * data.lm$temp.mean -
    coef(lm.best)['temp.mean.2'] * data.lm$temp.mean ^ 2 -
    coef(lm.best)['precip.mean'] * data.lm$precip.mean -
    coef(lm.best)['precip.mean.2'] * data.lm$precip.mean ^ 2 -
    coef(lm.best)['log.I.old.num'] * data.lm$log.I.old.num,
  50,col=1,main='',xlab='Partial residuals', las = 1)
abline(v=coef(lm.best)['(Intercept)'],lwd=2,col=2)

dev.off()




# layout(matrix(1:4,2,2))
# 
# plot(
#   log.I.old.vec,
#   resid(lm.best) +
#   coef(lm.best)['log.I.old'] * log.I.old.vec,
#   xlab = expression(log(I[old])),
#   ylab = '')
# abline(coef=c(coef(lm.best)['(Intercept)'],coef(lm.best)['log.I.old']))
# 
# plot(
#   temp.mean.vec,
#   log.I.new.vec -
#     log.S.old.vec -
#     coef(lm.best)['log.I.old'] * log.I.old.vec -
#     coef(lm.best)['temp.mean.2'] * temp.mean.vec ^ 2 -
#     coef(lm.best)['precip.mean.2'] * precip.mean.vec ^ 2,
#   xlab = 'mean temp',
#   ylab = '')
# abline(coef=c(coef(lm.best)['(Intercept)'],coef(lm.best)['temp.mean']))
# 
# plot(
#   temp.mean.vec ^ 2,
#   log.I.new.vec -
#     log.S.old.vec -
#     coef(lm.best)['temp.mean'] * temp.mean.vec -
#     coef(lm.best)['log.I.old'] * log.I.old.vec -
#     coef(lm.best)['precip.mean.2'] * precip.mean.vec ^ 2,
#   xlab = 'mean temp ^ 2',
#   ylab = '')
# abline(coef=c(coef(lm.best)['(Intercept)'],coef(lm.best)['temp.mean.2']))
# 
# plot(
#   precip.mean.vec ^ 2,
#   log.I.new.vec -
#     log.S.old.vec -
#     coef(lm.best)['temp.mean'] * temp.mean.vec -
#     coef(lm.best)['temp.mean.2'] * temp.mean.vec ^ 2 -
#     coef(lm.best)['log.I.old'] * log.I.old.vec,
#   xlab = 'precip mean ^ 2',
#   ylab = '')
# abline(coef=c(coef(lm.best)['(Intercept)'],coef(lm.best)['precip.mean.2']))



# > plot(log.I.old+log(pop),log.I.new+log(pop),xlab='ln(I_t)',ylab='ln(I_t+1)')
# > points(x[x>0],y[x>0],pch=19)
# > abline(lm(y[!is.infinite(y+x)]~x[!is.infinite(y+x)]),col=3,lwd=3)
# > abline(lm(y[!is.infinite(y+x)&x>0]~x[!is.infinite(y+x)&x>0]),col=2,lwd=3)
# > abline(v=0)


