# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code fits a spatial TSIR model to the data.



# reorder pops so that they're consistent with total.cum
pops = pops.all[
  sapply(colnames(total.cum)[3:ncol(total.cum)],function(nn)
    which(pops.all$gmi == nn)),]

# shift each variable in time to match each other
S =
  (t(pops$pop * matrix(1,ncol(total.cum)-2,nrow(total.cum))) - total.cum[,3:ncol(total.cum)]) /
  t(pops$pop * matrix(1,ncol(total.cum)-2,nrow(total.cum)))
I.old =
  total.inc[,3:ncol(total.inc)] /
  t(pops$pop * matrix(1,ncol(total.cum)-2,nrow(total.cum)))
colnames(I.old) = colnames(S)
I.new =
  autoch.inc[,3:ncol(autoch.inc)] /
  t(pops$pop * matrix(1,ncol(total.cum)-2,nrow(total.cum)))
colnames(I.new) = colnames(S)

# standardize countries and their order across all covariates and variables
pops$gmi = as.character(pops$gmi)
pops = rbind(pops, c('FLK', 2840), c('ANT', 191572), c('URY', 3332972))
pops = pops[order(pops$gmi),]; row.names(pops) = 1:nrow(pops)

S = cbind(S,t(matrix(1,3,nrow(S))))
colnames(S) = c(as.character(head(colnames(S),-3)),c('FLK','ANT','URY'))
S = S[,order(colnames(S))]

I.old = cbind(I.old,t(matrix(0,3,nrow(I.old))))
colnames(I.old) = c(as.character(head(colnames(I.old),-3)),c('FLK','ANT','URY'))
I.old = I.old[,order(colnames(I.old))]

I.new = cbind(I.new,t(matrix(0,3,nrow(I.new))))
colnames(I.new) = c(as.character(head(colnames(I.new),-3)),c('FLK','ANT','URY'))
I.new = I.new[,order(colnames(I.new))]

tMN = tMN[order(row.names(tMN)),]
pMN = pMN[order(row.names(pMN)),]


# assemble variables to match by week
week = head(total.inc[,'week'],-5) * matrix(1,nrow(total.cum)-5,ncol(total.cum)-2+3)
colnames(week) = pops$gmi

pop = matrix(as.numeric(rep(pops$pop,each=nrow(week))),nrow(week),nrow(pops))
colnames(pop) = pops$gmi

latitude = t(matrix(rep(longlats[,2],nrow(week)),ncol(week),nrow(week)))
colnames(latitude) = row.names(longlats)

log.I.new = log(I.new[6:nrow(I.new),])
colnames(log.I.new) = colnames(I.new)

log.I.old = log(
  weights.serialinterval[1] * I.old[5:(nrow(I.old)-1),] +
  weights.serialinterval[2] * I.old[4:(nrow(I.old)-2),] +  
  weights.serialinterval[3] * I.old[3:(nrow(I.old)-3),] +
  weights.serialinterval[4] * I.old[2:(nrow(I.old)-4),] +
  weights.serialinterval[5] * I.old[1:(nrow(I.old)-5),])
colnames(log.I.old) = colnames(S)

log.S.old = log(
  S[5:(nrow(S)-1),])
colnames(log.S.old) = colnames(S)

temp.mean =
  .2 * t(tMN)[total.inc[5:(nrow(total.inc)-1),'week'],] +
  .2 * t(tMN)[total.inc[4:(nrow(total.inc)-2),'week'],] +
  .2 * t(tMN)[total.inc[3:(nrow(total.inc)-3),'week'],] +
  .2 * t(tMN)[total.inc[2:(nrow(total.inc)-4),'week'],] +
  .2 * t(tMN)[total.inc[1:(nrow(total.inc)-5),'week'],]
colnames(temp.mean) = colnames(tMN)

precip.mean =
  .2 * t(pMN)[total.inc[5:(nrow(total.inc)-1),'week'],] +
  .2 * t(pMN)[total.inc[4:(nrow(total.inc)-2),'week'],] +
  .2 * t(pMN)[total.inc[3:(nrow(total.inc)-3),'week'],] +
  .2 * t(pMN)[total.inc[2:(nrow(total.inc)-4),'week'],] +
  .2 * t(pMN)[total.inc[1:(nrow(total.inc)-5),'week'],]
colnames(precip.mean) = colnames(pMN)



# remove zeros because they contain no information
to.remove =
  c(log.I.new) == -Inf |
   c(log.I.old) == -Inf |
  c(log.I.old + log(pop)) < 0 |
  c(log.I.new + log(pop)) < 0
week.vec = c(week)[!to.remove]
pop.vec = c(pop)[!to.remove]
latitude.vec = c(latitude)[!to.remove]
log.I.new.vec = c(log.I.new)[!to.remove]
log.I.old.vec = c(log.I.old)[!to.remove]
log.S.old.vec = c(log.S.old)[!to.remove]
temp.mean.vec = c(temp.mean)[!to.remove]
precip.mean.vec = c(precip.mean)[!to.remove]

# data frame to feed to fitting function
data.lm = data.frame(
  week = week.vec,
  log.pop = log(pop.vec),
  latitude = latitude.vec,
  log.I.new = log.I.new.vec,
  log.I.old.prop = log.I.old.vec,
  log.I.old.num = log.I.old.vec + log(pop.vec),
  log.S.old = log.S.old.vec,
  temp.mean = temp.mean.vec,
  precip.mean = precip.mean.vec,
  temp.mean.2 = temp.mean.vec ^ 2,
  precip.mean.2 = precip.mean.vec ^ 2,
  temp.precip = temp.mean.vec * precip.mean.vec)

lm.simple.prop = lm(
  log.I.new ~
    log.I.old.prop,
  offset = log.S.old,
  data = data.lm)

lm.simple.num = lm(
  log.I.new ~
    log.I.old.num,
  offset = log.S.old - log.pop,
  data = data.lm)

lm.full.prop = lm(
  log.I.new ~
    temp.mean +
    precip.mean +
    temp.mean.2 +
    precip.mean.2 +
    temp.precip +
    log.I.old.prop,
  offset = log.S.old,
  data = data.lm)

lm.full.num = lm(
  log.I.new ~
    temp.mean +
    precip.mean +
    temp.mean.2 +
    precip.mean.2 +
#     temp.precip +
    log.I.old.num,
  offset = log.S.old - log.pop,
  data = data.lm)

if(!require(MASS)){install.packages('MASS');library(MASS)}
lm.best.prop = stepAIC(lm.full.prop,trace=1,direction='both')
lm.best.num = stepAIC(lm.full.num,trace=1,direction='both')

lm.best = lm.best.num



# define function to compute beta as a function of input covariates
params = c(
  '(Intercept)' = 0,
  'temp.mean' = 0,
  'precip.mean' = 0,
  'temp.mean.2' = 0,
  'precip.mean.2' = 0,
#   'temp.precip' = 0,
  'log.I.old' = 0)
for(pp in names(coef(lm.best)))
  params[pp] = coef(lm.best)[pp]
calc.beta = function(tmn,pmn){
  exp(
    params['(Intercept)'] +
      params['temp.mean'] * tmn +
      params['precip.mean'] * pmn +
      params['temp.mean.2'] * tmn ^ 2 +
      params['precip.mean.2'] * pmn ^ 2) # +
#       params['temp.precip'] * tmn * pmn)
}
# predict.beta = function(){
#   exp(matrix(
#     predict(
#       lm.best,
#       list(
#         temp.mean=as.vector(tMN),
#         precip.mean=as.vector(pMN),
#         log.I.old=rep(-10,prod(dim(tMN))))),nrow(tMN),ncol(tMN)))
# }
# beta = predict.beta()
