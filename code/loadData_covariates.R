# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code loads monthly mean, minimum, maximum, and standard deviations
# for temperature and precipition in each country.



# load csv files
covs = read.csv('../data/covariates_monthly.csv')


# matrices for daily covariates
tMN.day = matrix(NA,nrow(covs),ncol=365)
pMN.day = matrix(NA,nrow(covs),ncol=365)

# daily temperature mean
day = 0
tMN.day[,(day+1):(day+31)] = covs$tjanMN / 10; day = 31
tMN.day[,(day+1):(day+28)] = covs$tfebMN / 10; day = day + 28
tMN.day[,(day+1):(day+31)] = covs$tmarMN / 10; day = day + 31
tMN.day[,(day+1):(day+30)] = covs$taprMN / 10; day = day + 30
tMN.day[,(day+1):(day+31)] = covs$tmayMN / 10; day = day + 31
tMN.day[,(day+1):(day+30)] = covs$tjunMN / 10; day = day + 30
tMN.day[,(day+1):(day+31)] = covs$tjulMN / 10; day = day + 31
tMN.day[,(day+1):(day+31)] = covs$taugMN / 10; day = day + 31
tMN.day[,(day+1):(day+30)] = covs$tsepMN / 10; day = day + 30
tMN.day[,(day+1):(day+31)] = covs$toctMN / 10; day = day + 31
tMN.day[,(day+1):(day+30)] = covs$tnovMN / 10; day = day + 30
tMN.day[,(day+1):(day+31)] = covs$tdecMN / 10; day = day + 31

# daily precipitation mean
day = 0
pMN.day[,(day+1):(day+31)] = covs$pjanMN; day = 31
pMN.day[,(day+1):(day+28)] = covs$pfebMN; day = day + 28
pMN.day[,(day+1):(day+31)] = covs$pmarMN; day = day + 31
pMN.day[,(day+1):(day+30)] = covs$paprMN; day = day + 30
pMN.day[,(day+1):(day+31)] = covs$pmayMN; day = day + 31
pMN.day[,(day+1):(day+30)] = covs$pjunMN; day = day + 30
pMN.day[,(day+1):(day+31)] = covs$pjulMN; day = day + 31
pMN.day[,(day+1):(day+31)] = covs$paugMN; day = day + 31
pMN.day[,(day+1):(day+30)] = covs$psepMN; day = day + 30
pMN.day[,(day+1):(day+31)] = covs$poctMN; day = day + 31
pMN.day[,(day+1):(day+30)] = covs$pnovMN; day = day + 30
pMN.day[,(day+1):(day+31)] = covs$pdecMN; day = day + 31

# matrices for weekly covariates
tMN = matrix(NA, nrow(covs), 52)
pMN = matrix(NA, nrow(covs), 52)

# calculate weekly covariates based on daily covariates
day = 0
for(ww in 1:52){
  tMN[,ww] = rowMeans(tMN.day[,(day+1):(day+7)])
  pMN[,ww] = rowMeans(pMN.day[,(day+1):(day+7)])
  day = day + 7
}

# clean up daily covariates
rm(tMN.day,pMN.day)

# set rownames as country GMI
row.names(tMN) = as.character(covs$GMI_CNTRY)
row.names(pMN) = as.character(covs$GMI_CNTRY)
