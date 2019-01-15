# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code loads population sizes by country for areas where Aedes aegypti
# is predicted to be present. These numbers are therefore a subset of each
# country's total population.



# load csv files
pops.eff = read.csv('../data/effective_pop_sizes.csv',header=T)
pops.all = read.csv('../data/total_pop_sizes.csv',header=F)
gmi = read.csv('../data/country_codes.csv',header=F)

# reduce population size data set to essentials
pops.eff = pops.eff[,c('GMI_CNTRY','SUM')]
colnames(pops.eff) = c('gmi','pop')
colnames(pops.all) = c('gmi','pop')

# reduce effective population size data set to the countries of interest
pops.eff = pops.eff[which(pops.eff$gmi %in% pops.all$gmi),]

# make country names consistent across data sets
colnames(gmi) = c('name','gmi')
gmi$name = gsub(' ','.',gmi$name)
gmi = gmi[1:50,]
