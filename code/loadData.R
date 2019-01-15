# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code loads all data used in the analysis and forecasting.



# load data on effective population size by country
source('loadData_population.R')

# load and process incidence data
source('loadData_cases.R')

# load weekly covariates
source('loadData_covariates.R')

# load data about latitudes of country capitals
source('loadData_longlat.R')

# compute weights for the serial interval
source('runSerialinterval.R')

# fit the TSIR model
source('fitModel.R')



