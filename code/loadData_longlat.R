# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code loads latitudes and longitudes of country capitals.



library(maps)
data(world.cities)
source('function_gcd.R')


getlonglat = function(country){
  c(
    mean(world.cities$long[world.cities$country.etc == country]),
    mean(world.cities$lat[world.cities$country.etc == country]))
}


longlats = matrix(0,nrow(gmi),2)
for(cc in 1:nrow(gmi)){
  longlats[cc,] = getlonglat(gmi$name[cc])
}
rownames(longlats) = gmi$name

longlats['Costa.Rica',] = getlonglat('Costa Rica')
longlats['El.Salvador',] = getlonglat('El Salvador')
longlats['Dominican.Republic',] = getlonglat('Dominican Republic')
longlats['French.Guiana',] = getlonglat('French Guiana')
longlats['Guadaloupe',] = getlonglat('Guadeloupe')
longlats['Puerto.Rico',] = getlonglat('Puerto Rico')
longlats['Saint.Barthelemy',] = getlonglat('Saint-Barthelemy')
longlats['Saint.Martin',] = getlonglat('Saint-Martin')
longlats['Antigua.and.Barbuda',] = getlonglat('Antigua and Barbuda')
longlats['Cayman.Islands',] = getlonglat('Cayman Islands')
longlats['Curacao',] = c(-69,12.1833)
longlats['Saint.Kitts.and.Nevis',] = getlonglat('Saint Kitts and Nevis')
longlats['Saint.Lucia',] = getlonglat('Saint Lucia')
longlats['Saint.Vincent',] = getlonglat('Saint Vincent and The Grenadines')
longlats['Sint.Maarten',] = c(-63.0500,18.0667)
longlats['Trinidad.and.Tobago',] = getlonglat('Trinidad and Tobago')
longlats['Turks.and.Caicos.Islands',] = getlonglat('Turks and Caicos')
longlats['Virgin.Islands.UK',] = getlonglat('British Virgin Islands')
longlats['Virgin.Islands.US',] = getlonglat('US Virgin Islands')

row.names(longlats) = gmi$gmi

longlats = rbind(
  longlats,
  getlonglat('Falkland Islands'),
  getlonglat('Netherlands Antilles'),
  getlonglat('Uruguay'))

row.names(longlats)[(-2:0)+nrow(longlats)] = c('FLK','ANT','URY')

longlats = longlats[order(row.names(longlats)),]