# (c) 2014 Alex Perkins, Andy Tatem, Jessica Metcalf
# taperkins@nd.edu, A.J.Tatem@soton.ac.uk, cmetcalf@princeton.edu
# CRAPL v0.1 license - http://matt.might.net/articles/crapl/
# See README.md in git repository for more info.
#
# This code loads monthly mean, minimum, maximum, and standard deviations
# for temperature and precipition in each country.



# load csv files
suspected = read.csv('../data/CHIKV_incidence_suspected.csv')
confirmed = read.csv('../data/CHIKV_incidence_confirmed.csv')
imported = read.csv('../data/CHIKV_incidence_imported.csv')


# remove NAs by assuming = 0
suspected[is.na(suspected)] = 0
confirmed[is.na(confirmed)] = 0
imported[is.na(imported)] = 0


# make sure that numbers are cumulative...
# if numbers go down, go back to last number and new one to it
# assuming that the new one is a point estimate rather than cumulative
for(jj in 3:ncol(suspected)){
  cum = 0
  for(ii in 1:nrow(suspected)){
    if(suspected[ii,jj] > cum){
      cum = suspected[ii,jj]
    }else if(suspected[ii,jj] < cum){
      suspected[ii,jj] = cum + suspected[ii,jj]
      cum = suspected[ii,jj]
    }
  }
}

for(jj in 3:ncol(confirmed)){
  cum = 0
  for(ii in 1:nrow(confirmed)){
    if(confirmed[ii,jj] > cum){
      cum = confirmed[ii,jj]
    }else if(confirmed[ii,jj] < cum){
      confirmed[ii,jj] = cum + confirmed[ii,jj]
      cum = confirmed[ii,jj]
    }
  }
}

for(jj in 3:ncol(imported)){
  cum = 0
  for(ii in 1:nrow(imported)){
    if(imported[ii,jj] > cum){
      cum = imported[ii,jj]
    }else if(imported[ii,jj] < cum){
      imported[ii,jj] = cum + imported[ii,jj]
      cum = imported[ii,jj]
    }
  }
}


# assign cumulative cases to separate variable
suspected.cum = as.matrix(suspected)
confirmed.cum = as.matrix(confirmed)
imported.cum = as.matrix(imported)
autoch.cum = suspected.cum + confirmed.cum
total.cum = suspected.cum + confirmed.cum + imported.cum


# take differences to get weekly incidence
suspected.inc = matrix(0,nrow(suspected.cum),ncol(suspected.cum))
confirmed.inc = matrix(0,nrow(confirmed.cum),ncol(confirmed.cum))
imported.inc = matrix(0,nrow(imported.cum),ncol(imported.cum))

suspected.inc[,1:2] = suspected.cum[,1:2]
confirmed.inc[,1:2] = confirmed.cum[,1:2]
imported.inc[,1:2] = imported.cum[,1:2]

suspected.inc[1,] = suspected.cum[1,]
confirmed.inc[1,] = confirmed.cum[1,]
imported.inc[1,] = imported.cum[1,]

for(jj in 3:ncol(suspected.cum)){
  suspected.inc[2:nrow(suspected.cum),jj] = diff(suspected.cum[,jj]) 
}

for(jj in 3:ncol(confirmed.cum)){
  confirmed.inc[2:nrow(confirmed.cum),jj] = diff(confirmed.cum[,jj]) 
}

for(jj in 3:ncol(imported.cum)){
  imported.inc[2:nrow(imported.cum),jj] = diff(imported.cum[,jj]) 
}


# combine suspected and confirmed into one variable for autochthonous incidence
autoch.inc = suspected.inc
autoch.inc[,-c(1,2)] = autoch.inc[,-c(1,2)] + confirmed.inc[,-c(1,2)]


# combine suspected, confirmed, and imported all into one variable for total incidence
total.inc = suspected.inc
total.inc[,-c(1,2)] = total.inc[,-c(1,2)] + confirmed.inc[,-c(1,2)] + imported.inc[,-c(1,2)]


# make country codes names of countries
colnames(autoch.cum) = 
  c('year','week',
    as.character(gmi$gmi[as.numeric(unlist(sapply(colnames(autoch.cum)[3:52],function(cc)which(gmi$name==cc))))]))
colnames(autoch.inc) = colnames(autoch.cum)
colnames(total.cum) = colnames(autoch.cum)
colnames(total.inc) = colnames(autoch.cum)
  