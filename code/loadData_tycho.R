

# load data from file
data = read.csv('../data/chik_data_tycho.csv')

# replace long country names with GMIs
countries = data$COUNTRY
countries = gsub(' ','.',countries)
for(ii in 1:length(countries)){
  if(countries[ii] %in% as.character(gmi$name)){
    countries[ii] = as.character(gmi$gmi[gmi$name == countries[ii]])
  }
}
countries[countries == 'Guadeloupe'] = 'GLP'
countries[countries == 'Saint.Barth̩lemy'] = 'BLM'
countries[countries == 'Saint-Martin'] = 'MAF'
countries[countries == 'United.States'] = 'USA'
countries[countries == 'Cura̤ao'] = 'CUW'
countries[countries == 'Saint.Vincent.and.the.Grenadines'] = 'VCT'
countries[countries == 'Virgin.Islands,.U.S.'] = 'VIR'
countries[countries == 'British.Virgin.Islands'] = 'VGB'
data$COUNTRY = countries


# allocate matrices to store case data
suspected = confirmed = imported =
  matrix(0,length(unique(paste(data$YEAR,data$WEEK))),2+length(unique(data$COUNTRY)))

# pad single-digit weeks with zeros
data$WEEK = paste('0',data$WEEK,sep='')
data$WEEK = substr(data$WEEK,nchar(data$WEEK)-1,nchar(data$WEEK))

# populate year and week for data matrices
for(ww in 1:length(unique(paste(data$YEAR,data$WEEK)))){
  ww.string = sort(unique(paste(data$YEAR,data$WEEK)))[ww]
  suspected[ww,1] = as.numeric(substr(ww.string,0,4))
  suspected[ww,2] = as.numeric(substr(ww.string,6,7))
  confirmed[ww,1] = as.numeric(substr(ww.string,0,4))
  confirmed[ww,2] = as.numeric(substr(ww.string,6,7))
  imported[ww,1] = as.numeric(substr(ww.string,0,4))
  imported[ww,2] = as.numeric(substr(ww.string,6,7))  
}

# make appropriate column names
colnames(suspected) = rep('',ncol(suspected))
colnames(suspected)[1:2] = c('year', 'week')
colnames(confirmed) = rep('',ncol(confirmed))
colnames(confirmed)[1:2] = c('year', 'week')
colnames(imported) = rep('',ncol(imported))
colnames(imported)[1:2] = c('year', 'week')
colnames(suspected)[2+(1:length(unique(data$COUNTRY)))] = sort(unique(data$COUNTRY))
colnames(confirmed)[2+(1:length(unique(data$COUNTRY)))] = sort(unique(data$COUNTRY))
colnames(imported)[2+(1:length(unique(data$COUNTRY)))] = sort(unique(data$COUNTRY))

# give each matrix a common variable for year-week combination
suspected = cbind(suspected[,'year']*100+suspected[,'week'],suspected)
confirmed = cbind(confirmed[,'year']*100+confirmed[,'week'],confirmed)
imported = cbind(imported[,'year']*100+imported[,'week'],imported)
data = cbind(as.numeric(data$YEAR)*100+as.numeric(data$WEEK),data)

# populate appropriate cells with incidence data
for(ii in 1:nrow(data)){
  suspected[
    suspected[,1]==data[ii,1],
    which(colnames(suspected)==data$COUNTRY[ii])] =
    as.numeric(data$Week.Sus.Cases[ii])
}
suspected = suspected[,-1]
suspected[suspected<0] = 0
suspected.inc = suspected

for(ii in 1:nrow(data)){
  confirmed[
    confirmed[,1]==data[ii,1],
    which(colnames(confirmed)==data$COUNTRY[ii])] =
    as.numeric(data$Week.conf.cases[ii])
}
confirmed = confirmed[,-1]
confirmed[confirmed<0] = 0
confirmed.inc = confirmed

for(ii in 1:nrow(data)){
  imported[
    imported[,1]==data[ii,1],
    which(colnames(imported)==data$COUNTRY[ii])] =
    as.numeric(data$Week.import.cases[ii])
}
imported = imported[,-1]
imported[imported<0] = 0
imported.inc = imported



# tabulate cumulative case numbers
suspected.cum = suspected
for(ii in 2:nrow(suspected)){
  suspected.cum[ii,3:ncol(suspected)] = 
    suspected.cum[ii-1,3:ncol(suspected)] +
    suspected.cum[ii,3:ncol(suspected)]
}

confirmed.cum = confirmed
for(ii in 2:nrow(confirmed)){
  confirmed.cum[ii,3:ncol(confirmed)] = 
    confirmed.cum[ii-1,3:ncol(confirmed)] +
    confirmed.cum[ii,3:ncol(confirmed)]
}

imported.cum = imported
for(ii in 2:nrow(imported)){
  imported.cum[ii,3:ncol(imported)] = 
    imported.cum[ii-1,3:ncol(imported)] +
    imported.cum[ii,3:ncol(imported)]
}



# combine into matrices describing autochthonous and total cases
autoch.inc = suspected.inc
autoch.inc[,3:ncol(autoch.inc)] = autoch.inc[,3:ncol(autoch.inc)] + confirmed.inc[,3:ncol(autoch.inc)]

autoch.cum = suspected.cum
autoch.cum[,3:ncol(autoch.cum)] = autoch.cum[,3:ncol(autoch.cum)] + confirmed.cum[,3:ncol(autoch.cum)]

total.inc = autoch.inc
total.inc[,3:ncol(autoch.inc)] = total.inc[,3:ncol(autoch.inc)] + imported.inc[,3:ncol(autoch.inc)]

total.cum = autoch.cum
total.cum[,3:ncol(autoch.cum)] = total.cum[,3:ncol(autoch.cum)] + imported.cum[,3:ncol(autoch.cum)]
