#install.packages("rvest")
library(rvest)
#URL of the HTML webpage we want to scrape
url="http://www.fia.com/events/formula-1-world-championship/season-2015/qualifying-classification"

pageGrabber=function(url,extender=FALSE,path='http://www.fia.com'){
  if (extender) url=paste(path,url,sep='')
  #Grab the page
  html(url)
}

#USAGE:
#mypage=pageGrabber(url)

#Listings page
#http://www.fia.com/events/fia-formula-1-world-championship/season-2015/formula-one

fiaListings=function(url='http://www.fia.com/events/fia-formula-1-world-championship/season-2015/formula-one'){
  html_page=pageGrabber(url)
  #Parse HTML
  html_page %>% html_nodes(xpath="//div[contains(@class, 'event') and contains(@class, 'cell')]/a") %>% html_attr("href")
}

fiaStubExtender=function(stub,path='http://www.fia.com'){
  paste(path,stub,sep='')
}

fiaClassificationListing=function(stub){
  html_page=pageGrabber(fiaStubExtender(stub))
  #Parse HTML //*[text()[contains(.,'Session Classifications')]]
  html_page %>% html_nodes(xpath="//a[text()[contains(.,'Session Classifications')]]") %>% html_attr("href")
}

fiaTableGrabber=function(html_page,num){
  #Parse HTML
  cc=html_nodes(html_page, xpath = "//table")[[num]] %>% html_table(fill=TRUE)
  #TO DO - extract table name

  #Set the column names
  colnames(cc) = cc[1, ]
  #Drop all NA column, omitting the first ("header" row)
  cc=Filter(function(x)!all(is.na(x)), cc[-1,])
  #Fill blanks with NA
  cc=apply(cc, 2, function(x) gsub("^$|^ $", NA, x))
  #would the dataframe cast handle the NA?
  as.data.frame(cc)
}

fiaTableCombiner=function(stubs,tablenum){
  df=data.frame()
  cnt=1
  for (stub in stubs) {
    classTable=fiaClassificationListing(stub)
    #GP
    ss=stringr:::str_split(stub,'/')[[1]]
    gp=str_replace(str_replace(ss[length(ss)],'grand-prix',''),"^-+|-+$",'')
    #year
    year=str_replace(ss[length(ss)-1],'season-','')

    df.tmp = fiaTableGrabber(pageGrabber(classTable,TRUE),tablenum)
    df.tmp['year']=year
    df.tmp['gp']=gp
    df.tmp['cnt']=cnt
    cnt=cnt+1

    df=rbind(df,df.tmp)
  }
  df
}

#links=fiaListings()[1:5]


dropNAcol=function(df){
  #Drop all NA column
  Filter(function(x)!all(is.na(x)), df)
}

library(stringr)

#Parse out the time
timeInS=function(t,basetime=0){
  if (is.na(t) | t=='') return(NA)
  if (!(stringr:::str_detect(t, ':'))) return(t)
  if (suppressWarnings(!is.numeric(basetime)))
    basetime=timeInS(basetime)
  s=1
  cnt=0
  for(n in rev(stringr:::str_split(t,':')[[1]])) {
    n=as.numeric(n)
    cnt=cnt + s*n
    s=s*60
  }
  cnt - basetime
}

#Session classifications
#1, 2, 3
fiaSessionClassPracticeTidy=function(xx){
  xx['laptime']=apply(xx['TIME'],1,timeInS)
  xx
}

# 4
fiaSessionClassQualifyingTidy=function(xx){
  fiaQualiClassTidy(xx)
}

# 5
fiaSessionClassGridTidy=function(xx){
  xx['laptime']=apply(xx['TIME'],1,timeInS)
  xx
}

# 6
fiaSessionClassRaceTidy=function(xx){
  xx['laptime']=apply(xx['TIME'],1,timeInS)
  xx
}


##TO DO ergastify
#QUlifying classification
#POS               DRIVER       Q1 Q1_laps       Q2 Q2_laps       Q3 Q3_laps q1time q2time q3time
#quali_progression_ergast_tx(fiaSessionClassQualifyingTidy(fiaTableGrabber(xx,1)))
#The following `from` values were not present in `x`: Q1_rank, Q2_rank, Q3_rank, position, code

driverCodes2=c("Lewis Hamilton"= "HAM", "Sebastian Vettel"= "VET", "Nico Rosberg"= "ROS", "Daniel Ricciardo"= "RIC",
              "Daniil Kvyat"= "KVY", "Max Verstappen"= "VES", "Felipe Massa" = "MAS", "Romain Grosjean"= "GRO",
              "Valtteri Bottas"= "BOT", "Marcus Ericsson"= "ERI", "Kimi Raikkonen"= "RAI", "Pastor Maldonado" = "MAL",
              "Nico Hulkenberg"= "HUL", "Sergio Perez"= "PER", "Carlos Sainz Jr."= "SAI", "Felipe Nasr"= "NAS",
              "Jenson Button" = "BUT", "Fernando Alonso"= "ALO", "Roberto Merhi Muntan"= "MER",
              "Will Stevens"="STE",'Stoffel Vandoorne'='VAN','Lance Stroll'='STR','Kevin Magnussen'='MAG',
              "Antonio Giovinazzi"='GIO','Esteban Ocon'='OCO','Jolyon Palmer'='PAL')
driverCode2=function(name) unname(driverCodes2[name])
ergastifyQualiClass=function(df){
  df['position']=df['POS']
  df['Q1_rank']=rank(df['Q1_time'])
  df['Q2_rank']=rank(df['Q2_time'])
  df['Q3_rank']=rank(df['Q3_time'])
  df['code']=apply(df['DRIVER'],2,function(x) driverCode2(x))
  #Mapping from driver names as they appear in FIA press releases
  ## to three letter driver codes
  df
}

##Qualifying and Race Pages
#1Q
fiaQualiClassTidy=function(xx){
  for (q in c('Q1','Q2','Q3')){
    cn=paste(q,'_time',sep='')
    xx[cn]=apply(xx[q],1,timeInS)
  }

  xx=dplyr:::rename(xx, Q1_laps=LAPS)
  xx=dplyr:::rename(xx, Q2_laps=LAPS.1)
  xx=dplyr:::rename(xx, Q3_laps=LAPS.2)
  xx
}

#2Q, 3R
fiaSectorTidy=function(xx){
  colnames(xx)=c('pos',
                 's1_driver','s1_nattime',
                 's2_driver','s2_nattime',
                 's3_driver','s3_nattime')
  for (s in c('s1','s2','s3')) {
    sn=paste(s,'_time',sep='')
    sm=paste(s,'_nattime',sep='')
    xx[sn]=as.numeric(apply(xx[sm],1,timeInS))
  }

  xx[-1,]
}

#3Q, 4R
fiaTrapTidy=function(xx){
  xx
}

# 4Q, 5R
fiaSpeedTidy=function(xx){
  colnames(xx)=c('pos',
                 'inter1_driver','inter1_speed',
                 'inter2_driver','inter2_speed',
                 'inter3_driver','inter3_speed')

  xx[-1,]
}

# 2R
fiaRaceFastlapTidy=function(xx){
  xx['time']=apply(xx['LAP TIME'],1,timeInS)
  xx
}

# 6R
fiaPitsSummary=function(xx){
  r=which(xx['NO']=='RACE - PIT STOP - DETAIL')
  xx['tot_time']=apply(xx['TOTAL TIME'],1,timeInS)
  Filter(function(x)!all(is.na(x)), xx[1:r-1,])
}

#6R
fiaPitsDetail=function(xx){
  colnames(xx)=xx[1,] #c('NO','DRIVER','LAP','TIME','STOP','NAT DURATION','TOTAL TIME')
  xx['tot_time']=apply(xx['TOTAL TIME'],1,timeInS)
  xx['time']=apply(xx['DURATION'],1,timeInS)
  r=which(xx['NO']=='RACE - PIT STOP - DETAIL')
  xx=xx[r+2:nrow(xx),]
  #Remove blank row - http://stackoverflow.com/a/6437778/454773
  xx[rowSums(is.na(xx)) != ncol(xx),]
}


#URL of the HTML webpage we want to scrape
#url="http://www.fia.com/events/formula-1-world-championship/season-2015/qualifying-classification"

#xx=fiaTableGrabber(url,1)
#xx

#fiaQualiClassTidy(xx)

#EXPERIMENTAL
#http://vitalflux.com/data-science-scale-normalize-numeric-data-using-r/
normalise = function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
pc = function(x) {
  return (x / min(x))
}

sectorTimeNormaliser=function(xx){
  for (s in c('s1','s2','s3')) {
    xx[paste(s,'timepc',sep='_')] = pc(xx[paste(s,'time',sep='_')])
    xx[paste(s,'timen',sep='_')] = normalise(xx[paste(s,'time',sep='_')])
  }
  zx=xx[,c('s1_driver','s1_time','s1_timen')]
  zx=merge(zx,xx[,c('s2_driver','s2_time','s2_timen')],by.x='s1_driver',by.y='s2_driver')
  zx=merge(zx,xx[,c('s3_driver','s3_time','s3_timen')],by.x='s1_driver',by.y='s3_driver')
  names(zx)[names(zx) == 's1_driver'] = 'name'
  zx
}

sectorTimeDeltas=function(xx){
  for (s in c('s1','s2','s3')) {
    cc=paste(s,'time',sep='_')
    xx=xx[order(xx[[cc]]), ]
    xx[paste(s,'rank',sep='_')] = rank(xx[cc])
    mintime=min(xx[cc])
    xx[paste(s,'gap',sep='_')] = xx[cc]-mintime
    xx[paste(s,'diff',sep='_')] = c(0,diff(xx[[cc]]))
  }
  xx
}

sectorTimeUltimate=function(xx){
  cc='ultimate'
  xx[cc]=xx['s1_time']+xx['s2_time']+xx['s3_time']
  xx[paste(cc,'pc',sep='_')]=pc(xx[cc])
  xx=xx[order(xx[[cc]]), ]
  xx[paste(cc,'rank',sep='_')]=rank(xx[cc])

  mintime=min(xx[cc])
  xx[paste(cc,'gap',sep='_')] = xx[cc]-mintime
  xx[paste(cc,'diff',sep='_')] = c(0,diff(xx[[cc]]))

  xx
}
#url='http://www.fia.com/events/formula-1-world-championship/season-2015/qualifying-classification-5'
#xx=fiaTableGrabber(pageGrabber(url),2)
#zz=fiaSectorTidy(xx)
#zz=sectorTimeNormaliser(zz)
#zz=sectorTimeUltimate(zz)
#zz=sectorTimeDeltas(zz)

