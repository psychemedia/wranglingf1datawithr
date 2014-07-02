library("RSQLite")
f1comDatabaseConnection=function(
                          drv="SQLite",
                          db="~/Dropbox/wranglingf1datawithr/src/scraperwiki.sqlite"
                        ){ dbConnect(drv=drv, dbname=db) }

#Currently calling database connection in functions from global scope.
#Better either to:
# pass in connection as function argument?
# set up connection in each function (is there an overhead to this?)
f1 = f1comDatabaseConnection()

sessionData=function(race,year,sessionType='Sectors',sessions=c('p1','p2','p3','quali')){
  df=data.frame()
  if (length(sessions)>=1)
    for (session in sessions) {
      sessionName=paste(session,sessionType,sep='')
      q=paste("SELECT * FROM ", sessionName, " WHERE race=UPPER('",race,"') AND year='",year,"'", sep="")
      #print(q)
      
      #The following line creates appropriately named dataframes in the global scope
      #containing the results of each seprate query
      assign(sessionName,dbGetQuery(conn=f1, statement=q), envir = .GlobalEnv)
      df.tmp=get(sessionName)
      df.tmp['session']=session
      df=rbind(df,df.tmp)
    }
  #Cleaning stage - make sure columns are appropriately typed
  for (i in c('year','laps', 'pos'))
    df[i]=as.integer(as.character(df[i]))
  for (i in c('gap','time'))
    df[i]=as.numeric(as.character(df[i]))
  df
}

sectorTimes=function(race,year,sessions=c('p1','p2','p3','quali')){
  sessionData(race,year,'Sectors',sessions)
}

sessionSpeeds=function(race,year,sessions=c('p1','p2','p3','quali')){
  sessionData(race,year,'Speeds',sessions)
}

#Usage:
#Get all the practice and qualifying session sector times for a specific race
#df=sectorTimes('AUSTRALIA','2012')

#Get P3 and Quali sector times
#df=sectorTimes('AUSTRALIA','2012',c('p3','quali'))

#Get the speeds from the quali session.
#df=sessionSpeeds('Australia','2012','quali')