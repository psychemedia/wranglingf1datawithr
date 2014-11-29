#The list of packages to be loaded
list.of.packages <- c("RJSONIO","plyr")

#You should be able to simply reuse the following lines of code as is
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)})

#Helper functions

#' Convert a string to a numeric
#' 
#' \code{getNum}
#' @param object item to be converted to a numeric
#' @return string or factor cast as numeric
getNum=function(x){as.numeric(as.character(x))}

#' Generate time in seconds from min:seconds format
#' 
#' \code{timeInS}
#' @param string time in format minutes:seconds
#' @return time in seconds as numeric
timeInS=function(tStr){
  x=unlist(strsplit(tStr,':'))
  tS=60*getNum(x[1])+getNum(x[2])
}


#My cacheing attempt is broken
#messing up scope somewhere?
#race.cache=list()

##==========  URL BUILDERS

API_PATH="http://ergast.com/api/f1/"

#?include a format adder function?

#' Get URL for races by year
#' 
#' \code{getRacesDataByYear.URL}
#' @param integer season year for required data
#' @param character data format (json, XML)
#' @return a URL
getRacesDataByYear.URL=function(year,format='json'){
  #Need additional callback parameter and handler if accept jsonp
  paste(API_PATH,year,".",format,sep='')
}

#' Get URL for laps by race-and-year
#' 
#' \code{getLapsByYearRace.URL}
#' @param integer season year 
#' @param integer race number in year
#' @param character data format (json, XML)
#' @return a URL
getLapsByYearRace.URL=function(year,raceNum,format='json'){
  paste(API_PATH,year,"/",raceNum,"/laps.",format,"?limit=2500",sep='')
}

#' Get URL for laps by race-and-year-and-driver
#' 
#' \code{getLapsByYearRaceDriver.URL}
#' @param integer season year 
#' @param integer race number in year
#' @param character driver
#' @param character data format (json, XML)
#' @return a URL
getLapsByYearRaceDriver.URL =function(year,raceNum,driverId,format='json'){
  paste(API_PATH,year,"/",raceNum,"/drivers/",driverId,"/laps.",format,"?limit=2500",sep='')
}


#' Get URL for results by race-and-year
#' 
#' \code{getRaceResultsByYearRace.URL}
#' @param integer season year 
#' @param integer race number in year
#' @param character data format (json, XML)
#' @return a URL
getRaceResultsByYearRace.URL=function(year,raceNum,format="json"){
  paste(API_PATH,year,"/",raceNum,"/results.",format,sep='')
}

#' Get URL for results by race-and-year
#' 
#' \code{getDriversByYear.URL}
#' @param integer season year
#' @param character data format (json, XML)
#' @return a URL
getDriversByYear.URL=function(year,format='json'){
  paste(API_PATH,year,"/drivers.",format,sep='')
}

#' Get URL for results by year and driver
#' 
#' \code{getDriverResultsByYear.URL}
#' @param integer season year
#' @param character driverRef
#' @param character data format (json, XML)
#' @return a URL
getDriverResultsByYear.URL=function(year,driverRef,format='json'){
  paste(API_PATH,year,"/drivers/",driverRef,"/results.",format,sep='')
}

##==========  URL BUILDERS END

##==========  JSON GRABBERS

#' Get JSON data
#' 
#' \code{getJSONbyURL}
#' @param character URL for data request
#' @return JSON data from ergast API
getJSONbyURL=function(URL){
  fromJSON(URL,simplify=FALSE)
}

##==========  JSON GRABBERS END

##==========  JSON PARSERS

#' Format laps data
#' 
#' \code{formatLapData}
#' @param object containing laps data
#' @return dataframe containing laps data
formatLapData=function(rd){
  #initialise lapdata frame
  lap.data <- data.frame(lap=numeric(),
                         driverID=character(), 
                         position=numeric(), strtime=character(),rawtime=numeric(),
                         stringsAsFactors=FALSE)
  
  for (i in 1:length(rd)){
    lapNum=getNum(rd[[i]]$number)
    for (j in 1:length(rd[[i]]$Timings)){
      lap.data=rbind(lap.data,data.frame(
        lap=lapNum,
        driverId=rd[[i]]$Timings[[j]]$driverId,
        position=as.integer(as.character(rd[[i]]$Timings[[j]]$position)),
        strtime=rd[[i]]$Timings[[j]]$time,
        rawtime=timeInS(rd[[i]]$Timings[[j]]$time)
      ))
    }
  }
  
  lap.data=ddply(lap.data,.(driverId),transform,cuml=cumsum(rawtime))
  
  #via http://stackoverflow.com/a/7553300/454773
  lap.data$diff <- ave(lap.data$rawtime, lap.data$driverId, FUN = function(x) c(NA, diff(x)))
  
  #lap.data=ddply(lap.data,.(driverId),transform,decmin=rawtime-min(rawtime))
  #lap.data$topdelta=lap.data$rawtime-min(lap.data$rawtime)
  lap.data
}

#' Extract Laps data from race data JSON object
#' 
#' \code{getLapsData.path}
#' @param object containg race data
#' @return object containing laps data
getLapsData.path=function(rd.laps){
  laps.data=rd.laps$MRData$RaceTable$Races[[1]]$Laps
  laps.data
}

#' Generate dataframe containing lap data for a given race
#' 
#' \code{lapsData.df}
#' @param integer season year for required data
#' @param integer round number for required data
#' @param character data format (json, XML)
#' @return dataframe containing lap data for a specific race
lapsData.df=function(year,raceNum,format='json'){
  rd.laps=getJSONbyURL(getLapsByYearRace.URL(year,raceNum))
  ld=getLapsData.path(rd.laps)
  formatLapData(ld)
}

#' Generate dataframe containing lap data for a specified driver
#' 
#' \code{lapsDataDriver.df}
#' @param integer season year for required data
#' @param integer round number for required data
#' @param character driverId for specified driver
#' @param character data format (json, XML)
#' @return dataframe containing lap data for a specific race
lapsDataDriver.df=function(year,raceNum,driver,format='json'){
  rd.laps=getJSONbyURL(getLapsByYearRaceDriver.URL(year,raceNum,driver))
  ld=getLapsData.path(rd.laps)
  formatLapData(ld)
}

#' Get dataframe for races by year
#' 
#' \code{racesData.df}
#' @param integer season year for required data
#' @return dataframe containing race data for each year
racesData.df=function(year){
  races.data=data.frame(
    round=numeric(),
    racename=character(),
    circuitId=character()
  )
  rd=getJSONbyURL(getRacesDataByYear.URL(year))
  races=rd$MRData$RaceTable$Races
  for (i in 1:length(races)){
    races.data=rbind(races.data,data.frame(
      round=races[[i]]$round,
      racename=races[[i]]$raceName,
      circuitId=races[[i]]$Circuit$circuitId
    ))
  }
  races.data
}

#' Get dataframe for drivers by year
#' 
#' \code{driversData.df}
#' @param integer season year for required data
#' @return dataframe containing race data for each year
driversData.df=function(year){
  drivers.data=data.frame(
    name=character(),
    driverId=character()
  )
  drivers.json=fromJSON(getDriversByYear.URL(year))
  drivers=drivers.json$MRData$DriverTable$Drivers
  for (i in 1:length(drivers)){
    if (is.na(drivers[[i]]['permanentNumber'])) permNumber=NA
    else permNumber=drivers[[i]]['permanentNumber']
    
    drivers.data=rbind(drivers.data,data.frame(
      driverId=drivers[[i]]['driverId'],
      name=drivers[[i]]['familyName'],
      code=drivers[[i]]['code'],
      permNumber=permNumber
    ))
  }
  drivers.data
}


#' Get dataframe containing results by driver
#' 
#' \code{driverResults.df}
#' @param integer season year
#' @param character driverRef reference code for specified driver
#' @return dataframe containing results data for a particular driver in a particular year
driverResults.df=function(year,driverRef){
  drj=getJSONbyURL(getDriverResultsByYear.URL(year,driverRef))
  drdr=drj$MRData$RaceTable$Races
  
  driver.results.data=data.frame(
    driverId=character(),
    code=character(),
    constructorId=character(),
    grid=numeric(),
    laps=numeric(),
    position=numeric(),
    positionText=character(),
    points=numeric(),
    status=character(),
    season=numeric(),
    round=numeric()
  )
  
  for (i in 1:length(drdr)){
    season=as.integer(drdr[[i]]$season)
    round=as.integer(drdr[[i]]$round)
    drd=drdr[[i]]$Results[[1]]
    driver.results.data=rbind(driver.results.data,data.frame(
      driverId=as.character(drd$Driver$driverId),
      code=as.character(drd$Driver$code),
      constructorId=as.character(drd$Constructor$constructorId),
      grid=as.integer(drd$grid),
      laps=as.integer(drd$laps),
      position=as.integer(drd$position),
      positionText=as.character(drd$positionText),
      points=as.integer(drd$points),
      status=as.character(drd$status),
      season=season,
      round=round
    ))
  }
  
  driver.results.data
}

#' Get dataframe for races by year
#' 
#' \code{resultsData.df}
#' @param integer season year
#' @param integer race number in season
#' @return dataframe containing results data for a particular race
resultsData.df=function(year,raceNum){
  rrj=getJSONbyURL(getRaceResultsByYearRace.URL(year,raceNum))#getRaceResultsData.full(raceNum)
  rrd=rrj$MRData$RaceTable$Races[[1]]$Results
  
  race.results.data=data.frame(
    carNum=numeric(),
    pos=numeric(),
    driverId=character(),
    constructorId=character(),
    grid=numeric(),
    laps=numeric(),
    status=character(),
    millitime=numeric(),
    fastlapnum=numeric(),
    fastlaptime=character(),
    fastlaprank=numeric()
  )
  
  for (i in 1:length(rrd)){
    race.results.data=rbind(race.results.data,data.frame(
      carNum=as.integer(as.character(rrd[[i]]$number)),
      pos=as.integer(as.character(rrd[[i]]$position)),
      driverId=rrd[[i]]$Driver$driverId,
      constructorId=rrd[[i]]$Constructor$constructorId,
      grid=as.integer(as.character(rrd[[i]]$grid)),
      laps=as.integer(as.character(rrd[[i]]$laps)),
      status=rrd[[i]]$status,
      #millitime=rrd[[i]]$Time$millis,
      fastlapnum=hack1(rrd[[i]]),
      fastlaptime=hack2(rrd[[i]]),
      fastlaprank=hack3(rrd[[i]])
    ))
  }
  race.results.data$driverId=reorder(race.results.data$driverId, race.results.data$carNum)
  race.results.data
}

#' Get race winner by season and year
#' 
#' \code{raceWinner}
#' @param integer season year
#' @param integer race number
#' @return driverId for winner of a particular race
raceWinner=function(year,raceNum){
  wURL=paste(API_PATH,year,"/",raceNum,"/results/1.json",sep='')
  wd=fromJSON(wURL,simplify=FALSE)
  wd$MRData$RaceTable$Races[[1]]$Results[[1]]$Driver$driverId
}

#' Parse JSON driver standings data
#' 
#' \code{_driverStandings.json.parse}
#' @param integer season year
#' @param integer race number
#' @return dataframe containing standings for each year, by race
ergast.json.parse.driverStandings.df=function(dURL){
  drj=getJSONbyURL(dURL)
  drd=drj$MRData$StandingsTable$StandingsLists
  driverStandings.data=data.frame()
  for (i in 1:length(drd)){
    for (j in 1:length(drd[[i]]$DriverStandings))
      driverStandings.data=rbind(driverStandings.data,data.frame(
        year=getNum(drd[[i]]$season),
        driverId=drd[[i]]$DriverStandings[[j]]$Driver$driverId,
        pos=getNum(drd[[i]]$DriverStandings[[j]]$position),
        points=getNum(drd[[i]]$DriverStandings[[j]]$points),
        wins=getNum(drd[[i]]$DriverStandings[[j]]$wins),
        car=drd[[i]]$DriverStandings[[j]]$Constructors[[1]]$constructorId)
      )
    
  }
  driverStandings.data
}


#' Get dataframe for season standings by year
#' 
#' \code{seasonStandings.df}
#' @param integer season year
#' @param integer race number
#' @return dataframe containing standings for each year, by race
seasonStandings=function(year,race=''){
  if (race=='')
    dURL= paste('http://ergast.com/api/f1/',year,'/driverStandings.json',sep='')
  else
    dURL= paste('http://ergast.com/api/f1/',year,'/',race,'/driverStandings.json',sep='')
  ergast.json.parse.driverStandings.df(dURL)
}

#' Get dataframe for individual driver standings by year
#' 
#' \code{driverCareerStandings.df}
#' @param character driverId
#' @return dataframe containing standings for each driver at the end of each year
driverCareerStandings.df=function(driverId){
  dURL=paste(API_PATH,'drivers/',driverId,'/driverStandings.json',sep='')
  ergast.json.parse.driverStandings.df(dURL)
}

##==========  JSON PARSERS END



hack1=function(crap){
   if (length(crap$FastestLap)>0)
     getNum(crap$FastestLap$lap)
   else NA
}
hack2=function(crap){
  if (length(crap$FastestLap)>0)
    timeInS(crap$FastestLap$Time$time)
  else NA
}
hack3=function(crap){
  if (length(crap$FastestLap)>0)
    getNum(crap$FastestLap$rank)
  else NA
}
