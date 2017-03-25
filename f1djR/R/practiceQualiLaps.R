library(plyr)
library(ggplot2)
#df <- read.csv("~/Dropbox/wranglingf1datawithr/src/df.csv")
#df <- read.csv("~/Dropbox/wranglingf1datawithr/src/quali.csv")

#Mapping from driver names as they appear in FIA press releases
## to three letter driver codes
driverCodes=c("L. HAMILTON"= "HAM", "S. VETTEL"= "VET", "N. ROSBERG"= "ROS", "D. RICCIARDO"= "RIC",
              "D. KVYAT"= "KVY", "M. VERSTAPPEN"= "VES", "F. MASSA" = "MAS", "R. GROSJEAN"= "GRO",
              "V. BOTTAS"= "BOT", "M. ERICSSON"= "ERI", "K. RAIKKONEN"= "RAI", "P. MALDONADO" = "MAL",
              "N. HULKENBERG"= "HUL", "S. PEREZ"= "PER", "C. SAINZ"= "SAI", "F. NASR"= "NAS",
              "J. BUTTON" = "BUT", "F. ALONSO"= "ALO", "R. MERHI"= "MER", "W. STEVENS"="STE",
              "R. HARYANTO"="HAR","P. WEHRLEIN"="WEH","J. PALMER"="PAL","K. MAGNUSSEN"="MAG",
              "E. GUTIERREZ"= "GUT", "S. VANDOORNE"= "VAN", "E. OCON"= "OCO", "M. CHILTON"= "CHI",
              "K. KOBAYASHI"= "KOB", "A. LOTTERER"= "LOT", "A. ROSSI"= "RSI", "L. STROLL" = "STR",
              "J. VERGNE"= "VER",  'A. GIOVINAZZI'='GIO')
driverCode=function(name) unname(driverCodes[name])

#A function to augment raw laptime from practice and qualifying sessions
## with derived data columns
rawLap_augment_laptimes = function(df){
  df['code']=apply(df['name'],2,function(x) driverCode(x))

  df=ddply(df,.(name),transform,cuml=cumsum(stime))
  df['pit']=df['pit']=='True'
  df=arrange(df,name, -lapNumber)
  df=ddply(df,.(name),transform,stint=1+sum(pit)-cumsum(pit))
  df=arrange(df,name, lapNumber)
  df=ddply(df,.(name,stint),transform,lapInStint=1:length(stint))
  df=arrange(df,name, lapNumber)
  df=ddply(df,.(name),transform,driverbest=cummin(c(9999,stime[2:length(stime)])))
  #Need a patch in case there is only an entry time.. ie stime length==1
  #TO DO - another correction to make a singleton time a pit lap
  df=df[!(is.na(df$driverbest)), ]
  df=arrange(df,cuml)
  df['purple']=sapply(df['driverbest'],cummin)
  df['colourx']=ifelse(df['stime']==df['purple'],
                       'purple',
                       ifelse(df['stime']==df['driverbest'],
                              'green',
                              'black'))
  df=arrange(df,name, lapNumber)
  df= ddply(df,.(name),transform,outlap=c(FALSE, diff(pit)==-1))
  df['outlap']= df['outlap'] | df['lapInStint']==1 |  (df['stime'] > 2.0 * min(df['purple']) & (!df['pit']) )
  df=ddply(df,
           .(name),
           transform,
           stint=cumsum(outlap),
           lapInStint=1:length(stint))
  df=ddply(df,
           .(name, stint),
           transform,
           lapInStint=1:length(stint))
  df
}

plot_session_utilisation_chart = function (df,size=2,session=''){
  g = ggplot(df)
  #Layer showing in-laps (laps on which a driver pitted) and out-laps
  g = g + geom_point(data=df[df['outlap'] | df['pit'],],
                     aes(x=cuml, y=name#, color=factor(colourx)
                         ), pch=1)
  #Further annotation to explicitly identify pit laps (in-laps)
  g = g + geom_point(data=df[df['pit']==TRUE,],
                     aes(x=cuml, y=name), pch='.')
  #Layer showing full laps with rounded laptimes and green/purple lap highlights
  g = g + geom_text(data=df[!df['outlap'] & !df['pit'],],
                    aes(x=cuml, y=name,
                        label=floor(stime*10)/10, color=factor(colourx)
                        ),
                    size=size, angle=45)
  g = g + scale_colour_manual(values=c('darkgrey','darkgreen','purple'))

  g = g + xlab(NULL) + ylab(NULL)
  g + guides(colour=FALSE) + theme_bw()
}

plot_session_utilisation_chart_toggle_gap=function(df,size=2){
   df=ddply(df,.(name,stint),transform,diff=c(0,diff(stime)))
   df['coloury']=ifelse(df$colourx=='black',
                                ifelse(df$diff>=0.0,'red','yellow'),
                                df$colourx)
   g = ggplot(df)
   #Layer showing in-laps (laps on which a driver pitted) and out-laps
   g = g + geom_point(data=df[df['outlap'] | df['pit'],],
                      aes(x=cuml, y=name, color=factor(colourx)), pch=1)
   #Further annotation to explicitly identify pit laps (in-laps)
   g = g + geom_point(data=df[df['pit']==TRUE,],
                      aes(x=cuml, y=name),pch='.')
   #Layer showing start of stint laptimes and green/purple lap highlights
   g = g + geom_text(data=df[df['lapInStint']==2 & !df['pit'],],
                     aes(x=cuml, y=name,
                         label=stime,#floor(stime*10)/10,
                         color=factor(colourx)),
                     size=size, angle=45)
   #Layer showing stint laptime deltas and green/purple lap highlights
   g = g + geom_text(data=df[df['lapInStint']>2 & !df['pit'],],
                     aes(x=cuml, y=name,
                         label=round(diff,2),
                         color=factor(coloury)),
                     size=size, angle=45)
   g = g + scale_colour_manual(values=c('darkgrey','darkgreen','purple','red','blue'))

   g + xlab(NULL) + ylab(NULL) + guides(colour=FALSE) + theme_bw()
 }

plot_session_utilisation_chart_stint_diff=function(df,size=2){
  df=ddply(df,.(name,stint),mutate,sf=stime[2],sfd=sf-stime)
  #sfd is junk in rows 1 and 2 of a stint
  df['colourz']=ifelse(df$colourx=='black',
                               ifelse(df$sfd>=0.0,'red','yellow'),
                               df$colourx)
  g = ggplot(df)
  #Layer showing in-laps (laps on which a driver pitted) and out-laps
  g = g + geom_point(data=df[df['outlap'] | df['pit'],],
                     aes(x=cuml, y=name), pch=1) # aes: color=factor(colourx),
  #Further annotation to explicitly identify pit laps (in-laps)
  g = g + geom_point(data=df[df['pit']==TRUE,],
                     aes(x=cuml, y=name),pch='.')
  #Layer showing start of stint laptimes and green/purple lap highlights
  g = g + geom_text(data=df[df['lapInStint']==2 & !df['pit'],],
                    aes(x=cuml, y=name,
                        label=stime,#floor(stime*10)/10,
                        color=factor(colourx)),
                    size=size, angle=45)
  #Layer showing stint laptime deltas and green/purple lap highlights
  g = g + geom_text(data=df[df['lapInStint']>2 & !df['pit'],],
                    aes(x=cuml, y=name,
                        label=-round(sfd,2),
                        color=factor(colourz)),
                    size=size, angle=45, fontface="italic")
  g = g + scale_colour_manual(values=c('darkgrey','darkgreen','purple','blue','red'))

  g + xlab(NULL) + ylab(NULL) + guides(colour=FALSE) + theme_bw()
}

augmented_session_utilisation_chart=function(df,size=2,lapcount=TRUE,
                                             gap=TRUE,besttime=TRUE,session=''){
  df=arrange(df,name,lapNumber)
  spurple=min(df['purple'])
  dfClass=ddply(df[df['driverbest']<9999,],
                .(name),
                here(summarise),
                driverbest=min(driverbest),
                gap=min(driverbest)-spurple)
  dfClass=arrange(dfClass,driverbest)
  dfClass['diff']=c(0,diff(dfClass$gap))
  dfClass$pos=1:nrow(dfClass)
  #dfClass
  g=plot_session_utilisation_chart(df,size)
  #g=g+geom_text(data=dfClass,aes(x=-800,y=name,label=pos),size=size,fontface='bold')

  if (gap) {
    g=g+geom_text(data=dfClass,
                         aes(x=-400,y=name,label=paste(round(gap,3)," (",round(diff,3),")",sep='')),
                         size=size)
  }

  sess_util_formatter = function(x) {
    lab=ifelse(x<0, '', x)
  }
  txt="Session Utilisation Chart"
  if (session !='') txt=paste0(txt,' (',session,')')
  g=g+scale_x_continuous(label=sess_util_formatter)+ggtitle(txt)

  dfn=count(df,"name")
  if (lapcount) {
    g=g+geom_text(data=dfn,aes(x=-800,y=name,label=paste('(',freq,')',sep='')),
                  size=size,fontface='bold')
  }

  if (besttime){
    g=g+geom_text(data=dfClass,aes(x=-1000,y=name,label=driverbest),size=size,fontface='bold')
  }


  #Order the chart by driver session position
  #If there are extra levels in the names, clear them out
  df$name=droplevels(df$name)
  dfClass$name=droplevels(dfClass$name)
  levels(df$name) = factor(dfClass$name, levels = levels(dfClass$name[order(dfClass$pos)]))
  g+scale_y_discrete(limits=rev(levels(df$name))) +xlab('Accumulated session time (s)') +ylab(NULL)
}

#esp_p2a=rawLap_augment_laptimes(esp_p2)
#augmented_session_utilisation_chart(esp_p2a,3)

#source('streakiness.R')
stintFinder=function(df){
  stints=data.frame()
  for (name in levels(df$name)){
    dft=df[df$name==name,]
    dft=streaks(dft$stint)
    dft['name']=name
    dft=dft[c('name','start','end','l')]
    stints=rbind(stints,dft)
  }

  stints['name']=factor(stints$name)
  ddply(stints,.(name),transform,stintNumber=1:length(l))
}

longrunFinder=function(stints,df,stintlen=8){
  longruns=merge(stints[abs(stints['l'])>=stintlen,],
                 df,by.x=c('name','stintNumber'),
                 by.y=c('name','stint'))
  arrange(longruns,name,lapNumber)
}

longrunsplot_min=function(longruns){
  g= ggplot(longruns[!longruns['outlap'] & !longruns['pit'],])
  g=g+geom_line(aes(x=lapInStint, y=stime, group=stintNumber,
                    colour=factor(stintNumber)))
  g+facet_wrap(~name)
}

longrunsplot_model=function(longruns,
                            m='loess',
                            cutoffpc=1.07,
                            drivers=c('L. HAMILTON', 'K. RAIKKONEN','S. VETTEL' )){
  lr=longruns[!longruns['outlap'] & !longruns['pit'] & longruns$name %in% drivers
              & longruns['stime']<cutoffpc*min(longruns['purple']),]
  g= ggplot(lr,
            aes(x=lapInStint, y=stime,colour=interaction(stintNumber,name)))
  g+geom_smooth(method = m,
                aes( group=interaction(stintNumber,name))) + geom_point(aes(shape=interaction(stintNumber,name)))+ scale_colour_brewer(palette="Set1")
}
#st=stintFinder(esp_p2a)
#longruns=longrunFinder(st,esp_p2a)
#longrunsplot_min(longruns)
#longrunsplot_model(longruns,'lm')
#longrunsplot_model(longruns[longruns['stime']<96,],'lm')


#Try to identify gaps between qualifying sessions

rawLap_augment_quali=function(df){
  df=rawLap_augment_laptimes(df)
  df=arrange(df,cuml)
  df['gap']=c(0,diff(df[,'cuml']))
  df['gapflag']= (df['gap']>=300)
  df['qsession']=1+cumsum(df[,'gapflag'])
  df
}

qsessionOverride=function(df,t1start,t2start,t3start){
  df[(df['cuml']>t3start),]['qsession']=3
  df[(df['cuml']>t2start) & (df['cuml']<t3start),]['qsession']=2
  df[(df['cuml']>t1start) & (df['cuml']<t2start),]['qsession']=1
  df
}

#Colour laptimes according to purple/green within separate quali sessions
quali_purplePatch=function(df){
  df=arrange(df,name, lapNumber)
  df=ddply(df,.(qsession,name),transform,driverqbest=cummin(c(9999,stime[2:length(stime)])))
  df=arrange(df,cuml)
  df=ddply(df,.(qsession),transform,qpurple=cummin(driverqbest))
  df['colourx']=ifelse(df['stime']==df['qpurple'],
                       'purple',
                       ifelse(df['stime']==df['driverqbest'] & !df['pit'] & !df['outlap'],
                             'green',
                              'black'))
  df=arrange(df,name, lapNumber)
  df
}



