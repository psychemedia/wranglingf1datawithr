#source('ergastR-core.R')
library(plyr)
library(reshape2)


quali_session_times_plots=function(df,session,evolution=TRUE,purple=FALSE,cutoff=FALSE,
                                   cutoffvals=c(15,10,1)){
  #Update this to assume we are doing a specified session
  df['qsession']=session
  sessionbest=ddply(df[!df['pit'] & !df['outlap'],],
              .(qsession),
              summarise,
              sbest=min(stime),
              sb107=1.07*sbest)
  df=merge(df,sessionbest,by='qsession')
  df=df[!df['outlap'] & !df['pit'] & df['stime']<=df['sb107'],]
  g=ggplot(df)
  if (purple) {
    g=g+geom_line(data=df[df['colourx']=='purple',],aes(x=cuml,y=qpurple),colour='darkgrey')
  }
  df=ddply(df,.(code,qsession),transform,driverqsbest=min(driverqbest))
  df=ddply(df,.(qsession),transform,qspurple=min(qpurple))

   #if (cutoff){

    df=arrange(df,cuml)
    dfc=data.frame()

    n=cutoffvals[session]
    for (r in 1:nrow(df)) {
      #dfcc=ddply(df[1:r,],.(qsession,code),summarise,dbest=min(stime))
      dfcc=ddply(df[1:r,],.(code),summarise,dbest=min(stime))
      #session=df[r,]$qsession
      #dfcc=arrange(dfcc[dfcc['qsession']==session,],dbest)
      dfcc=arrange(dfcc,dbest)
      #n=cutoffvals[df[r,]$qsession]
      if (nrow(dfcc) >=n){
        #may get a mismatch here if a time is over the 1.07x? TO DO
        dfc=rbind(dfc,data.frame(df[r,]['qsession'],df[r,]['code'],df[r,]['cuml'],dfcc[n,]['dbest']) )
      }
    }
  if (cutoff){
    #Stepped chart
    g=g+geom_step(data=dfc,aes(x=cuml,y=dbest),colour='lightgrey',direction = "hv")
    #g=g+geom_line(data=dfc,aes(x=cuml,y=dbest))
    #line to show the cutoff
    #g=g+geom_hline(data=dfc,
    #               aes(yintercept=dbest),colour='yellow')
    #to do - somehow limit to only show when there is a change?
    #g=g+geom_segment(data=dfc,aes(x=0,y=dbest,xend=cuml,yend=dbest),colour='red')

  }


  if (evolution){
    #WOULD IT BE HANDY TO SHOW IN BOLD EACH DRIVER'S BEST IN SESSION?
    g=g+geom_text(#data=df[df['stime']!=df['driverqsbest'],],
                  aes(x=cuml,y=stime,label=code,colour=factor(colourx)),
                  angle=45,size=3)
    #g=g+geom_text(data=df[df['stime']==df['driverqsbest'],],
    #              aes(x=cuml,y=stime,label=code,colour=factor(colourx)),
    #              angle=45,size=3,fontface='bold')
  } else {
    df['coloury']=ifelse(df['colourx']=='black','black',
                                ifelse(df['stime']==df['qspurple'],'purple','green'))
    g=g+geom_text(data=df[df['stime']!=df['driverqsbest'],],
                  aes(x=cuml,y=stime,label=code,colour=factor('black')),
                  angle=45,size=3)
    g=g+geom_text(data=df[df['stime']==df['driverqsbest'],],
                  aes(x=cuml,y=stime,label=code,colour=factor(coloury)),
                  angle=45,size=3)
  }
  #How about a colour if you miss the cut with best time?

  dfx=df
  df=merge(df,dfc,by=c('qsession','code','cuml'))
  dfdd=ddply(df,.(qsession),summarise,cutoffdbest=min(dbest))
  #df=merge(df,dfdd,by='qsession')
  dfx=merge(dfx,dfdd,by='qsession')
  g=g+geom_text(data=dfx[dfx['stime']==dfx['driverqsbest'] & dfx['driverqsbest']>dfx['cutoffdbest'],],
                aes(x=cuml,y=stime,label=code,colour=factor('red')),
                angle=45,size=3)
  #line to show the cutoff
  g=g+geom_hline(data=dfdd,
                 aes(yintercept=cutoffdbest),linetype='dotted')

  #g=g+facet_wrap(~qsession,scale="free")
  g=g+facet_wrap(~qsession,scale="free", ncol = 1)
  g=g+ scale_colour_manual(values=c('darkgrey','darkgreen','purple','red'))

  g+ guides(colour=FALSE)  +xlab('Session time (s)') +ylab('Laptime (s)')
}

#source('practiceQualiLaps.R')
#bah_q=rawLap_augment_quali(bah_q)
#bah_q=quali_purplePatch(bah_q)
#quali_session_times_plots(bah_q)
#quali_session_times_plots(bah_q,TRUE,TRUE,TRUE,c(15,10,2))


slopeblank=theme(panel.border = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 axis.line = element_blank(),
                 axis.ticks = element_blank(),
                 axis.text = element_blank())

core_qualifying_rank_slopegraph= function(qualiResults,qm,spacer=0.25,
                                          cutoff=c(15,10),reverse=FALSE){
  #http://stackoverflow.com/questions/10659133/local-variables-within-aes
  .e = environment()
  g=ggplot(qualiResults,aes(x=session,y=laptime), environment = .e)
  g= g+geom_text(data=qm[qm['session']=='q1time',],
                 aes(x=1,y=qspos,label=driverName,
                     colour=(qspos>cutoff[1] )
                 ), size=3)
  g= g+geom_text(data=qm[qm['session']=='q2time',],
                 aes(x=2,y=qspos,label=driverName,
                     colour=(qspos>cutoff[2] )
                 ), size=3)

  g= g+geom_text(data=qm[qm['session']=='q3time',],
                 aes(x=3,y=qspos,label=driverName,
                     colour=TRUE
                 ), size=3)
  g=g+geom_segment(data=qualiResults[!is.na(qualiResults['q2time']),],
                   x=1+spacer,xend=2-spacer,
                   aes(y=q1pos,yend=q2pos,group=driverName),
                   colour='slategrey')
  g=g+geom_segment(data=qualiResults[!is.na(qualiResults['q3time']),],
                   x=2+spacer,xend=3-spacer,
                   aes(y=q2pos,yend=q3pos,group=driverName),
                   colour='slategrey')
  g=g+scale_colour_manual(values=c('slategrey','blue'))
  g=g+guides(colour=FALSE)
  if (reverse) g=g+scale_y_reverse()
  g+theme_bw()+xlab(NULL)+ylab(NULL)+xlim(0.5,3.5)+slopeblank
}

#The following chart really needs updating to include intelligent label spacing
#I wonder if there is anything in directlabels that can help with this?
core_qualifying_time_slopegraph= function(qualiResults,qm,spacer=0.25,cutoff=c(15,10)){
  #http://stackoverflow.com/questions/10659133/local-variables-within-aes
  .e = environment()
  g=ggplot(qualiResults,aes(x=session,y=laptime), environment = .e)
  g= g+geom_text(data=qm[qm['session']=='q1time',],
                 aes(x=1,y=laptime,label=driverName,
                     colour=(qspos>cutoff[1] )
                 ), size=3)
  g= g+geom_text(data=qm[qm['session']=='q2time',],
                 aes(x=2,y=laptime,label=driverName,
                     colour=(qspos>cutoff[2] )
                 ), size=3)

  g= g+geom_text(data=qm[qm['session']=='q3time',],
                 aes(x=3,y=laptime,label=driverName,
                     colour=TRUE
                 ), size=3)
  g=g+geom_segment(data=qualiResults[!is.na(qualiResults['q2time']),],
                   x=1+spacer,xend=2-spacer,
                   aes(y=q1time,yend=q2time,group=driverName),
                   colour='slategrey')
  g=g+geom_segment(data=qualiResults[!is.na(qualiResults['q3time']),],
                   x=2+spacer,xend=3-spacer,
                   aes(y=q2time,yend=q3time,group=driverName),
                   colour='slategrey')
  g=g+scale_colour_manual(values=c('slategrey','blue'))
  g=g+guides(colour=FALSE)
  g+theme_bw()+xlab(NULL)+ylab(NULL)+xlim(0.5,3.5)+slopeblank
}

quali_progression_ergast_tx=function(qr){
  #Transform the data so that we can use it in the original function

  #Rename columns so they work with the original charting function
  #q1time, q2time, q3time,q1pos,q2pos,q3pos,driverName,qspos
  plyr:::rename(qr, c("Q1_time"="q1time", "Q2_time"="q2time", "Q3_time"="q3time",
                  "Q1_rank"="q1pos", "Q2_rank"="q2pos", "Q3_rank"="q3pos",
                  "position"="qspos","code"="driverName"))
}

quali_progression_ergast_melt=function(qr){
  #Generate qm equivalent by melting elements of qualifying results dataframe
  #driverName, qspos, session (q1time, q2time, q3time)
  qrm=reshape2:::melt(qr,
           id=c('driverName'),
           measure=c('q1time','q2time','q3time'),
           variable.name='session',
           value.name='laptime')
  qrm$driverName=paste(qrm$driverName," (",qrm$laptime,")",sep='')
  qrm=ddply(qrm,'session',mutate,qspos=rank(laptime))

  #Make sure that the laptime values are treated as numeric quantities
  qrm$laptime=as.numeric(qrm$laptime)
  #If a laptime is recorded as 0 seconds, set it to NA
  qrm[qrm == 0] <- NA
  #Drop any rows with NA laptime values
  qrm[with(qrm,!is.na(laptime)),]
}

#qr=qualiResults.df(2015,2)
#OR xx=pageGrabber('http://www.fia.com/events/formula-1-world-championship/season-2015/session-classifications-15')
#qr=quali_progression_ergast_tx(ergastifyQualiClass(fiaSessionClassQualifyingTidy(fiaTableGrabber(xx,4))))
#qr=quali_progression_ergast_tx(qr)
#qrm=quali_progression_ergast_melt(qr)
#core_qualifying_rank_slopegraph(qr,qrm,spacer=0.21)
