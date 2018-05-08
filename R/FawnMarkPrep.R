#' @title Daily Fawn Update
#
#' @description Run daily parturition fawn updates data manipulation and plotting
#' @param vecpath path to vectronic data folder
#' @param ATSUsers character vector of ATS user names
#' @param ATSPass character vector of ATS passwords
#' @param tempdir temporary folder for downloading data
#' @param CleanRep
#' @param spp Species of animal
#' @param ncpu number of CPU cores for multithreaded work
#' @param lookup path to lookup table
#' @param plotfolder path to store plots in
#' @param mortvec character vector of mortalities 
#' @param datastore RDS file to store data as (full path)
#' @param PrettyDataStore RDS file to store the pretty data (full path)
#' @return Creates all data needed for fawnmark
#' @keywords fawnmark, prep
#' @export
#' @examples
#' \donttest{vecVit<-vecVitDat(path='F:/Box Sync/DEER/Data/Vectronic/VecData')}
#'

FawnMarkPrep<-function(vecpath,ATSUsers,ATSPass,tempdir,CleanRep,
                        spp,ncpu,lookup,plotfolder,mortvec,
                        datastore,PrettyDataStore){
  
  dat<-CombDat(vecpath=vecpath,
               ATSUsers=ATSUsers,ATSPass=ATSPass,
               tempdir=tempdir)
  dd<-ColDownload(username = ATSUsers,password=ATSPass,
                  dirdown = tempdir)
  vi<-dd[[2]]
  names(vi)[1]<-'CollarSerialNumber'
  names(vi)[2]<-'Date'
  vi$Da<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Date'],' '))[[1]]})
  vi$Ti<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Date'],' '))[[2]]})
  vi$PM<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Date'],' '))[[3]]})
  
  vi$Month<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Da'],'/'))[[1]]})
  vi$Day<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Da'],'/'))[[2]]})
  vi$Year<-apply(vi,1,FUN=function(x) {unlist(strsplit(x['Da'],'/'))[[3]]})
  
  vi$Ti<-ifelse(nchar(vi$Ti)==7,paste0('0',vi$Ti),vi$Ti)
  vi$Month<-ifelse(nchar(vi$Month)==1,paste0('0',vi$Month),vi$Month)
  vi$Day<-ifelse(nchar(vi$Day)==1,paste0('0',vi$Day),vi$Day)
  
  vi$Da<-paste(vi$Month,vi$Day,vi$Year,sep='/')
  
  vi$Date<-paste(vi$Da,vi$Ti, vi$PM,sep=' ')
  dat$X2D.3D<-ifelse(dat$X2D.3D=='val. GPS-3D',6,dat$X2D.3D)
  dat$X2D.3D<-ifelse(dat$X2D.3D=='GPS-3D',6,dat$X2D.3D)
  dat$X2D.3D<-as.numeric(dat$X2D.3D)
  
  
  Cdat<-cleanFun(dat,filename=CleanRep,spp=spp)
  
  mdat<-as.data.frame(Cdat[[1]])
  mdat$CollarSerialNumber<-as.character(mdat$CollarSerialNumber)
  
  mdat<-mdat[which(mdat$TelemDate>=as.POSIXct('2018-03-01 00:00:00','%Y-%m-%d %H:%M:%S',tz='MST')),]
  
  atab<-as.data.frame(table(mdat$CollarSerialNumber))
  atab<-atab[which(atab$Freq>100),]
  
  mdat<-mdat[which(mdat$CollarSerialNumber %in% atab$Var1),]
  
  mdat2<-BGBFun(mdat,xname='Easting',yname='Northing',timename='TelemDate',
                idname='CollarSerialNumber',projstring=proj4string(Cdat[[1]]),ncpus=ncpu)
  
  vecVit<-vecVitDat(path=vecpath)
  vi<-rbind(vi,vecVit)
  viout<-locFun(vidat=vi,locdat=mdat2)
  
  viout<-viout[which(viout$CollarSerialNumber %in% mdat2$CollarSerialNumber),]
  
  vhist<-read.csv(lookup,stringsAsFactors = F)
  vhist<-vhist[complete.cases(vhist$New.Serial),]
  vhist<-vhist[which(vhist$spp==spp),]
  vhist<-vhist[which(!(is.na(vhist$VIT.Freq))),]
  
  #es<-Sys.time()
  
  vi$Date<-as.POSIXct(vi$Date,format='%m/%d/%Y %I:%M:%S %p')
  
  names(vi)[12]<-'Event'
  names(vhist)[1]<-'Serial.Number'
  
  #mtime<-as.POSIXct('2017-04-01 00:00:00',format='%Y-%m-%d %H:%M:%S')
  vi<-vi[which(vi$Date>=as.POSIXct('2017-05-01 00:00:00',format='%Y-%m-%d %H:%M:%S')),]
  
  
  system.time({ vitMap(locdat=mdat2,vidat=vi,vhist=vhist,fold=plotfolder,spp=spp) })
  
  
  mlist<-mortvec
  #This function creates a table with updated stats for all animals
  tabby<-tabFun(vhist=vhist,mlist=mlist,vi=vi,viout=viout,outtra=mdat2,spp='deer')
  
  tabby<-tabby[,c(1,4,8,11:19)]
  
  colnames(tabby)<-c('Serial','Mom Freq','VIT Freq','VitStatusChange',
                     'VitStatusChange_3Day','EventEasting','EventNorthing','CurrentVitStatus','LatestTelemdate','LatestEasting',
                     'LatestNorthing','DistFromEvent')
  
  #tabby<-tabby[,-8]
  
  saveRDS(tabby,file=datastore)
  
  
  # ilist<-list.files('C:/Users/mhayes1/Desktop/fawnStuff/nimages',full.names=T,pattern='.png$')
  # for(i in 1:length(ilist)){
  #   file.remove(ilist[i])
  # }
  # system.time({ nmaps<-nmapFun(ppi=75,vhist=vhist,mlist=mlist,vi=vi,viout=viout,outtra=mdat2,fold='/home/mhayes1/Desktop/DEERPTesting/nimages') })
  
  
  #####
  
  #system.time({ densmap(gd=id,tit='Elk Densities: May 2016',time=14,filen='C:/Users/mhayes1/Desktop/DensMapElk_05242016.png') })
  # idl<-read.table('C:/Users/mhayes1/Desktop/fawnStuff/DeerDat.txt',sep=',',header=T)
  
  PrettyData(dat=mdat2,idl=tabby,filen=PrettyDataStore)
  
  }