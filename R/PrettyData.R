#' @title Make data pretty
#'
#' @description Merge movement data and last fix information into pretty data for markdown
#' @param dat movement data
#' @param idl output of PartTab function
#' @param filen path to save rds file
#'
#' @return Pretty data for markdown file
#' @keywords markdown
#' @export
#' @examples
#' \donttest{L12(dat=mdat2,idl=tabby,filen='/home/mhayes1/Desktop/DEERPTesting/L12.rds')}
#'

L12<-function(dat,idl,filen='C:/Users/mhayes1/Desktop/fawnStuff/L12.rds'){

  dat$CollarSerialNumber<-as.character(as.numeric(as.character(dat$CollarSerialNumber)))

  idl$Serial<-as.character(idl$Serial)

  dat<-merge(dat,idl,by.x='CollarSerialNumber',by.y='Serial')

  uni<-unique(dat[,19])

  #dat<-as.data.frame(dat)

  outs<-data.frame()
  for(i in 1:length(uni)){
    s<-dat[which(dat[,19]==uni[i]),]

    s<-s[order(s$TelemDate,decreasing=T),]

    s<-s[c(1:12),]

    s<-s[,c(19,20,1,2,6,7)]

    outs<-rbind(outs,s)

  }

  outs$Easting<-floor(outs$Easting)
  outs$Northing<-floor(outs$Northing)

  saveRDS(outs,file=filen)


}
