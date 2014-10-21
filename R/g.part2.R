g.part2 = function(metadatadir=c(),f0=c(),f1=c(),strategy = 1, hrs.del.start = 0.5,hrs.del.end = 0.5,
                   maxdur = 7, includedaycrit = 16,
                   L5M5window = c(0,24), M5L5res = 10, winhr = 5,
                   qwindow=c(0,24), qlevels = c(0.1),
                   ilevels = c(0,10), mvpathreshold = c(100),
                   boutcriter = 0.8,ndayswindow=7,idloc=1,do.imp=TRUE) {
  # verify whether path1 is a directory or a list of files
#   path1 = datadir
#   path2 = outputdir
  outputfolder = unlist(strsplit(metadatadir,"/output_"))[2]
  outputfolder = paste("/output_",outputfolder,sep="")
  path1 = unlist(strsplit(metadatadir,"/output"))[1]
  snloc= 1
#   require(mmap)
  if (maxdur != 0) {
    durplot = maxdur
  } else {
    durplot = 7 #how many DAYS to plot? (only used if maxdur is not specified as a number above zero)
  }
  #---------------------------------
  # Specifying directories with meta-data and extracting filenames 
  path = paste(path1,outputfolder,"/meta/basic/",sep="")  #values stored per long epoch, e.g. 15 minutes
  fnames = dir(path)
  #---------------------------------
  # house keeping variables
  pdfpagecount = 1 # counter to keep track of files being processed (for pdf)
  pdffilenumb = 1 #counter to keep track of number of pdf-s being generated
  daySUMMARY = c()
  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)
  #---------------------------------
  # Loop through all the files
  for (i in f0:f1) { #1:length(fnames) length(fnames)
    print(paste("File ",i,sep=""))
    if (pdfpagecount == 301) { # generate new pdf for every 300 plots
      pdfpagecount = 1
      pdffilenumb = pdffilenumb + 1
      dev.off()
    }
    if (pdfpagecount == 1) {
      pdf(paste(path1,outputfolder,"/results/QC/plots to check data quality ",pdffilenumb,".pdf",sep=""),width=7,height=7)
    }
    M = c()
    load(paste(path,fnames[i],sep="")) #reading RData-file
    if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
      IMP = g.impute(M,I,strategy=strategy,hrs.del.start=hrs.del.start,
                     hrs.del.end=hrs.del.end,maxdur=maxdur,ndayswindow = ndayswindow)
      if (do.imp==FALSE) { #for those interested in sensisitivity analysis
        IMP$metashort = M$metashort
        IMP$metalong = M$metalong
      }
      SUM = g.analyse(I,C,M,IMP,qlevels=qlevels,qwindow=qwindow,L5M5window=L5M5window,M5L5res=M5L5res,
                      includedaycrit=includedaycrit,ilevels=ilevels,winhr=winhr,idloc=idloc,
                      mvpathreshold =mvpathreshold ,boutcriter=boutcriter)
#       get.TAC.vars=FALSE,anglethreshold=30
      Q = g.plot(IMP,M,I,durplot)
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        if (i == 1 | i == f0) {
          SUMMARY = SUM$summary
          SUM$summary$pdffilenumb = SUMMARY$pdffilenumb = pdffilenumb
          SUM$summary$pdfpagecount = SUMMARY$pdfpagecount = pdfpagecount
          daySUMMARY = SUM$daysummary
        } else {
          SUM$summary$pdffilenumb = pdffilenumb
          SUM$summary$pdfpagecount = pdfpagecount
          if (ncol(SUMMARY) == ncol(SUM$summary)) {
          } else {
            SUM$summary = cbind(SUM$summary,matrix(" ",1,(ncol(SUMMARY) - ncol(SUM$summary))))
            colnames(SUM$summary) = colnames(SUMMARY)
          }
          if (ncol(daySUMMARY) == ncol(SUM$daysummary)) {
          } else {
            SUM$daysummary = cbind(SUM$daysummary,matrix(" ",1,(ncol(daySUMMARY) - ncol(SUM$daysummary))))
            colnames(SUM$daysummary) = colnames(daySUMMARY)
          }
          if (length(which(colnames(daySUMMARY) != names(SUM$daysummary)) ) > 0) {
            names(SUM$daysummary) =   colnames(daySUMMARY)
          }
          SUMMARY = rbind(SUMMARY,SUM$summary)
          daySUMMARY = rbind(daySUMMARY,SUM$daysummary)
        }
      }
    }
    #-----------------
    # create data quality report
    if (length(C$cal.error.end) == 0) C$cal.error.end = " "
    if (M$filetooshort == TRUE | M$filecorrupt == TRUE) { 
      C$cal.error.start = 0
      C$npoints = 0
    }
    tm = which(colnames(M$metalong) == "temperaturemean")
    if (length(tm) > 0) {
      tmean = as.character(mean(as.numeric(as.matrix(M$metalong[1:(nrow(M$metalong)-1),tm]))))
    } else {
      tmean= ""
    }
    #=========
    header= I$header
    mon = I$monn
    hnames = rownames(header)
    hvalues = as.character(as.matrix(header))
    pp = which(hvalues == "")
    hvalues[pp] = c("not stored in header")
    if (mon == "genea") {
      SN = hvalues[which(hnames == "Serial_Number")] #serial number
    } else if (mon == "geneactive") {
      SN = hvalues[which(hnames == "Device_Unique_Serial_Code")] #serial number
      if (I$dformn == "csv") { #if it was stored in csv-format then underscores were replaced by spaces (by company)
        SN = hvalues[which(hnames == "Device Unique Serial Code")] #serial number  		
      }
    } else if (mon == "actigraph") { #todo: create automatic extraction of information from actigraph fileheader
      SN = "not extracted" #gender
    }
    if (length(C$offset) == 0) {
      C$offset = C$translate
    }
    if (length(C$cal.error.start) == 0) {
      C$cal.error.start = " "
    }
    if (length(C$npoints) == 0) {
      C$npoints = " "
    }
    QC = data.frame(filename=fnames[i],
                    file.corrupt=M$filecorrupt,
                    file.too.short=M$filetooshort,
                    use.temperature=C$use.temp,
                    scale.x=C$scale[1], scale.y=C$scale[2], scale.z=C$scale[3],
                    offset.x=C$offset[1], offset.y=C$offset[2], offset.z=C$offset[3],
                    temperature.offset.x=C$tempoffset[1],  temperature.offset.y=C$tempoffset[2],
                    temperature.offset.z=C$tempoffset[3], 
                    cal.error.start=C$cal.error.start,
                    cal.error.end=C$cal.error.end,
                    n.10sec.windows=C$npoints,
                    n.hours.considered = C$nhoursused, QCmessage=C$QCmessage,mean.temp=tmean,
                    device.serial.number=SN,stringsAsFactors=FALSE)
    if (i == 1 | i == f0) {
      QCout = QC
    } else {
      if (ncol(QCout) == ncol(QC)) {
      } else {
        QC = cbind(QC,matrix(" ",1,(ncol(QCout) - ncol(QC))))
        colnames(QC) = colnames(QCout)
      }
      QCout = rbind(QCout,QC)
    }
    #---------------------------------------------------------------
    if (pdfpagecount == 100 | pdfpagecount == 200 | pdfpagecount == 300) {
      #store matrix temperarily to keep track of process
      write.csv(SUMMARY,paste(path1,outputfolder,"/results/summary.csv",sep=""),row.names=F)
      write.csv(daySUMMARY,paste(path1,outputfolder,"/results/daysummary.csv",sep=""),row.names=F)
      write.csv(QCout,paste(path1,outputfolder,"/results/QC/data_quality_report.csv",sep=""),row.names=F)
    }
    pdfpagecount = pdfpagecount + 1
    if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
    rm(M); rm(I)
  }
  dev.off()
#   # store final matrices
#   write.csv(SUMMARY,paste(path1,outputfolder,"/results/summary.csv",sep=""),row.names=F)
#   write.csv(daySUMMARY,paste(path1,outputfolder,"/results/daysummary.csv",sep=""),row.names=F)
#   write.csv(QCout,paste(path1,outputfolder,"/results/QC/data_quality_report.csv",sep=""),row.names=F)
  #===============================================================================
  #now delete rows with incorrect participants
  # store final matrices again
  write.csv(SUMMARY,paste(path1,outputfolder,"/results/summary.csv",sep=""),row.names=F)
  write.csv(daySUMMARY,paste(path1,outputfolder,"/results/daysummary.csv",sep=""),row.names=F)
  write.csv(QCout,paste(path1,outputfolder,"/results/QC/data_quality_report.csv",sep=""),row.names=F)
  SI = sessionInfo()  
  save(SI,file=paste(path1,outputfolder,"/results/QC/sessioninfo_part2.RData",sep=""))
}