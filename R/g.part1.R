g.part1 = function(datadir=c(),outputdir=c(),f0=1,f1=c(),windowsizes = c(5,900,3600),
                   desiredtz = "Europe/London",chunksize=c(),studyname=c(),
                   do.enmo = TRUE,do.angle = FALSE,
                   do.lfenmo = FALSE,do.en = FALSE,
                   do.bfen = FALSE,do.hfen=FALSE,
                   do.hfenplus = FALSE,do.teLindert2013=FALSE,do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,
                   do.enmoa = FALSE,
                   do.cal = TRUE,
                   lb = 0.2, hb = 15,  n = 4,
                   use.temp=TRUE,spherecrit=0.3,minloadcrit=72,printsummary=FALSE,print.filename=FALSE) {
  if (length(datadir) == 0 | length(outputdir) == 0) {
    if (length(datadir) == 0) {
      print("Variable datadir is not defined")
    }
    if (length(outputdir) == 0) {
      print("Variable outputdir is not specified")
    }
  }
  if (f1 == 0) print("Warning: f1 = 0 is not a meaningful value")
  path1 = datadir
  path2 = outputdir
  filelist = FALSE
  #   verify whether path1 is a directory or a list of files
  if (length(path1) == 1) { #could be a directory or one file
    if (length(unlist(strsplit(path1,".bi")))>1) filelist = TRUE
    if (length(unlist(strsplit(path1,".cs")))>1) filelist = TRUE
  } else { #multiple files
    filelist = TRUE    
  }
  # create output directory if it does not exist
  mainDir = path2
  if (filelist == TRUE) {
    if (length(studyname) == 0) {
      studyname = "mystudy"
      print("Error: studyname not specified in part1. Needed for analysing lists of files")
    } else {
      outputfolder = paste("/output_",studyname,sep="")
    }
  } else {
    outputfolder = unlist(strsplit(path1,"/"))
    outputfolder = paste("/output_",outputfolder[length(outputfolder)],sep="")
  }
  if (file.exists(paste(mainDir,outputfolder,sep=""))) {
  } else {
    dir.create(file.path(mainDir,outputfolder))
    dir.create(file.path(mainDir,outputfolder,"meta"))
    dir.create(file.path(mainDir,paste(outputfolder,"/meta",sep=""),"basic"))
    dir.create(file.path(mainDir,outputfolder,"results"))
    dir.create(file.path(mainDir,paste(outputfolder,"/results",sep=""),"QC"))
  }
  path3 = paste(path2,outputfolder,sep="") #where is output stored?
  use.temp = TRUE; 
  daylimit = FALSE
  #   dohfenplus = FALSE
  #=================================================================
  # loading required functions and packages
  library(MASS)
  require(bitops)
  require(signal)
  require(matlab)
  require(mmap)
  require(GENEAread)
#   require(GGIR)
  if (do.angle == TRUE | do.anglex == TRUE | do.angley == TRUE | do.anglez == TRUE) {
    require(zoo)
  }
  #=================================================================
  # Other parameters:
  # list of all csv and bin files
  if (filelist == FALSE) {
    fnames = c(dir(path1,recursive=TRUE,pattern="csv"),dir(path1,recursive=TRUE,pattern="bin"))
  } else {
    fnames = path1
  }
  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)

  if (is.na(fnames[1]) == TRUE) {
    print("Error: File path not clearly identified. Check path name")
  }
  if (f0 > length(fnames)) f0 = 1
  if (f1 > length(fnames)) f1 = length(fnames)
  #========================================================================
  # check which files have already been processed, such that no double work is done
  # ffdone a matrix with all the binary filenames that have been processed
  ffdone = fdone = dir(paste(path2,outputfolder,"/meta/basic",sep=""))
  if (length(fdone) > 0) {
    for (ij in 1:length(fdone)) {
      tmp = unlist(strsplit(fdone[ij],".RData"))
      tmp2 = unlist(strsplit(tmp[1],"meta_"))
      ffdone[ij] = tmp2[2] 
    }
  } else {
    ffdone = c()
  }
  #=================================================================
  # THE LOOP TO RUN THROUGH ALL BINARY FILES AND PROCES THEM
  if (length(fnames) == 0) {
    print("no files to analyse")
  }
  filelocationkey = matrix("",length(fnames),3)
  fnames = sort(fnames)
  for (j in f0:f1) { #f0:f1 #j is file index (starting with f0 and ending with f1)
    if (print.filename == TRUE) {
      print(paste("File name: ",fnames[j],sep=""))
    }
    if (filelist == TRUE) {
      datafile = as.character(fnames[j])
    } else {
      datafile = paste(path1,"/",fnames[j],sep="")
    }
    print(paste("File ",j,sep=""))
    #=========================================================
    #check whether file has already been processed
    #by comparing filename to read with list of processed files
    fnames_without = as.character(unlist(strsplit(as.character(fnames[j]),".csv"))[1])
    # create list of both file name and full directory
    filelocationkey[j,1] = fnames_without
    #remove / if it was a list
    fnames_without2 = fnames_without
    teimp = unlist(strsplit(as.character(fnames_without),"/"))
    if (length(teimp) > 1) {
      fnames_without2 = teimp[length(teimp)]
    } else {
      fnames_without2 = fnames_without
    }
    fnames_without = fnames_without2
    filelocationkey[j,2] = fnames_without
    if (length(ffdone) > 0) {
      ffdone_without = 1:length(ffdone) #dummy variable
      for (index in 1:length(ffdone)) {
        ffdone_without[index] = as.character(unlist(strsplit(as.character(ffdone[index]),".csv"))[1])
      }
      if (length(which(ffdone_without == fnames_without)) > 0) { 
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    #================================================================
    # Inspect file (and store output later on)
    options(warn=-1) #turn off warnings
    I = g.inspectfile(datafile) 
    options(warn=0) #turn on warnings
    if (skip == 0) { #if skip = 1 then skip the analysis as you already processed this file
      
      if (do.cal ==TRUE) {
        print("---------------------------------------------")
        print("investigate calibration of the sensors...")
       C = g.calibrate(datafile,use.temp=use.temp,spherecrit=spherecrit,
                       minloadcrit=minloadcrit,printsummary=printsummary,chunksize=chunksize)
       } else {
        C = list(cal.error.end=0,cal.error.start=0)
        C$scale=c(1,1,1)
        C$offset=c(0,0,0)
        C$tempoffset=  c(0,0,0)
        C$QCmessage = "Autocalibration not done"
        C$npoints = 0
        C$nhoursused= 0
        C$use.temp = use.temp
      }
      cal.error.end = C$cal.error.end
      cal.error.start = C$cal.error.start
      
      if (length(cal.error.start) == 0) {
        #file too shortcorrupt to even calculate basic calibration value
        cal.error.start = NA
      }    
      if (is.na(cal.error.start) == T | length(cal.error.end) == 0) {
        C$scale = c(1,1,1); C$offset = c(0,0,0);       C$tempoffset=  c(0,0,0)
      } else {
        if (cal.error.start < cal.error.end) {
          C$scale = c(1,1,1); C$offset = c(0,0,0);       C$tempoffset=  c(0,0,0)
        }
      }
      #       print(windowsizes)
      #------------------------------------------------
      print("get meta data...")
      M = g.getmeta(datafile,                  
                    do.bfen=do.bfen,
                    do.enmo=do.enmo,
                    do.angle=do.angle,
                    do.lfenmo=do.lfenmo,
                    do.en=do.en,
                    do.hfen=do.hfen,
                    do.hfenplus=do.hfenplus,
                    do.teLindert2013=do.teLindert2013,
                    do.anglex=do.anglex,do.angley=do.angley,do.anglez=do.anglez,
                    do.enmoa=do.enmoa,
                    lb = lb, hb = hb,  n = n,
                    desiredtz=desiredtz,daylimit=daylimit,windowsizes=windowsizes,
                    tempoffset=C$tempoffset,scale=C$scale,offset=C$offset,
                    meantempcal=C$meantempcal,chunksize=chunksize)
      #------------------------------------------------
      print("save .RData-file with: calibration report, file inspection report and all meta data...")
      # remove directory in filename if present
      filename = unlist(strsplit(fnames[j],"/"))
      if (length(filename) > 0) {
        filename = filename[length(filename)]
      } else {
        filename = fnames[j]
      }
      save(M,I,C,file = paste(path3,"/meta/basic/meta_",filename,".RData",sep=""))
      SI = sessionInfo()  
      save(SI,file=paste(path3,"/results/QC/sessioninfo_part1.RData",sep=""))
      rm(M); rm(I); rm(C)
    } else {
      #  print("file skipped because it was analysed before")
    }
    if(length(filelocationkey) > 0) {
      filelocationkey[,3] = datadir
      filelocationkey = rbind(c("Filename with full path","Filename","data directory"),filelocationkey)
      write.csv(filelocationkey,paste(path3,"/results/QC/filelocationkey.csv",sep=""),row.names=FALSE)
    }
    closeAllConnections()
  }
}