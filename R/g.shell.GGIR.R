g.shell.GGIR = function(mode=c(1,2),datadir=c(),outputdir=c(),studyname=c(),f0=1,f1=0,windowsizes = c(5,900,3600),
                   desiredtz = "Europe/London", 
                   do.enmo = TRUE,do.angle = FALSE,
                   do.lfenmo = FALSE,do.en = FALSE,
                   do.bfen = FALSE,do.hfen=FALSE,
                   do.hfenplus = FALSE,do.teLindert2013=FALSE,
                   do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,
                   do.enmoa = FALSE,
                   chunksize=1,do.cal=TRUE,
                   strategy = 1,hrs.del.start = 0.5,hrs.del.end = 0.5, maxdur = 7,
                   includedaycrit = 16, L5M5window = c(0,24), M5L5res = 10, 
                   winhr = 5, qwindow=c(0,24), qlevels = c(0.1), ilevels = c(0,10),
                   mvpathreshold = c(100),  boutcriter = 0.8,do.imp=TRUE,
                   idloc=1, lb = 0.2, hb = 15,  n = 4,
                   use.temp=TRUE,spherecrit=0.3,minloadcrit=72,printsummary=FALSE,ndayswindow=7,print.filename=FALSE) {
  
  
  # verify whether datadir is a directory or a list of files
  filelist = FALSE
  if (length(datadir) == 1) { #could be a directory or one file
    if (length(unlist(strsplit(datadir,"[.]bi")))>1) filelist = TRUE
    if (length(unlist(strsplit(datadir,"[.]cs")))>1) filelist = TRUE
  } else { #multiple files
    filelist = TRUE
  }
  derivef0f1 = FALSE
  if (length(f0) == 0 | length(f1) == 0) {
    derivef0f1 = TRUE
  } else {
    if (f0 == 0 | f1 == 0) derivef0f1 = TRUE
  }
  # What file to start with?
  if (derivef0f1 == TRUE) {
    f0 = 1
    # What file to end with?
    if (filelist == FALSE) {
      f1 = length(c(dir(datadir,recursive=TRUE,pattern="csv"),dir(datadir,recursive=TRUE,pattern="bin"))) #10
    } else {
      f1 = length(dir(datadir))
    }
  }
  dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = FALSE
  if (length(which(mode == 0)) > 0) {
    dopart1 = TRUE
    dopart2 = TRUE
  } else {
    if (length(which(mode == 1)) > 0) dopart1 = TRUE
    if (length(which(mode == 2)) > 0) dopart2 = TRUE
  }
  outputfoldername = unlist(strsplit(datadir,"/"))[length(unlist(strsplit(datadir,"/")))]
  metadatadir = paste(outputdir,"/output_",outputfoldername,sep="")
  if (dopart1 == TRUE) {
    print("=======================================================")
    print("Running g.shell: Part 1...")
    g.part1(datadir=datadir,outputdir=outputdir,f0=f0,f1=f1,windowsizes = windowsizes, 
            desiredtz = "Europe/London",chunksize=chunksize,studyname=studyname,
            do.enmo = do.enmo,do.angle = do.angle,
            do.lfenmo = do.lfenmo,do.en = do.en,
            do.bfen = do.bfen,do.hfen=do.hfen,
            do.hfenplus = do.hfenplus,
            do.teLindert2013=do.teLindert2013,
            do.anglex=do.anglex,do.angley=do.angley,do.anglez=do.anglez,
            do.enmoa = do.enmoa,
            do.cal = do.cal,print.filename=print.filename)
  }
  if (dopart2 == TRUE) {
    if (f1 == 0) { #for part 2 only
      f1 = length(dir(paste(metadatadir,"/meta/basic",sep="")))
    }
    print("=======================================================")
    print("Running g.shell: Part 2...")
    g.part2(metadatadir=metadatadir,f0=f0,f1=f1,strategy = strategy, hrs.del.start = hrs.del.start,hrs.del.end = hrs.del.end,
            maxdur =  maxdur, includedaycrit = includedaycrit,
            L5M5window = L5M5window, M5L5res = M5L5res, winhr = winhr,
            qwindow=qwindow, qlevels = qlevels,
            ilevels = ilevels, mvpathreshold = mvpathreshold,
            boutcriter = boutcriter,ndayswindow=ndayswindow,idloc=idloc,do.imp=do.imp)
  }
}