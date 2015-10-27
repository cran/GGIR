g.shell.GGIR = function(mode=c(1,2),datadir=c(),outputdir=c(),studyname=c(),f0=1,f1=0,
                        do.report=c(2),overwrite=FALSE,visualreport=FALSE,...) {
  #get input variables
  input = list(...)
  for (i in 1:length(names(input))) {
    txt = paste(names(input)[i],"=",input[i],sep="")
    if (class(unlist(input[i])) == "character") {
      txt = paste(names(input)[i],"='",unlist(input[i]),"'",sep="")
    }
    eval(parse(text=txt))
  }
  if (length(which(ls() == "timewindow")) != 0) timewindow = input$timewindow
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
      f1 = length(datadir) #modified
    }
  }
  dopart1 = dopart2 = dopart3 = dopart4 = dopart5 = FALSE
  if (length(which(mode == 0)) > 0) {
    dopart1 = TRUE
    dopart2 = TRUE
    dopart3 = TRUE
    dopart4 = TRUE
  } else {
    if (length(which(mode == 1)) > 0) dopart1 = TRUE
    if (length(which(mode == 2)) > 0) dopart2 = TRUE
    if (length(which(mode == 3)) > 0) dopart3 = TRUE; do.anglez = TRUE
    if (length(which(mode == 4)) > 0) dopart4 = TRUE
  }
  if (filelist == TRUE) {
    metadatadir = paste(outputdir,"/output_",studyname,sep="")
  } else {
    outputfoldername = unlist(strsplit(datadir,"/"))[length(unlist(strsplit(datadir,"/")))]
    metadatadir = paste(outputdir,"/output_",outputfoldername,sep="")
  }
  # obtain default parameter values if not provided:
  if (length(which(ls() == "overwrite")) == 0)  overwrite = FALSE
  if (length(which(ls() == "strategy")) == 0)  strategy = 1
  if (length(which(ls() == "maxdur")) == 0)  maxdur = 7
  if (length(which(ls() == "do.cal")) == 0)  do.cal = TRUE
  if (length(which(ls() == "hrs.del.start")) == 0)  hrs.del.start = 0
  if (length(which(ls() == "hrs.del.end")) == 0)  hrs.del.end = 0
  if (length(which(ls() == "loglocation")) == 0)  loglocation = c()
  if (length(which(ls() == "storefolderstructure")) == 0)  storefolderstructure = FALSE
  if (length(which(ls() == "windowsizes")) == 0)  windowsizes = c(5,900,3600)
  if (length(which(ls() == "desiredtz")) == 0)  desiredtz = "Europe/London"
  if (length(which(ls() == "chunksize")) == 0)  chunksize = 1
  if (length(which(ls() == "do.enmo")) == 0)  do.enmo = TRUE
  if (length(which(ls() == "do.lfenmo")) == 0)  do.lfenmo = FALSE
  if (length(which(ls() == "do.en")) == 0)  do.en = FALSE
  if (length(which(ls() == "do.bfen")) == 0)  do.bfen = FALSE
  if (length(which(ls() == "do.hfen")) == 0)  do.hfen = FALSE
  if (length(which(ls() == "do.hfenplus")) == 0)  do.hfenplus = FALSE
  if (length(which(ls() == "do.teLindert2013")) == 0)  do.teLindert2013 = FALSE
  if (length(which(ls() == "do.anglex")) == 0)  do.anglex = FALSE
  if (length(which(ls() == "do.angley")) == 0)  do.angley = FALSE
  if (length(which(ls() == "do.anglez")) == 0)  do.anglez = FALSE
  if (length(which(ls() == "do.enmoa")) == 0)  do.enmoa = FALSE
  if (length(which(ls() == "printsummary")) == 0)  printsummary = FALSE
  if (length(which(ls() == "includedaycrit")) == 0)  includedaycrit = 16
  if (length(which(ls() == "L5M5window")) == 0)  L5M5window = c(0,24)
  if (length(which(ls() == "M5L5res")) == 0)  M5L5res = 10
  if (length(which(ls() == "winhr")) == 0)  winhr = 5
  if (length(which(ls() == "qwindow")) == 0)  qwindow = c(0,24)
  if (length(which(ls() == "qlevels")) == 0)  qlevels = c()
  if (length(which(ls() == "ilevels")) == 0)  ilevels = c()
  if (length(which(ls() == "mvpathreshold")) == 0)  mvpathreshold = 100
  if (length(which(ls() == "boutcriter")) == 0)  boutcriter = 0.8
  if (length(which(ls() == "ndayswindow")) == 0)  ndayswindow = 7
  if (length(which(ls() == "idloc")) == 0) idloc = 1
  if (length(which(ls() == "do.imp")) == 0) do.imp = TRUE
  if (length(which(ls() == "anglethreshold")) == 0)  anglethreshold = 5
  if (length(which(ls() == "timethreshold")) == 0)  timethreshold = 5
  if (length(which(ls() == "ignorenonwear")) == 0)  ignorenonwear = FALSE
  if (length(which(ls() == "idloc")) == 0)  idloc=1
  if (length(which(ls() == "coldid")) == 0)  colid=1
  if (length(which(ls() == "coln1")) == 0)  coln1=1
  if (length(which(ls() == "nnights")) == 0)  nnights=1
  if (length(which(ls() == "outliers.only")) == 0)  outliers.only=FALSE
  if (length(which(ls() == "excludefirstlast")) == 0)  excludefirstlast=FALSE
  if (length(which(ls() == "criterror")) == 0)  criterror=3
  if (length(which(ls() == "includenightcrit")) == 0)  includenightcrit=16
  if (length(which(ls() == "relyonsleeplog")) == 0)  relyonsleeplog=FALSE
  if (length(which(ls() == "sleeplogidnum")) == 0)  sleeplogidnum=TRUE
  if (length(which(ls() == "def.noc.sleep")) == 0)  def.noc.sleep=c()
  if (length(which(ls() == "do.visual")) == 0)  do.visual=FALSE
  if (length(which(ls() == "excludefirstlast")) == 0)  excludefirstlast = FALSE
  if (length(which(ls() == "viewingwindow")) == 0)  viewingwindow = 1
  if (length(which(ls() == "dofirstpage")) == 0)  dofirstpage = TRUE
  if (length(which(ls() == "visualreport")) == 0)  visualreport = FALSE
  if (length(which(ls() == "print.filename")) == 0)  print.filename = FALSE
  
  #-----------------------------------------------------------------
  if (dopart1 == TRUE) {
    print("=======================================================")
    print("Running g.shell: Part 1...")
    g.part1(datadir=datadir,outputdir=outputdir,f0=f0,f1=f1,windowsizes = windowsizes, 
            desiredtz = desiredtz,chunksize=chunksize,studyname=studyname,
            do.enmo = do.enmo,
            do.lfenmo = do.lfenmo,do.en = do.en,
            do.bfen = do.bfen,do.hfen=do.hfen,
            do.hfenplus = do.hfenplus,
            do.teLindert2013=do.teLindert2013,
            do.anglex=do.anglex,do.angley=do.angley,do.anglez=do.anglez,
            do.enmoa = do.enmoa,printsummary=printsummary,
            do.cal = do.cal,print.filename=print.filename,
            overwrite=overwrite)
  }
  if (dopart2 == TRUE) {
    print("=======================================================")
    print("Running g.shell: Part 2...")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/basic",sep="")))
    g.part2(datadir =datadir ,metadatadir=metadatadir,f0=f0,f1=f1,strategy = strategy, 
            hrs.del.start = hrs.del.start,hrs.del.end = hrs.del.end,
            maxdur =  maxdur, includedaycrit = includedaycrit,
            L5M5window = L5M5window, M5L5res = M5L5res, winhr = winhr,
            qwindow=qwindow, qlevels = qlevels,
            ilevels = ilevels, mvpathreshold = mvpathreshold,
            boutcriter = boutcriter,ndayswindow=ndayswindow,idloc=idloc,do.imp=do.imp,
            storefolderstructure=storefolderstructure,overwrite=overwrite)
  }
  if (dopart3 == TRUE) {
    print("=======================================================")
    print("Running g.shell: Part 3...")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/basic",sep="")))
    g.part3(metadatadir=metadatadir,f0=f0,
            f1=f1,anglethreshold=anglethreshold,timethreshold=timethreshold,
            ignorenonwear=ignorenonwear,overwrite=overwrite)
  }
  if (dopart4 == TRUE) {
    print("=======================================================")
    print("Running g.shell: Part 4...")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/ms3.out",sep="")))
    g.part4(datadir=datadir,metadatadir=metadatadir,loglocation = loglocation,
            f0=f0,f1=f1,idloc=idloc, colid = colid,coln1 = coln1,nnights = nnights,
            outliers.only = outliers.only,
            excludefirstlast=excludefirstlast,criterror = criterror,
            includenightcrit=includenightcrit,relyonsleeplog=relyonsleeplog,
            sleeplogidnum=sleeplogidnum,def.noc.sleep=def.noc.sleep,do.visual = do.visual, #
            storefolderstructure=storefolderstructure,overwrite=overwrite)
  }
  #==========================
  # Report generation:
  # check a few basic assumptions before continuoing
  if (length(which(do.report==4)) > 0 | visualreport==TRUE) {
    if (file.exists(paste(metadatadir,"/meta/ms4.out",sep=""))) {
    } else {
      print("ERROR: First run g.shell.GGIR with mode = 4 to generate required milestone data")
      stop()
    }
  }  
  if (visualreport==TRUE & length(do.report) == 0) {
    if (file.exists(paste(metadatadir,"/meta/ms4.out",sep=""))) {
    } else {
      do.report=c(2,4) #visual report is impossible without creating quantitative reports first
    }
  }  
  if (length(which(do.report == 2)) > 0) {
    print("=======================================================")
    print("Generate report for part 2...")
    N.files.ms2.out = length(dir(paste(metadatadir,"/meta/ms2.out",sep="")))
    if (N.files.ms2.out < f1) f1 = N.files.ms2.out
    if (f1 == 0) f1 = N.files.ms2.out
    g.report.part2(metadatadir=metadatadir,f0=f0,f1=f1,maxdur=maxdur)
  }
  if (length(which(do.report == 4)) > 0) {
    print("=======================================================")
    print("Generate report for part 4...")
    N.files.ms4.out = length(dir(paste(metadatadir,"/meta/ms4.out",sep="")))
    if (N.files.ms4.out < f1) f1 = N.files.ms4.out
    if (f1 == 0) f1 = N.files.ms4.out
    g.report.part4(datadir=datadir,metadatadir=metadatadir,loglocation =loglocation,f0=f0,f1=f1,
                   storefolderstructure=storefolderstructure)
  }
  
  if (visualreport == TRUE) {
    print("=======================================================")
    print("Generate visual reports...")
    if (f1 == 0) f1 = length(dir(paste(metadatadir,"/meta/ms4.out",sep="")))
    g.plot5(metadatadir=metadatadir,dofirstpage=dofirstpage,
            viewingwindow=viewingwindow,f0=f0,f1=f1,overwrite=overwrite)
  }
  
}