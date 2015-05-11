g.shell.GGIR = function(mode=c(1,2),datadir=c(),outputdir=c(),studyname=c(),f0=1,f1=0,
                        overwrite=FALSE,do.report=c(2),...) {
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
  dopart1 = dopart2 = FALSE
  if (length(which(mode == 0)) > 0) {
    dopart1 = TRUE
    dopart2 = TRUE
  } else {
    if (length(which(mode == 1)) > 0) dopart1 = TRUE
    if (length(which(mode == 2)) > 0) dopart2 = TRUE
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
  if (length(which(ls() == "hrs.del.start")) == 0)  hrs.del.start = 0
  if (length(which(ls() == "hrs.del.end")) == 0)  hrs.del.end = 0
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
  if (length(which(ls() == "backup.cal.coef")) == 0)  backup.cal.coef = c()
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
  if (length(which(ls() == "do.cal")) == 0) do.cal = TRUE
  if (length(which(ls() == "print.filename")) == 0) print.filename = TRUE

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
  #==========================
  # Report generation:
  # check a few basic assumptions before continuoing
  if (length(which(do.report == 2)) > 0) {
    print("=======================================================")
    print("Generate report for part 2...")
    N.files.ms2.out = length(dir(paste(metadatadir,"/meta/ms2.out",sep="")))
    if (N.files.ms2.out < f1) f1 = N.files.ms2.out
    if (f1 == 0) f1 = N.files.ms2.out
    if (f1 == 0) print("Error: First run part2 (mode = 2) before you can generate reports")
    g.part2.report(metadatadir=metadatadir,f0=f0,f1=f1,maxdur=maxdur)
  }  
}
