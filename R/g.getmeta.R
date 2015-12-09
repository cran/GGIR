g.getmeta = function(datafile,desiredtz = "Europe/London",windowsizes = c(5,900,3600),
                     daylimit=FALSE,offset=c(0,0,0),scale=c(1,1,1),tempoffset = c(0,0,0),
                     do.bfen=FALSE,do.enmo=TRUE,do.lfenmo=FALSE,
                     do.en=FALSE,do.hfen=FALSE,
                     do.hfenplus=FALSE,do.teLindert2013=FALSE,
                     do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,do.enmoa=FALSE,
                     lb = 0.2, hb = 15,  n = 4,meantempcal=c(),chunksize=c()) {
  if (length(chunksize) == 0) chunksize = 1
  if (chunksize > 1) chunksize = 1
  if (chunksize < 0.2) chunksize = 0.2 
  nmetrics = sum(c(do.bfen,do.enmo,do.lfenmo,do.en,do.hfen,do.hfenplus,
                   do.teLindert2013,do.anglex,do.angley,do.anglez,do.enmoa))
  if (length(nmetrics) == 0) {
    cat("\nWARNING: No metrics selected\n")
  }
  filename = unlist(strsplit(as.character(datafile),"/"))
  filename = filename[length(filename)]
  #   #parameters
  ws3 = windowsizes[1] ; ws2 = windowsizes[2]; ws = windowsizes[3]  #window sizes
  if ((ws2/300) != round(ws2/300)) {
    ws2 = as.numeric(300 * round(ws2/300))
    cat("\nWARNING: The long windowsize needs to be a multitude of five minutes periods. The\n")
    cat(paste("\nlong windowsize has now been automatically adjusted to: ",ws2," seconds in order to meet this criteria.\n",sep=""))
  }
  if ((ws2/ws3) != round(ws2/ws3)) {
    def = c(1,5,10,15,20,30,60)
    def2 = abs(def - ws3)
    ws3 = as.numeric(def[which(def2 == min(def2))])
    cat("\nWARNING: The long windowsize needs to be a multitude of short windowsize. The \n")
    cat(paste("\nshort windowsize has now been automatically adjusted to: ",ws3," seconds in order to meet this criteria.\n",sep=""))
  }
  windowsizes = c(ws3,ws2,ws)
  start_meas = ws2/60 #ensures that first window starts at logical timepoint relative to its size (15,30,45 or 60 minutes of each hour) 
  monnames = c("genea","geneactive","actigraph") #monitor names
  filecorrupt = FALSE
  filetooshort = FALSE
  i = 1 #counter to keep track of which binary block is being read
  count = 1 #counter to keep track of the number of seconds that have been read
  count2 = 1 #count number of blocks read with length "ws2" (15 minutes or whatever is specified above)
  LD = 2 #dummy variable used to identify end of file and to make the process stop
  bsc_cnt = 0
  bsc_qc = data.frame(time=c(),size=c())
  # inspect file
  options(warn=-1)
  INFI = g.inspectfile(datafile)  # Check which file type and monitor brand it is
  options(warn=0)
  mon = INFI$monc
  dformat = INFI$dformc
  sf = INFI$sf
  if (sf == 0) sf = 80 #assume 80Hertz in the absense of any other info
  header = INFI$header
  options(warn=-1)
  decn =g.dotorcomma(datafile,dformat,mon=mon)
  options(warn=0)
  # setting size of blocks that are loaded (too low slows down the process)
  # the setting below loads blocks size of 24 hours (modify if causing memory problems)
  blocksize = round(14512 * (sf/50) * chunksize)
  blocksizegenea = round(21467 * (sf/80)  * chunksize)
  if (mon == 3 & dformat == 2) { #added 28/7/2014
    blocksize = round(blocksize/5)
  }
  if (mon == 1) {
    blocksize = blocksizegenea
  } 
  if (dformat == 1) {
    if (mon == 1) { #reading the binary file
      id = as.character(header[which(header[,1] == "Volunteer_Number"),2])
    } else if (mon == 2) { #reading the binary file
      id = as.character(header[which(header[,1] == "Subject_Code"),2])
    }
  } else if (dformat == 2) {
    if (mon == 2) {
      id = as.character(header[which(as.character(header[,1]) == "Subject Code"),2])
    } else if (mon == 3) {
      id = filename #id not stored in fileheader, but filename instead
    }
  }
  #creating matrixes for storing output
  S = matrix(0,0,4) #dummy variable needed to cope with head-tailing succeeding blocks of data
  NR = ceiling((90*10^6) / (sf*ws3)) + 1000 #NR = number of 'ws3' second rows (this is for 10 days at 80 Hz) 
  metashort = matrix(" ",NR,(1+nmetrics)) #generating output matrix for acceleration signal
  if (mon == 1 | mon == 3) {
    temp.available = FALSE
  } else if (mon == 2){
    temp.available = TRUE
  }
  if (temp.available == FALSE) {
    metalong = matrix(" ",(((90*10^6)/(sf*ws2))+100),4) #generating output matrix for 15 minutes summaries
  } else if (temp.available == TRUE){
    metalong = matrix(" ",(((90*10^6)/(sf*ws2))+100),7) #generating output matrix for 15 minutes summaries
  }
  #===============================================
  # Read file
  switchoffLD = 0 #dummy variable part "end of loop mechanism"
  while (LD > 1) {
    P = c()
    cat(paste("\nLoading block: ",i,"\n",sep=""))
    options(warn=-1) #turn off warnings (code complains about unequal rowlengths
    #when trying to read files of a different format)
    if (mon == 1 & dformat == 1) {
      try(expr={P = g.binread(binfile=datafile,(blocksize*(i-1)),(blocksize*i))},silent=TRUE)
      if (length(P) > 1) {
        if (nrow(P$rawxyz) < ((sf*ws*2)+1) & i == 1) {
          P = c() ; switchoffLD = 1 #added 30-6-2012
          cat("\nError: data too short for doing non-wear detection 1\n")		
          filetooshort = TRUE
        }
      } else {
        P = c()
        if (i == 1) {
          filecorrupt = TRUE
        }
        cat("\nEnd of file reached\n")
      }
    } else if (mon == 2 & dformat == 1) {
      try(expr={P = GENEAread::read.bin(binfile=datafile,start=(blocksize*(i-1)),
                                        end=(blocksize*i),calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
      if (length(P) <= 2) {
        cat("\ninitial attempt to read data unsuccessful, try again with mmap turned on:\n")
        #try again but now with mmap.load turned on
        try(expr={P = GENEAread::read.bin(binfile=datafile,start=(blocksize*(i-1)),
                                          end=(blocksize*i),calibrate=TRUE,do.temp=TRUE,mmap.load=TRUE)},silent=TRUE)
        if (length(P) != 0) {
          cat("\ndata read succesfully\n")
        } else {
          switchoffLD = 1
        }
      }
      if (length(P) > 0) {
        if (nrow(P$data.out) < (blocksize*300)) { #last block
          cat("\nlast block\n")
          switchoffLD = 1
        }
      }
      if (length(P) == 0) { #if first block doens't read then probably corrupt
        if (i == 1) {
          #try to read without specifying blocks (file too short)
          try(expr={P = GENEAread::read.bin(binfile=datafile,calibrate=TRUE,do.temp=TRUE,mmap.load=FALSE)},silent=TRUE)
          if (length(P) == 0) {
            cat("\nError: file possibly corrupt\n")
            P= c(); switchoffLD = 1
            filecorrupt = TRUE
          } #if not then P is now filled with data
        } else {
          P= c() #just no data in this last block
          cat("\nnot enough data in this block 3\n")
        }
      }
      if (length(P) > 0) { #check whether there is enough data
        if (nrow(P$data.out) < ((sf*ws*2)+1) & i == 1) {
          P = c();  switchoffLD = 1
          cat("\nError code 2: data too short for doing non-wear detection\n")
          filetooshort = TRUE
        }
      }
      #===============
    } else if (mon == 2 & dformat == 2) {
      cat("\nGeneactiv in csv-format\n")
      try(expr={P = read.csv(datafile,nrow = (blocksize*300), skip=(100+(blocksize*300*(i-1))),header = FALSE,dec=decn)},silent=TRUE)
      if (length(P) > 1) {
        P = as.matrix(P)
        if (nrow(P) < ((sf*ws*2)+1) & i == 1) {
          P = c() ; switchoffLD = 1 #added 30-6-2012
          cat("\nError code 1: data too short for doing non-wear detection\n")		
          filetooshort = TRUE
        }
      } else {
        P = c()
        cat("\nEnd of file reached\n")
      }
    } else if (mon == 3 & dformat == 2) {
      try(expr={P = read.csv(datafile,nrow = (blocksize*300), skip=(10+(blocksize*300*(i-1))),dec=decn)},silent=TRUE)
      if (length(P) > 1) {
        P = as.matrix(P)
        if (nrow(P) < ((sf*ws*2)+1) & i == 1) {
          P = c() ; switchoffLD = 1 #added 30-6-2012
          cat("\nError code 1: data too short for doing non-wear detection\n")
          filetooshort = TRUE
        }
      } else {
        P = c()
        cat("\nEnd of file reached\n")
      }
    }
    options(warn=0) #turn on warnings
    #============
    #process data as read from binary file
    if (length(P) > 0) { #would have been set to zero if file was corrupt or empty
      if (mon == 1 & dformat == 1) {
        data = P$rawxyz / 1000 #convert g output to mg for genea
      } else if (mon == 2  & dformat == 1) {
        data = P$data.out
      } else if (dformat == 2) {
        data = P #as.matrix(P,dimnames = list(rownames(P),colnames(P)))
      }
      #add left over data from last time 
      if (nrow(S) > 0) {
        data = rbind(S,data)
      }
      if (mon == 2) {
        meantemp = mean(as.numeric(data[,7]),na.rm=TRUE)
        if (is.na(meantemp) == T) { #mean(as.numeric(data[1:10,7]))
          cat("\ntemperature is NA\n")
          meantemp = 0
        } else if (mean(as.numeric(data[1:10,7])) > 50) {
          cat("\ntemperature value is unreaslistically high (> 50 Celcius)\n")
          meantemp = 0
        }
      }
      # extraction and modification of starting point of measurement
      if (i == 1) { #only do this for first block of data
        if (mon  == 1 & dformat == 1) {
          starttime = P$timestamps2[1]
          lengthheader = nrow(header)
        } else if (mon == 2 & dformat == 1) {
          starttime = P$page.timestamps[1]
          if (length(unlist(strsplit(as.character(starttime),":"))) < 2) {
            starttime = P$page.timestamps[2] #needed for MaM study where first timestamp does not have clock time in it
          }
          lengthheader = nrow(header) #length(unlist(H))
        } else if (dformat == 2 & mon == 2) {
          starttime = as.character(P[1,1])
          starttime = as.POSIXlt(starttime)
          lengthheader = 20
        } else if (dformat == 2 & (mon == 3 | mon == 4)) {
          if (mon == 3) {
            tmph = read.csv(datafile,nrow=8,skip=1)
            tmphi = 1
            while (tmphi < 10) {
              if (length(unlist(strsplit(as.character(tmph[tmphi,1]),"Start Time"))) > 1) {
                break
              }
              tmphi = tmphi + 1
            }
            starttime = unlist(strsplit(as.character(tmph[tmphi,1]),"Start Time"))[2]
            #-------------------------------
            tmphi = 1
            while (tmphi < 10) {
              if (length(unlist(strsplit(as.character(tmph[tmphi,1]),"Start Date"))) > 1) {
                break
              }
              tmphi = tmphi + 1
            }
            startdate = unlist(strsplit(as.character(tmph[tmphi,1]),"Start Date"))[2]
            startdate = as.character(unlist(strsplit(as.character(startdate)," "))) 
            starttime = as.character(unlist(strsplit(as.character(starttime)," ")))
          }
          #-----------------------------------------
          #remove possible spaces in date or time
          newstarttime = starttime #20-11-2014
          newstartdate = startdate #20-11-2014
          if (length(startdate) > 1) {
            for (rpsi in 1:length(startdate)) {
              if (length(unlist(strsplit(startdate[rpsi],"")))>1) {
                newstartdate = startdate[rpsi]
              }
            }
          }
          if (length(starttime) > 1) {
            for (rpsi in 1:length(starttime)) {
              if (length(unlist(strsplit(starttime[rpsi],"")))>1) {
                newstarttime = starttime[rpsi]
              }
            }
          }
          starttime = newstarttime
          startdate = newstartdate
          #-----------------------------------------
          # flexible four date/time formats
          starttime = paste(startdate," ",starttime,sep="")
          getOption("digits.secs")
          options(digits.secs = 3)
          if (mon == 3) {
            options(warn=-1)
            topline = as.matrix(colnames(as.matrix(read.csv(datafile,nrow=1,skip=0))))
            options(warn=0)
            B1 = length(unlist(strsplit(topline,"MM[.]dd[.]yyyy")))
            B2 = length(unlist(strsplit(topline,"M[.]d[.]yyyy")))
            B3 = length(unlist(strsplit(topline,"dd[.]MM[.]yyyy")))
            B4 = length(unlist(strsplit(topline,"d[.]M[.]yyyy")))
            if (B1 > 1) {
              starttime = as.POSIXlt(starttime,format='%m/%d/%Y %H:%M:%S')
            } else if (B2 > 1) {
              starttime = as.POSIXlt(starttime,format='%m/%d/%Y %H:%M:%S')
            } else if (B3 > 1) {
              starttime = as.POSIXlt(starttime,format='%d/%m/%Y %H:%M:%S')
            } else if (B4 > 1) {
              starttime = as.POSIXlt(starttime,format='%d/%m/%Y %H:%M:%S')
            }
            lengthheader = 9
          }
        }
        #==================================================
        #inspection timezone
        timezone = attr(unclass(as.POSIXlt(starttime[1])),which="tzone")
        starttimebefore = as.POSIXlt(starttime)
        # assuming that timestamps is good, but that timezone might be lost in conversion from string to POSIXct
        if (dformat == 1) { #not sure whether this is required for csv-format (2)
          if (length(which(timezone == "GMT")) > 0) {
            starttime = as.POSIXlt(starttime[1],tz=desiredtz)
          }
        }
        #================================================
        #assess weekday
        wday = unclass(as.POSIXlt(starttime[1]))$wday #day of the week 0-6 and 0 is Sunday
        wday = wday + 1
        weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
        wdayname = weekdays[wday]
        #======================================================
        #assess how much data to delete till next 15 minute period
        temp = unlist(strsplit(as.character(starttime)," "))
        starttime2 = as.numeric(unlist(strsplit(temp[2],":")))
        if (length(which(is.na(starttime2) ==  TRUE)) > 0 | length(starttime2) ==0) { #modified on 5may2015
          starttime2 = c(0,0,0)
        }
        start_hr = as.numeric(starttime2[1])
        start_min = as.numeric(starttime2[2])
        start_sec = as.numeric(starttime2[3])
        secshift = 60 - start_sec #shift in seconds needed
        start_min = start_min +1 #shift in minutes needed (+1 one to account for seconds comp)
        #-----------
        minshift = start_meas - (((start_min/start_meas) - floor(start_min/start_meas)) * start_meas)
        minshift = minshift - 1
        #-----------
        sampleshift = (minshift*60*sf) + (secshift*sf) #derive sample shift
        data = data[-c(1:floor(sampleshift)),] #delete data accordingly
        newmin = start_min+minshift #recalculate first timestamp
        newsec = start_sec+secshift
        if (newsec >= 60) {
          newsec = newsec - 60
          newmin = newmin + 1
        }
        if (newmin >= 60) {
          newmin = newmin - 60
          start_hr = start_hr + 1
          if (start_hr == 24) { #if measurement is started in 15 minutes before midnight
            starttime3 = as.character(starttime) # forget about timeshift for now, not plausible
          }
        }
        starttime3 = paste(temp[1]," ",start_hr,":",newmin,":",newsec,sep="") #<<<====  changed 17-12-2013
        #create timestamp from string (now desiredtz is added)
        starttime_a = as.POSIXct(starttime3,format="%d/%m/%Y %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
        starttime_b = as.POSIXct(starttime3,format="%d-%m-%Y %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
        starttime_c = as.POSIXct(starttime3,format="%Y/%m/%d %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
        starttime_d = as.POSIXct(starttime3,format="%Y-%m-%d %H:%M:%S",tz=desiredtz) #,origin="1970-01-01"
        if (is.na(starttime_a) == FALSE) {
          starttime = starttime_a
        } else {
          if (is.na(starttime_b) == FALSE) {
            starttime = starttime_b
          } else {
            if (is.na(starttime_c) == FALSE) {
              starttime = starttime_c
            } else {
              if (is.na(starttime_d) == FALSE) {
                starttime = starttime_d
              } else {
                cat("\ndate not recognized\n")
              }
            }
          }
        }
      }
      LD = nrow(data)
      if (LD < (ws*sf)) {
        cat("\nError: data too short for doing non-wear detection 3\n")
        switchoffLD = 1
        LD = 0 #ignore rest of the data and store what has been loaded so far.
      }
      #store data that could not be used for this block, but will be added to next block
      if (LD != 0) {
        use = (floor(LD / (ws*sf))) * (ws*sf) #number of datapoint to use
        if (use != LD) {
          S = as.matrix(data[(use+1):LD,]) #store left over
          if (ncol(S) == 1) {
            S = t(S)
          }
        } else { #use all data
          S = matrix(0,0,ncol(data))
        }
        data = as.matrix(data[1:use,])
        LD = nrow(data) #redefine LD because there is less data
        ##==================================================
        # Feature calculation
        dur = nrow(data)	#duration of experiment in data points
        durexp = nrow(data) / (sf*ws)	#duration of experiment in hrs
        data = as.matrix(data)
        # Initialization of variables
        if (mon == 1) {
          data[,1:3] = scale(data[,1:3],center = -offset, scale = 1/scale) #rescale data
          Gx = as.numeric(data[,1]); Gy = as.numeric(data[,2]); Gz = as.numeric(data[,3])
        } else if (mon == 2 & dformat == 1) {
          yy = as.matrix(cbind(as.numeric(data[,7]),as.numeric(data[,7]),as.numeric(data[,7])))
          data[,2:4] = scale(as.matrix(data[,2:4]),center = -offset, scale = 1/scale) +
            scale(yy, center = rep(meantemp,3), scale = 1/tempoffset)  #rescale data
          Gx = as.numeric(data[,2]); Gy = as.numeric(data[,3]); Gz = as.numeric(data[,4])
        } else if (dformat == 2 & mon != 4) {
          data2 = matrix(NA,nrow(data),3)
          if (ncol(data) == 3) extra = 0
          if (ncol(data) >= 4) extra = 1
          for (jij in 1:3) {
            data2[,jij] = as.numeric(data[,(jij+extra)])
          }
          if (mon == 3) {
            data2[,1:3] = scale(data2[,1:3],center = -offset, scale = 1/scale)  #rescale data
          } else if (mon == 2) {
            yy = as.matrix(cbind(as.numeric(data[,7]),as.numeric(data[,7]),as.numeric(data[,7])))
            meantemp = mean(as.numeric(data[,7]))
            # meantemp replaced by meantempcal # 19-12-2013
            if (length(meantempcal) == 0) {
              meantempcal = meantemp
            }
            data2[,1:3] = scale(data2[,1:3],center = -offset, scale = 1/scale) +
              scale(yy, center = rep(meantempcal,3), scale = 1/tempoffset)  #rescale data
          }
          Gx = as.numeric(data2[,1]); Gy = as.numeric(data2[,2]); Gz = as.numeric(data2[,3])
        }       
        #--------------------------------------------------
        # BFEN = band pass filtered signals followed by Euclidean norm
        lb = 0.2; hb = 15; n = 4; TW = 1/lb
        if (sf <= (hb *2)) { #avoid having a higher filter boundary higher than sf/2
          hb = round(sf/2) - 1
        }
        if (do.bfen == TRUE) {
          BFEN = g.metric(Gx,Gy,Gz,n,sf=sf,ii=7,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
          #averaging per second
          BFEN2 =cumsum(c(0,BFEN))
          select = seq(1,length(BFEN2),by=sf)
          BFEN3 = diff(BFEN2[round(select)]) / abs(diff(round(select)))
          select = seq(1,length(BFEN2),by=sf*ws3)
          BFEN3b = diff(BFEN2[round(select)]) / abs(diff(round(select)))
        }
        #-----------------------------------------------------
        #deriving metric ENMO (Euclidean Norm Minus One)
        if (do.enmo == TRUE) {
          EN = sqrt(Gx^2 + Gy^2 + Gz^2)
          ENMO = EN - 1
          ENMO[which(ENMO < 0)] = 0 #turning negative values into zero
          #averaging per second
          ENMO2 =cumsum(c(0,ENMO))
          select = seq(1,length(ENMO2),by=sf)
          ENMO3 = diff(ENMO2[round(select)]) / abs(diff(round(select)))
          select = seq(1,length(ENMO2),by=sf*ws3)
          ENMO3b = diff(ENMO2[round(select)]) / abs(diff(round(select)))
        }
        #------------------------------------------------------------
        # space for extra metrics
        if (do.lfenmo == TRUE) {
          LFENMO =g.metric(Gx,Gy,Gz,n,sf=sf,ii=9,TW=TW,lb=lb,hb=3.5) #calling function metric.R to do the calculation
          LFENMO[which(LFENMO < 0)] = 0
          #averaging HFEN per ws3 seconds
          LFENMO2 =cumsum(c(0,LFENMO))
          select = seq(1,length(LFENMO),by=sf*ws3)
          LFENMO3b = diff(LFENMO2[round(select)]) / abs(diff(round(select)))
        }
        if (do.hfen == TRUE) {
          HFEN =g.metric(Gx,Gy,Gz,n,sf=sf,ii=1,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
          #averaging HFEN per ws3 seconds
          HFEN2 =cumsum(c(0,HFEN))
          select = seq(1,length(HFEN),by=sf*ws3)
          HFEN3b = diff(HFEN2[round(select)]) / abs(diff(round(select)))
        }
        # do this one anyway
        EN = g.metric(Gx,Gy,Gz,n,sf=sf,ii=3,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
        EN2 =cumsum(c(0,EN))
        select = seq(1,length(EN),by=sf*ws3)
        EN3b = diff(EN2[round(select)]) / abs(diff(round(select)))
        if (do.hfenplus == TRUE) {
          HFENplus = g.metric(Gx,Gy,Gz,n,sf=sf,ii=5,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
          HFENplus[which(HFENplus < 0)] = 0
          #averaging HFENplus per ws3 seconds
          HFENplus2 =cumsum(c(0,HFENplus))
          select = seq(1,length(HFENplus),by=sf*ws3)
          HFENplus3b = diff(HFENplus2[round(select)]) / abs(diff(round(select)))
        }
        if (do.teLindert2013 == TRUE) {
          teLindert2013 = g.metric(Gx,Gy,Gz,sf=sf,ii=10) #calling function metric.R to do the calculation
          teLindert2013 = teLindert2013[seq(1+round(sf/2),length(teLindert2013),by=sf)] #per second
          teLindert2013 = teLindert2013 * 3.07
          select = seq(1,length(teLindert2013),by=ws3) #per 5 seconds
          teLindert2013 =cumsum(c(0,teLindert2013))
          teLindert2013b = diff(teLindert2013[round(select)]) # not deviding by abs... because this is the sum
          # this is now a count per 5 seconds, later on this needs to be added over 15 sec epoch
          # next. there is a weighting formula
        }
        if (do.anglex == TRUE | do.angley == TRUE | do.anglez == TRUE) {
          angle = g.metric(Gx,Gy,Gz,n,sf=sf,ii=11,TW=TW,lb=lb,hb=hb) #calling function metric.R to do the calculation
          angle_x = angle[,1]; angle_y = angle[,2]; angle_z = angle[,3]
          #averaging per ws3 seconds
          angle_x2 =cumsum(c(0,angle_x)); select = seq(1,length(angle_x),by=sf*ws3)
          angle_x3b = diff(angle_x2[round(select)]) / abs(diff(round(select)))
          angle_y2 =cumsum(c(0,angle_y)); select = seq(1,length(angle_y),by=sf*ws3)
          angle_y3b = diff(angle_y2[round(select)]) / abs(diff(round(select)))
          angle_z2 =cumsum(c(0,angle_z)); select = seq(1,length(angle_z),by=sf*ws3)
          angle_z3b = diff(angle_z2[round(select)]) / abs(diff(round(select)))
        }
        if (do.enmoa == TRUE) {
          ENMOa =g.metric(Gx,Gy,Gz,n,sf=sf,ii=12,TW=TW,lb=lb,hb=3.5) #calling function metric.R to do the calculation
          ENMOa[which(ENMOa < 0)] = 0
          ENMOa2 =cumsum(c(0,ENMOa))
          select = seq(1,length(ENMOa),by=sf*ws3)
          ENMOa3b = diff(ENMOa2[round(select)]) / abs(diff(round(select)))
        }
      }
      if (LD != 0) {
        #-----------------------------------------------------
        #extend out if it is expected to be too short
        if (count > (nrow(metashort) - (2.5*(3600/ws3) *24))) {  
          extension = matrix(" ",((3600/ws3) *24),ncol(metashort)) #add another day to metashort once you reach the end of it
          metashort = rbind(metashort,extension)
          cat("\nvariabel metashort extended\n")
        }
        col_msi = 2
        if (do.bfen == TRUE) {
          metashort[(count):(count-1+length(BFEN3b)),col_msi] = BFEN3b; col_msi = col_msi + 1
          rm(BFEN3b)
        }
        if (do.enmo == TRUE) {
          metashort[(count):(count-1+length(ENMO3b)),col_msi] = ENMO3b; col_msi = col_msi + 1
          rm(ENMO3b)
        }
        if (do.lfenmo == TRUE) {
          metashort[(count):(count-1+length(LFENMO3b)),col_msi] = LFENMO3b; col_msi = col_msi + 1
          rm(LFENMO3b)
        }
        if (do.en == TRUE) {
          metashort[(count):(count-1+length(EN3b)),col_msi] = EN3b; col_msi = col_msi + 1
          rm(EN3b)
        }
        if (do.hfen == TRUE) {
          metashort[(count):(count-1+length(HFEN3b)),col_msi] = HFEN3b; col_msi = col_msi + 1
          rm(HFEN3b)
        }
        if (do.hfenplus == TRUE) {
          metashort[(count):(count-1+length(HFENplus3b)),col_msi] = HFENplus3b; col_msi = col_msi + 1
          rm(HFENplus3b)
        }
        if (do.teLindert2013 == TRUE) {
          metashort[(count):(count-1+length(teLindert2013b)),col_msi] = teLindert2013b; col_msi = col_msi + 1
          rm(teLindert2013b)
        }
        if (do.anglex == TRUE) {
          metashort[(count):(count-1+length(angle_x3b)),col_msi] = angle_x3b; col_msi = col_msi + 1
          rm(angle_x3b)
        }
        if (do.angley == TRUE) {
          metashort[(count):(count-1+length(angle_y3b)),col_msi] = angle_y3b; col_msi = col_msi + 1
          rm(angle_y3b)
        }
        if (do.anglez == TRUE) {
          metashort[(count):(count-1+length(angle_z3b)),col_msi] = angle_z3b; col_msi = col_msi + 1
          rm(angle_z3b)
        }
        if (do.enmoa == TRUE) {
          metashort[(count):(count-1+length(ENMOa3b)),col_msi] = ENMOa3b; col_msi = col_msi + 1
          rm(ENMOa3b)
        }
        count = count + length(EN3b) #increasing "count" the indicator of how many seconds have been read
        rm(Gx); rm(Gy); rm(Gz)
        # reduce blocksize if memory is getting higher
        gco = gc()
        memuse = gco[2,2] #memuse in mb
        bsc_qc = rbind(bsc_qc,c(memuse,Sys.time()))
        if (memuse > 4000) {
          if (bsc_cnt < 5) {
            if ((chunksize * (0.8 ^ bsc_cnt)) > 0.2) {
              blocksize = round(blocksize * 0.8)
              bsc_cnt = bsc_cnt + 1
            }
          }
        }
        #--------------------------------------------
        if (mon == 2) { #extract extra info from data if it is a Geneactive
          light = as.numeric(data[,5])
          temperature = as.numeric(data[,7])
        }
        ##==================================================
        # MODULE 2 - non-wear time & clipping
        #cat("\nmodule 2\n") #notice that windows overlap for non-wear detecting
        window2 = ws2 * sf #window size in samples
        window = ws * sf #window size in samples
        nmin = floor(LD/(window2)) #nmin = minimum number of windows that fit in this block of data
        CW = NW = matrix(0,nmin,3) #CW is clipping, NW is non-wear
        TS1W = TS2W = TS3W = TS4W = TS5W = TS6W = TS7W = CWav = NWav = matrix(0,nmin,1)
        for (h in 1: nmin) { #number of windows
          cliphoc1 = (((h-1)*window2)+ window2*0.5 ) - window2*0.5 #does not use "window"
          cliphoc2 = (((h-1)*window2)+ window2*0.5 ) + window2*0.5
          for (jj in  1:3) {
            #hoc1 & hoc2 = edges of windows
            #window is bigger& window2 is smaller one
            crit = ((window/window2)/2)+1
            if (h <= crit) {
              hoc1 = 1
              hoc2 = window
            } else if (h >= (nmin - crit)) {
              hoc1 = (nmin-crit)*window2
              hoc2 = nmin*window2 #end of data
            } else if (h > crit & h < (nmin - crit)) {
              hoc1=(((h-1)*window2)+ window2*0.5 ) - window*0.5
              hoc2=(((h-1)*window2)+ window2*0.5 ) + window*0.5
            }
            if (dformat == 1) {
              sdwacc = sd(as.numeric(data[(1+hoc1):hoc2,(jj+(mon-1))]),na.rm=TRUE)
              maxwacc = max(as.numeric(data[(1+hoc1):hoc2,(jj+(mon-1))]),na.rm=TRUE)
              minwacc = min(as.numeric(data[(1+hoc1):hoc2,(jj+(mon-1))]),na.rm=TRUE)
            } else if (dformat == 2) {
              if (ncol(data) == 3) extra = 0
              if (ncol(data) >= 4) extra = 1
              sdwacc = sd(as.numeric(data[(1+hoc1):hoc2,(jj+extra)]),na.rm=TRUE)
              maxwacc = max(as.numeric(data[(1+hoc1):hoc2,(jj+extra)]),na.rm=TRUE)
              minwacc = min(as.numeric(data[(1+hoc1):hoc2,(jj+extra)]),na.rm=TRUE)
            }
            #estimate number of data points of clipping based on raw data at about 87 Hz
            if (mon == 1) {
              clipthres = 5.5
            } else if (mon == 2) {
              clipthres = 7.5
            } else if (mon == 3) {
              clipthres = 7.5 #ADJUSTMENT NEEDED FOR ACTIGRAPH???????????
            }
            if (dformat == 1) {
              CW[h,jj] = length(which(abs(as.numeric(data[(1+cliphoc1):cliphoc2,(jj+(mon-1))])) > clipthres))
            } else if (dformat == 2) {
              if (ncol(data) == 3) extra = 0
              if (ncol(data) >= 4) extra = 1
              CW[h,jj] = length(which(abs(as.numeric(data[(1+cliphoc1):cliphoc2,(jj+extra)])) > clipthres))
            }
            #non-wear criteria are monitor specific
            if (mon == 1) {
              sdcriter = 0.003
              racriter = 0.05
            } else if (mon == 2) {
              sdcriter = 0.013 #0.0109 in rest test
              racriter = 0.15 #0.1279 measured in rest test
            } else if (mon == 3) {
              sdcriter = 0.013 #ADJUSTMENT NEEDED FOR ACTIGRAPH???????????
              racriter = 0.15 #ADJUSTMENT NEEDED FOR ACTIGRAPH???????????
            }
            if (sdwacc < sdcriter) { 
              if (abs(maxwacc - minwacc) < racriter) { 
                NW[h,jj] = 1
              }
            } else {
            }
          }
          CW = CW / (window2) #changed 30-1-2012, was window*sf
          NWav[h,1] = (NW[h,1] + NW[h,2] + NW[h,3]) #indicator of non-wear
          CWav[h,1] = max(c(CW[h,1],CW[h,2],CW[h,3])) #indicator of clipping
        }
        col_mli = 2
        metalong[count2:((count2-1)+nrow(NWav)),col_mli] = NWav; col_mli = col_mli + 1
        metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = CWav; col_mli = col_mli + 1
        if (mon == 2) { #going from sample to ws2
          #light (running mean)
          lightc = cumsum(c(0,light))
          select = seq(1,length(lightc),by=(ws2*sf))
          lightmean = diff(lightc[round(select)]) / abs(diff(round(select)))
          #light (running max)
          lightmax = matrix(0,length(lightmean),1)
          for (li in 1:(length(light)/(ws2*sf))) {
            tempm = max(light[((li-1)*(ws2*sf)):(li*(ws2*sf))])
            if (length(tempm) > 0) {
              lightmax[li] = tempm[1]
            } else {
              lightmax[li] = max(light[((li-1)*(ws2*sf)):(li*(ws2*sf))])
            }
          }
          #temperature (running mean)
          temperaturec = cumsum(c(0,temperature))
          select = seq(1,length(temperaturec),by=(ws2*sf))
          temperatureb = diff(temperaturec[round(select)]) / abs(diff(round(select)))
        }
        #EN going from sample to ws2
        ENc = cumsum(c(0,EN))
        select = seq(1,length(ENc),by=(ws2*sf))
        ENb = diff(ENc[round(select)]) / abs(diff(round(select)))
        if (mon == 2) {
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = lightmean; col_mli= col_mli + 1
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = lightmax; col_mli= col_mli + 1
          metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = temperatureb; col_mli= col_mli + 1
        }
        metalong[(count2):((count2-1)+nrow(NWav)),col_mli] = ENb; col_mli= col_mli + 1
        count2  = count2 + nmin
        #				print(paste("number of non-wear periods (15min): ",length(which(NWav[,1] >= 2)) / nmin,sep="")		
        rm(P)
        gc()
      } #end of section which is skipped when switchoff == 1
    } else {
      LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
      cat("\nstop reading because there is not enough data in this block\n")
    }
    if (switchoffLD == 1) {
      LD = 0
    }
    if (ceiling(daylimit) != FALSE) {
      if (i == ceiling(daylimit)) { #to speed up testing only read first 'i' blocks of data
        LD = 0 #once LD < 1 the analysis stops, so this is a trick to stop it
        cat(paste("\nstopped reading data because this analysis is limited to ",ceiling(daylimit)," days\n",sep=""))
      }
    }
    i = i + 1 #go to next block
  }
  # deriving timestamps
  if (filecorrupt == FALSE & filetooshort == FALSE) {
    cut = (count+1):nrow(metashort)
    if (length(cut) > 1) {
      metashort = as.matrix(metashort[-cut,])
    }
    if (nrow(metashort) > 1) {
      starttime3 = round(as.numeric(starttime)) #numeric time but relative to the desiredtz
      time5 = seq(starttime3,(starttime3+((nrow(metashort)-1)*ws3)),by=ws3)
      time6 = as.POSIXlt(time5,origin="1970-01-01",tz=desiredtz)
      metashort[,1] = as.character(time6)
    }
    cut2 = (count2+1):nrow(metalong)
    if (length(cut2) > 1) {
      metalong = as.matrix(metalong[-cut2,])
    }
    if (nrow(metalong) > 2) {
      starttime4 = round(as.numeric(starttime)) #numeric time but relative to the desiredtz
      time1 = seq(starttime4,(starttime4+(nrow(metalong)*ws2)-1),by=ws2)
      time2 = as.POSIXlt(time1,origin="1970-01-01",tz=desiredtz)
      metalong[,1] = as.character(time2)
    }
    metricnames_short = c("timestamp","BFEN","ENMO","LFENMO","EN","HFEN","HFENplus","teLindert2013","anglex","angley","anglez","ENMOa") #
    metricnames_short = as.character(metricnames_short[c(TRUE,do.bfen,do.enmo,do.lfenmo,do.en,do.hfen,do.hfenplus,
                                                         do.teLindert2013,do.anglex,do.angley,do.anglez,do.enmoa)]) #
    metashort = data.frame(A = metashort)
    names(metashort) = metricnames_short
    if (mon == 1 | mon == 3) {
      metricnames_long = c("timestamp","nonwearscore","clippingscore","en")
    } else if (mon == 2) {
      metricnames_long = c("timestamp","nonwearscore","clippingscore","lightmean","lightpeak","temperaturemean","EN")
    }
    metalong = data.frame(A = metalong)
    names(metalong) = metricnames_long
    closeAllConnections()
  } else {
    metalong=metashort=wday=wdayname=windowsizes = c()
  }
  invisible(list(filecorrupt=filecorrupt,filetooshort=filetooshort,
                 metalong=metalong, metashort=metashort,wday=wday,wdayname=wdayname,windowsizes=windowsizes,bsc_qc=bsc_qc))  
}