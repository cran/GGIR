g.analyse <-
  function(I,C,M,IMP,qlevels=c(),qwindow=c(0,24),quantiletype = 7,L5M5window = c(0,24),M5L5res=10,
           includedaycrit = 16,ilevels=c(),winhr=5,idloc=1) {
    # Arguments:
    # IMP - output from g.impute()
    # M - output from g.getmeta()
    # I - output from g.inspectfile
    # qlevels - percentiles for which value needs to be extracted
    # L5M5window - start [1] in 24 hour clock hours and [2] end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window)
    # qwindow -  start and of 24 hour clock hours over which distribution in short epoch values needs to be evaluated
    # quantiletype - type of quantile function to use
    # includedaycrit - numbeactir of hours of valid data needed for a day specific summary, this is not used for general summary measures
    # boutduration - in minutes
    # boutcriterion - ratio of a bout for which the acceleration needs to be above the lower boundary
    # M5L5res - resultion of L5 and M5 in minutes
    #
    # Value:
    # summary - one line of summary measures for the file that was analysed
    # daysummary - summary measures per day for the file that was analysed
    
    
    fname=I$filename
    averageday = IMP$averageday
    
    strategy = IMP$strategy
    
    hrs.del.start = IMP$hrs.del.start
    hrs.del.end = IMP$hrs.del.end
    maxdur = IMP$maxdur
    #   idloc = 1 # used to be flexible towards different studies...clean this up
    #   if (length(IMP$idloc) > 0) {
    #     idloc = IMP$idloc
    #   }
    #   print(paste("idloc = ",idloc,sep=""))
    
    windowsizes = M$windowsizes
    metalong = M$metalong
    metashort = IMP$metashort
    rout = IMP$rout
    wdaycode = M$wday
    wdayname = M$wdayname
    LC2 = IMP$LC2
    LC = IMP$LC
    dcomplscore = IMP$dcomplscore
    r1 = as.numeric(as.matrix(rout[,1]))
    r2 = as.numeric(as.matrix(rout[,2]))
    r3 = as.numeric(as.matrix(rout[,3]))
    r4 = as.numeric(as.matrix(rout[,4]))
    r5 = as.numeric(as.matrix(rout[,5]))
    ws3 = windowsizes[1]
    ws2 = windowsizes[2]
    vi = 1
    # Time window for L5 & M5 analysis
    t0_LFMF = L5M5window[1] #start in 24 hour clock hours
    t1_LFMF = L5M5window[2]+(winhr-(M5L5res/60)) #end in 24 hour clock hours (if a value higher than 24 is chosen, it will take early hours of previous day to complete the 5 hour window
    # Time window for distribution analysis
    t_TWDI = qwindow #start and of 24 hour clock hours
    if (length(qlevels) > 0) {
      doquan = TRUE    
    } else {
      doquan = FALSE
    }
    #==========================================================================================
    #==========================================================================================
    #==========================================================================================
    # Setting paramters (NO USER INPUT NEEDED FROM HERE ONWARDS)
    #------------------------------------------------------
    if (doquan == TRUE) {
      QLN = rep(" ",length(qlevels))
      for (QLNi in 1:length(qlevels)) {
        QLN[QLNi] = paste((round((qlevels[QLNi]) * 10000))/100,"th perc btwn ",
                          t_TWDI[1],"h & ",t_TWDI[2],"h",sep="")
      }
    }
    #==============================
    #extract day summary?
    doperday = 1
    #=================================
    # What is the minimum number of accelerometer axis needed to meet the criteria for nonwear in order for the data to be detected as nonwear?
    wearthreshold = 2 #needs to be 0, 1 or 2
    #=================================================================
    summary = matrix(" ",1,100) #matrix to be stored with summary per participant
    s_names = rep(" ",ncol(summary))
    
    nfeatures = 50+length(levels)+length(ilevels)
    summaryperday = matrix(0,(2*24),nfeatures) #matrix to be stored with summary per participant
    i = 1  
    #==================================
    # Extracting basic meta_short variables
    
    hvars = g.extractheadervars(I)
    id = hvars$id;              iid =hvars$iid; idd =hvars$idd
    HN = hvars$HN;              BL = hvars$BL
    SX=hvars$SX;                SN = hvars$SN
    
    n_ws2_perday = (1440*60) / ws2
    n_ws3_perday = (1440*60) / ws3
    if (((nrow(metalong)/((1440*60)/ws2)*10) - (nrow(metashort)/((60/ws3)*1440)) * 10) > 1) {
      print("Matrices 'metalong' and 'metashort' are not compatible")
    }  
    #     #----------------------
    #     # Pelotas specific
    id2 = id
    iid2 = iid
    if (idloc == 3) { #remove hyphen in id-name for Pelotas id-numbers
      for (j in 1:length(id)) {
        temp = unlist(strsplit(id,"-"))
        if (length(temp) == 2) {
          id2[j] = as.character(temp[1])
        } else {
          id2[j] = as.character(id[j])
        }
      }
      for (j in 1:length(iid)) {
        temp = unlist(strsplit(iid,"-"))
        if (length(temp) == 2) {
          iid2[j] = as.character(temp[1])
        } else {
          iid2[j] = as.character(iid[j])
        }
      }
    }
    #======================================
    # detect first and last midnight and all midnights
    tsi = which(colnames(metalong) == "timestamp")
    time = as.character(metalong[,tsi])
    startt = as.character(metalong[1,tsi])
    
    #====================================
    # Deriving file characteristics from 15 min summary files
    LD = nrow(metalong) * (ws2/60) #length data in minutes
    ND = nrow(metalong)/n_ws2_perday #number of days
    
    
    #==============================================
    # Generating time variable
    timeline = seq(0,ceiling(nrow(metalong)/n_ws2_perday),by=1/n_ws2_perday)	
    timeline = timeline[1:nrow(metalong)]
    
    tooshort = 0
    dmidn = g.detecmidnight(ND,time)
    firstmidnight=dmidn$firstmidnight;  firstmidnighti=dmidn$firstmidnighti
    lastmidnight=dmidn$lastmidnight;    lastmidnighti=dmidn$lastmidnighti
    midnights=dmidn$midnights;          midnightsi=dmidn$midnightsi
    starttimei = 1
    endtimei = nrow(M$metalong)
    if (strategy == 2) {
      starttimei = firstmidnighti 
      endtimei = lastmidnighti - 1
    }
    
    #extracting calibration value during periods of non-wear
    if (length(which(r1==1)) > 0) {
      CALIBRATE = mean(as.numeric(as.matrix(metalong[which(r1==1 & r2 != 1),which(colnames(M$metalong) == "EN")]))) #mean EN during non-wear time and non-clipping time
    } else {
      CALIBRATE = c()
      is.na(CALIBRATE) = T
    }
    
    #========================================================================================
    # Impute ws3 second data based on ws2 minute estimates of non-wear time
    r5long = matrix(0,length(r5),(ws2/ws3))
    r5long = replace(r5long,1:length(r5long),r5)
    r5long = t(r5long)
    dim(r5long) = c((length(r5)*(ws2/ws3)),1)
    ENMOi = which(colnames(metashort) == "ENMO")
    LFENMOi = which(colnames(metashort) == "LFENMO")
    BFENi = which(colnames(metashort) == "BFEN")
    HFENi = which(colnames(metashort) == "HFEN")
    HFENplusi = which(colnames(metashort) == "HFENplus")
    ENi = which(colnames(metashort) == "EN")
    anglei = which(colnames(metashort) == "angle")
    if (length(ENMOi) == 0) ENMOi = -1
    if (length(LFENMOi) == 0) LFENMOi = -1
    if (length(BFENi) == 0) BFENi = -1
    if (length(HFENi) == 0) HFENi = -1
    if (length(HFENplusi) == 0) HFENplusi = -1
    if (length(ENi) == 0) ENi = -1
    if (length(anglei) == 0) anglei = -1
    if (doquan == TRUE) {
      QUAN = qlevels_names = c()
      ML5AD = ML5AD_names = c()
      for (quani in 1:ncol(averageday)) {
        QUANtmp =  quantile(averageday[((t_TWDI[1]*(3600/ws3))+1):(t_TWDI[2]*(3600/ws3)),quani],probs=qlevels,na.rm=T,type=quantiletype)
        QUAN = c(QUAN,QUANtmp)
        qlevels_namestmp = rep(" ",length(qlevels))
        for (QLNi in 1:length(qlevels)) {
          qlevels_namestmp[QLNi] = paste(QLN[QLNi]," of ",colnames(M$metashort)[(quani+1)]," (mg)",sep="")
        }
        qlevels_names = c(qlevels_names,qlevels_namestmp)
        #--------------------------------------
        #M5L5 based on the average day
        avday = averageday[,quani]
        avday = c(avday[(firstmidnighti*(ws2/ws3)):length(avday)],avday[1:((firstmidnighti*(ws2/ws3))-1)])
        
        if (mean(avday) > 0 & nrow(as.matrix(M$metashort)) > 1440*(60/ws3)) {
          ML5ADtmp = g.getM5L5(avday,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
          ML5AD = c(ML5AD,c(ML5ADtmp$DAYL5HOUR, ML5ADtmp$DAYL5VALUE, ML5ADtmp$DAYM5HOUR, ML5ADtmp$DAYM5VALUE, ML5ADtmp$V5NIGHT))
        } else {
          ML5AD = c(ML5AD," "," "," "," "," ")
        }
        #c(ML5AD$DAYL5HOUR, ML5AD$DAYL5VALUE, ML5AD$DAYM5HOUR, ML5AD$DAYM5VALUE, ML5AD$V5NIGHT)
        
        ML5AD_namestmp = rep(" ",5)      
        ML5N = c("L5 hour","L5 value","M5 hour","M5 value","Accelerationa 1am-6am value")
        for (ML5ADi in 1:5) {
          ML5AD_namestmp[ML5ADi] = paste(ML5N[ML5ADi]," of ",colnames(M$metashort)[(quani+1)]," (mg)",sep="")
        }
        ML5AD_names = c(ML5AD_names,ML5AD_namestmp)
        #==========================
        rm(QUANtmp); rm(ML5AD_namestmp)
      }
    }
    
    #===============================================
    # Extract features from the imputed data
    qcheck = r5long
    LW = length(which(as.numeric(qcheck) != 1)) / (60/ws3) #number of minutes wear time between first and last midnights
    nfulldays = (lastmidnighti - firstmidnighti) / ((3600/ws2)*24)
    
    ndays = length(midnights) + 1 #ceiling(nfulldays + 2) # ceiling to cope with days with 23 hours
    
    if (ndays != round(ndays)) { #day saving time causing trouble?
      print("One day in this measurement is longer or shorter than 24 hours (probably related to day saving time)")
    }
    
    
    #--------------------------------------------------------------
    # Features per day
    if (doperday == 1) { # extract fe
      startatmidnight = 0  #new 28-11-2012
      if (firstmidnighti == 1) {  #new 28-11-2012 #if measurement starts at midnight
        ndays = ndays - 1  #new 28-11-2012
        startatmidnight =  1   #new 28-11-2012
        print("measurement starts at midnight or there is no midnight")  #new 28-11-2012
      }
      endatmidnight = 0
      if (lastmidnight == time[length(time)] ) {	#if measurement ends at midnight
        ndays = ndays - 1
        endatmidnight = 1
        print("measurement ends at midnight or there is no midnight")
      }
      if (lastmidnighti == firstmidnighti) {
        tooshort = 1
      }
      daysummary = matrix("",ceiling(ndays),nfeatures)
      ds_names = rep("",nfeatures)
      
      
      #===============================================
      # Features per day (based on on single variables)
      for (di in 1:ndays) { #run through days
        
        #extract day from matrix D and qcheck
        if (startatmidnight == 1 & endatmidnight == 1) {
          qqq1 = midnightsi[di]*(ws2/ws3) 	#a day starts at 00:00
          qqq2 = (midnightsi[(di+1)]*(ws2/ws3))-1 
        } else if (startatmidnight == 1 & endatmidnight == 0) {
          if (di < floor(ndays)) { #applying floor because when day there is day saving time then ndays is not an integer
            qqq1 = midnightsi[di]*(ws2/ws3)
            qqq2 = (midnightsi[(di+1)]*(ws2/ws3))-1
          } else if (di == floor(ndays)) {
            qqq1 = midnightsi[di]*(ws2/ws3)
            qqq2 = nrow(metashort)
          }
        } else if (startatmidnight == 0 & endatmidnight == 0) {
          if (di == 1) {
            qqq1 = 1
            qqq2 = (midnightsi[di]*(ws2/ws3))-1
          } else if (di > 1 & di < floor(ndays)) {
            qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
            qqq2 = (midnightsi[di]*(ws2/ws3))-1
          } else if (di == floor(ndays)) {
            qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
            qqq2 = nrow(metashort)
          }
        } else if (startatmidnight == 0 & endatmidnight == 1) {
          if (di == 1) {
            qqq1 = 1
            qqq2 = (midnightsi[di]*(ws2/ws3))-1
          } else if (di > 1 & di <= floor(ndays)) {
            qqq1 = midnightsi[(di-1)]*(ws2/ws3) # a day starts at 00:00
            qqq2 = (midnightsi[di]*(ws2/ws3))-1
          }
        }
        vari = as.matrix(metashort[qqq1:qqq2,])
        val = qcheck[qqq1:qqq2]
        
        #--------------------------------
        val = as.numeric(val)
        nvalidhours = length(which(val == 0))/ (3600/ws3) #valid hours per day (or half a day)
        nhours = length(val)/ (3600/ws3) #valid hours per day (or half a day)
        #start collecting information about the day
        fi = 1
        daysummary[di,fi] = unlist(strsplit(fname,"_"))[1] #participant ID
        if (idloc == 2) {
          daysummary[di,fi] = unlist(strsplit(fname,"_"))[1] #id
        } else if (idloc == 4) {
          daysummary[di,fi] = idd
        } else if (idloc == 1) {
          daysummary[di,fi] = id
        } else if (idloc == 3) {
          daysummary[di,fi] = id2
        }
        ds_names[fi] = "id";      fi = fi + 1
        daysummary[di,fi] = fname
        ds_names[fi] = "filename";  fi = fi + 1
        
        #-----------------------------------
        calenderdate = unlist(strsplit(as.character(vari[1,1])," "))[1]
        daysummary[di,fi] = calenderdate               
        daysummary[di,(fi+1)] =BL
        daysummary[di,(fi+2)] = nvalidhours
        daysummary[di,(fi+3)] = nhours
        ds_names[fi:(fi+3)] = c("calender date","body location","N valid hours","N hours")
        fi = fi + 4
        #--------------------------------------      
        weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
                     ,"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
                     ,"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"
                     ,"Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
        if (di == 1) {
          daysummary[di,fi] = wdayname
        } else {
          daysummary[di,fi] = weekdays[wdaycode + (di-1)]
        }
        daysummary[di,(fi+1)] = di #day number relative to start of measurement
        ds_names[fi:(fi+1)] = c("day of the week","day of measurement")
        fi = fi + 2
        if (tooshort == 0) {
          #--------------------------------------      
          # extract time spent in activity levels (there are possibly many more features that can be extracted from this)
          if (nvalidhours >= includedaycrit) {
            #============================================================
            for (mi in 2:ncol(metashort)) { #run through metrics (for features based on single metrics)
              varnum = as.numeric(as.matrix(vari[,mi]))
              # if this is the first or last day and it has more than includedaycrit number of days then expand it
              if (length(which(is.na(varnum) == FALSE)) != length(IMP$averageday[,(mi-1)])) { #
                difference = length(which(is.na(varnum) == FALSE)) - length(IMP$averageday[,(mi-1)])
                if (di == 1) {
                  varnum = c(IMP$averageday[1:abs(difference),(mi-1)],varnum)
                  #print("expanded at the beginning")
                } else {
                  a56 = length(IMP$averageday[,(mi-1)]) - abs(difference)
                  a57 = length(IMP$averageday[,(mi-1)])
                  varnum = c(varnum,IMP$averageday[a56:a57,(mi-1)])
                  #print("expanded at the end")
                }
              }
              if (mi == ENMOi) { #currently intensity/activity level features are based on metric ENMO, but by copy-pasting this to another metric this should work the same.
                ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                if (length(ML5$DAYL5HOUR) > 0) {
                  daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean acceleration ",colnames(metashort)[mi]," (mg) between 1am and 6am",sep=""); fi=fi+1
                }
                keepindex_0 = fi
                daysummary[di,fi] = mean(varnum)* 1000; ds_names[fi] = "mean acceleration over the 24hr day (ENMO in mg)"; fi=fi+1 #ENMO 
                
                #newly added on 9-7-2013, percentiles of acceleration in the specified window:
                q46 = quantile(varnum[((qwindow[1]*60*(60/ws3))+1):(qwindow[2]*60*(60/ws3))],probs=qlevels,na.rm=T,type=quantiletype) * 1000 #times 1000 to convert to mg
                keepindex_46 = c(fi,(fi+(length(qlevels)-1)))
                daysummary[di,fi:(fi+(length(qlevels)-1))] = q46; ds_names[fi:(fi+(length(qlevels)-1))] = rownames(as.matrix(q46));fi = fi+length(qlevels)
                
                ####################
                # TO DO: add time distrution between acceleration levels
                breaks = ilevels
                q47 = cut((varnum[((qwindow[1]*60*(60/ws3))+1):(qwindow[2]*60*(60/ws3))]*1000),breaks,right=FALSE)
                q47 = table(q47)
                q48  = (as.numeric(q47) * ws3)/60 #converting to minutes
                
                keepindex_48 = c(fi,(fi+(length(q48)-1)))
                daysummary[di,fi:(fi+(length(q48)-1))] = q48; ds_names[fi:(fi+(length(q48)-1))] = rownames(q47); fi = fi+length(q48)
                
                
              }
              if (mi == LFENMOi) {
                ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                if (length(ML5$DAYL5HOUR) > 0) {
                  daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean acceleration ",colnames(metashort)[mi]," (mg) between 1am and 6am",sep=""); fi=fi+1
                }
                daysummary[di,fi] = mean(varnum) * 1000; ds_names[fi] = "mean acceleration over the 24hr day (LFENMO in mg)"; fi=fi+1 #LFENMO 
              }
              if (mi == BFENi) {
                ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                if (length(ML5$DAYL5HOUR) > 0) {
                  daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean acceleration ",colnames(metashort)[mi]," (mg) between 1am and 6am",sep=""); fi=fi+1
                }
                daysummary[di,fi] = mean(varnum) * 1000; ds_names[fi] = "mean acceleration over the 24hr day (BFEN in mg)"; fi=fi+1 #BFEN
              }
              if (mi == ENi) {
                ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                if (length(ML5$DAYL5HOUR) > 0) {
                  daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean acceleration ",colnames(metashort)[mi]," (mg) between 1am and 6am",sep=""); fi=fi+1
                }
                daysummary[di,fi] = mean(varnum) * 1000; ds_names[fi] = "mean acceleration over the 24hr day (EN in mg)"; fi=fi+1 #EN
              }
              if (mi == HFENi) {
                ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                if (length(ML5$DAYL5HOUR) > 0) {
                  daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean acceleration ",colnames(metashort)[mi]," (mg) between 1am and 6am",sep=""); fi=fi+1
                }
                daysummary[di,fi] = mean(varnum) * 1000;  ds_names[fi] = "mean acceleration over the 24hr day (HFEN in mg)"; fi=fi+1 #HFEN
              }
              if (mi == HFENplusi) {
                ML5 = g.getM5L5(varnum,ws3,t0_LFMF,t1_LFMF,M5L5res,winhr)
                if (length(ML5$DAYL5HOUR) > 0) {
                  daysummary[di,fi] = ML5$DAYL5HOUR; ds_names[fi] = paste("L5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYL5VALUE; ds_names[fi] = paste("L5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5HOUR; ds_names[fi] = paste("M5 hour ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$DAYM5VALUE;  ds_names[fi] = paste("M5 ",colnames(metashort)[mi]," (mg)",sep=""); fi=fi+1
                  daysummary[di,fi] = ML5$V5NIGHT;  ds_names[fi] = paste("mean acceleration ",colnames(metashort)[mi]," (mg) between 1am and 6am",sep=""); fi=fi+1
                }
                daysummary[di,fi] = mean(varnum) * 1000;  ds_names[fi] = "mean acceleration over the 24hr day (HFENplus in mg)"; fi=fi+1 #HFEN+
              }
              if (mi == anglei) { #characteristation of angle (experimental, not finished)
                difangle = abs(diff(varnum)) #absolute change in angle
                posturechange = rep(0,length(difangle))
                posturechange[which(difangle > 10)] = 1
                pc1 = c(0,posturechange,0) #to find posture change periods
                pc2 = c(1,posturechange,1) #to find non-posture change periods
                pc1up = which(diff(pc1) == 1)
                pc1down = which(diff(pc1) == -1)
                pc2up = which(diff(pc2) == 1)
                pc2down = which(diff(pc2) == -1)
                pc3 = pc1down - pc1up # duration of posture change periods
                pc4 = pc2up - pc2down # duration of non-posture change periods
                #                 overv = matrix(0,(length(pc4)+length(pc3)),3)
                #                 if(posturechange[1] == 1) {
                #                   overv[seq(1,(length(pc3)*2),by=2),1] = pc3
                #                   overv[seq(1,(length(pc3)*2),by=2),2] = "posture change"
                #                   overv[seq(2,(length(pc4)*2),by=2),1] = pc4
                #                   overv[seq(2,(length(pc4)*2),by=2),2] = "no posture change"
                #                 } else {
                #                   overv[seq(1,(length(pc4)*2),by=2),1] = pc4
                #                   overv[seq(1,(length(pc4)*2),by=2),1] = "no posture change"
                #                   overv[seq(2,(length(pc3)*2),by=2),2] = pc3
                #                   overv[seq(2,(length(pc3)*2),by=2),2] = "posture change"
                #                 }
                daysummary[di,fi] = length(pc4); ds_names[fi] = "number of non-posture change periods per day"; fi =fi + 1
                daysummary[di,fi] = length(pc3); ds_names[fi] = "number of posture change periods per day"; fi =fi + 1
                posture_dur = (pc4* 5) / 60 # duration of non-posture change periods (minutes)
                interup_dur = (pc3* 5) / 60 # duration of posture change periods (minutes)
                daysummary[di,fi] = median(posture_dur); ds_names[fi] = "median duration postures (minutes)"; fi =fi + 1
                daysummary[di,fi] = mean(posture_dur); ds_names[fi] = "mean duration postures (minutes)"; fi =fi + 1
                daysummary[di,fi] = median(interup_dur); ds_names[fi] = "median duration posture interruptions (minutes)"; fi =fi + 1
                daysummary[di,fi] = mean(interup_dur); ds_names[fi] = "mean duration posture interruptions (minutes)"; fi =fi + 1
              }
            }
          }
          #features based on multiple metrics
          if (BFENi != -1)       BFEN = as.numeric(vari[,BFENi])
          if (anglei != -1)      angle = as.numeric(vari[,anglei])
          
          #			angled = c(abs(diff(angle)),0)
          val = as.numeric(val)
          #         if (nvalidhours >= includedaycrit) {
          #           if (BFENi != -1) {
          #             timeindyn = which(BFEN > 0.25) #time spent in dynamic activities
          #             timeinsta = which(BFEN <= 0.25) #time spent in static/passive activities
          #             #					daysummary[di,(6+length(metricnames))] = length(timeindyn) / (60/ws3) 
          #           }
          #           if (anglei != -1 & BFENi != -1) {
          #             daysummary[di,fi] = length(which(angle[timeinsta] <= 45 & angle[timeinsta] >= -45)) / (60/ws3) #horizontal
          #             daysummary[di,(fi+1)] = length(which(angle[timeinsta] > 45 | angle[timeinsta] < -45)) / (60/ws3) #vertical
          #             ds_names[fi:(fi+1)] = c("time spent with arms horizontal and inactive",
          #                                     "time spent with arms horizontal and active")
          #             fi = fi + 2
          #           }
          #         }
          rm(val); rm(vari)
        } else { #day did not have any valid data
          #				print("no valid hours in the day")
        }
        
      }
      
    }
    
    
    #D is shortened from midgnight to midnight if requested (strategy 2)
    if (strategy == 2) {
      if (starttimei == 1) {
        metashort = as.matrix(metashort[starttimei:(endtimei*(ws2/ws3)),])
      } else {
        metashort = as.matrix(metashort[(starttimei*(ws2/ws3)):(endtimei*(ws2/ws3)),])
      }
    }
    ND = nrow(metashort) / n_ws3_perday #update this because of reduction in datapoints (added on 20-11-2012)
    LW = length(which(r5 < 1)) * (ws2/60) #length wear in minutes (for entire signal)
    LWp = length(which(r5[which(r4 == 0)] < 1)) * (ws2/60) #length wear in minutes (for protocol)
    LMp = length(which(r4 == 0)) * (ws2/60) #length protocol
    #================================================
    # average per 24 hr
    lookat = c(2:ncol(metashort))
    MA = matrix(NA,length(lookat),1)
    if (length(which(r5 == 0)) > 0) { #to catch strategy 2 with only 1 midnight and other cases where there is no valid data
      for (h in 1:length(lookat)) {
        average24h = matrix(0,n_ws3_perday,1)
        average24hc = matrix(0,n_ws3_perday,1)
        if (floor(ND) != 0) {
          for (j in 1:floor(ND)) {
            string = as.numeric(as.matrix(metashort[(((j-1)*n_ws3_perday)+1):(j*n_ws3_perday),lookat[h]]))
            val = which(is.na(string) == F)
            average24h[val,1] = average24h[val,1] + string[val] #mean acceleration
            average24hc[val,1] = average24hc[val,1] +1
          }
        }
        if (floor(ND) < ND) {
          if (floor(ND) == 0) {
            string = as.numeric(as.matrix(metashort[,lookat[h]]))
          } else {
            string = as.numeric(as.matrix(metashort[((floor(ND)*n_ws3_perday)+1):nrow(metashort),lookat[h]]))
          }
          val = which(is.na(string) == F)
          average24h[val,1] = average24h[val,1] + string[val]  #mean acceleration
          average24hc[val,1] = average24hc[val,1] +1
        }
        average24h = average24h / average24hc
        MA[h] = mean(average24h) #average acceleration in an average 24 hour cycle
      }
    } else {
      print("particpant skipped for general average")
    }
    
    
    id[which(id == "NA")] =iid[which(id == "NA")]
    id2[which(id2 == "NA")] =iid2[which(id2 == "NA")]
    #check whether there was enough data in a day by looking at r5long
    weekdays = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    weekdayi = which(weekdays == wdayname)
    #-----------------------------------
    if (idloc == 2) {
      summary[vi] = unlist(strsplit(fname,"_"))[1] #id
      summary[(vi+1)] = unlist(strsplit(fname,"_"))[2] #SN
    } else if (idloc == 4) {
      summary[vi:(vi+1)] = c(idd,SN)
    } else if (idloc == 1) {
      summary[vi:(vi+1)] = c(id,SN)
    } else if (idloc == 3) {
      summary[vi:(vi+1)] = c(id2,SN)
    }
    s_names[vi:(vi+1)] = c("ID","Device Serial number")
    vi = vi+2
    #-----------------------------------
    summary[vi] = BL
    summary[(vi+1)] = fname
    summary[(vi+2)] = startt # starttime of measurement
    
    s_names[vi:(vi+2)] = c("body location","file name","start time experiment")
    vi = vi+3
    #-----------------------------------
    summary[vi] = wdayname # weekday on which measurement started
    summary[(vi+1)] = I$sf
    summary[(vi+2)] = I$monn
    s_names[vi:(vi+2)] = c("start day","sample frequency","device")
    vi = vi+3
    #-----------------------------------
    summary[vi] = LC2  / ((LD/1440)*96)
    summary[(vi+1)] = LD/1440 #measurement duration in days
    s_names[vi:(vi+1)] = c("clipping score", #paste("number of ",ws2/60," minute time windows with potential signal clipping (> 80% of time > 7.5 g)",sep="") divided by number of 15 minute periods
                           "measurement duration (days)")
    vi = vi+2
    #-----------------------------------
    summary[vi] = dcomplscore #completeness of the day
    summary[(vi+1)] = LMp/1440 #measurement duration according to protocol
    summary[(vi+2)] = LWp/1440 #wear duration in days (out of measurement protocol)
    s_names[vi:(vi+2)] = c("Completeness of 24hr cycle", # day (fraction of 24 hours for which data is available at all)
                           "measurement duration according to protocol (days)","wear duration according to protocol (days)")
    vi = vi+3
    #-----------------------------------
    if (length(C$cal.error.end) == 0)   C$cal.error.end = c(" ")
    summary[vi] = C$cal.error.end #CALIBRATE
    summary[vi+1] = C$QCmessage #CALIBRATE
    for (la in 1:length(lookat)) {
      MA[la] = 	MA[la] * 1000
    }
    whichangle = which(colnames(metashort) == "angle")
    if (length(whichangle) > 0) {
      MA[(whichangle-1)] = MA[(whichangle-1)] / 1000
    }
    
    q0 = length(MA) + 1
    summary[(vi+2):(vi+q0)] = MA
    s_names[vi:(vi+q0)] = c("Calibration error (static estimate)",
                            "Calibration status",colnames(metashort)[2:ncol(metashort)])
    vi = vi+q0+2
    #---------------------------------------
    if (doquan == TRUE) {
      q1 = length(QUAN)
      summary[vi:((vi-1)+q1)] = QUAN*1000
      s_names[vi:((vi-1)+q1)] = qlevels_names
      
      if (length(whichangle) > 0) {
        nv = length(qlevels_names) / (length(colnames(metashort))-1)
        selectnv = c((vi+(nv*(whichangle-2))):(vi+(nv*(whichangle-2))+nv-1))
        summary[selectnv] = as.numeric(summary[selectnv]) / 1000
      }
      vi = vi + q1
      q1 = length(ML5AD)
      summary[vi:((vi-1)+q1)] = ML5AD
      if (length(whichangle) > 0) {
        nv = length(ML5AD_names) / (length(colnames(metashort))-1)
        selectnv = c((vi+(nv*(whichangle-2))):(vi+(nv*(whichangle-2))+nv-1))
        summary[selectnv] = " " #M5L5 is not meaningful for angle
      }
      s_names[vi:((vi-1)+q1)] = ML5AD_names
      vi = vi + q1
    }
    #---------------------------------------
    if (tooshort == 0) {
      #--------------------------------------------------------------
      # expand with extracted values from features per day: do this for ENMO and activity levels
      wkend  = which(daysummary[,7] == "Saturday" | daysummary[,7] == "Sunday")
      v1 = which(is.na(as.numeric(daysummary[wkend,10])) == F)
      wkend = wkend[v1]
      wkday  = which(daysummary[,7] != "Saturday" & daysummary[,7] != "Sunday")
      v2 = which(is.na(as.numeric(daysummary[wkday,10])) == F)	
      wkday = wkday[v2]
      summary[vi] = length(wkend) # number of weekend days
      summary[(vi+1)] = length(wkday) # number of week day
      s_names[vi:(vi+1)] = c("N valid weekend days","N valid weekdays")
      vi = vi + 2
      
      
      #############################################################
      
      ################
      #metrics - summarise with stratification to weekdays and weekend days
      daytoweekvar = c(which(ds_names == "L5 ENMO (mg)"),
                       which(ds_names == "M5 ENMO (mg)"),
                       which(ds_names == "mean acceleration ENMO (mg) between 1am and 6am"),
                       which(ds_names == "mean acceleration over the 24hr day (ENMO in mg)"),
                       which(ds_names == "mean acceleration over the 24hr day (LFENMO in mg)"),
                       which(ds_names == "mean acceleration over the 24hr day (BFEN in mg)"),
                       which(ds_names == "mean acceleration over the 24hr day (EN in mg)"),
                       which(ds_names == "mean acceleration over the 24hr day (HFEN in mg)"),
                       which(ds_names == "mean acceleration over the 24hr day (HFENplus in mg)"))
      
      dtwtel = 0
      if (length(daytoweekvar) >= 1) {
        sp = length(daytoweekvar) + 1
        for (dtwi in daytoweekvar) {
          v4 = mean(as.numeric(daysummary[,dtwi]),na.rm=TRUE) #plain average of available days
          summary[(vi+1+(dtwtel*sp))] = v4 # #average all availabel days
          s_names[(vi+1+(dtwtel*sp))] = paste("V1_",ds_names[daytoweekvar[dtwtel+1]]," all days (plain average of all available days, no weighting)",sep="") 
          
          #========================================================================
          # attempt to stratify to week and weekend days
          # Average of available days
          dtw_wkday = as.numeric(daysummary[wkday,dtwi])
          dtw_wkend = as.numeric(daysummary[wkend,dtwi])
          v1 = mean(dtw_wkend,na.rm=TRUE)
          v2 = mean(dtw_wkday,na.rm=TRUE)
          summary[(vi+2+(dtwtel*sp))] = v1 # #weekend average
          summary[(vi+3+(dtwtel*sp))] = v2 # #weekday average
          s_names[(vi+2+(dtwtel*sp))] = paste("V2_",ds_names[daytoweekvar[dtwtel+1]]," on weekend days (plain average of all available days, no weighting)",sep="") #(in case double days are present and in case there are more than 2 weekend days then the two double days are replaced by one day being the average of the two)
          s_names[(vi+3+(dtwtel*sp))] = paste("V3_",ds_names[daytoweekvar[dtwtel+1]]," on week days (plain average of all available days, no weighting)",sep="") #(in case double days are present and in case there are more than 5 week days then the two double days are replaced by one day being the average of the two)",
          #==================================================
          # Weighted average of available days
          if (length(dtw_wkend) > 2) {
            dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
          }
          if (length(dtw_wkday) > 5) {
            dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
          }
          v1 = mean(dtw_wkend,na.rm=TRUE)
          v2 = mean(dtw_wkday,na.rm=TRUE)
          summary[(vi+4+(dtwtel*sp))] = v1 # #weekend average
          summary[(vi+5+(dtwtel*sp))] = v2 # #weekday average
          s_names[(vi+4+(dtwtel*sp))] = paste("V4_",ds_names[daytoweekvar[dtwtel+1]]," on weekend days (weighted average)",sep="") #(in case double days are present and in case there are more than 2 weekend days then the two double days are replaced by one day being the average of the two)
          s_names[(vi+5+(dtwtel*sp))] = paste("V5_",ds_names[daytoweekvar[dtwtel+1]]," on week days (weighted average)",sep="") #(in case double days are present and in case there are more than 5 week days then the two double days are replaced by one day being the average of the two)",
          dtwtel = dtwtel + 1
        }
        vi = vi+6+((dtwtel*sp)-1)
        
        #===========================================================================
        # SUMMARISE Percentiles (q46)
        if (length(q46) > 0) {
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            v4 = mean(as.numeric(daysummary[,ki46]),na.rm=TRUE) #plain average of available days
            summary[vi] = v4 # #average all availabel days
            s_names[vi] = paste("V6_",ds_names[ki46]," all days (plain average of all available days, no weighting)",sep="") 
            vi = vi + 1
          }
          
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki46])
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("V7_",ds_names[ki46]," on weekend days (plain average of all available days, no weighting)",sep="") #(in case double days are present and in case there are more than 2 weekend days then the two double days are replaced by one day being the average of the two)
            vi = vi + 1
          }
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki46])
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("V8_",ds_names[ki46]," on week days (plain average of all available days, no weighting)",sep="") #(in case double days are present and in case there are more than 5 week days then the two double days are replaced by one day being the average of the two)",        vi = vi + 1
            vi = vi + 1
          }
          # Weighted average of available days
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki46])
            if (length(dtw_wkend) > 2) {
              dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
            }
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("V9_",ds_names[ki46]," on weekend days (weighted average)",sep="") #(in case double days are present and in case there are more than 2 weekend days then the two double days are replaced by one day being the average of the two)
            vi = vi + 1
          }
          for (ki46 in keepindex_46[1]:keepindex_46[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki46])
            if (length(dtw_wkday) > 5) {
              dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
            }
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("V10_",ds_names[ki46]," on week days (weighted average)",sep="") #(in case double days are present and in case there are more than 5 week days then the two double days are replaced by one day being the average of the two)",
            vi = vi+1
          }
        }
        #======================================================
        # SUMMARISE acceleration distribution(q48)
        if (length(q48) > 0) {
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            v4 = mean(as.numeric(daysummary[,ki48]),na.rm=TRUE) #plain average of available days
            summary[vi] = v4 # #average all availabel days
            s_names[vi] = paste("V11_",ds_names[ki48]," all days (plain average of all available days, no weighting)",sep="") 
            vi = vi + 1
          }
          
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki48])
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("V12_",ds_names[ki48]," on weekend days (plain average of all available days, no weighting)",sep="") #(in case double days are present and in case there are more than 2 weekend days then the two double days are replaced by one day being the average of the two)
            vi = vi + 1
          }
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki48])
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("V13_",ds_names[ki48]," on week days (plain average of all available days, no weighting)",sep="") #(in case double days are present and in case there are more than 5 week days then the two double days are replaced by one day being the average of the two)",        vi = vi + 1
            vi = vi + 1
          }
          # Weighted average of available days
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkend = as.numeric(daysummary[wkend,ki48])
            if (length(dtw_wkend) > 2) {
              dtw_wkend = c((dtw_wkend[1]+dtw_wkend[3])/2,dtw_wkend[2])
            }
            v1 = mean(dtw_wkend,na.rm=TRUE)
            summary[vi] = v1 # #weekend average
            s_names[vi] = paste("V14_",ds_names[ki48]," on weekend days (weighted average)",sep="") #(in case double days are present and in case there are more than 2 weekend days then the two double days are replaced by one day being the average of the two)
            vi = vi + 1
          }
          for (ki48 in keepindex_48[1]:keepindex_48[2]) {
            dtw_wkday = as.numeric(daysummary[wkday,ki48])
            if (length(dtw_wkday) > 5) {
              dtw_wkday = c((dtw_wkday[1]+dtw_wkday[6])/2,dtw_wkday[2:5])
            }
            v2 = mean(dtw_wkday,na.rm=TRUE)
            summary[vi] = v2 # #weekday average
            s_names[vi] = paste("V15_",ds_names[ki48]," on week days (weighted average)",sep="") #(in case double days are present and in case there are more than 5 week days then the two double days are replaced by one day being the average of the two)",
            vi = vi+1
          }
        }
      }
      
      summary[vi] = strategy 
      summary[(vi+1)] = hrs.del.start
      summary[(vi+2)] = hrs.del.end
      summary[(vi+3)] = maxdur
      summary[(vi+4)] = windowsizes[1]
      s_names[vi:(vi+4)] = c("data exclusion stategy (value=1, ignore specific hours; value=2, ignore all data before the first midnight and after the last midnight)",
                             "number of hours ignored at the start of the measurement (if strategy = 1)",
                             "number of hours ignored at the end of the measurement (if strategy = 1)",
                             "number of days of measurement after which all data is ignored (if strategy = 1)",
                             "epoch size toch which acceleration was averaged (seconds)")
      vi = vi + 5
    }
    #---------------------------------------------------------------
    rm(metashort); rm(LD); rm(LW); rm(HN); rm(id); rm(metalong)
    #------------------------
    mw = which(is.na(daysummary)==T)
    if (length(mw) > 0) {
      daysummary[which(is.na(daysummary)==T)] = " "
    }
    cut = which(ds_names == " ")
    if (length(cut > 0)) {
      ds_names = ds_names[-cut]
      daysummary = daysummary[,-cut]
    }
    if(min(dim(as.matrix(daysummary))) == 1) {
      if (nrow(as.matrix(daysummary)) != 1) {
        daysummary = t(daysummary) #if there is only one day of data
      }
    }
    daysummary = data.frame(value=daysummary)
    names(daysummary) = ds_names  
    #--------------
    mw = which(is.na(summary)==T)
    if (length(mw) > 0) {
      summary[which(is.na(summary)==T)] = " "
    }  
    cut = which(as.character(s_names) == " ")
    if (length(cut) > 0) {
      s_names = s_names[-cut]
      summary = summary[-cut]
    }
    summary = data.frame(value=t(summary)) #needs to be t() because it will be a column otherwise
    names(summary) = s_names
    invisible(list(summary=summary,daysummary=daysummary))
  }
