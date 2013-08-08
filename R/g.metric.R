g.metric <-
function(Gx,Gy,Gz,n,fs,ii,TW,lb,hb,mon) {
	#--------------------------------------
	#Input:
	# G = acceleration signal
	# lb & hb = cut-off frequencies for filter
	# fs = sample frequency
	# n = filter order
	# i = metric selector
	# TW = time window
	#--------------------------------------
	# opening required libraries

	durexp = length(Gx)	#duration of experiment
	Gxfil = matrix(0,durexp,1)
	Gyfil = matrix(0,durexp,1)
	Gzfil = matrix(0,durexp,1)
	Gfil = matrix(0,durexp,1)
	GxCP = matrix(0,durexp,1) #for centripital acceleration
	GyCP = matrix(0,durexp,1) #for centripital acceleration
	GzCP = matrix(0,durexp,1) #for centripital acceleration
	GCP = matrix(0,durexp,1) #for centripital acceleration
	gravity = 1
	if (mon == 1) {
		gravity = 1
	} else if (mon == 2) {
		gravity = 1
	}

	if (ii == 1) {
		# high pass filter
		bf = butter(n,c(lb/(fs/2)),type=c("high")) #creating filter coefficients
		Gxfil[,1] = filter(bf,Gx)	#filtering
		Gyfil[,1] = filter(bf,Gy)	#filtering
		Gzfil[,1] = filter(bf,Gz)	#filtering
		Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
	} else if (ii == 2) {
		# moving average
		for (j in 1:length(Gx)) {
			if (j < TW/2) {	
				Gxfil[j,1] = Gx[j] - mean(Gx[1:TW])
				Gyfil[j,1] = Gy[j] - mean(Gy[1:TW])
				Gzfil[j,1] = Gz[j] - mean(Gz[1:TW])
			} else if (j > (length(Gx) - (TW/2))) {
				Gxfil[j,1] = Gx[j] - mean(Gx[(length(Gx)-TW):length(Gx)])
				Gyfil[j,1] = Gy[j] - mean(Gy[(length(Gx)-TW):length(Gx)])
				Gzfil[j,1] = Gz[j] - mean(Gz[(length(Gx)-TW):length(Gx)])
			} else if (j < (length(Gx) - (TW/2)) & j > TW/2) {
				Gxfil[j,1] = Gx[j] - mean(Gx[(j-(TW/2)):(j+(TW/2))])
				Gyfil[j,1] = Gy[j] - mean(Gy[(j-(TW/2)):(j+(TW/2))])
				Gzfil[j,1] = Gz[j] - mean(Gz[(j-(TW/2)):(j+(TW/2))])

			}
		}
		Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
	} else if (ii == 3) {
		# no subtraction, just vector magnitude
		Gfil[,1] = sqrt((Gx^2) + (Gy^2) + (Gz^2))
	} else if (ii == 4) {
		# vector magnitude minus one
		Gfil[,1] = sqrt((Gx^2) + (Gy^2) + (Gz^2)) - gravity
	} else if (ii == 5) {
		# filter + correction for centripital acc
		bf = butter(n,c(lb/(fs/2)),type=c("low")) #creating filter coefficients
		GxCP[,1] = filter(bf,Gx)	#filtering
		GyCP[,1] = filter(bf,Gy)	#filtering
		GzCP[,1] = filter(bf,Gz)	#filtering
		GCP[,1] = (sqrt((GxCP[,1]^2) + (GyCP[,1]^2) + (GzCP[,1]^2))) - gravity #vector magnitude minus 1
		bf = butter(n,c(lb/(fs/2)),type=c("high")) #creating filter coefficients
		Gxfil[,1] = filter(bf,Gx)	#filtering
		Gyfil[,1] = filter(bf,Gy)	#filtering
		Gzfil[,1] = filter(bf,Gz)	#filtering
		Gfil[,1] = (sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))) + GCP[,1]
	} else if (ii == 6) {
		# moving average + correction for centripital acc
		for (j in 1:length(Gx)) {
			if (j < TW/2) {	
				GxCP[j,1] = mean(Gx[1:TW])
				GyCP[j,1] = mean(Gy[1:TW])
				GzCP[j,1] = mean(Gz[1:TW])
			} else if (j > (length(Gx) - (TW/2))) {
				GxCP[j,1] = mean(Gx[(length(Gx)-TW):length(Gx)])
				GyCP[j,1] = mean(Gy[(length(Gx)-TW):length(Gx)])
				GzCP[j,1] = mean(Gz[(length(Gx)-TW):length(Gx)])
			} else if (j < (length(Gx) - (TW/2)) & j > TW/2) {
				GxCP[j,1] = mean(Gx[(j-(TW/2)):(j+(TW/2))])
				GyCP[j,1] = mean(Gy[(j-(TW/2)):(j+(TW/2))])
				GzCP[j,1] = mean(Gz[(j-(TW/2)):(j+(TW/2))])
			}
		}
		GCP[,1] = (sqrt((GxCP[,1]^2) + (GyCP[,1]^2) + (GzCP[,1]^2))) - gravity
		for (j in 1:length(Gx)) {
			if (j < TW/2) {	
				Gxfil[j,1] = Gx[j] - mean(Gx[1:TW])
				Gyfil[j,1] = Gy[j] - mean(Gy[1:TW])
				Gzfil[j,1] = Gz[j] - mean(Gz[1:TW])
			} else if (j > (length(Gx) - (TW/2))) {
				Gxfil[j,1] = Gx[j] - mean(Gx[(length(Gx)-TW):length(Gx)])
				Gyfil[j,1] = Gy[j] - mean(Gy[(length(Gx)-TW):length(Gx)])
				Gzfil[j,1] = Gz[j] - mean( Gz[(length(Gx)-TW):length(Gx)])
			} else if (j < (length(Gx) - (TW/2)) & j > TW/2) {
				Gxfil[j,1] = Gx[j] - mean(Gx[(j-(TW/2)):(j+(TW/2))])
				Gyfil[j,1] = Gy[j] - mean(Gy[(j-(TW/2)):(j+(TW/2))])
				Gzfil[j,1] = Gz[j] - mean(Gz[(j-(TW/2)):(j+(TW/2))])
			}
		}
		Gfil[,1] = (sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))) + GCP[,1]
	} else if (ii == 7) {
		# band pass filtered eucludian norm
		Wc = matrix(0,2,1)
		Wc[1,1] = lb / (fs/2)
		Wc[2,1] = hb / (fs/2)
		bf = butter(n,Wc,type=c("pass")) #creating filter coefficients
		Gxfil[,1] = filter(bf,Gx)	#filtering
		Gyfil[,1] = filter(bf,Gy)	#filtering
		Gzfil[,1] = filter(bf,Gz)	#filtering
		Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2))
	} else if (ii == 8) { #angle
		#print("angle")
Gym = rollmedian(Gy,k=501,na.pad=TRUE)
		Gym[which(is.na(Gym[1:1000]) ==T)] = Gym[which(is.na(Gym[1:1000]) ==F)[1]]
		p1 = which(is.na(Gym) ==F)
		Gym[which(is.na(Gym) ==T)] = Gym[p1[length(p1)]]
		angle = Gym
		angle[which(angle < -1)] = -1 #rounding to -1 and 1 as sine cant deal with numbers outside this range
		angle[which(angle > 1)] = 1
		angle = (asin(angle) / (2*pi)) * 360# conversion to degrees
		if (nrow(Gfil) != length(angle)) {
			warning
		}
		Gfil[,1] = angle
	} else if (ii == 9) { #Low pass filtered followed by ENMO  
	  bf = butter(n,c(hb/(fs/2)),type=c("low")) #creating filter coefficients
	  Gxfil[,1] = filter(bf,Gx)	#filtering
	  Gyfil[,1] = filter(bf,Gy)	#filtering
	  Gzfil[,1] = filter(bf,Gz)	#filtering
	  # vector magnitude minus one
    Gfil[,1] = sqrt((Gxfil[,1]^2) + (Gyfil[,1]^2) + (Gzfil[,1]^2)) - gravity
	}
	metric = Gfil[,1]
}
