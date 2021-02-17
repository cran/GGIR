## ---- echo=FALSE, out.width = "100%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

## ----eval=FALSE---------------------------------------------------------------
#  theta = sqrt(rowSums(gyr ^ 2)) # theta (magnitude of angular velocity around orientation vector OV)
#  OV = matrix(0, N, 3) # N is number of samples.
#  nozero = which(theta > 0)
#  OV[nozero,] = as.numeric(gyr[nozero,] / theta[nozero])

## ----eval=FALSE---------------------------------------------------------------
#  theta = theta / sf

## ----eval=FALSE---------------------------------------------------------------
#  lb = 0.5 # cut-off frequency for the filter in Hertz
#  lowpf = signal::butter(n=4,c(lb/(sf/2)),type=c("low")) #creating filter coefficients
#  acc_lf = acc_hf = matrix(NA, nrow(acc), ncol(acc)) # initialize matrices
#  for (i in 1:3) {
#      # note: acc_lf will also be used for orientation of gravity in the
#      # absence of movement further down, so this calculate serves two purposes
#      acc_lf[,i] <- signal::filter(lowpf, acc[,i]) # low-pass filtered
#  }
#  acc_hf <- acc - acc_lf # high-pass filtered

## ----eval=FALSE---------------------------------------------------------------
#  weight = pmin(pmax((rowSums(abs(acc_hf)) - 0.04),0) / 0.01, 1)

## ----eval=FALSE---------------------------------------------------------------
#  maxweight = 1-(0.5/sf)
#  weight = ifelse(weight > maxweight, yes = maxweight, no = weight)

## ----eval=FALSE---------------------------------------------------------------
#  weight = ifelse(weight < 0.01, yes = 0, no = weight)

## ----eval=FALSE---------------------------------------------------------------
#    RotArr = array(dim = c(N, 3, 3)) # this is a rotation matrix for every timestep
#    costheta = cos(theta)
#    sintheta = sin(theta)
#    RotArr[,1,1:3] = cbind(costheta +
#                             OV[,1]^2 * (1-costheta), OV[,1] * OV[,2] * (1- costheta) -
#                             OV[,3] * sintheta, OV[,1] * OV[,3] * (1- costheta) +
#                             OV[,2] * sintheta)
#    RotArr[,2,1:3] = cbind(OV[,2] * OV[,1] * (1- costheta) +
#                             OV[,3] * sintheta, costheta +
#                             OV[,2]^2 * (1-costheta),OV[,2] * OV[,3] * (1- costheta) -
#                             OV[,1] * sintheta)
#    RotArr[,3,1:3] = cbind(OV[,3] * OV[,1] * (1- costheta) -
#                             OV[,2] * sintheta, OV[,3] * OV[,2] * (1- costheta) +
#                             OV[,1] * sintheta,  costheta +
#                             OV[,3]^2 * (1-costheta))

## ----eval=FALSE---------------------------------------------------------------
#  gvector = acc_lf # initialize gvector as equivalent of acc_lf
#  weight_not_zero = which(weight > 0)
#  if (weight_not_zero[1] == 1) weight_not_zero = weight_not_zero[2:length(weight_not_zero)]
#  for (j in weight_not_zero) {
#    gvector[j,] = (crossprod(RotArr[j-1,,],  gvector[j-1,]) * weight[j]) +
#      (acc_lf[j,] * (1-weight[j]))
#  }
#  acclocal = acc - gvector

## ----eval=FALSE---------------------------------------------------------------
#  library("GGIR")
#  # acc: a 3-column matrix with your accelerometer data
#  # gyr: a 3-column matrix with your gyroscope data
#  # sf: sample frequency in Hertz
#  output = separategravity(acc = acc, gyr = gyr, sf = sf)`
#  acclocal = output$acclocal
#  gvector = output$gvector

## ----eval=FALSE---------------------------------------------------------------
#  library("GGIR")
#  datadir = "/your/data/directory" # with .cwa files from the AX6 sensor (Axivity Ltd)
#  outputdir = "/your/output/directory"
#  g.shell.GGIR(datadir=datadir, outputdir=outputdir, acc.metric="sgAccEN")

## ---- echo=FALSE, out.width = "100%", out.extra='style="border: 0; padding:20px"'----
knitr::include_graphics("GGIR-MASTERLOGO-RGB.png")

