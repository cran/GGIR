g.part5.addfirstwake =function(ts, summarysleep_tmp2, nightsi, sleeplog, ID, Nepochsinhour, Nts, sptwindow_HDCZA_end, ws3new) {
  # Note related to if first and last night were ignored in part 4:
  # - diur lacks the first and last night at this point in the code.
  # - nightsi has all the midnights, so it is possible to check here
  # whether a wakeup time is missing on the first full day.
  # - if it is missing, then we will impute it in order for part5 to
  # the wake-to-wake analys on the second recording day.
  # Previously we accounted only for this later on in the code, which
  # did not benefit the exported timeseries.
  clock2numtime = function(x) { # function used for converting sleeplog times to hour times
    x2 = as.numeric(unlist(strsplit(x,":"))) / c(1,60,3600)
    return(sum(x2))
  }
  firstwake = which(diff(ts$diur) == -1)[1]
  firstonset = which(diff(ts$diur) == 1)[1]
  # test whether wake for second day is missing
  # if the full sleep period happens before midnights
  if (firstwake > nightsi[2] | (summarysleep_tmp2$sleeponset[1] < 18 & summarysleep_tmp2$wakeup[1] < 18 & firstwake < nightsi[2])) { 
    wake_night1_index =c()
    if (length(sleeplog) > 0) {
      # use sleeplog for waking up after first night
      wake_night1 = sleeplog$sleepwake[which(sleeplog$ID == ID & sleeplog$night == 1)]
      if (length(wake_night1) != 0) {
        wake_night1_hour = clock2numtime(wake_night1)
        # express hour relative to midnight within the noon-noon:
        if (wake_night1_hour > 12) wake_night1_hour = wake_night1_hour - 24 # express hour relative to midnight
        wake_night1_index = nightsi[1] + round(wake_night1_hour * Nepochsinhour)
        if (wake_night1_index > Nts) wake_night1_index = Nts
        if (wake_night1_index < 1) wake_night1_index = 1
      } else { # use HDCZA algorithm as plan B
        wake_night1_index = nightsi[1] + round((sptwindow_HDCZA_end[1]-24) * Nepochsinhour)
      }
    } else if (length(sptwindow_HDCZA_end) > 0 & length(sleeplog) == 0) {
      # use HDCZA algortihm for waking up after first night
      # if there was no sleep log
      if (is.na(sptwindow_HDCZA_end[1]) == FALSE) {
        if (sptwindow_HDCZA_end[1] != 0) {
          wake_night1_index = round(sptwindow_HDCZA_end[1] * Nepochsinhour)
        }
      }
    }
    if (length(wake_night1_index) == 0) {
      # use waking up from next day and subtract 24 hours,
      # the final option if neither of the above routes works
      wake_night1_index = (firstwake - (24* ((60/ws3new)*60))) + 1
    }
    if (wake_night1_index < firstwake & wake_night1_index > 1 & (wake_night1_index-1) > nightsi[1]) {
      ts$diur[nightsi[1]:(wake_night1_index-1)] = 1
    } else {
      # Person slept only during the afternoon on day 2
      # And there is no sleep data available for the first night
      # We will now add 5 minutes of dummy waking time before it this now, 
      # and label it as non-wear.
      # We do this to make sure that the day numbering
      # and merging of the sleep variables is still consistent with
      # the other recording.
      dummywake = max(firstonset - round(Nepochsinhour/12), nightsi[1])
      ts$diur[nightsi[1]:dummywake] = 1 
      ts$nonwear[nightsi[1]:firstonset] = 1
    }
  }
  return(ts)
}