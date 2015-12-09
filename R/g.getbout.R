g.getbout = function(rr,boutdur2,boutcriter=100,p) {
  rrt = rr # zeros as long as there are epochs in the timewindow
  jmvpa = 1
  LRR = length(rr)
  while(jmvpa <= length(p)) { # go thourgh all epochs that are possibly part of a bout
    endi = p[jmvpa]+boutdur2
    if (endi <= LRR) { #does bout fall without measurement?
      if (sum(rr[p[jmvpa]:endi]) > (boutdur2*boutcriter)) {
        #if boutcriter.mvpa = 0.9 then 90% of the bout needs to meet the criteria
        select = p[jmvpa:max(which(p < endi))] # changed on 20-9-2015, now only the epochs with the acceleration value in the right range
        # are labelled as part of the bout. as a result an MVPA bout, Light bout or inactivity bout can never become ambigous.
        rrt[select] = 2 #remember that this was a bout
      } else {
        select = 0
        rr[p[jmvpa]] = 0
      }      
    } else { #bout does not fall within measurement
      select = 0
      if (length(p) > 1 & jmvpa > 2) {
        rr[p[jmvpa]] = rr[p[jmvpa-1]]
      }        
    }
    jmvpa = jmvpa + length(select)
  }
  rr[which(rrt == 2)] = 1
  g.getbout = rr
}