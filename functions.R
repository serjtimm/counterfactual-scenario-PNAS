### function: construct  period LT  ----------------------------------------------
compute_LT <- function(age, sex, mx){
  # input is:
  # (1) age - vector of ages 
  # (2) sex - "1" for males, "2" for females
  # (3) mx - vector of age-specific death rates
  
  #preparation
  mx [mx < 0] = 0 #NaN
  last <- length(mx)
  nx <- c(diff(age), 1)
  #ax
  ax<-rep(0.5,last)
  #a0
  if(sex==1){
    if (mx[1]<0.02300) {ax[1]<-0.14929-(1.99545*mx[1])}
    if ((0.0230<= mx[1])&(mx[1]<0.08307)) {ax[1]<-0.02832+(3.26021*mx[1])}
    if (0.08307<= mx[1]) {ax[1]<-0.29915}
  }
  
  if(sex==2){
    if (mx[1]<0.01724) {ax[1]<-0.14903-(2.05527*mx[1])}
    if ((0.01724 <= mx[1])&(mx[1]< 0.06891)) {ax[1]<-0.04667+(3.88089*mx[1])}
    if (0.06891<= mx[1]) {ax[1]<-0.31411}
  }
  #a_last
  if(mx[last]>0){ax[last]<-1/mx[last]}
  
  #qx and px
  qx<-nx*mx/(1+nx*(1-ax)*mx)
  qx[last]<-1             
  
  px<-1-qx
  
  #lx and dx 
  lx<-100000
  for(i in 1:(last-1)){          
    lx[i+1]<-lx[i]*px[i]}
  
  dx<-lx*qx
  dx[last]<-lx[last]
  
  #Lx and Tx
  Lx = nx * lx - nx * (1 - ax) * dx                
  Lx[last] = lx[last] * ax[last]
  
  Tx = rev(cumsum(rev(Lx)))
  
  #ex
  ex<-Tx/lx
  
  #data frame
  lt <- data.frame (age, mx=round(mx,5), ax=round(ax,3), qx=round(qx,5), lx=round(lx,0), 
                    dx=round(dx,0), Lx=round(Lx,0), Tx=round(Tx,0), ex=round(ex,2))
}

### function: construct  period LT based on qx  ----------------------------------------------
compute_LT.qx <- function(age, sex, qx, ax_last){
  # input is:
  # (1) age - vector of ages 
  # (2) sex - "1" for males, "2" for females
  # (3) qx - vector of age-specific probabilities to die
  # (4) ax_last - last ax
  
  #preparation
  qx[is.na(qx)] <- 0 #NaN
  last <- length(mx)
  nx <- c(diff(age), 1)
  #ax
  ax <- rep(0.5,last)
  #a0
  if(sex==1){
    if (qx[1]>=0.1) {ax[1]<-0.350}
    else {ax[1] <- 0.05 + 3*qx[1]}
  }
  
  if(sex==2){
    if (qx[1]>=0.1) {ax[1]<-0.33}
    else {ax[1] <- 0.0425 + 2.875*qx[1]}
  }
  
  #a_last
  ax[last] <- ax_last
  
  # px
  px<-1-qx
  
  #lx and dx 
  lx<-100000
  for(i in 1:(last-1)){          
    lx[i+1]<-lx[i]*px[i]}
  
  dx<-lx*qx
  dx[last]<-lx[last]
  
  #Lx and Tx
  Lx = nx * lx - nx * (1 - ax) * dx                
  Lx[last] = lx[last] * ax[last]
  
  Tx = rev(cumsum(rev(Lx)))
  
  #ex
  ex<-Tx/lx
  
  #data frame
  lt <- data.frame (age, ax=round(ax,3), qx=round(qx,5), lx=round(lx,0), 
                    dx=round(dx,0), Lx=round(Lx,0), Tx=round(Tx,0), ex=round(ex,2))
}