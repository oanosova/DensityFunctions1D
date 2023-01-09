## corner points for eta_k_i (Th 7)
eta_corner <- function(k,i){
  d <- min(dt_pts[i,dist], dt_pts[pt_nu1 == ((i+k) %% nu_pts),dist])
  print(paste0("d=",d))
  
  if(k==1){
    s <- 0
  }else{ # k>1
    dt_pts[, dist_tmp := 0]
    for(j in ((i+1):(i+k-1))){
      dt_pts[pt_nu1 == (j %% nu_pts), dist_tmp := dist_tmp+dist]
    }
    rm(j)
    s <- sum(dt_pts$dist_tmp, na.rm = TRUE)
    dt_pts[, dist_tmp := NULL]
    print(paste0("s=",s))
  }
  
  dt <- data.table(corner_nu = seq(0,3))
  dt[corner_nu == 0, paste0("eta",k,"_",i,"_x") := signif(s/2, digits = 10)] 
  dt[corner_nu == 0, paste0("eta",k,"_",i,"_y") := 0]
  
  dt[corner_nu == 1, paste0("eta",k,"_",i,"_x") := signif((dt_pts[i,dist]+s)/2, digits = 10)] 
  dt[corner_nu == 1, paste0("eta",k,"_",i,"_y") := d]
  
  dt[corner_nu == 2, paste0("eta",k,"_",i,"_x") := signif((dt_pts[pt_nu1 == ((i+k) %% nu_pts), dist]+s)/2, digits = 10)] 
  dt[corner_nu == 2, paste0("eta",k,"_",i,"_y") := d]
  
  dt[corner_nu == 3, paste0("eta",k,"_",i,"_x") := signif((dt_pts[i,dist] + dt_pts[pt_nu1 == ((i+k) %% nu_pts), dist]+s)/2, digits = 10)] 
  dt[corner_nu == 3, paste0("eta",k,"_",i,"_y") := 0]
  
  dt[, corner_nu := NULL ]
  
  rm(s,d)
  return(dt)
}