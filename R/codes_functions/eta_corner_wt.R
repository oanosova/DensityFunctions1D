## corner points for eta_k_i (Th 4.2 & 5.2)
eta_corner_wt <- function(k,i){
  if(k==1){
    g <- min(dt_pts[i,gap], dt_pts[pt_nu1 == ((i+1) %% nu_pts),gap])
    print(paste0("g=",g))
    
    dt <- data.table(corner_nu = seq(0,3))
    dt[corner_nu == 0, paste0("eta",k,"_",i,"_x") := 0] 
    dt[corner_nu == 0, paste0("eta",k,"_",i,"_y") := 2*dt_pts[i,rad]]
    
    dt[corner_nu == 1, paste0("eta",k,"_",i,"_x") := signif((dt_pts[i,gap])/2, digits = 10)] 
    dt[corner_nu == 1, paste0("eta",k,"_",i,"_y") := g + 2*dt_pts[i,rad]]
    
    dt[corner_nu == 2, paste0("eta",k,"_",i,"_x") := signif((dt_pts[pt_nu1 == ((i+1) %% nu_pts), gap])/2, digits = 10)] 
    dt[corner_nu == 2, paste0("eta",k,"_",i,"_y") := g + 2*dt_pts[i,rad]]
    
    dt[corner_nu == 3, paste0("eta",k,"_",i,"_x") := signif((dt_pts[i,gap] + dt_pts[pt_nu1 == ((i+1) %% nu_pts), gap])/2 + dt_pts[i, rad], digits = 10)] 
    dt[corner_nu == 3, paste0("eta",k,"_",i,"_y") := 0]
    
    dt[, corner_nu := NULL ]
    
  }else{ # k>1
    g <- min(dt_pts[i,gap+2*rad], dt_pts[pt_nu1 == ((i+k) %% nu_pts),gap] + 
               2* dt_pts[pt_nu1 == ((i+k-1) %% nu_pts), rad])
    g1 <- max(dt_pts[i,gap+2*rad], dt_pts[pt_nu1 == ((i+k) %% nu_pts),gap] + 
                2* dt_pts[pt_nu1 == ((i+k-1) %% nu_pts), rad])                   
    
    print(paste0("g= ", g, ", g1 = ",g1))
    
    
    dt_pts[, gap_tmp:=0]
    for(j in ((i+1):(i+k-1))){
      dt_pts[pt_nu1 == (j %% nu_pts), gap_tmp := gap_tmp+gap]
    }
    rm(j)
    
    dt_pts[, rad_tmp := 0]
    if((i+1) <= (i+k-2)){
      for(j in ((i+1):(i+k-2))){
        dt_pts[pt_nu1 == (j %% nu_pts), rad_tmp := rad_tmp+rad]
      }
      rm(j)
    }
    
    s <- sum(dt_pts$gap_tmp, na.rm = TRUE)+ 2*sum(dt_pts$rad_tmp, na.rm = TRUE)
    print(paste0("s=",s))
    dt_pts[, ":="(gap_tmp = NULL,rad_tmp = NULL)]
    
    
    dt <- data.table(corner_nu = seq(0,3))
    dt[corner_nu == 0, paste0("eta",k,"_",i,"_x") := s/2] 
    dt[corner_nu == 0, paste0("eta",k,"_",i,"_y") := 0]
    
    dt[corner_nu == 1, paste0("eta",k,"_",i,"_x") := signif((g+s)/2, digits = 10)] 
    dt[corner_nu == 1, paste0("eta",k,"_",i,"_y") := g]
    
    dt[corner_nu == 2, paste0("eta",k,"_",i,"_x") := signif((s+g1)/2, digits = 10)] 
    dt[corner_nu == 2, paste0("eta",k,"_",i,"_y") := g]
    
    dt[corner_nu == 3, paste0("eta",k,"_",i,"_x") := signif((g+s+g1)/2, digits = 10)] 
    dt[corner_nu == 3, paste0("eta",k,"_",i,"_y") := 0]
    
    dt[, corner_nu := NULL ]
  }
  
  rm(s)
  rm(g,g1)
  return(dt)
}