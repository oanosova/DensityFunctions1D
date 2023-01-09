## coefficients of piecewise eta functions
eta_coef <- function(dt, k,i){  
  
  #### coefficients of piecewise eta functions
  ### y = y1 + (y2−y1)*(x-x1)/(x2−x1) = ax+b
  ### a = (y2−y1)/(x2−x1)
  ### b = y1 - x1* (y2−y1)/(x2−x1)
  ## need: x1, eta1_1_a and eta1_1_b
  
  ## ordering by x coordinate
  keycols <- c(paste0("eta",k,"_",i,"_x"))
  setkeyv(dt, keycols)
  dt <- unique(dt)
  
  ## coefficients for piecewise linear function
  dt[, paste0("eta",k,"_",i,"_a") := 
       c(tail(get(paste0("eta",k,"_",i,"_y")),-1) - head(get(paste0("eta",k,"_",i,"_y")),-1),0)/c(tail(get(paste0("eta",k,"_",i,"_x")),-1) - head(get(paste0("eta",k,"_",i,"_x")),-1),0)
  ][
    is.nan(get(paste0("eta",k,"_",i,"_a"))), paste0("eta",k,"_",i,"_a") := 0
  ]
  dt[, paste0("eta",k,"_",i,"_b") := 
       get(paste0("eta",k,"_",i,"_y")) - get(paste0("eta",k,"_",i,"_x"))*c(tail(get(paste0("eta",k,"_",i,"_y")),-1) - head(get(paste0("eta",k,"_",i,"_y")),-1),0)/c(tail(get(paste0("eta",k,"_",i,"_x")),-1) - head(get(paste0("eta",k,"_",i,"_x")),-1),0)
  ][
    is.nan(get(paste0("eta",k,"_",i,"_b"))), paste0("eta",k,"_",i,"_b") := 0
  ]
  dt[, x1 := get(paste0("eta",k,"_",i,"_x"))]
  #dt <- signif(dt, digits = 10)
  return(dt)
}