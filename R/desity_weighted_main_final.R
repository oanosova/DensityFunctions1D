library(data.table)
library(ggplot2)
#options(digits = 7)


##### Changeable input: 
## point coordinates on the line (ordered)
#pi_vect <- c(0,1/3,1/2) 
pi_vect <- c(0,1,3,4,5,7,9,10,12)/15 #S15
#pi_vect <- c(0,1,3,4,6,8,9,12,14)/15 #Q15

## weights (as radii)
#pi_rad <- c(1/12,0,1/12)
pi_rad <- c(1,1,1,1,1,2,1,1,2)/30 #for S15
#pi_rad <- c(1,1,1,1,2,1,1,2,1)/30 #for Q15

## number of psi functions
nu_psi <- 9
##### End of changeable input 

## number of points
nu_pts <- length(pi_vect)

## table of points & intervals
dt_pts <- data.table(pt_nu = seq(1,nu_pts+1), pts = 0, rad = 0, 
                     low_end = 0, up_end = 0, gap = 0)
dt_pts[, pts := c(pi_vect,pi_vect[1]+1)]

dt_pts[, rad := c(pi_rad,pi_rad[1])]

dt_pts[1, low_end := pts]
dt_pts[2:(.N), low_end := pts - rad]

dt_pts[(.N), up_end := pts]
dt_pts[1:(.N-1), up_end := pts + rad]

# gaps from the up_end to the next low_end
dt_pts[, gap := c(tail(low_end,-1) - head(up_end,-1),0)]
dt_pts[, gap := shift(gap, n=1L, fill = dt_pts$gap[[nu_pts]], "lag")] #gap_i = gap from i to (i-1)
dt_pts <- head(dt_pts,-1)


####### psi_0 function (from Th 3.2)
setkey(dt_pts, gap)
dt_psi_0 <- data.table(corner_nu = seq(0,nu_pts), psi0_x = 0, psi0_y = 0)
l <- sum(pi_rad)*2
dt_psi_0[corner_nu == 0, ":="(psi0_x = 0,psi0_y = 1-l)]

for(i in 1:nu_pts){
  dt_psi_0[corner_nu == i, ":="(psi0_x = 0.5*dt_pts[i, gap], psi0_y = 1- l- sum(head(dt_pts$gap,(i-1))) - (nu_pts - i+1)*dt_pts[i,gap])]
}
rm(i)

dt_psi_0 <- unique(dt_psi_0[,.(psi0_x,psi0_y)])
rm(l)

### list of psi function tables
psi_list <- list(dt_psi_0)

## plot of psi_0
p_psi0 <- ggplot(data = dt_psi_0) +
  labs(title=  paste0("Psi_0 for points ", paste0(signif(pi_vect,2), collapse = ",")))+
  geom_line(aes(x = psi0_x, y= psi0_y, color="psi0"), size = 1) + geom_point(aes(x = psi0_x, y= psi0_y, color="black"),size = 3) + 
  scale_colour_manual("", breaks = c("psi0"),
                      values = c("psi0" = "cyan"))
p_psi0

# saving the plot in the folder "pts_point_coordinates"
output_dir <- file.path(paste0("./data/output_weights/pts_",paste0(signif(pi_vect,2), collapse = ","),"_",paste0(signif(pi_rad,2), collapse = ",")))
if (!dir.exists(output_dir)){ dir.create(output_dir, recursive = TRUE) }
ggsave(paste0(output_dir,"/psi_",0,".png"), width = 7, height = 7, p_psi0)
rm(output_dir)

########### eta functions 

## reordering the points table
setkey(dt_pts,pt_nu)

dt_pts[, pt_nu1 := pt_nu %% nu_pts] # mod nu_pts
#dt_pts[, rad_lag := shift(rad, n=1L, fill = dt_pts$rad[[nu_pts]], "lag")] # r_{i-1}


### corner points for eta_k_i (Th 4.2 & 5.2)
source("./codes/codes_functions/eta_corner_wt.R") # corner point coordinates
source("./codes/codes_functions/eta_coef.R") # coefficients of piecewise linear functions

for(k in 1:nu_psi){
  print(paste0("k=",k))
  for(i in 1:nu_pts){
    print(paste0("i=",i))
    assign(paste0("dt_eta_",k,"_",i), eta_coef(eta_corner_wt(k,i),k,i))
  }
  rm(i)
}
rm(k)

### calculating psi_k from eta_k_i, plotting psi and eta for each k and adding psi_k to `psi_list`
source("./codes/codes_functions/psi_eta_plotting.R")

for(k in 1:nu_psi){
  ## merging eta_k (with the same k) into one table
  dt_etasum <- (get(paste0("dt_eta_",k,"_",1))[,.SD, .SDcols = c("x1", paste0("eta",k,"_",1,"_a"),paste0("eta",k,"_",1,"_b"))])
  for(i in 2:nu_pts){
    dt_etasum <- merge(dt_etasum, get(paste0("dt_eta_",k,"_",i))[,.SD, .SDcols = c("x1", paste0("eta",k,"_",i,"_a"), paste0("eta",k,"_",i,"_b"))], by = "x1", all = TRUE)
  }
  rm(i)
  
  ## filling missing coefficients with previous values if those exist
  setkey(dt_etasum,x1)
  setnafill(dt_etasum, type = "locf")
  
  ## filling the rest of missing coefficients with 0
  for(j in seq_len(ncol(dt_etasum))){
    set(dt_etasum,which(is.na(dt_etasum[[j]])),j,0)
  }
  rm(j)
  
  dt_etasum[, y1_a := Reduce(`+`, .SD), .SDcols = grep("_a", names(dt_etasum), value = T)]
  dt_etasum[, y1_b := Reduce(`+`, .SD), .SDcols = grep("_b", names(dt_etasum), value = T)]
  
  dt_etasum[, y1 := y1_a*x1+y1_b]
  
  ## adding psi_k to the list of all psi function tables 
  psi_list[[k+1]] <- dt_etasum
  
  ## plotting & columns psi_i
  psi_eta_plotting(dt_etasum,k,wt = TRUE, tofile = TRUE) # psi and all eta on the same plot
  psi_eta_plotting(dt_etasum,k, separate = TRUE, wt = TRUE, tofile = TRUE) # psi and eta on separate plots

  rm(dt_etasum)
}
rm(k)


### all tables from psi_list in one graph

setnames(psi_list[[1]],"psi0_x","x1")
setnames(psi_list[[1]],"psi0_y","psi_0")

source("./codes/codes_functions/all_psi.R")

p <- all_psi(psi_list, nu_psi, wt = TRUE, tofile = FALSE)
p