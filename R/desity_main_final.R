library(data.table)
library(ggplot2)
#options(digits = 7)


##### Changeable input: 
## point coordinates on the line
#pi_vect <- c(0,1/3,1/2) 
pi_vect <- c(0,1,3,4,5,7,9,10,12)/15
#pi_vect <- c(0,1,3,4,6,8,9,12,14)/15
## number of psi functions
nu_psi <- 9
##### End of changeable input

## number of points
nu_pts <- length(pi_vect)

## table of periodic points (period = 1)
dt_pts <- data.table(pt_nu = seq(1,nu_pts+1), pts = 0, dist = 0)
dt_pts[, pts := c(pi_vect,pi_vect[1]+1)]

# distances to the next point
dt_pts[, dist := c(tail(pts,-1) - head(pts,-1),0)]
dt_pts <- head(dt_pts,-1)


######### psi_0 (defined in Th 5)
setkey(dt_pts, dist)
dt_psi_0 <- data.table(corner_nu = seq(0,nu_pts), psi0_x = 0, psi0_y = 0)
dt_psi_0[corner_nu == 0, ":="(psi0_x = 0,psi0_y = 1)]

for(i in 1:nu_pts){
  #dt_psi_0[corner_nu == i, ":="(psi0_x = 0.5*dt_pts[i, dist], psi0_y = 1- sum(head(dt_pts$dist,(i-1))) - (nu_pts - i+1)*dt_pts[i,dist])]
  dt_psi_0[corner_nu == i, ":="(psi0_x = signif(0.5*dt_pts[i, dist], digits = 10), psi0_y = signif(1- sum(head(dt_pts$dist,(i-1))) - (nu_pts - i+1)*dt_pts[i,dist]), digits = 10)]
}
rm(i)
dt_psi_0 <- unique(dt_psi_0[,.(psi0_x,psi0_y)])

### list of psi function tables
psi_list <- list(dt_psi_0)

## plot of psi_0
p_psi0 <- ggplot(data = dt_psi_0) +
  labs(title=  paste0("Psi_0 for points ", paste0(signif(pi_vect,2), collapse = ",")))+
  geom_line(aes(x = psi0_x, y= psi0_y, color="psi0"), size = 1) + geom_point(aes(x = psi0_x, y= psi0_y, color="black"),size = 3) +
  scale_colour_manual("", breaks = c("psi0"),
                      values = c("psi0" = "cyan"))
print(p_psi0)

# saving the plot in the folder "pts_point_coordinates"
output_dir <- file.path(paste0("./data/output/pts_",paste0(signif(pi_vect,2), collapse = ",")))
if (!dir.exists(output_dir)){ dir.create(output_dir, recursive = TRUE) }
ggsave(paste0(output_dir,"/psi_",0,".png"), width = 7, height = 7, p_psi0)
rm(output_dir)



########### eta functions 

## reordering the points table
setkey(dt_pts,pt_nu)

dt_pts[, pt_nu1 := pt_nu %% nu_pts]

## corner points for eta_k_i (Th 7)
source("./codes/codes_functions/eta_corner.R") # corner point coordinates
source("./codes/codes_functions/eta_coef.R") # coefficients of piecewise linear functions

for(k in 1:nu_psi){
  print(paste0("k=",k))
  for(i in 1:nu_pts){
    print(paste0("i=",i))
    assign(paste0("dt_eta_",k,"_",i), eta_coef(eta_corner(k,i),k,i))
  }
  rm(i)
}
rm(k)

### calculating psi_k from eta_k_i, plotting psi and eta for each k and adding psi_k to `psi_list`
source("./codes/codes_functions/psi_eta_plotting.R")

for(k in 1:nu_psi){
  ## merging eta_k (same k)
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
  
  ## coefficients of the summative psi function
  dt_etasum[, y1_a := Reduce(`+`, .SD), .SDcols = grep("_a", names(dt_etasum), value = T)]
  dt_etasum[, y1_b := Reduce(`+`, .SD), .SDcols = grep("_b", names(dt_etasum), value = T)]
  
  ## values of psi function
  dt_etasum[, y1 := y1_a*x1+y1_b]
  
  ## adding psi_k to the list of psi function tables 
  #assign(paste0("dt_psi_",k),copy(dt_etasum))
  psi_list[[k+1]] <- dt_etasum
  
  ## plotting 
  psi_eta_plotting(dt_etasum,k) # psi and all eta on the same plot
  psi_eta_plotting(dt_etasum,k, separate = TRUE) # psi and eta on separate plots 
  
  rm(dt_etasum)
}
rm(k)


### all tables from psi_list in one graph

setnames(psi_list[[1]],"psi0_x","x1")
setnames(psi_list[[1]],"psi0_y","psi_0")

source("./codes/codes_functions/all_psi.R")

p <- all_psi(psi_list, nu_psi, wt = FALSE, tofile = TRUE)
p
