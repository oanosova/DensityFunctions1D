## plotting `nu_psi1` of the psi functions from the `fun_list` on the same plot
all_psi <- function(fun_list, nu_psi1, wt = FALSE, tofile = TRUE){
  dt <- fun_list[[1]]
  dt <- signif(dt, digits = 5)
  
  for(j in 1:nu_psi1){
    dt_tmp <- unique(signif(fun_list[[j+1]][,.SD, .SDcols = c("x1",paste0("psi_",j))],digits =5))
    dt <- merge(dt, dt_tmp, all = TRUE)
    rm(dt_tmp)
  }
  rm(j)
  
  dt_long <- melt(dt, id.vars="x1")
  
  if(wt == FALSE){
    p_psi_all <- ggplot(data = dt_long, aes(x = x1, y = value, col = variable)) +
      labs(y= paste0("psi_k"),
           x = paste0("t"),
           title=  paste0("Psi functions for points ", paste0(signif(pi_vect,2), collapse = ",")), 
           colour = "") +
      geom_line(data = dt_long[!is.na(value)],size = 1) +
      geom_point(size = 2)
  }else{ #with weights
    p_psi_all <- ggplot(data = dt_long, aes(x = x1, y = value, col = variable)) +
      labs(y= paste0("psi_k"),
           x = paste0("t"),
           title=  paste0("Psi functions for points ", paste0(signif(pi_vect,2), collapse = ","),"\n with weights ", 
                          paste0(signif(pi_rad,2), collapse = ",")), 
           colour = "") +
      geom_line(data = dt_long[!is.na(value)],size = 1) +
      geom_point(size = 2) 
  }
  
  
  if(tofile == TRUE){
    if(wt == FALSE){
      output_dir <- file.path(paste0("./data/output/pts_",paste0(signif(pi_vect,2), collapse = ",")))
    }else{ #with weights
      output_dir <- file.path(paste0("./data/output_weights/pts_",paste0(signif(pi_vect,2), collapse = ","),"_",paste0(signif(pi_rad,2), collapse = ",")))
    }
    if (!dir.exists(output_dir)){ dir.create(output_dir, recursive = TRUE) }
    
    ggsave(paste0(output_dir,"/psi_all_",nu_psi1,".png"), width = 8, height = 4, p_psi_all)
    rm(output_dir)
  }
  
  rm(dt, dt_long)
  return(p_psi_all)
}