## plotting eta functions on the same plot
psi_plotting <- function(fun_list, k1, separate = FALSE){
  dt <- fun_list[[1]]
  
  
  for(i in 1:nu_pts){
    dt[, paste0("eta_",k,"_",i) := get(paste0("eta",k,"_",i,"_a"))*x1 + get(paste0("eta",k,"_",i,"_b"))]
  }
  rm(i)
  dt[, paste0("psi_",k):= y1]
  dt_long <- melt(dt[,.SD, .SDcols = c(1, (ncol(dt)-nu_pts): ncol(dt))], id.vars="x1")
  
  output_dir <- file.path(paste0("./data/output/pts_",paste0(signif(pi_vect,2), collapse = ",")))
  if (!dir.exists(output_dir)){ dir.create(output_dir, recursive = TRUE) }
  
  if(separate == FALSE){
    p_psi <- ggplot(data = dt_long, aes(x = x1, y = value,col = variable)) +
      labs(title=  paste0("Psi_",k," and trapezoid eta functions for points ", paste0(signif(pi_vect,2), collapse = ",")), 
           y= paste0("psi_",k,", eta_",k,"_i"),
           colour = "") +
      geom_line(size = 1) +
      geom_point(size = 3)
    
    ggsave(paste0(output_dir,"/psi_",k,"_eta.png"), width = 7, height = 7, p_psi)
    
  }else{#separate plots for psi and eta
    p_psi <- ggplot(data = dt_long, aes(x = x1, y = value,col = variable)) +
      labs(title=  paste0("Psi_",k," and trapezoid eta functions for points ", paste0(signif(pi_vect,2), collapse = ",")), 
           y= paste0("psi_",k,", eta_",k,"_i"),
           colour = "") +
      geom_line(size =1) + 
      geom_point(size = 3) +
      facet_wrap(~variable)
    ggsave(paste0(output_dir,"/psi_",k,"_eta_separate.png"), width = 7, height = 7, p_psi)
  }
  rm(output_dir)
}
