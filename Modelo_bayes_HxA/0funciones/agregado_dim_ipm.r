agregado_dim_ipm <-
  function(poststrat,
           epredmat,
           byMap = c("depto", "etnia", "sexo")) {
    
    
    ## Creación de variable para el calculo nacional
    if(is.null(byMap)){
      poststrat  %<>% mutate(Nacional = "Nacional")
      byMap <- "Nacional"
    }
    ## Creación de indicadora poscición
    poststrat2 <- poststrat %>% ungroup() %>%
      mutate(Posi = 1:n()) %>%
      group_by_at(byMap) %>% group_nest()
    
    ## Creación de alertas por eliminar las categosrías de anoest
    
    if(any(byMap == "anoest")){
      poststrat2 %<>% filter(!anoest %in% c("99", "98"))
      cat("
     ############################# NOTA #################################
     # En las tabla de escolaridad (anoest) se eliminan los conteos de  #
     # NA y NS/NR                                                       #
     ############################# NOTA #################################
      ")
      
    }
    
    ## Estimado los mrp
    
    Estimado_mrp <- poststrat2 %>%
      mutate(Estimado_mrp =
               map(data,
                   function(subgrupo) {
                     filtering_condition <- subgrupo$Posi
                     n_filtered <- subgrupo$n
                     epred_mat_filtered <-
                       epredmat[, filtering_condition]
                     
                     if (length(n_filtered) > 1) {
                       mrp_estimates <-
                         epred_mat_filtered %*% n_filtered / sum(n_filtered)
                       
                     } else{
                       mrp_estimates <- as.numeric(epred_mat_filtered)
                     }
                     
                     data.frame(
                       estimate = mean(mrp_estimates),
                       estimate_se = sd(mrp_estimates)
                     )
                   }), data = NULL) %>% unnest("Estimado_mrp")
    
    
    return(Estimado_mrp)
  }
