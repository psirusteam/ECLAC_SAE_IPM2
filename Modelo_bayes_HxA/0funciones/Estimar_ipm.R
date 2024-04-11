estime_IPM <-
  function(poststrat,
           chain_ci,
           chain_ind,
           byMap = c("dam", "etnia", "sexo")) {

    ## Creación de variable para el calculo nacional
    if(is.null(byMap)){
      poststrat  %<>% mutate(Nacional = "Nacional")
      byMap <- "Nacional"
    }
    
    numIPM <- t(chain_ci) %>%
      as.data.frame() %>%
      mutate_all(~ . * poststrat$n) %>%
      as.matrix()
    
    chain_N <- t(chain_Ind) %>%
      as.data.frame() %>%
      mutate_all(~ . * poststrat$n) %>%
      as.matrix()
    
    ## Creación de indicadora poscición
    poststrat2 <- poststrat %>% ungroup() %>%
      mutate(Posi = 1:n()) %>%
      group_by_at(byMap) %>% group_nest()
    
    ## Creación de alertas por eliminar las categosrías de anoest
    ## Estimado los mrp
    Estimado_mrp <- poststrat2 %>%
      mutate(Estimado_mrp =
               map(data,
                   function(subgrupo) {
                     filtering_condition <- subgrupo$Posi
                     n_filtered <- subgrupo$n
                     numIPM_sub <- numIPM[filtering_condition,]
                     chain_N_sub <- chain_N[filtering_condition,]
                     
                     if(nrow(subgrupo)==1){
                       temp <- data.frame(H = NA,
                                  H_sd = NA,
                                  A = NA,
                                  A_sd = NA,
                                  IPM = NA,
                                  IPM_sd = NA)
                       return(temp)
                     }else{
                       IPM_l <- colSums(numIPM_sub)/sum(n_filtered)
                       Nz_l <- colSums(chain_N_sub)
                       H_l <- Nz_l/sum(n_filtered)
                       A_l <- colSums(numIPM_sub)/Nz_l
                     }
                    
                     data.frame(H = mean(H_l),
                                H_sd = sd(H_l),
                                A = mean(A_l),
                                A_sd = sd(A_l),
                                IPM = mean(IPM_l),
                                IPM_sd = sd(IPM_l))
                     
                   }), data = NULL) %>% unnest("Estimado_mrp")
 
    return(Estimado_mrp)
  }
