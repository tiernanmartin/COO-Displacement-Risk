estimate_m2m <- function(acs_GEOID6_2010){
        
        if(!exists('cw')){cw <- read_rds(root_file('1-data/4-interim/cw.rds'))}
        
        if(!exists('chng_white_2014_orig_acs')){chng_white_2014_orig_acs <- read_rds(root_file('1-data/4-interim/chng-white-2014-orig-acs.rds'))}
        acs_GEOID6_2010 <- tr_chng_m2m_2010[2]
        
        acs_GEOID6_2000 <- 
                cw %>% 
                filter(GEOID6_2010 %in% acs_GEOID6_2010) %>% 
                arrange(GEOID_2010,GEOID_2000) %>% 
                select(GEOID6_2000) %>% 
                unlist(use.names = FALSE)
        
        acs_weight_2000 <- 
                cw %>% 
                filter(GEOID6_2010 %in% acs_GEOID6_2010) %>% 
                arrange(GEOID_2010,GEOID_2000) %>% 
                select(weight) %>% 
                unlist(use.names = FALSE)
        
        tmp <- 
                chng_white_2009_m2m_acs[geography(chng_white_2009_m2m_acs)$tract %in% acs_GEOID6_2000,]

        weighted <- acs::apply(tmp,MARGIN = 1,FUN = function(x) x*acs_weight_2000)
        
        sum <- acs::apply(X = weighted,MARGIN = 1,FUN = sum)
        
        
        258 50.2521994134898
        
        acs_GEOID6_2000 <- 
                cw %>% 
                filter(GEOID6_2010 %in% acs_GEOID6_2010) %>% 
                select(GEOID6_2000) %>% 
                unlist(use.names = FALSE)
        
        acs_m2m_transformed <- chng_white_2009_m2m_acs[geography(chng_white_2009_m2m_acs)$tract %in% acs_GEOID6_2000,] * acs_weight
        
        acs_geo_2010 <- 
                chng_white_2014_acs %>% 
                geography() %>% 
                filter(tract %in% acs_GEOID6_2010)
        
        geography(acs_m2m_transformed) <- acs_geo_2010
        
        return(acs_m2m_transformed)
        
}

cw %>%  
        filter(changetype %in% c('4')) %>% 
        select(1:5,changetype) %>% 
        arrange(GEOID_2010,GEOID_2000) %>% 
        as.data.frame() %>% View
        split(.$GEOID_2010)

