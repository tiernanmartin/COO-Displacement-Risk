if(!exists('inds')){inds <- read_rds(root_file('./1-data/5-tidy/coo-acs-inds-not-acs-sf.rds'))}

tbl_popup <- read_rds(root_file('./1-data/4-interim/htmltable-popups.rds')) %>% 
        gather(TBL,POPUP,-NAME) %>% 
        mutate(CAT_TOP = str_replace_all(TBL,'_POPUP','')) %>% 
        select(-TBL)

map_data <- 
        inds %>% 
        filter(GEOGRAPHY %in% c('tract','county subdivision')) %>% 
        filter(SEACCD_LGL|NAME %in% 'SEACCD') %>% 
        select(NAME,GEOGRAPHY,matches('EST')) %>% 
        select(-matches('VULN_CNT_2009|VULN_PCT_2009|SEACCD')) %>% 
        mutate(row = 1:nrow(.)) %>% 
        gather(TBL,VAL,-NAME,-GEOGRAPHY,-geometry,-row) %>% 
        mutate(CAT = str_sub(TBL,1,4) %>% str_replace('_',''),
               TOPIC = str_extract(TBL,'B[[:digit:]]{5}'),
               CAT_TOP = str_c(CAT,TOPIC,sep = '_')) %>% 
        filter(str_detect(TBL, 'DEMO_CHNG_PCT|VULN_PCT')) %>% 
        left_join(tbl_popup, by = c('NAME','CAT_TOP'))


make_acs_map <- function(long_sf,.geog,.category,.topic,.pal,.legend_title){
        
        # browser()
        
        data <- 
                long_sf %>% 
                filter(GEOGRAPHY %in% .geog) %>% 
                filter(CAT %in% .category) %>% 
                filter(TOPIC %in% .topic) 
        
        
        pal <- colorNumeric(.pal,data$VAL)
                        
        myLfltGrey() %>%
                myLfltOpts() %>%
                addPolygons(data = as(data,'Spatial'),
                            fillOpacity = .75,
                            fillColor = ~pal(VAL),
                            color = col2hex('white'),
                            opacity = 1,
                            weight = .5,
                            smoothFactor = 0,
                            popup = ~POPUP) %>%
                addLegend(title = .legend_title,
                          position = 'topright',
                          pal = pal,
                          values = data %>% extract2('VAL'),
                          opacity = .75,
                          labFormat = labelFormat(suffix = '%',transform = function(x) 100 * x)) %>%
                styleWidget(style = "text-transform:uppercase;text-shadow:-1px 0 #FFFFFF,0 1px #FFFFFF,1px 0 #FFFFFF,0 -1px #FFFFFF")       
        
        
}


make_acs_map(long_sf = map_data,
             .geog = 'tract',
             .category = 'VULN',
             .topic = 'B03002',
             .pal = 'plasma',
             .legend_title = "CHANGE IN SHARE<br>PEOPLE OF COLOR<hr>2009-2015")
