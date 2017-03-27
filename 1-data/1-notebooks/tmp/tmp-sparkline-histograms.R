# Example from SO

tmp <- 
        ChickWeight %>% 
        mutate(cut_weight = as.numeric(cut(weight, c(0, 50, 100, 150, 200, 300) , labels = c(1:5)))) %>% 
        group_by(Chick, cut_weight) %>% 
        tally %>% 
        complete(cut_weight = c(1:5), fill = list(n = 0)) %>% 
        summarize(
                cut_weight = spk_chr(
                        cut_weight,
                        type = "bar",
                        barWidth = 20,
                        barSpacing = 5,
                        highlightColor = 'orange',
                        tooltipFormat = '{{offset:levels}} : {{value}}',
                        tooltipValueLookups = list(
                                levels = list( 
                                        '0' = '0-50',
                                        '1' = '51-100',
                                        '2' = '101-150',
                                        '3' = '151-200',
                                        '4' = '201-300'
                                )
                        )
                )
        ) 

# Work in progress...
tmp_df <- 
        crossing(c('VULN_PCT_2015','DEMO_CHNG_PCT'),
                 c('SEACCD','KC'),
                 c('B03002_EST','B15002_EST', 'B19001_EST', 'B25033_EST')) %>% 
        
        transpose %>% 
        map_chr(str_c, collapse = '_') %>% 
        tibble('BASELINE_IND' = .,
               'TOPIC' = c('race', 'housing', 'ed', 'income') %>% rep(4),
               'CATEGORY' = c(rep('DEMO_CHNG',8),rep('VULN',8))) %>% 
        mutate('BASELINE' = str_extract(BASELINE_IND, 'SEACCD|KC'),
               'IND' = str_replace(BASELINE_IND, '_SEACCD_|_KC_','_')) %>% 
        select(TOPIC, CATEGORY,BASELINE,IND,BASELINE_IND) 

inds <- 
        read_rds(root_file('./1-data/5-tidy/coo-acs-inds-not-acs-sf.rds')) %>% 
        unclass %>% 
        as_tibble()

inds_cuts <- 
        read_rds(root_file('./1-data/5-tidy/coo-acs-inds-not-acs-sf.rds')) %>%  
        select(NAME,NAME_FULL,GEOGRAPHY,GEOG_FCT,COMMUNITY,SEACCD_LGL,matches('EST')) %>% 
        select(-matches('VULN_CNT_2009|VULN_PCT_2009|SEACCD_B|KC')) %>% 
        mutate(row = row_number()) %>% 
        gather(TBL,VAL,-NAME,-NAME_FULL,-GEOGRAPHY,-GEOG_FCT,-COMMUNITY,-SEACCD_LGL,-geometry,-row) %>% 
        mutate(CAT = str_sub(TBL,1,4) %>% str_replace('_',''),
               TOPIC = str_extract(TBL,'B[[:digit:]]{5}'),
               CAT_TOP = str_c(CAT,TOPIC,sep = '_')) %>% 
        filter(str_detect(TBL, 'DEMO_CHNG_PCT|VULN_PCT')) %>%
        group_by(CAT_TOP) %>% 
        nest %>% 
        crossing('SEACCD_ONLY_LGL' = c(TRUE,FALSE)) 

inds_cuts %>% 
        mutate(SPARKLINE = map2_dbl(data, SEACCD_ONLY_LGL,~ .x %>% 
                                       {if(.y) filter(., SEACCD_LGL) else .} %>% 
                                       extract2('VAL') %>% sparkline( type = 'box', 
                                                                      tooltipFormatFieldlist = c('med', 'lq', 'uq'), 
                                                                      tooltipFormatFieldlistKey =  'field')))
