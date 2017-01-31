if(!exists('inds')){inds <- read_rds(root_file('./1-data/5-tidy/coo-acs-inds-not-acs-sf.rds'))}

# inds %>% unclass %>% as_tibble() %>% View

# make_lbl <- function(x){
#         
#         if(x %>% as.numeric() > 1){
#                 x %>% as.numeric() %>% scales::comma()
#         }else{
#                 x %>% as.numeric() %>% multiply_by(100) %>% round_any(1) %>% paste0('%')
#         }
# }

make_popup_tbl <- function(.category,.topic,.tbl_cols){
        #dataset - should be full
        # browser()
        data <-
                inds %>% 
                unclass %>% 
                as_tibble() %>% 
                filter(GEOGRAPHY %in% c('tract','county subdivision')) %>% 
                filter(SEACCD_LGL|NAME %in% 'SEACCD') %>% 
                group_by(GEOGRAPHY) 
        
        if(.category %in% 'VULN'){
                remove_cols <- '2009'
                
                
        }else{
                remove_cols <- 'NOTFOUND'
                
                
        }
        
        # after the table is produced
        popup_tbl_df <- 
                data %>% 
                select(NAME,GEOGRAPHY,matches('EST')) %>% 
                select(-matches('VULN_CNT_2009|VULN_PCT_2009|SEACCD'))%>% 
                gather(TBL,VAL,-NAME,-GEOGRAPHY) %>% 
                mutate(CAT = str_sub(TBL,1,4) %>% str_replace('_',''),
                       TOPIC = str_extract(TBL,'B[[:digit:]]{5}')) %>%
                mutate(TYPE = str_extract(TBL,'_[[:alpha:]]{3}_'),
                       TYPE = str_replace_all(TYPE,'_','')) %>%
                mutate(VAL_LBL = if_else(TYPE %in% 'PCT',
                                         VAL %>% multiply_by(100) %>% round_any(1) %>% paste0('%'),
                                         VAL %>% round_any(1) %>% scales::comma())) %>% 
                mutate(VAL_LBL = if_else(TYPE %in% 'PCT' & VAL>=0 & str_detect(TBL, 'DEMO_CHNG'),
                                         str_c('+',VAL_LBL),
                                         VAL_LBL)) %>% 
                select(-VAL,-TYPE) %>% 
                filter(CAT %in% c(.category,'TOT') & TOPIC %in% .topic) %>% 
                filter(!str_detect(TBL,remove_cols)) %>% 
                select(-CAT) %>% 
                spread(TBL,VAL_LBL) %>% 
                as.data.frame() %>% 
                mutate(NAME_FULL = if_else(str_detect(NAME,'SEACCD'),
                                           NAME,
                                           str_c('Tract',NAME,sep = ' '))) %>% 
                set_rownames(extract2(.,'NAME_FULL')) 
                
        
        if(.category %in% 'VULN'){
                
                popup_tbl_df <- 
                        popup_tbl_df %>% 
                        as.data.frame(check.names = FALSE) %>% 
                        select('#' = matches('CNT'),
                               '%' = matches('PCT'),
                               '#' = matches('TOT')
                        )  %>% as.data.frame(check.names = FALSE)
                
                
                n.cgroup <- c(2,1)
                align_cgroup <- paste(rep('c',3),collapse='')
                
                
        }else{
                
                popup_tbl_df <- 
                        popup_tbl_df %>% 
                        as.data.frame(check.names = FALSE) %>% 
                        select('#' = matches('CNT_2009'),
                               '%' = matches('PCT_2009'),
                               '#' = matches('TOT_2009'),
                               '#' = matches('CNT_2015'),
                               '%' = matches('PCT_2015'),
                               '#' = matches('TOT_2015'),
                               '%' = matches('CHNG')
                        ) %>% as.data.frame(check.names = FALSE)
                
                n.cgroup <- c(3,3,1)
                align_cgroup <- paste(rep('c',7),collapse='')
                
        }
        
        
        seaccd_row <- which(rownames(popup_tbl_df) %in% 'SEACCD')[[1]]
        
        cgroup <-  .tbl_cols
        
        
        
        
        n.cgroup 
        
        lapply(seq_along(1:nrow(popup_tbl_df)), 
                                        function(n){
                                                tibble('NAME' = popup_tbl_df %>% rownames() %>% extract2(n) %>% str_replace('Tract ' ,''),
                                                       'CATEGORY' = .category,
                                                       'TOPIC' = .topic,
                                                       'POPUP' = htmlTable::htmlTable(popup_tbl_df[c(n,seaccd_row),],
                                                                                      cgroup = cgroup,
                                                                                      n.cgroup = n.cgroup,
                                                                                      align = 'r',
                                                                                      align.cgroup = align_cgroup))
                                        } 
               )[!rownames(popup_tbl_df) %in% 'SEACCD'] %>% 
                reduce(bind_rows)
                
        
        # tibble('NAME' = rownames(popup_tbl_df)[[row]],
        #        'CATEGORY' = .category,
        #        'TOPIC' = .topic,
        #        'popup' = htmlTable::htmlTable(popup_tbl_df[c(row,seaccd_row),],
        #                                       cgroup = cgroup,
        #                                       n.cgroup = n.cgroup))[!rownames(popup_tbl_df) %in% 'SEACCD',]
        
        # lapply(seq_along(1:nrow(popup_tbl_df)), 
        #                          function(row) htmlTable::htmlTable(popup_tbl_df[c(row,seaccd_row),],
        #                                                             cgroup = cgroup,
        #                                                             n.cgroup = n.cgroup))[!rownames(popup_tbl_df) %in% 'SEACCD']
               
        
        
}

tbl <- 
        tibble(
                'category' = c(rep(c('DEMO'),4),
                               rep(c('VULN'),4)),
                'topic' = rep(c('B03002',
                                'B15002',
                                'B19001',
                                'B25033'),2),
                'tbl_cols' = list(
                        c('2009','2015','Change'),
                        c('2009','2015','Change'),
                        c('2009','2015','Change'),
                        c('2009','2015','Change'),
                        c('PoC','Tot. Pop.'),
                        c('l/t BA','Tot. +25'),
                        c('Low Inc','Tot. HH'),
                        c('Rent','Tot. HU')
                )
        ) %>% 
        invoke_rows(list,.,.to = 'RESULT') %>% 
        extract2('RESULT') %>% 
        map(
                ~ make_popup_tbl(.category = extract2(.x,'category'),
                                 .topic = extract2(.x,'topic'),
                                 .tbl_cols = extract2(.x,'tbl_cols'))
        ) %>% 
        reduce(bind_rows) %>% 
        mutate(POPUP = map(POPUP, ~ as(.x,'htmlTable'))) %>% 
        unite(CAT_TOP, CATEGORY,TOPIC, sep = '_') %>% 
        mutate(CAT_TOP = paste0(CAT_TOP,'_POPUP')) %>% 
        spread(CAT_TOP,POPUP)
        


tbl %>% write_rds(root_file('./1-data/4-interim/htmltable-popups.rds'))

