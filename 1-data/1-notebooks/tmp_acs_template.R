poc_acs <- {
        if (!exists("poc_acs")) {
                if (!file.exists("./2_inputs/2_intermediate/poc_acs.rds")) {
                        make_poc_acs <- function() {
                                
                                # DOWNLOAD ACS DATA
                                # -------------------------------------------------------
                                
                                tbl <- "B03002"  # census table code
                                
                                acs.fetch(endyear = 2014, geography = kc_tr_acs, 
                                          table.number = tbl) %>%
                                        subset(.@geography$tract %in% sea_tr) -> poc_acs1  # subset to include only Seattle tracts
                                
                                
                                # PROCESSING
                                # -------------------------------------------------------
                                
                                # find the column codes acs.fetch(endyear = 2014, geography =
                                # kc_tr_acs, table.number = tbl,col.names = 'pretty') %>%
                                # acs.colnames() %>% cbind(acs.colnames(col1)) %>% View
                                # 
                                # the code for all races is 'B03002_001' and for non-hispanic whites is 'B03002_003'
                                
                                poc_acs1[, "B03002_001"] - poc_acs1[, "B03002_003"] -> poc_acs2
                                
                                acs.colnames(poc_acs2) <- "POC"
                                
                                poc_acs2 %<>%
                                        cbind(poc_acs1, .)
                                
                                # Find the proportion of People of Color to the Total population
                                
                                apply(
                                        X = poc_acs2[, 22],
                                        MARGIN = 1,
                                        FUN = divide.acs,
                                        denominator = poc_acs2[, 1],
                                        method = "proportion",
                                        verbose = FALSE
                                ) -> poc_acs3
                                
                                acs.colnames(poc_acs3) <- "POC_PCT"
                                
                                poc_acs <- poc_acs3
                                
                                # SAVING
                                # ----------------------------------------------------------
                                
                                saveRDS(poc_acs, file = "./2_inputs/2_intermediate/poc_acs.rds")  #save an R data file
                                
                                # PLOTTING
                                # ----------------------------------------------------------
                                
                                # plot(poc_acs)
                                
                                # LAST LINE
                                # ----------------------------------------------------------
                                
                                poc_acs
                                
                        }
                        poc_acs <- make_poc_acs()
                        rm(make_poc_acs)
                        poc_acs
                } else poc_acs <- read_rds(path = "./2_inputs/2_intermediate/poc_acs.rds")
                
        } else poc_acs
        
}

poc_df <- {
        if (!exists("poc_df")) {
                if (!file.exists("./2_inputs/poc_df.csv")) {
                        make_poc_df <- function() {
                                
                                # LOAD THE ACS FILE
                                # -------------------------------------------------------
                                
                                if (!exists("poc_acs")) 
                                        poc_acs <- read_rds("./2_inputs/2_intermediate/poc_acs.rds")
                                
                                
                                # CREATE A DATA FRAME
                                
                                df1 <- 
                                        data.frame(
                                                geography(poc_acs)["tract"], 
                                                estimate(poc_acs), 
                                                1.645 * standard.error(poc_acs)) %>% 
                                        `colnames<-`(., c("GEOID6", "POC_PCT_EST","POC_PCT_MOE")) %>% 
                                        mutate(UPPER = POC_PCT_EST + POC_PCT_MOE, 
                                               LOWER = POC_PCT_EST - POC_PCT_MOE, 
                                               UPPER = if_else(UPPER > 1, 1, UPPER), 
                                               LOWER = if_else(LOWER < 0, 0, LOWER))
                                
                                # SAVING
                                # ----------------------------------------------------------
                                
                                write_csv(df1, "./2_inputs/2_intermediate/poc_df.csv")
                                
                                # PLOTTING
                                # --------------------------------------------------------
                                
                                poc_df <- read_csv("./2_inputs/2_intermediate/poc_df.csv")
                                
                                view_poc_dotplot <<- function() {
                                        gg <- ggplot(poc_df, aes(x = reorder(GEOID6, 
                                                                             POC_PCT_EST), y = POC_PCT_EST))
                                        gg <- gg + geom_linerange(aes(ymin = LOWER, 
                                                                      ymax = UPPER), size = 1.5, alpha = 0.25)
                                        gg <- gg + geom_point_interactive(aes(tooltip = GEOID6), 
                                                                          size = 3, shape = 21, color = "white", fill = "grey30")
                                        gg <- gg + scale_x_discrete(expand = c(0.01, 
                                                                               0))
                                        gg <- gg + scale_y_continuous(labels = scales::percent)
                                        gg <- gg + coord_flip()
                                        gg <- gg + theme_minimal()
                                        gg <- gg + theme(panel.grid.major.y = element_blank(), 
                                                         panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(linetype = 3, 
                                                                                                                                 color = "grey30"), panel.grid.minor.x = element_blank(), 
                                                         axis.text.y = element_blank())
                                        gg <- gg + labs(x = NULL, y = NULL, title = "POC", 
                                                        subtitle = "Description", caption = "Source: ACS Table B03002, Five-Year Estimates (2010-2014)")
                                        gg
                                }
                                
                                # view_poc_dotplot()
                                
                                view_poc_dotplot_int <<- function() {
                                        ggiraph(code = {
                                                print(view_poc_dotplot())
                                        }, width = 0.66, tooltip_extra_css = "padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;", 
                                        hover_css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                                }
                                
                                # view_poc_dotplot_int()
                                
                                
                                # LAST LINE
                                # ----------------------------------------------------------
                                
                                poc_df
                                
                        }
                        
                        poc_df <- make_poc_df()
                        rm(make_poc_df)
                        poc_df
                } else {
                        make_poc_df <- function() {
                                
                                # LOAD THE CSV FILE
                                # -------------------------------------------------------
                                poc_df <- read_csv("./2_inputs/2_intermediate/poc_df.csv")
                                
                                view_poc_dotplot <<- function() {
                                        gg <- ggplot(poc_df, aes(x = reorder(GEOID6, 
                                                                             POC_PCT_EST), y = POC_PCT_EST))
                                        gg <- gg + geom_linerange(aes(ymin = LOWER, 
                                                                      ymax = UPPER), size = 1.5, alpha = 0.25)
                                        gg <- gg + geom_point_interactive(aes(tooltip = GEOID6), 
                                                                          size = 3, shape = 21, color = "white", fill = "grey30")
                                        gg <- gg + scale_x_discrete(expand = c(0.01, 
                                                                               0))
                                        gg <- gg + scale_y_continuous(labels = scales::percent)
                                        gg <- gg + coord_flip()
                                        gg <- gg + theme_minimal()
                                        gg <- gg + theme(panel.grid.major.y = element_blank(), 
                                                         panel.grid.minor.y = element_blank(), panel.grid.major.x = element_line(linetype = 3, 
                                                                                                                                 color = "grey30"), panel.grid.minor.x = element_blank(), 
                                                         axis.text.y = element_blank())
                                        gg <- gg + labs(x = NULL, y = NULL, title = "POC", 
                                                        subtitle = "Description", caption = "Source: ACS Table B03002, Five-Year Estimates (2010-2014)")
                                        gg
                                }
                                
                                # view_poc_dotplot()
                                
                                view_poc_dotplot_int <<- function() {
                                        ggiraph(code = {
                                                print(view_poc_dotplot())
                                        }, width = 0.66, tooltip_extra_css = "padding:2px;background:rgba(70,70,70,0.1);color:black;border-radius:2px 2px 2px 2px;", 
                                        hover_css = "fill:#1279BF;stroke:#1279BF;cursor:pointer;")
                                }
                                
                                # view_poc_dotplot_int()
                                
                                # LAST LINE
                                # ----------------------------------------------------------
                                
                                poc_df
                                
                                
                        }
                        poc_df <- make_poc_df()
                        rm(make_poc_df)
                        poc_df
                }
        } else poc_df
        
}

poc_sp <- {
        if (!exists("poc_sp")) {
                if (!file.exists("./2_inputs/2_intermediate/poc_sp.gpkg")) {
                        make_poc_sp <- function() {
                                
                                # JOIN DATAFRAME TO SP OBJECT
                                # -------------------------------------------------------
                                
                                poc_sp <- seattle_tr
                                
                                poc_sp@data %<>% left_join(poc_df, by = "GEOID6")
                                
                                # SAVING
                                # ----------------------------------------------------------
                                
                                writeOGR(obj = poc_sp, dsn = "./2_inputs/2_intermediate/poc_sp.gpkg", 
                                         layer = "poc_sp", driver = "GPKG", verbose = FALSE, 
                                         overwrite_layer = TRUE)
                                
                                
                                # PLOTTING
                                # ----------------------------------------------------------
                                
                                view_poc_sp <<- function() {
                                        abbr <- poc_sp
                                        
                                        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                        max <- max(abbr@data$POC_PCT_EST) %>% round_any(0.1, 
                                                                                        ceiling)
                                        pal <- colorNumeric(palette = myYlOrRd, domain = c(0, 
                                                                                           max))
                                        # pal <- colorFactor(palette = 'Set2', domain =
                                        # as.factor(abbr@data$POC_PCT_EST))
                                        
                                        myLflt() %>% addPolygons(data = abbr, smoothFactor = 0, 
                                                                 color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                                 fillColor = ~pal(POC_PCT_EST), fillOpacity = 0.75, 
                                                                 popup = ~GEOID6) %>% addLegend(position = "topright", 
                                                                                                title = "CHANGE_THIS", pal = pal, values = c(0, 
                                                                                                                                             max), opacity = 0.75, labFormat = labelFormat(suffix = "%", 
                                                                                                                                                                                           between = ", ", transform = function(x) 100 * 
                                                                                                                                                                                                   x))
                                        
                                        # myLflt() %>% addPolygons(data = abbr, smoothFactor = 0,
                                        # color = col2hex('white'),weight = 1.5,opacity = .5,
                                        # fillColor = pal(abbr@data$POC_PCT_EST),fillOpacity = .75) %>%
                                        # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                        # = pal, values = as.factor(abbr@data$POC_PCT_EST), opacity =
                                        # .75, labFormat = labelFormat())
                                        
                                }
                                
                                # LAST LINE
                                # ----------------------------------------------------------------
                                
                                poc_sp
                                
                        }
                        
                        poc_sp <- make_poc_sp()
                        rm(make_poc_sp)
                        poc_sp
                } else {
                        make_poc_sp <- function() {
                                
                                poc_sp <- readOGR(dsn = "./2_inputs/2_intermediate/poc_sp.gpkg", 
                                                  layer = "poc_sp", verbose = FALSE, p4s = crs_proj@projargs)
                                
                                # PLOTTING
                                # ----------------------------------------------------------
                                
                                view_poc_sp <<- function() {
                                        abbr <- poc_sp
                                        
                                        myYlOrRd <- RColorBrewer::brewer.pal(9, "YlOrRd")[2:7]
                                        max <- max(abbr@data$POC_PCT_EST) %>% round_any(0.1, 
                                                                                        ceiling)
                                        pal <- colorNumeric(palette = myYlOrRd, domain = c(0, 
                                                                                           max))
                                        # pal <- colorFactor(palette = 'Set2', domain =
                                        # as.factor(abbr@data$POC_PCT_EST))
                                        
                                        myLflt() %>% addPolygons(data = abbr, smoothFactor = 0, 
                                                                 color = col2hex("white"), weight = 1.5, opacity = 0.5, 
                                                                 fillColor = ~pal(POC_PCT_EST), fillOpacity = 0.75, 
                                                                 popup = ~GEOID6) %>% addLegend(position = "topright", 
                                                                                                title = "People of Color", pal = pal, values = c(0, 
                                                                                                                                                 max), opacity = 0.75, labFormat = labelFormat(suffix = "%", 
                                                                                                                                                                                               between = ", ", transform = function(x) 100 * 
                                                                                                                                                                                                       x))
                                        
                                        # myLflt() %>% addPolygons(data = abbr, smoothFactor = 0,
                                        # color = col2hex('white'),weight = 1.5,opacity = .5,
                                        # fillColor = pal(abbr@data$POC_PCT_EST),fillOpacity = .75) %>%
                                        # addLegend(position = 'topright', title = 'CHANGE_THIS', pal
                                        # = pal, values = as.factor(abbr@data$POC_PCT_EST), opacity =
                                        # .75, labFormat = labelFormat())
                                        
                                }
                                
                                # LAST LINE
                                # ----------------------------------------------------------------
                                
                                poc_sp
                                
                                
                        }
                        poc_sp <- make_poc_sp()
                        rm(make_poc_sp)
                        poc_sp
                }
        } else poc_sp
        
}
