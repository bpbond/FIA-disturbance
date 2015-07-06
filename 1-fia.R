# Template for R analysis script
# Ben Bond-Lamberty March 2015

SCRIPTNAME		<- "1-fia.R"

DATADIR <- "~/Data/FIA"
STATELIST <- c("MI", "WI", "OH", "PA", "VT", "NY")

# -----------------------------------------------------------------------------
plotstate <- function(d) {
  state <- paste(unique(d$STATE), collapse="-")

  d$Disturbed <- "Undisturbed"
  d$Disturbed[d$Disturbance_group != "None"] <- "Disturbed"
  
  d1 <- d %>%
    group_by(STDAGE, Disturbed, leafHabit, Productivity) %>%
    summarise(ANN_NET_GROWTH_ACRE = mean(ANN_NET_GROWTH_ACRE))
  
  p <- qplot(STDAGE, ANN_NET_GROWTH_ACRE, data=d1, color=Disturbed) 
  p <- p + facet_grid(leafHabit ~ Productivity)
  p <- p + xlab("Stand age (yr)") + ylab("Annual net growth (ft3/acre)")
  p <- p + coord_cartesian(xlim=c(-10, 200), ylim=c(-5, 120))
  p <- p + geom_smooth(fill=NA, method='loess')
  p <- p + ggtitle(state)
  print(p)
  save_plot(paste0("Age_growth_prod_", state))
  
  d2 <- d %>%
    group_by(STDAGE, Disturbance_group, leafHabit) %>%
    summarise(ANN_NET_GROWTH_ACRE = mean(ANN_NET_GROWTH_ACRE))
  
  p <- qplot(STDAGE, ANN_NET_GROWTH_ACRE, data=d2, color=leafHabit) 
  p <- p + facet_wrap(~Disturbance_group)
  p <- p + xlab("Stand age (yr)") + ylab("Annual net growth (ft3/acre)")
  p <- p + coord_cartesian(xlim=c(-10, 200), ylim=c(-5, 120))
  p <- p + geom_smooth(fill=NA, method='lm')
  p <- p + ggtitle(state)
  print(p)
  save_plot(paste0("Age_growth_prod_dstrb_", state))
  
  p1 <- qplot(disturbanceAge, ANN_NET_GROWTH_ACRE, data=d, color=leafHabit)
  p1 <- p1 + geom_jitter() + geom_smooth(method='lm') 
  p1 <- p1 + xlim(c(0, 5))  # remove negative ages (?)
  p1 <- p1 + xlab("Years since disturbance") + ylab("Annual net growth (ft3/acre)")
  p1 <- p1 + coord_cartesian(ylim=c(-250,250))
  p1 <- p1 + ggtitle(state)
  print(p1)
  save_plot(paste0("Disturbance_growth_", state))
}

# ==============================================================================
# Main

source("0-functions.R")

# Setup, packages, reproducibility
sink(file.path(outputdir(), paste0(SCRIPTNAME, ".log.txt")), split=T) # open log

printlog("Welcome to", SCRIPTNAME)

library(ggplot2)
theme_set(theme_bw())
library(reshape2)
library(dplyr)


d_all <- data.frame()
for(STATE in STATELIST) {
  CONDITION_TABLE <- paste0(STATE, "_COND.CSV.gz")
  GROWTH_TABLE <- paste0(STATE, "_TREE_GRM_ESTN.CSV.gz")
  POPSTRATUM_TABLE <- paste0(STATE, "_POP_STRATUM.CSV.gz")
  
  
  d_g <- read_csv(GROWTH_TABLE, datadir = file.path(DATADIR, STATE))
  print_dims(d_g)
  
  #popstratum <- read_csv(POPSTRATUM_TABLE, datadir=file.path(DATADIR, STATE))
  
  # TODO: this isn't producing correct numbers - the relative change over time
  # looks good, but values (0-500) are much too large; average range should be
  # 30-80 ft3/acre/yr
  
  # Average annual net growth estimate. The net change in the estimate per year of this tree. 
  # Because this value is net growth, it may be a negative number. Negative values are usually 
  # due to mortality but can also occur on live trees that have a net loss because of damage, 
  # rot, broken top, or other causes. To expand to a per acre value, multiply by TPAGROW_UNADJ.
  printlog("Calculating plot growth...")
  d_g_smry <- d_g %>%
    filter(ESTN_TYPE == "AL", LAND_BASIS == "FORESTLAND") %>%
    group_by(STATECD, INVYR, PLT_CN) %>%
    summarise(ANN_NET_GROWTH_ACRE = sum(ANN_NET_GROWTH * TPAGROW_UNADJ, na.rm=TRUE))
  
  
  # Read condition table. We specify the types of DSTRBCD1 and DSTRBYR1
  # because otherwise readr::read_csv guesses them wrong
  printlog("Reading", CONDITION_TABLE)
  d_cond <- readr::read_csv(file.path(DATADIR, STATE, CONDITION_TABLE),
                            col_types = list(
                              DSTRBCD1 = col_numeric(),
                              DSTRBYR1 = col_numeric(),
                              SITECLCD = col_numeric()
                            ))
  d_cond$DSTRBYR1[d_cond$DSTRBYR1 == 9999] <- NA
  
  d_cond <- d_cond %>%
    filter(COND_STATUS_CD == 1) %>%
    select(STATECD, INVYR, UNITCD, COUNTYCD, PLT_CN, FORTYPCD, SITECLCD,
           STDAGE, DSTRBCD1, DSTRBYR1)
  
  # Read forest type code names. These are just the group codes, so need to 
  # round FORTYPCD to lowest 10
  FORTYPCD_names <- read_csv("FORTYPCD_codes.csv", datadir=DATADIR)
  printlog("Merging with forest type code names...")
  d_cond$FORTYPCD <- floor(d_cond$FORTYPCD / 10.0) * 10
  FORTYPCD_names$leafHabit <- "Deciduous"
  FORTYPCD_names$leafHabit[FORTYPCD_names$FORTYPCD < 400] <- "Evergreen"
  d_cond <- merge(d_cond, FORTYPCD_names)
  
  # Read state code names
  STATECD_names <- read_csv("STATECD_codes.csv", datadir=DATADIR)
  STATECD_names$STATECD <- as.numeric(STATECD_names$STATECD)
  printlog("Merging with state code names...")
  d_cond <- merge(d_cond, STATECD_names)
  
  # Read disturbance code information
  DSTRBCD_codes <- read_csv("DSTRBCD_codes.csv", datadir=DATADIR)
  DSTRBCD_codes$Disturbance_type <- NULL  # we just care about aggregated group info
  printlog("Merging with disturbance code information")
  d_cond <- merge(d_cond, DSTRBCD_codes)
  d_cond$Disturbance_group <- relevel(factor(d_cond$Disturbance_group), "None")
  
  printlog("Merging condition and growth data...")
  d <- merge(d_cond, d_g_smry)
  
  # Disturbance data
  d$disturbanceAge <- d$INVYR - d$DSTRBYR1
  d$Productivity <- "Low (≥4) productivity"
  d$Productivity[d$SITECLCD < 4] <- "High (<4) productivity"
  
  d_summarized <- d %>%
    group_by(STATE, Disturbance_group, leafHabit, STDAGE, Productivity) %>% 
    summarise(ANN_NET_GROWTH_ACRE = mean(ANN_NET_GROWTH_ACRE),
              disturbanceAge = mean(disturbanceAge))
  
  plotstate(d_summarized)  
  
  d_all <- rbind(d_all, d_summarized)
}

plotstate(d_all)

printlog("All done with", SCRIPTNAME)
print(sessionInfo())
sink() # close log
