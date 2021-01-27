ds_data_prep <- function(dat, season.beg, season.end, condensed) {
    # prepare dataset for ds_data.r
    # This funciton was modified by Dan and Orla on Jan 26, 2021 
    # The modification allows for all legtypes except 0
    # The purpose of the modfication is to extend the definition of on-effort
    # to additional legtypes so we can compute effort for SPUE and sighting rates metrics
    # Where LEGNOs do not exist, we are inserting 999, and we have altered newLegnos.csv and renamed it newLegnos999.csv
    
    # discard LEGNO 222 (by changed it to NA) because it is E-W
    I = which(dat$LEGNO == 222)
    #plot(dat$LONGITUDE[I],dat$LATITUDE[I])
    dat$LEGNO[I] = NA
    
    # discard the diagonal version of LEGNO 111 (by changing it to NA), when it is east of -71.0.
    # The N-S LEGNO 111 is at 71 deg W, 16 min, and we want to retain that line but discard the weird diagonal line.
    # view the dualling LEGNO 111s
    # do this to see the dualing lines
        I = which(dat$LEGNO == 111)
        #plot(dat$LONGITUDE[I],dat$LATITUDE[I])
    #plot LEGNO 111 east of -71.0, then assign those LEGNOs to be NA
    I = which(dat$LEGNO == 111 & dat$LONGITUDE > -71)
    #plot(dat$LONGITUDE[I],dat$LATITUDE[I])
    dat$LEGNO[I] = NA
    # check to make sure you got rid of all the diagonal LEGNO 111
    I = which(dat$LEGNO == 111)
    #plot(dat$LONGITUDE[I],dat$LATITUDE[I], xlim = c(-72, -67), ylim = c(40, 46))
    
    # discard LEGNO = 301, 302, 303, 304 (by changing to NA) because these were one-off surveys of some kind (leatherbacks?)
    I = which(dat$LEGNO == 301 |
                  dat$LEGNO == 302 | dat$LEGNO == 303 | dat$LEGNO == 304)
    #plot(dat$LONGITUDE[I],dat$LATITUDE[I], xlim = c(-70.5, -70), ylim = c(40.5, 42))
    dat$LEGNO[I] <- NA
    
    # discard Nova Scotia surveys (they have LEGNO 1:6, so they will trip us up if we don't get rid of them)
    I <- which(dat$LEGNO >= 1 & dat$LEGNO <= 6)
    #plot(dat$LONGITUDE[I], dat$LATITUDE[I])
    I <- which(dat$LONGITUDE > -68 & dat$LATITUDE > 42)
    dat$LEGNO[I] <- NA
        #check lines 1-6
        I <- which(dat$LEGNO >= 1 & dat$LEGNO <= 6)
        #plot(dat$LONGITUDE[I], dat$LATITUDE[I])
    
    # look at all legnos in the data
    sort(unique(dat$LEGNO))
    
    ### Taking this out for Encounter rate mileage calcs
    # discard all data that does not have a LEGNO 
    #dat <- dat %>%
     #   filter(!is.na(LEGNO))

    # all 999s are flight without newLegNos (circling, crossleg, on watch)
    I <- which(is.na(dat$LEGNO))
    dat$LEGNO[I] <- 999

    
    # name the columns to remove/keep from BK data 'dat' file as unnecessary/necessary
    keep.cols <- c(
        "FILEID",
        "EVENTNO",
        "YEAR",
        "MONTH",
        "DAY",
        "TIME",
        "LATITUDE",
        "LONGITUDE",
        "LEGTYPE",
        "LEGSTAGE",
        "LEGNO",
        "ALT",
        "BEAUFORT",
        "VISIBLTY",
        "SPECCODE",
        "STRIP",
        "IDREL",
        "NUMBER",
        "NUMCALF",
        "S_LAT",
        "S_LONG",
        "S_TIME",
        "SIGHTNO",
        "GLAREL",
        "GLARER",
        "STRIP",
        "CLOUD",
        "PHSTRIP"
    )
    
    dat <- dat %>%
        select(keep.cols)
    
    # import legnos info for all surveys, strata, years
    newLegnos <- read.csv("newLegnos999.csv")
    # create new column "LEGNO2" that has unique identifier for each "LEGNO". the identifier is the longitude of each legno.
    newLegnos$LEGNO2 = NA
    for (i in 1:nrow(newLegnos)) {
        if (newLegnos$lon.min[i] < 10) {
            a <-
                as.character(paste(newLegnos$lon.deg[i], "0", newLegnos$lon.min[i], sep =
                                       ""))
            newLegnos$LEGNO2[i] <- substr(a, start = 1, stop = 5)
        } else {
            a <-
                as.character(paste(newLegnos$lon.deg[i], newLegnos$lon.min[i], sep = ""))
            newLegnos$LEGNO2[i] <-
                substr(a, start = 1, stop = 5)
        }
    }
    newLegnos$LEGNO2 <- as.numeric(newLegnos$LEGNO2)
    sort(unique(newLegnos$LEGNO))
    
    # remove/keep unnecessary/necessary columns
    keep.cols <- c("stratum", "LEGNO", "LEGNO2")
    newLegnos <- newLegnos %>%
        select(keep.cols)
    
    # create column of the first character of FILEID. For surveys 2017 and later, "a" is for general surveys
    #   and "b" is for condensed surveys. We need this to filter out these surveys.
    fid <- as.character(dat$FILEID)
    fid <- substr(fid, start = 1, stop = 1)
    dat$fid <-
        fid # add 1st character of FILEID as a new column in dat
    
    #### create datasets for each timeperiod/"stratum", as desired. ####
    # Oct 2011 - Oct 2012
    I <- which(newLegnos$stratum == "general.2011_2012")
    newLegnos.new <- newLegnos[I, ]
    I <- which(dat$YEAR <= 2012 & dat$MONTH < 12)
    dat.new <- dat[I, ]
    dat.general.2011_2012 <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO') #WHY IS THE SIZE OF THIS INCORRECT?
    dat.general.2011_2012$stratum <- "general.2011_2012"
    #plot(dat.general.2011_2012$LONGITUDE, dat.general.2011_2012$LATITUDE, xlim = c(-72, -63), ylim = c(40, 45))
    
    # Nov 2012 - June 2015 general stratum
    newLegnos.new <- newLegnos %>%
        filter(stratum == "general.2013_2015")
    dat.new <- dat %>%
        filter((YEAR == 2012 &
                    MONTH == 12) | (YEAR >= 2013 & YEAR <= 2015)) %>%
        filter(LEGNO < 100 | LEGNO == 999)
    dat.general.2013_2015 <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO')
    dat.general.2013_2015$stratum <- "general.2013_2015"
    #points(dat.general.2013_2015$LONGITUDE, dat.general.2013_2015$LATITUDE, col = "blue")
    
    # RI stratum
    newLegnos.new <- newLegnos %>%
        filter(stratum == "RI")
    dat.new <- dat %>%
        filter((YEAR == 2012 &
                    MONTH == 12) | (YEAR >= 2013 & YEAR <= 2015)) %>%
        filter(LEGNO > 100)
    dat.RI <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO') #WHY IS THE SIZE OF THIS INCORRECT?
    dat.RI$stratum <- "RI"
    #points(dat.RI$LONGITUDE, dat.RI$LATITUDE, col = "red")
    
    # 2017 - 2019 general stratum
    newLegnos.new <- newLegnos %>%
        filter(stratum == "general.2017_2020")
    dat.new <- dat %>%
        filter(YEAR >= 2017) %>%
        filter(fid == "a")
    dat.general.2017_2020 <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO')
    dat.general.2017_2020$stratum <- "general.2017_2020"
    #points(dat.general.2017_2020$LONGITUDE, dat.general.2017_2020$LATITUDE, col = "yellow")
    
    # 2017 - 2018 condensed-east stratum
    newLegnos.new <- newLegnos %>%
        filter(stratum == "condensed.east.2017_2018")
    dat.new <- dat %>%
        filter(YEAR >= 2017 & YEAR <= 2018) %>%
        filter(LONGITUDE > -70.675) %>%
        filter(fid == "b")
    dat.condensed.east.2017_2018 <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO')
    dat.condensed.east.2017_2018$stratum <-
        "condensed.east.2017_2018"
    #points(dat.condensed.east.2017_2018$LONGITUDE, dat.condensed.east.2017_2018$LATITUDE, col = "magenta")
    
    # 2017 - 2018 condensed-west stratum
    newLegnos.new <- newLegnos %>%
        filter(stratum == "condensed.west.2017_2018")
    dat.new <- dat %>%
        filter(YEAR >= 2017 & YEAR <= 2018) %>%
        filter(LONGITUDE <= -70.675) %>%
        filter(fid == "b")
    dat.condensed.west.2017_2018 <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO')
    dat.condensed.west.2017_2018$stratum <-
        "condensed.west.2017_2018"
    #points(dat.condensed.west.2017_2018$LONGITUDE, dat.condensed.west.2017_2018$LATITUDE, col = "green")
    
    # 2019 - 2020 condensed-west stratum
    newLegnos.new <- newLegnos %>%
        filter(stratum == "condensed.2019_2020")
    dat.new <- dat %>%
        filter(YEAR >= 2019) %>%
        filter(fid == "b")
    dat.condensed.2019_2020 <-
        left_join(dat.new, newLegnos.new, by = 'LEGNO')
    dat.condensed.2019_2020$stratum <-
        "condensed.2019_2020"
    #points(dat.condensed.2019_2020$LONGITUDE, dat.condensed.2019_2020$LATITUDE, col = "coral4")
    
    # reconstruct dat by rbinding the strata
    if (condensed == 1) { #condensed == 1 means we want to include condensed surveys
    dat <- rbind(
        dat.general.2011_2012,
        dat.general.2013_2015,
        dat.RI,
        dat.general.2017_2020,
        dat.condensed.east.2017_2018,
        dat.condensed.west.2017_2018,
        dat.condensed.2019_2020
    )
    } else if (condensed == 0){ #condensed == 0 means we do not want to include condesned surveys
        dat <- rbind(
            dat.general.2011_2012,
            dat.general.2013_2015,
            dat.RI,
            dat.general.2017_2020
        )
    }
    
    # reduce data to the season specified above
    yearmonth = NA
    for (i in 1:nrow(dat)) {
        if (dat$MONTH[i] < 10) {
            yearmonth[i] <-
                as.numeric(paste(dat$YEAR[i], ".0", dat$MONTH[i], sep = ""))
        } else {
            yearmonth[i] <-
                as.numeric(paste(dat$YEAR[i], ".", dat$MONTH[i], sep = ""))
        }
    }
    I <- which(yearmonth >= season.beg & yearmonth <= season.end)
    dat <- dat[I, ] # subset dat to only seasons of interest
    rm(yearmonth)
    
    #sort data by FILEID, EVENTNO,
    dat <- arrange(dat, YEAR, MONTH, DAY, FILEID, EVENTNO)
    
    # remove any records that do not have LEGNO2 because they are not on-transect
    I <- which(!is.na(dat$LEGNO2))
    dat <- dat[I, ]
    
    # convert altitude from factor to number. needed for on/off-effort identification
    dat$ALT <- as.integer(as.character(dat$ALT))
    
    # flag on/off-effort records
    # Legtype and legstage can be:
    # 2 1 #start line
    # 2 2 #continue line
    # 2 3 #start circling
    # 2 4 #end circling
    # 2 5 #end line
    # 2 6 # e.g., pilot sighting
    # 2 7 # camera sighting
    
    # find row numbers that are on-effort
    I1 <-
        which(dat$LEGTYPE != 0) # all LEGTYPE 1, 2, 3, 4 are acceptable as on-effort
    I2 <-
        which(dat$VISIBLTY >= 2 &
                  dat$ALT < 366 &
                  dat$BEAUFORT < 4) # find row numbers with acceptable VIZ, ALT and BFT
    Iall <-
        intersect(I1, I2) # the intersection of I1 and I2 are all row numbers that satisfy on-effort
    
    # make column for on/off effort based upon the conditions above
    dat$OnOff.Effort <- 0 #initialize with zero for off-effort
    dat$OnOff.Effort[Iall] <-
        1 #set OnOff.Effort=1 for on-effort for the row numbers having acceptable conditions (Iall)
    
    # ! IMPORTANT !
    # DO NOT REMOVE LEGSTAGE !=3 FROM dat, BECAUSE WE NEED THOSE RECORDS FOR CORRECT ESTIMATION OF DISTANCE/EFFORT ON EACH TRACKLINE
    
    return(dat)
}