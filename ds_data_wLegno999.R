ds_data_wLegno999 <- function(dat, target_dir, spp, dist.method){
    #dat = data prepared using ds_data_prep.r
    #target_dir is the directory that files will be written to
    #spp = "RIWH" or other species of interest
    #dist.method 'rdk' or 'eab' indicates which distance calc method is used. rdk is from a bob kenney paper and eab is from elizabeth becker's code
    
    uSpp <- length(spp) #species = spp
    
    #### measure distance between consecutive on-effort points fore each transect
    
    # create array to hold distances for each legno and survey
    #   the array needs to have a column for FILEID, YEAR, MONTH, DAY, LEGNO2, DISTANCE
    #   Question: how many rows should there be? Answer is the number of FILEID
    
    # create array to hold summed effort ('effort_summary') for each unique FILEID and LEGNO combination, use nrow(new) to determine the number of rows needed in this array
    effort_summary <-
        as.data.frame(matrix(
            data = NA,
            nrow = length(unique(dat$FILEID)),
            ncol = 5
        ))
    names(effort_summary) <-
        c("FILEID", "YEAR", "MONTH", "DAY", "Effort")
    
    #add in fields for species - number sightings and number animals
    for (i in 1:uSpp){
        #number sightings
        cmd = paste("effort_summary$", spp[i], ".sights", "= NA", sep = "")
        eval(parse(text = cmd))
        #number animals
        cmd = paste("effort_summary$", spp[i], ".ind", "= NA", sep = "")
        eval(parse(text = cmd))
    }
    
    # distance calculation function. formula from Kenney & Winn, 1986
    dist.rdk <- function(lat1, lat2, lon1, lon2) {
        111.12 * acos((
            sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2)
        ))
    }
    
    # FUNCTION to calculate the great circle distance (in km) between two lat/lons
    # This came from Elizabeth Becker's segchopr code
    fn.grcirclkm <- function(lat1,lon1,lat2,lon2) {
        R <- pi/180      #angle in radians = angle in degrees * R
        D <- 180/pi      #angle in degrees = angle in radains * D
        dist <- 0
        
        NAcheck <- sum(is.na(c(lat1,lon1,lat2,lon2)))
        if (NAcheck==0) {             #only continue if no NA positions
            if ((lat1!=lat2) | (lon1!=lon2))  {
                dlat1 <- lat1 * R              # convert to radian values:
                dlng1 <- lon1 * R
                dlat2 <- lat2 * R
                dlng2 <- lon2 * R
                las <- sin(dlat1) * sin(dlat2);   # compute distance
                lac <- cos(dlat1) * cos(dlat2) * cos(dlng1 - dlng2)
                laf <- las + lac
                if (laf < -1) {
                    laf <- -1
                    dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)))
                } else if (laf < 1) {
                    dacos <- (pi/2) - atan(laf/sqrt(1-(laf*laf)));
                } else {
                    error ('laf value out of bounds')
                }
                dist <- (dacos * D * 60) * 1.852           #calculate distance in km
            }
        }
        dist <- dist
    }
    
    
    dat$pt2pt.effort <-
        NA # initialize column to hold distance values between consecutive points
    dat$Effort <-
        NA # initialize column to hold total distance, "Effort", for each LEGNO2
    ufid <-
        unique(dat$FILEID) # unique FILEIDs needed for looping about each survey
    ctr = 0 # counter needed to index effort_summary
    
    # loop through each FILEID and compute distance between consecutive on-effort points
    for (i in 1:length(ufid)) {
        # for each FILEID ...
        I <-
            which(dat$FILEID == ufid[i]) # get indices for dat$FILEID[i]
        tmp.dat <- dat[I,] # temporary dataset for only this FILEID
        ctr = ctr + 1 # update counter
        
        numRecs <-
            nrow(tmp.dat) - 1 # count number of records in this temporary dataset
        for (k in 1:numRecs) {
            # for each record in this FILEID > LEGNO2
            if (tmp.dat$OnOff.Effort[k] == 1 &
                tmp.dat$OnOff.Effort[k + 1] == 1) {
                # if on-effort at two consecutive records
                # apply distance function to calculate distance between on-effort records
                if (dist.method == "rdk"){
                    tmp.dat$pt2pt.effort[k] <-
                        dist.rdk(
                            tmp.dat$LATITUDE[k],
                            tmp.dat$LATITUDE[k + 1],
                            tmp.dat$LONGITUDE[k],
                            tmp.dat$LONGITUDE[k + 1]
                        )
                    # Also! insert calculated distance into original dataset
                    dat$pt2pt.effort[I[k]] <-
                        dist.rdk(dat$LATITUDE[I[k]],
                                 dat$LATITUDE[I[k + 1]],
                                 dat$LONGITUDE[I[k]],
                                 dat$LONGITUDE[I[k + 1]])
                } else if (dist.method == "eab"){
                    # if on-effort at two consecutive records
                    # apply distance function to calculate distance between on-effort records
                    tmp.dat$pt2pt.effort[k] <-
                        fn.grcirclkm(
                            tmp.dat$LATITUDE[k],
                            tmp.dat$LONGITUDE[k],
                            tmp.dat$LATITUDE[k + 1],
                            tmp.dat$LONGITUDE[k + 1]
                        )
                    # Also! insert calculated distance into original dataset
                    dat$pt2pt.effort[I[k]] <-
                        fn.grcirclkm(dat$LATITUDE[I[k]],
                                     dat$LONGITUDE[I[k]],
                                     dat$LATITUDE[I[k + 1]],
                                     dat$LONGITUDE[I[k + 1]])
                }
            }
        }
        
        
        #must store these separately or else they get messed up because they are factors
        effort_summary$FILEID[i] <- as.character(ufid[i])
        effort_summary$YEAR[ctr] <- unique(tmp.dat$YEAR)
        effort_summary$MONTH[ctr] <- unique(tmp.dat$MONTH)
        effort_summary$DAY[ctr] <- unique(tmp.dat$DAY)
        effort_summary$Effort[ctr] <-
            sum(tmp.dat$pt2pt.effort, na.rm = TRUE)
        
        #get count of species. # THIS WILL SUM *ALL* SIGHTINGS AND ANIMALS
        for (j in 1:uSpp) {
            # find all of target species, exclude camera sightings, include legstage only == 2 or NA, and restrict idrel to 2 or 3.
            J = which(tmp.dat$SPECCODE == spp[j] & (tmp.dat$LEGSTAGE != 7 | is.na(tmp.dat$LEGSTAGE)))
            cmd = paste("effort_summary$", spp[j], ".sights", "[i]=length(J)", sep = "")
            eval(parse(text = cmd))
            
            if (!is_empty(J)){
                cmd = paste("effort_summary$", spp[j], ".ind", "[i]=sum(tmp.dat$NUMBER[J])", sep = "")
                eval(parse(text = cmd))
            } else {
                cmd = paste("effort_summary$", spp[j], ".ind", "[i]=0", sep = "")
                eval(parse(text = cmd))
            }
        }
    }
    
    effort_summary #display summary of effort for each FILEID and LEGNO2
    
    #write out .csvs
    
    fn = paste(spp, collapse = '_')
    season_beg = paste(substr(season.beg, start = 1, stop = 4),
                       substr(season.beg, start = 6, stop = 7),
                       sep = '')
    season_end = paste(substr(season.end, start = 1, stop = 4),
                       substr(season.end, start = 6, stop = 7),
                       sep = '')
    fn = paste(fn, season_beg, season_end, sep = "_") 
    fn = paste(fn, dist.method, sep = "_")
    
    #write.csv(effort_summary, file = "~/Desktop/effort_summary.csv", row.names = FALSE)
    cmd = paste(
        "write.csv(effort_summary, file = '",
        target_dir,
        "effort_summary_",
        fn,
        ".csv'",
        ", row.names = FALSE)",
        sep = ""
    )
    eval(parse(text = cmd))
    
    #write.csv(dat, file = "~/Desktop/dat.csv", row.names = FALSE)
    cmd = paste(
        "write.csv(dat, file = '",
        target_dir,
        "dat_with_dist_",
        fn,
        ".csv'",
        ", row.names = FALSE)",
        sep = ""
    )
    eval(parse(text = cmd))
    
    return(effort_summary)
    
}