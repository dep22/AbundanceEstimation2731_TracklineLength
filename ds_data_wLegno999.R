#ds_data_wLegno999 <- function(dat, target_dir, spp, phstrip) {
    #dat = data prepared using ds_data_prep.r
    #target_dir is the directory that files will be written to
    #spp = "RIWH" or other species of interest
    
    #### measure distance between consecutive on-effort points fore each transect
    
    # create array to hold distances for each legno and survey
    #   the array needs to have a column for FILEID, YEAR, MONTH, DAY, LEGNO2, DISTANCE
    #   Question: how many rows should there be? create 'new' below to find the answer
    new <-
        data.frame(dat$FILEID, dat$LEGNO2) #make array of only FILEID and LEGNO2, count unique rows in this array. that is the number of rows you need
    I <-
        !(duplicated(new)) #find rows that are not duplicated (will have value = TRUE)
    new <- new[I,] #remove all duplicated rows
    
    # create array to hold summed effort ('effort_summary') for each unique FILEID and LEGNO combination, use nrow(new) to determine the number of rows needed in this array
    effort_summary <-
        as.data.frame(matrix(
            data = NA,
            nrow = nrow(new),
            ncol = 6
        ))
    names(effort_summary) <-
        c("FILEID", "LEGNO2", "YEAR", "MONTH", "DAY", "Effort")
    # newUfid will be used to temporarily hold FILEID, for later insertion into effort_summary.
    #   this is a workaround because FILEID is a factor and it cannot be easliy inserted into effort_summary inside of a loop.
    newUfid <- matrix(data = NA,
                      nrow = nrow(new),
                      ncol = 1)
    rm(new)
    
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
    
    # loop through each FILEID, each LEGNO2 within each FILEID
    for (i in 1:length(ufid)) {
        # for each FILEID ...
        
        I <-
            which(dat$FILEID == ufid[i]) # get indices for dat$FILEID[i]
        tmp.dat <- dat[I,] # temporary dataset for only this FILEID
        
        #DON'T NEED THIS
        # uLegno <-
        #     unique(tmp.dat$LEGNO2) # LEGNOs flown during this survey
        
        #for (j in 1:nrow(tmp.dat)) { #for each record, from top to bottom, for the current survey
            
            #MIGHT NOT NEED THIS
            ctr = ctr + 1 # update counter
            
            # J <-
            #     which(tmp.dat$LEGNO2 == uLegno[j]) # find only this LEGNO2
            # tmp.tmp.dat <-
            #     tmp.dat[J,] # create temporary dataset to work with

            # REASSIGN TMP.DAT TO TMP.TMP.DAT FOR A WORKAROUND SINCE NOT USING LEGNO2
            tmp.tmp.dat <- tmp.dat
                        
            numRecs <-
                nrow(tmp.tmp.dat) - 1 # count number of records in this temporary dataset
            for (k in 1:numRecs) {
                # for each record in this FILEID > LEGNO2
                if (tmp.tmp.dat$OnOff.Effort[k] == 1 &
                    tmp.tmp.dat$OnOff.Effort[k + 1] == 1) {
                    # # if on-effort at two consecutive records
                    # # apply distance function to calculate distance between on-effort records
                    # tmp.tmp.dat$pt2pt.effort[k] <-
                    #     dist.rdk(
                    #         tmp.tmp.dat$LATITUDE[k],
                    #         tmp.tmp.dat$LATITUDE[k + 1],
                    #         tmp.tmp.dat$LONGITUDE[k],
                    #         tmp.tmp.dat$LONGITUDE[k + 1]
                    #     )
                    # # Also! insert calculated distance into original dataset
                    # dat$pt2pt.effort[I[J[k]]] <-
                    #     dist.rdk(dat$LATITUDE[I[J[k]]],
                    #              dat$LATITUDE[I[J[k + 1]]],
                    #              dat$LONGITUDE[I[J[k]]],
                    #              dat$LONGITUDE[I[J[k + 1]]])
                    # if on-effort at two consecutive records
                    # apply distance function to calculate distance between on-effort records
                    tmp.tmp.dat$pt2pt.effort[k] <-
                        fn.grcirclkm(
                            tmp.tmp.dat$LATITUDE[k],
                            tmp.tmp.dat$LONGITUDE[k],
                            tmp.tmp.dat$LATITUDE[k + 1],
                            tmp.tmp.dat$LONGITUDE[k + 1]
                        )
                    # Also! insert calculated distance into original dataset
                    dat$pt2pt.effort[I[k]] <-
                        fn.grcirclkm(dat$LATITUDE[I[k]],
                                     dat$LONGITUDE[I[k]],
                                     dat$LATITUDE[I[k + 1]],
                                     dat$LONGITUDE[I[k + 1]])
                }
            }
            
            # # store values into effort_summary
            # newUfid[ctr] <-
            #     as.character(ufid[i]) #must store these separately or else they get messed up because they are factors
            # # add newUfid back into effort_summary
            effort_summary$FILEID[i] <- as.character(ufid[i])
            # rm(newUfid)
            #effort_summary$LEGNO2[ctr] <- uLegno[j]
            effort_summary$YEAR[ctr] <- unique(tmp.tmp.dat$YEAR)
            effort_summary$MONTH[ctr] <- unique(tmp.tmp.dat$MONTH)
            effort_summary$DAY[ctr] <- unique(tmp.tmp.dat$DAY)
            effort_summary$Effort[ctr] <-
                sum(tmp.tmp.dat$pt2pt.effort, na.rm = TRUE)
    }
    
    
    
    
    # #reduced effort_summary has total effort for each line surveyed within the time period of this analysis
    # uLegno <- unique(effort_summary$LEGNO2)
    # effort_summary_reduced <-
    #     as.data.frame(matrix(
    #         data = NA,
    #         nrow = length(uLegno),
    #         ncol = 2
    #     ))
    # names(effort_summary_reduced) <- c("LEGNO2", "Effort")
    # for (i in 1:length(uLegno)) {
    #     effort_summary_reduced$LEGNO2[i] = uLegno[i]
    #     
    #     I <- which(effort_summary$LEGNO2 == uLegno[i])
    #     effort_summary_reduced$Effort[i] = sum(effort_summary$Effort[I])
    # }
    # 
    # percReflown = (1 - (nrow(effort_summary_reduced) / nrow(effort_summary))) *
    #     100
    # print(paste(round(percReflown, 2), "% of lines were reflown", sep =
    #                 ""))
    effort_summary #display summary of effort for each FILEID and LEGNO2
    
    
    # DAN IS GOING TO COMMMENT OUT EVERYTHING BELOW THIS, EXCEPT WRITING OUT OF EFFORT_SUMMARY
    # 
    # # effort_summary_reduced #display summary of effort for each LEGNO2 (with FILEIDs combined)
    # #plot(effort_summary$LEGNO2,-1*(effort_summary$Effort)) #plot realized effort for each trackline flown (remember that there will be gaps between minutes = 61:100)
    # #plot(effort_summary_reduced$LEGNO2,-1*(effort_summary_reduced$Effort))
    # 
    # # camera view ends at 233 ft on each side of transect, so bin 1 & 2 start at 233ft and goes to 760ft
    # # Strip numbers and their distances from the trackline in    nm                       km
    # # Strip 1 & 2 0 - 1/8 nm.   = (((0.125 - .038)/2) + 0.038) = 0.0815; 0.0815 * 1.852 = 0.150938
    # # Strip 3 & 4 1/8 - 1/4 nm. = ((1/4 - 1/8)/2 + 1/8)        = 0.1875; 0.1875 * 1.852 = 0.34725
    # # Strip 5 & 6 1/4 - 1/2 nm. = ((1/2 - 1/4)/2 + 1/4)        = 0.375; 0.375 * 1.852   = 0.6945
    # # Strip 7 & 8 1/2 - 1 nm.   = ((1 - 1/2)/2 + 1/2)          = 0.75; 0.75 * 1.852     = 1.389
    # # Strip 9 & 10 1-2 nm.      = ((2 - 1)/2 + 1)              = 1.5; 1.5 * 1.852       = 2.778
    # # Strip 11 & 12 2 - 4 nm.   = ((4 - 2)/2 + 2)              = 3; 3 * 1.852           = 5.556
    # # Strip 13 & 14 4+ nm       = ((6 - 4)/2 + 4)              = 5; 5 * 1.852           = 9.26
    # 
    # strip_distances = matrix(data = NA,
    #                          nrow = 14,
    #                          ncol = 2)
    # strip_distances[1,] = c(1, 0.150938)
    # strip_distances[2,] = c(2, 0.150938)
    # strip_distances[3,] = c(3, 0.34725)
    # strip_distances[4,] = c(4, 0.34725)
    # strip_distances[5,] = c(5, 0.6945)
    # strip_distances[6,] = c(6, 0.6945)
    # strip_distances[7,] = c(7, 1.389)
    # strip_distances[8,] = c(8, 1.389)
    # strip_distances[9,] = c(9, 2.778)
    # strip_distances[10,] = c(10, 2.778)
    # strip_distances[11,] = c(11, 5.556)
    # strip_distances[12,] = c(12, 5.556)
    # strip_distances[13,] = c(13, 9.26)
    # strip_distances[14,] = c(14, 9.26)
    # 
    # dat$distance <- NA #this will be distance in km
    # # create distance column that has distances instead of STRIP values
    # for (i in 1:14) {
    #     #14 possible strip values
    #     I <- which(dat$STRIP == strip_distances[i, 1])
    #     #print(dat$STRIP[I[1]])
    #     dat$distance[I] <- strip_distances[i, 2]
    #     #print(strip_distances[i,2])
    # }
    # 
    # # add some necessary columns to dat
    # dat$Region.Label <-
    #     "MyStratum" #dat$stratum #we may not want this column. Fill with dummy variable. Functionality not yet built-in
    # dat$Sample.Label <-
    #     dat$LEGNO2 #Sample.Label is the sampling unit, which is the transectID/LEGNO2 in our case
    # dat$Area <-
    #     5811 # sq km, this could be put in the function call. this will be different for different years
    # dat$size <- dat$NUMBER #insert the number of animals sighted
    # 
    # # now we need to reduce dat to only sightings of our species of interest
    # Iall = NA
    # ctr = 1
    # uSpp <- length(spp) #species = spp
    # for (i in 1:uSpp) {
    #     # find all of target species, exclude camera sightings, include legstage only == 2 or NA, and restrict idrel to 2 or 3.
    #     if (phstrip == 1){
    #         I <-
    #             which(dat$SPECCODE == spp[i] &
    #                       (dat$LEGSTAGE == 2 | is.na(dat$LEGSTAGE)) &
    #                       (dat$IDREL == 2 | dat$IDREL == 3))
    #     } else if (phstrip == 0){
    #         I <-
    #             which(dat$SPECCODE == spp[i] &
    #                       (dat$LEGSTAGE == 2) &
    #                       (dat$IDREL == 2 | dat$IDREL == 3))
    #     }
    #     
    #     if (length(I) == 0){ #if species spp[i] does not occurr in the season you are analyzing, skip to next species.
    #         next
    #     }
    #     
    #     #Iall is a dynamically generated vector of all row numbers that meet the conditions above.
    #     Iall[ctr:(ctr + length(I) - 1)] <- I
    #     ctr = length(Iall) + 1
    # }
    # 
    # newdat <- dat[Iall,]
    # dat.spp <-
    #     cbind.data.frame(
    #         newdat$Region.Label,
    #         newdat$Area,
    #         newdat$Sample.Label,
    #         newdat$Effort,
    #         newdat$distance,
    #         newdat$size,
    #         newdat$BEAUFORT,
    #         newdat$GLAREL,
    #         newdat$GLARER,
    #         newdat$STRIP,
    #         newdat$CLOUD,
    #         newdat$SPECCODE,
    #         newdat$VISIBLTY,
    #         newdat$FILEID,
    #         newdat$EVENTNO,
    #         newdat$PHSTRIP
    #     )
    # names(dat.spp) <-
    #     c("Region.Label",
    #       "Area",
    #       "Sample.Label",
    #       "Effort",
    #       "distance",
    #       "size",
    #       "beaufort",
    #       "glareL",
    #       "glareR",
    #       "strip",
    #       "cloud",
    #       "speccode",
    #       "visibility",
    #       "fileid",
    #       "eventno",
    #       "phstrip")
    # head(dat.spp)
    # 
    # #this version of dat.spp has the incorrect effort. we need to add up the effort from all flights of the line within this period of time.
    # #the info you need to do this is in effort_summary_reduced
    # for (i in 1:nrow(effort_summary_reduced)) {
    #     I <- which(effort_summary_reduced$LEGNO2[i] == dat.spp$Sample.Label)
    #     if (length(I) > 0) {
    #         #if this line is in the data, then replace incorrect Effort with correct Effort
    #         dat.spp$Effort[I] = effort_summary_reduced$Effort[i]
    #     } else {
    #         dat.spp[(nrow(dat.spp) + 1),]         <-
    #             dat.spp[nrow(dat.spp),] #duplicate last row into a new row
    #         dat.spp$Sample.Label[nrow(dat.spp)] <-
    #             effort_summary_reduced$LEGNO2[i]
    #         dat.spp$Effort[nrow(dat.spp)]       <-
    #             effort_summary_reduced$Effort[i]
    #         dat.spp$distance[nrow(dat.spp)]     <- NA
    #         dat.spp$size[nrow(dat.spp)]         <- NA
    #         dat.spp$beaufort[nrow(dat.spp)]     <- NA
    #         dat.spp$glareL[nrow(dat.spp)]       <- NA
    #         dat.spp$glareR[nrow(dat.spp)]       <- NA
    #         dat.spp$strip[nrow(dat.spp)]        <- NA
    #         dat.spp$cloud[nrow(dat.spp)]        <- NA
    #         dat.spp$speccode[nrow(dat.spp)]     <- NA
    #         dat.spp$visibility[nrow(dat.spp)]   <- NA
    #         dat.spp$fileid[nrow(dat.spp)]       <- NA
    #         dat.spp$eventno[nrow(dat.spp)]      <- NA
    #         dat.spp$phstrip[nrow(dat.spp)]      <- NA
    #     }
    # }
    # dat.spp
    # 
    # fn = paste(spp, collapse = '_')
    # season_beg = paste(substr(season.beg, start = 1, stop = 4),
    #                    substr(season.beg, start = 6, stop = 7),
    #                    sep = '')
    # season_end = paste(substr(season.end, start = 1, stop = 4),
    #                    substr(season.end, start = 6, stop = 7),
    #                    sep = '')
    # fn = paste(fn, season_beg, season_end, sep = "_")
    # if (condensed == 1){
    #     fn = paste(fn, "GenCond", sep = "_")
    # } else if (condensed == 0){
    #     fn = paste(fn, "Gen", sep = "_")
    # }
    # if (phstrip == 1){
    #     fn = paste(fn, "phstrip1", sep = "_")
    # }
    
    #write out .csvs
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
    
    # #write out .csvs
    # #write.csv(effort_summary, file = "~/Desktop/effort_summary_reduced.csv", row.names = FALSE)
    # cmd = paste(
    #     "write.csv(effort_summary_reduced, file = '",
    #     target_dir,
    #     "effort_summary_reduced_",
    #     fn,
    #     ".csv'",
    #     ", row.names = FALSE)",
    #     sep = ""
    # )
    # eval(parse(text = cmd))
    
    # #write.csv(dat, file = "~/Desktop/dat.csv", row.names = FALSE)
    # cmd = paste(
    #     "write.csv(dat, file = '",
    #     target_dir,
    #     "dat_with_dist_",
    #     fn,
    #     ".csv'",
    #     ", row.names = FALSE)",
    #     sep = ""
    # )
    # eval(parse(text = cmd))
    
    # #write.csv(dat.RIWH, file = "~/Desktop/datRIWH.csv", row.names = FALSE)
    # cmd = paste(
    #     "write.csv(dat.spp, file = '",
    #     target_dir,
    #     "dat_for_ds_",
    #     fn,
    #     ".csv'",
    #     ", row.names = FALSE)",
    #     sep = ""
    # )
    # eval(parse(text = cmd))
    
    #return(effort_summary)
#}
