### Function to modify data.frame to have regular time series intervals without gaps
### written by Alison Appling

library(lubridate)

#' Ensure a regular time series, filling in gaps where necessary
#' 
#' Lengthens data.frame to include any missing ~15-minute observations. Assumes 
#' that date-times are in a column called "DATETIME" and that a column called DayID
#' already exists; fills gaps above 1200 seconds
regularizeTimeSeries <- function(chem_inst) {
  
  # Isolate the date column, make sure the dates are ordered, and note any gaps
  chem_inst <- chem_inst[order(chem_inst$DATETIME),]
  
  # Remove any too-short gaps ('crunches', < 8 minutes). Do identical ones 
  # first, all at once, because removing one won't change the other 'crunches'
  diffs <- as.numeric(diff(chem_inst$DATETIME), units="mins")
  dupes <- which(diffs < 0.5)
  to_remove <- unlist(lapply(dupes, function(dupe) {
    option1 <- chem_inst[dupe,]
    option2 <- chem_inst[dupe+1,]
    row.names(option1) <- row.names(option2) <- NULL
    if(identical(option1, option2)) {
      cat(paste0("Eliminating 0 DATETIME gap between identical rows at ", chem_inst$DATETIME[dupe], " and ", chem_inst$DATETIME[dupe+1], ": removing #2\n"))
      dupe + 1
    } else {
      to_remove <- ifelse(sum(is.na(option1)) > sum(is.na(option2)), dupe, dupe+1)
      cat(paste0("Eliminating 0 DATETIME gap between non-identical rows at ", option1$DATETIME, " and ", option2$DATETIME, ": removing #", 1+to_remove-dupe, "\n"))
      to_remove
    }
  }))
  if(length(to_remove) > 0) chem_inst <- chem_inst[-to_remove, ]
  rm(to_remove)
  
  # Next do non-identical rows. In each batch of removals just remove the first
  # of each run of short gaps because each removal could change another gap size
  diffs <- as.numeric(diff(chem_inst$DATETIME), units="mins")
  diffsrle <- as.data.frame(unclass(rle(diffs)))
  mindiff <- min(diffs)
  while(mindiff < 8) {
    crunches <- which(unlist(lapply(seq_len(nrow(diffsrle)), function(r) { 
      d <- diffsrle[r,]
      if(d$values == mindiff) {
        c(TRUE, rep(FALSE, d$lengths - 1))
      } else {
        rep(FALSE, d$lengths)
      }
    })))
    to_remove <- sapply(crunches, function(crunch) {
      option1 <- chem_inst[crunch,]
      option2 <- chem_inst[crunch+1,]
      # Keep the one with fewer missing values
      to_remove <- ifelse(sum(is.na(option1)) > sum(is.na(option2)), crunch, crunch+1)
      cat(paste0("Eliminating short DATETIME gap between non-identical rows at ", option1$DATETIME, " and ", option2$DATETIME, ": removing #", 1+to_remove-crunch, "\n"))
      to_remove
    })
    chem_inst <- chem_inst[-to_remove, ]
    # Check the new dataset for lingering 'crunches'
    diffs <- as.numeric(diff(chem_inst$DATETIME), units="mins")
    diffsrle <- as.data.frame(unclass(rle(diffs)))
    mindiff <- min(diffs)
  }
  
#################################################################################
####this is used to insert rows at 15-min intervals into gaps of >27 minutes
  # Note any gaps larger than 27 minutes and insert new dates within them
  ts_inst <- chem_inst$DATETIME
  gaps <- which(as.numeric(diff(ts_inst), units="mins") > 27)
  ts_inst_reg <- do.call(c, c(
    list(ts_inst), 
    lapply(gaps, function(g) {
      # Define the bounds of the gap
      gapbounds <- c(ts_inst[g], ts_inst[g+1])
      
      # If the gap isn't ridiculously big, fill it in (just adding timestamps, not 'data')
      if(as.numeric(diff(gapbounds), units="days") <= 60) {
        # Create a sequence of DATETIMEs to fill in the time series gap
        newdates <- seq(gapbounds[1], gapbounds[2]+as.difftime(13, units="mins"), by=as.difftime(15, units="mins"))
        # Remove the dates on the end if they're too close to the gapbounds
        newdates <- newdates[-which(abs(difftime(newdates, gapbounds[1], units="mins")) < 8 | 
                                      abs(difftime(newdates, gapbounds[2], units="mins")) < 8)]
        # Add these new dates
        newdates
      } else {
        NULL
      }
    })))
  ts_inst_reg <- sort(ts_inst_reg)
  
  # Add in new rows for any new DATETIME values
  ts_inst_df <- data.frame(DATETIME=ts_inst_reg)
  chem_inst <- merge(chem_inst, ts_inst_df, by='DATETIME', all.y = TRUE)
  

###############################################################################################################
#####for each day, where my days start and end at 4 am for the sake of metabolism modeling, 
#####using the first DATETIME of the day at its exact value and rounding all other DATETIMEs so they're at exactly 15-minute interval  
  
  # Smooth over irregularities in reporting, e.g., timestamps that increment by 
  # 14:30, 15:10, 15:10, 15:10, 14:30, etc. mins (this seems to be common).
  # Normalizing by hour means we might give up a day or ten per timeseries, but
  # mostly it works.
  
chem_inst <- transform(chem_inst,
    DATETIME = if(length(unique(diff(DATETIME))) > 1) {
      DATETIME[1] + (60*15) * round((DATETIME - DATETIME[1])/(60*15)) # round to even 15-min intervals relative to DateTime[1]
    } else DATETIME)
  

###############################################################################################################
#####  remove any duplicates that appeared due to the rounding in step 
  # Remove duplicates again - this time if duplicates exist, it's because they
  # both got rounded to the same 15-minute interval
  
  diffs <- as.numeric(diff(chem_inst$DATETIME), units="mins")
  dupes <- which(diffs < 0.5)
  to_remove <- unlist(lapply(dupes, function(dupe) {
    option1 <- chem_inst[dupe,]
    option2 <- chem_inst[dupe+1,]
    row.names(option1) <- row.names(option2) <- NULL
    if(identical(option1, option2)) {
      cat(paste0("Eliminating 0 DATETIME gap between rounded identical rows at ", chem_inst$DATETIME[dupe], " and ", chem_inst$DATETIME[dupe+1], ": removing #2\n"))
      dupe + 1
    } else {
      to_remove <- ifelse(sum(is.na(option1)) > sum(is.na(option2)), dupe, dupe+1)
      cat(paste0("Eliminating 0 DATETIME gap between rounded non-identical rows at ", option1$DATETIME, " and ", option2$DATETIME, ": removing #", 1+to_remove-dupe, "\n"))
      to_remove
    }
  }))
  if(length(to_remove) > 0) chem_inst <- chem_inst[-to_remove, ]
  rm(to_remove)

###########################################################################################################

  
  # Fix up Year, Day, and Hour columns
  chem_inst <- chem_inst %>% mutate(
    DayID = ordered(format(DATETIME, "%Y-%m-%d")),
    Year = as.integer(format(DATETIME, "%Y")),
    Month = as.integer(format(DATETIME, "%M")),
    YearDay = as.integer(format(DATETIME, "%j")),
    Hour = as.double(DATETIME - as.POSIXct(strptime(format(DATETIME, "%Y-%m-%d"), format="%Y-%m-%d"), tz=tz(DATETIME)), units="hours"),
    Minute = as.numeric(strftime(DATETIME, "%M")),
    Second = as.numeric(strftime(DATETIME, "%S"))
  )
  
  chem_inst
}
  
  