myscript <- paste0(getwd(),"/RSS_subscribe.R")

taskscheduleR::taskscheduler_create(taskname = "getRSS", 
                     rscript = myscript, 
                     schedule = "ONCE", 
                     starttime = format(Sys.time() + 62, "%H:%M"),
                     startdate = format(Sys.Date(), "%m/%d/%Y"))

taskscheduleR::taskscheduler_delete(taskname = "getRSS")
