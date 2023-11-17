myscript <- paste0(getwd(),"/src/RSS_subscribe.R")

taskscheduleR::taskscheduler_create(taskname = "getRSS", 
                     rscript = myscript, 
                     schedule = "DAILY", starttime = "12:04",
                     startdate = format(Sys.Date(), "%m/%d/%Y"))
taskscheduleR::taskscheduler_delete(taskname = "getRSS")
