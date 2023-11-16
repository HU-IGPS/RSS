myscript <- paste0(getwd(),"/src/RSS_subscribe.R")

taskscheduleR::taskscheduler_create(taskname = "getRSS", 
                     rscript = myscript, 
                     schedule = "DAILY", starttime = "12:00",
                     startdate = format(Sys.Date()+1, "%m/%d/%Y"))
taskscheduleR::taskscheduler_delete(taskname = "getRSS")

taskscheduleR::taskscheduler_delete(taskname = "getRSSt")

taskscheduleR::taskscheduler_create(taskname = "getRSSt", 
                                    rscript = myscript, 
                                    schedule = "DAILY", 
                                    starttime = "12:30",
                                    startdate = format(Sys.Date(), "%m/%d/%Y")
                                    )
