# https://data.nasa.gov/resource/eva.json (with modifications)
input_file = 'eva_data.json'
output_file = 'eva_data.csv'
graph_file = 'cummulative_eva_graph.png'
fieldnames <- c("EVA #", "Country", "Crew    ", "Vehicle", "Date", "Duration", "Purpose")

library(lubridate)
library(jsonlite)

j_l <- read_json(input_file)
data=as.data.frame(j_l[[1]])

for( i in 2:374){
  r = j_l[[i]]
    print(r)
    data =merge(data, as.data.frame(r),  all=TRUE)
}
#data.pop(0)
## Comment out this bit if you don't want the spreadsheet
write.csv(output_file)



time <- c()
date = Date()


j=1
for (i in rownames(data)){
    print(data[j, ])
    # and this bit
    # w.writerow(data[j].values())
    if (!is.na(data[j,]$duration)){
        duration_str=data[j,]$duration
        if(duration_str == ''){
          #do nothing
        }else{
            duration_dt=as.POSIXlt(duration_str,format='%H:%M')
            duration_hours <- as.numeric(as.difftime(hour(t), units = 'hours')+as.difftime(minute(duration_dt), units='mins')+as.difftime(second(t), units='secs'))/(60*60)
            print(duration_dt,duration_hours)
            time <- c(time, duration_hours)
            if(!is.na(data[j,]$date)){
                date= c(date, as.Date(substr(data[j,'date'], 1, 10), format = '%Y-%m-%d'))
                #date.append(data[j]['date'][0:10])

            }else{
              time <- time[1:length(time) -1]
                }
            }
        }
    j = j+1
}

t=0
for(i in time)
    t <- c(t, t[length(t)]+i)


df <- data.frame(
date, time
)[order(date, time), ]

date <- df$date
time <- df$time


png(graph_file)
plot(date,t[2:length(t)],
xlab = 'Year', ylab= 'Total time spent in space to date (hours)'
)
dev.off()
plot(date,t[2:length(t)],
xlab = 'Year', ylab= 'Total time spent in space to date (hours)'
)
