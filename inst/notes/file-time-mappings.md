# File -- time mapping

There are various modes of "read" that we want

- specific date-time (nearest-neighbour, findInterval, or continuous slice-mapping)
- date-time range (pull out every slice included)
- all times available

That covers 'readsst(dates)', `extract(readsst, xyt, ctstime = TRUE/FALSE)`, so the methods for time matching would be "resolve matches - unique nearest neighbour", "inclusive range", "continuous" , and "all times" for a XY or bbox extract query. 

function      | time type                           | notes
--------------|-------------------------------------|-------
ocfiles       | various standard intervals          | 
amps_d1files  | complex overlapping                 | overlapping forecast intervals, model spin-up time down-weighted
chlafiles     | standard (8D / MO)                  | 
currentsfiles | standard (daily, weekly)            | weekly is legacy now
derivicefiles | standard daily                      | 
fasticefiles  | bespoke 3-weekly                    | 
ghrsstfiles   | standard daily                      |
icefiles      | standard (daily/monthly)            |
prodfiles     | standard (8D)                       |  
windfiles     | 6hourly (within yearly), dailyavg   | 
sshfiles      | standard daily                      | 
sstfiles      | standard daily                      |
sstfiles      | single-file month, single-file week |
