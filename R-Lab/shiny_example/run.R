library(data.table)
library(shiny)
library(nycflights13)
## library(ggplot2) ## Finally I have not used.
library(lattice)

aip <- data.table(airports)
fli <- data.table(flights)
ali <- data.table(airlines)
pln <- data.table(planes)

str(fli)
aip <- aip[, .(faa, name)]
fli <- fli[, .(year, month, day, dep_delay, arr_delay, carrier, origin, dest, air_time, distance, tailnum)]
pln <- pln[, .(tailnum, manufacturer, type, model)]

fli <- fli[!is.na(arr_delay), ]
fli <- fli[!is.na(dep_delay), ]
fli <- fli[!is.na(distance), ]
fli <- fli[!is.na(air_time), ]
fli <- fli[!is.na(origin), ]
fli <- fli[!is.na(dest), ]
setnames(ali, 'name', 'carrier_name') ## renaming in advance
## setnames(pln, 'year', 'man_year') ## renaming in advance
## pln[, speed := NULL] ## this column is so gappy that it is useless
pln[manufacturer == 'AIRBUS INDUSTRIE', manufacturer := 'AIRBUS'] ## these are the same manufacturers
pln[manufacturer == 'CANADAIR LTD', manufacturer := 'BOMBARDIER INC'] ## these are the same manufacturers
pln[manufacturer == 'CANADAIR', manufacturer := 'BOMBARDIER INC'] ## these are the same manufacturers
pln[manufacturer == 'MCDONNELL DOUGLAS AIRCRAFT CO', manufacturer := 'MCDONNELL DOUGLAS'] ## these are the same manufacturers
pln[manufacturer == 'MCDONNELL DOUGLAS CORPORATION', manufacturer := 'MCDONNELL DOUGL'] ## these are the same manufacturers

fli[, date := as.Date(paste(fli[, year], fli[, month], fli[, day], sep = '-'))] ## creating a collapsed date field
fli[, year := NULL] ## it has no added value left

setkey(fli, 'origin')
setkey(aip, 'faa')
fli <- fli[aip, nomatch = 0]

setkey(fli, 'dest')
fli <- fli[aip, nomatch = 0]
setnames(fli, gsub('^i\\.', 'dest_', colnames(fli)))

fli <- fli[ali, on = 'carrier', nomatch = 0]

fli <- fli[pln, on = 'tailnum', nomatch = 0] 


