##  *** https://github.com/trendct/walkthroughs/blob/master/0315-meetup-analysis/meetups-analysis.R ***
## Meetup.com
## http://www.meetup.com/meetup_api/apps/

require(jsonlite)
require(data.table)
require(sqldf)
require(curl)
require(RCurl)
require(XML)
require(tm)
require(utils)
require(NCmisc)
require(stringr)
require(stringi)
require(httr)
require(rio)

options(StringAsFactor=FALSE)
options(datatable.verbose=TRUE)

creds <- import('creds.csv')
Key <- paste('key = ', creds[ProjectTitle == 'meetup_com' & AppName == 'network_science', Key], sep = '')
Sig <- paste('sig = ', creds[ProjectTitle == 'meetup_com' & AppName == 'network_science', Signature], sep = '')
SigId <- paste('sig_id = ', creds[ProjectTitle == 'meetup_com' & AppName == 'network_science', SignatureId], sep = '')

getwd()
setwd('./CEU_final_project')
getwd()

## Let's check what kind of categories we have.
categories <- data.table(fromJSON(paste('https://api.meetup.com/2/categories?offset=0&format=json&photo-host=public&page=500&order=shortname&desc=false&', SigId, '&', Sig, '', sep = ''), flatten = TRUE)[[1]])
## name = 'Tech'
## id = '34'


## http://www.countrycallingcodes.com/iso-country-codes/europe-codes.php ##
## http://unstats.un.org/unsd/methods/m49/m49regin.htm#europe ##
ecc <- read.csv(file = 'background_infos/European_country_codes.csv', header=TRUE, sep = ';')
setDT(ecc)


#### Oslo
#### 1800
#### lat: 59.91
#### lon: 10.75
#### paste('https://api.meetup.com/2/groups?offset=0&format=json&lon=10.75&category_id=34&photo-host=public&page=500&radius=1800.0&fields=&lat=59.91&order=id&desc=false&', SigId, '&', Sig, sep = '')
linkchunk <- 'https://api.meetup.com/2/groups?offset='
linkchunk2 <- paste('&format=json&lon=10.75&category_id=34&photo-host=public&page=500&radius=1800.0&fields=&lat=59.91&order=id&desc=false&', SigId, '&', Sig, sep = '')
link <- paste(linkchunk, 0, linkchunk2, sep = '')
## Let's download the first offset. (literally it is named as 0th.)
techgroups_raw_Oslo <- data.table(fromJSON(link, flatten = TRUE)[[1]])
meta <- data.table(fromJSON(link, flatten = TRUE)[[2]])
offset <- floor(as.numeric(unlist(meta[3]))/200)
for (i in (1:offset))
{
  link <- paste(linkchunk, i, linkchunk2, sep = '')
  temp <- data.table(fromJSON(link, flatten = TRUE)[[1]])
  if (length(temp) > 0) {techgroups_raw_Oslo <- rbind(techgroups_raw_Oslo, temp, fill = TRUE)}
  wait((runif(1, 0, 1)/10000), unit = 's') ## this delay helps pulling data without interruption
}

## Let's peel HTML code off.
for (i in (1: nrow(techgroups_raw_Oslo))) {techgroups_raw_Oslo[i, description := description %>% str_replace_all("<(.*?)>", "")]}

## Let's make it distinct
setkey(techgroups_raw_Oslo, country, visibility, city, created, id, lon, lat, join_mode)
techgroups_raw_Oslo <- unique(techgroups_raw_Oslo)

## Let's save the techgroupsraw_Oslo file
export(techgroups_raw_Oslo, 'techgroups_raw_Oslo.RData')
## techgroups_raw_Oslo <- as.data.table(import('techgroups_raw_Oslo.RData'))


#### Cagliari
#### 1600
#### lat: 39.22
#### lon: 9.12
#### paste('https://api.meetup.com/2/groups?offset=0&format=json&lon=9.12&category_id=34&photo-host=public&page=500&radius=1600.0&fields=&lat=39.22&order=id&desc=false&', SigId, '&', Sig, sep = '')
linkchunk <- 'https://api.meetup.com/2/groups?offset='
linkchunk2 <- paste('&format=json&lon=9.12&category_id=34&photo-host=public&page=500&radius=1600.0&fields=&lat=39.22&order=id&desc=false&', SigId, '&', Sig, sep = '')
link <- paste(linkchunk, 0, linkchunk2, sep = '')
## Let's download the first offset. (literally it is named as 0th.)
techgroups_raw_Cagliari <- data.table(fromJSON(link, flatten = TRUE)[[1]])
meta <- data.table(fromJSON(link, flatten = TRUE)[[2]])
offset <- floor(as.numeric(unlist(meta[3]))/200)
for (i in (1:offset))
{
  link <- paste(linkchunk, i, linkchunk2, sep = '')
  temp <- data.table(fromJSON(link, flatten = TRUE)[[1]])
  if (length(temp) > 0) {techgroups_raw_Cagliari <- rbind(techgroups_raw_Cagliari, temp, fill = TRUE)}
  wait((runif(1, 0, 1)/10000), unit = 's') ## this delay helps pulling data without interruption
}

## Let's peel HTML code off.
for (i in (1: nrow(techgroups_raw_Cagliari))) {techgroups_raw_Cagliari[i, description := description %>% str_replace_all("<(.*?)>", "")]}

## Let's make it distinct
setkey(techgroups_raw_Cagliari, country, visibility, city, created, id, lon, lat, join_mode)
techgroups_raw_Cagliari <- unique(techgroups_raw_Cagliari)

## Let's save the techgroups_raw_Cagliari file
export(techgroups_raw_Cagliari, 'techgroups_raw_Cagliari.RData')
## techgroups_raw_Cagliari <- as.data.table(import('techgroups_raw_Cagliari.RData'))


#### Kharkiv
#### 1800
#### lat: 49.99
#### lon: 36.23
#### paste('https://api.meetup.com/2/groups?offset=0&format=json&lon=36.23&category_id=34&photo-host=public&page=500&radius=1800.0&fields=&lat=49.99&order=id&desc=false&', SigId, '&', Sig, sep = '')
linkchunk <- 'https://api.meetup.com/2/groups?offset='
linkchunk2 <- paste('&format=json&lon=36.23&category_id=34&photo-host=public&page=500&radius=1800.0&fields=&lat=49.99&order=id&desc=false&', SigId, '&', Sig, sep = '')
link <- paste(linkchunk, 0, linkchunk2, sep = '')
## Let's download the first offset. (literally it is named as 0th.)
techgroups_raw_Kharkiv <- data.table(fromJSON(link, flatten = TRUE)[[1]])
meta <- data.table(fromJSON(link, flatten = TRUE)[[2]])
offset <- floor(as.numeric(unlist(meta[3]))/200)
for (i in (1:offset))
{
  link <- paste(linkchunk, i, linkchunk2, sep = '')
  temp <- data.table(fromJSON(link, flatten = TRUE)[[1]])
  if (length(temp) > 0) {techgroups_raw_Kharkiv <- rbind(techgroups_raw_Kharkiv, temp, fill = TRUE)}
  wait((runif(1, 0, 1)/10000), unit = 's') ## this delay helps pulling data without interruption
}

## Let's peel HTML code off.
for (i in (1: nrow(techgroups_raw_Kharkiv))) {techgroups_raw_Kharkiv[i, description := description %>% str_replace_all("<(.*?)>", "")]}

## Let's make it distinct
setkey(techgroups_raw_Kharkiv, country, visibility, city, created, id, lon, lat, join_mode)
techgroups_raw_Kharkiv <- unique(techgroups_raw_Kharkiv)

## Let's save the techgroups_raw_Kharkiv file
export(techgroups_raw_Kharkiv, 'techgroups_raw_Kharkiv.RData')
## techgroups_raw_Kharkiv <- as.data.table(import('techgroups_raw_Kharkiv.RData'))


## Let's create union of the 3
techgroups_raw <- rbind(rbind(techgroups_raw_Kharkiv,techgroups_raw_Cagliari),techgroups_raw_Oslo)
## Let's make it distinct
setkey(techgroups_raw, country, visibility, city, created, id, lon, lat, join_mode)
techgroups_raw <- unique(techgroups_raw)
## Let's remove the remaining HTML codes
for (i in (1: nrow(techgroups_raw))) 
{
  techgroups_raw[i, description := str_replace_all(description, "(\\Q<U+0\\E)(\\d\\d\\d)(\\Q>\\E)", "")] 
  techgroups_raw[i, description := str_replace_all(description, "&nbsp;", "")]
  techgroups_raw[i, description := str_replace_all(description, "br&gt;", "")]
  techgroups_raw[i, description := str_replace_all(description, "&amp;", "")]
  techgroups_raw[i, description := str_replace_all(description, "amp;", "")]
  techgroups_raw[i, description := str_replace_all(description, "&lt;", "")]
  techgroups_raw[i, description := str_replace_all(description, "a href=;", "")]
  techgroups_raw[i, description := str_replace_all(description, "document.write", "")]
  techgroups_raw[i, description := str_replace_all(description, "\\n", "")]
  techgroups_raw[i, description := str_trim(description)]
}
## Converting the creation date into a more readable format
techgroups_raw[, created_date := as.POSIXct(created/1000, origin = '1970-01-01')]

## Let's drop the not European countries 
## inner join with European countries
setkey(ecc, country)
setkey(techgroups_raw, country)
techgroups_raw <- techgroups_raw[ecc, nomatch=0]
## dropping the Asian timezones
techgroups_raw <- techgroups_raw[!(timezone %like% '^Asia'),]

## Let's save the techgroups_raw file
export(techgroups_raw, 'techgroups_raw.RData')
## techgroups_raw <- as.data.table(import('techgroups_raw.RData'))


## Let's do some further modification
techgroups <- copy(techgroups_raw)
## Extracting the year and yearmonth of creation
techgroups[, year := as.numeric(format(created_date,"%Y"))]
techgroups[, yearmonth := as.numeric(format(created_date,"%Y%m"))]
## creating a technically necessary columns
techgroups[, member_offset := floor(as.numeric(members)/200)]
## Let's convert city name into a more usable format
techgroups[, city := stri_trans_general(city, 'Latin-ASCII')]

## deleting unnecessary columns
techgroups[, created := NULL]
techgroups[, utc_offset := NULL]
techgroups[, category.shortname := NULL]
techgroups[, organizer.photo.thumb_link := NULL]
techgroups[, group_photo.thumb_link := NULL]
techgroups[, group_photo.highres_link := NULL]
techgroups[, organizer.photo.highres_link := NULL]
techgroups[, state := NULL]
techgroups[, organizer.photo.photo_link := NULL]
techgroups[, group_photo.photo_link := NULL]
techgroups[, category.name := NULL]
techgroups[, category.id := NULL]
techgroups[, organizer.photo.photo_id := NULL]
techgroups[, group_photo.photo_id := NULL]

## changing column names
setnames(techgroups, 'organizer.name', 'organizer_name')
setnames(techgroups, 'organizer.member_id', 'organizer_member_id')
setnames(techgroups, 'topics', 'topics_list')

## Let's convert topic list into a string
techgroups[, topic_text_list := paste0(' | ', unlist(unlist(techgroups[, topics_list], recursive=FALSE, use.names=TRUE)[1]), collapse = '')]

## Let's save the techgroups file
export(techgroups, 'techgroups.RData')
write.table(subset(techgroups, select=-topics_list), file = 'background_infos/techgroups.csv', sep = '¤', row.names = FALSE)
## techgroups <- as.data.table(import('techgroups.RData'))



## Decomposing topics fields into a detailed topics-group_id table
techgroups_topics <- NULL
for (i in (1:nrow(techgroups)))
{
  if (length(techgroups[i, topics_list][1]) > 0)
  {
    temp <- cbind(setDT(unlist(techgroups[i, topics_list], recursive=FALSE, use.names=TRUE)), group_id=techgroups[i, id])
    techgroups_topics <- rbind(techgroups_topics, temp)
  }
}
## changing column names of topic table
setnames(techgroups_topics, 'urlkey', 'topic_urlkey')
setnames(techgroups_topics, 'name', 'topic_name')
setnames(techgroups_topics, 'id', 'topic_id')
## Let's merge the techgroups with their topics
techgroups_topics <- merge(x=techgroups, y=techgroups_topics, by.x = 'id', by.y = 'group_id', all=TRUE)
techgroups_topics[, topics_list := NULL]
techgroups_topics[, topic_text_list := NULL]
## Let's save the techgroups_topics file
export(techgroups_topics, 'techgroups_topics.RData')
write.table(techgroups_topics, file = 'background_infos/techgroups_topic.csv', sep = ';', row.names = FALSE)
## techgroups_topics <- as.data.table(import('techgroups_topics.RData'))


## Let's create a unique topic list 
setkey(techgroups_topics, topic_id, topic_urlkey, topic_name)
unique_topic_list <- techgroups_topics[, .N, by=c('topic_id', 'topic_urlkey', 'topic_name')]
unique_topic_list <- unique_topic_list[order(-N)]
## Let's save the unique_topic_list file
export(unique_topic_list, 'unique_topic_list.RData')
write.table(unique_topic_list, file = 'background_infos/unique_topic_list.csv', sep = '|', row.names = FALSE)
## unique_topic_list <- as.data.table(import('unique_topic_list.RData'))


## Let's sum up members of countries
country_sum_members <- data.table(techgroups[, sum(members), by=country])  
country_sum_members <- country_sum_members[order(country)]
setnames(country_sum_members, 'V1', 'sum_of_membership')
setnames(city_sum_members, 'V1', 'sum_of_membership') 
## Let's save the country_sum_members file
export(country_sum_members, 'country_sum_members.RData')
write.table(country_sum_members, file = 'background_infos/country_sum_members.csv', sep = '|', row.names = FALSE)
## country_sum_members <- as.data.table(import('country_sum_members.RData'))


## Let's sum up members of cities
city_sum_members <- data.table(techgroups[, sum(members), by=.(country, city)])
city_sum_members[, city := stri_trans_general(city, 'Latin-ASCII')]
city_sum_members <- city_sum_members[order(country, city, sum_of_membership)]
## Let's save the city_sum_members file
export(city_sum_members, 'city_sum_members.RData')
write.table(city_sum_members, file = 'background_infos/city_sum_members.csv', sep = '|', row.names = FALSE)
## city_sum_members <- as.data.table(import('city_sum_members.RData'))


## Let's download all the members from all groups
# city_sum_members <- as.data.table(import('city_sum_members.RData'))
# techgroups <- as.data.table(import('techgroups.RData'))
sample <- data.table(fromJSON(paste('https://api.meetup.com/2/members?', Key, '&sign=true&format=json&order=name&group_id=19111951&offset=0', sep = ''), simplifyVector = FALSE)[[1]])
sample[, group_id := '1111111']
sample[, member_id := '207331461']
sample[, member_name := 'ooo']
sample[, joined_date := as.POSIXct(1.462467e+12/1000, origin = '1970-01-01')]
sample[, visited_date := as.POSIXct(1.462467e+12/1000, origin = '1970-01-01')]
sample[, topics := list(V1)]
sample[, V1 := NULL]
sample <- sample[0]
members_raw_NO_UA <- copy(sample)
for (i in city_sum_members[, .I[country == 'UA' & city == 'Kyiv']]:city_sum_members[, .I[country == 'UA' & city == 'Zaporizhzhya']])  
{
  for (j in techgroups[country == city_sum_members[i, country] & city == city_sum_members[i, city], id])
  {
    for (k in 0:techgroups[id == j, member_offset])
    {
      memberlink <- paste('https://api.meetup.com/2/members?', Key, '&sign=true&format=json&order=name&group_id = ', j, '&offset = ', k, sep = '')
      temp <- copy(data.table(fromJSON(memberlink, flatten = TRUE)[[1]]))
      wait((runif(1, 0, 1)/500), unit = 's') ## this delay helps pulling data without interruption
      if (length(temp) > 0)
      {
        temp[, member_name := name]
        temp[, member_id := id]
        temp[, group_id := j]
        temp[, joined_date := as.POSIXct(joined/1000, origin = '1970-01-01')]
        temp[, visited_date := as.POSIXct(visited/1000, origin = '1970-01-01')]
        members_raw_NO_UA <- rbind(members_raw_NO_UA, temp[, .(group_id, member_id, member_name, joined_date, visited_date, topics)], fill= TRUE)
      }
    }
  }
  print(paste(i, 'th ', city_sum_members[i, country], ' - ', city_sum_members[i, city], ': ', city_sum_members[i, sum(sum_of_membership)], ' members', sep = ''))
  export(members_raw_NO_UA, 'members_raw_NO_UA.RData')
}


## Let's join the partial member_raws into an all_members table
members <- as.data.table(import('members.RData'))
members <- rbind(members, members_raw_NO_UA)

## Let's drop the not distinct records
setkey(members, group_id, member_id, member_name)
members <- unique(members)

## Let's drop those members who has not got member_id
members <- members[is.na(member_id) == FALSE, ]

## Let's convert topic list into a string
setkey(members, group_id, member_id)
members[, topic_text_list := paste0(' | ', unlist(unlist(members[, topics], recursive=FALSE, use.names=TRUE)[1]), collapse = '')]
setnames(members, 'topics', 'topic_list')

## Let's save the modified members file
export(members, 'members.RData')
## members <- as.data.table(import('members.RData'))

bers)



## Let's create a unique member table
unique_members <- as.data.table(copy(members[, .(member_id, member_name)]))
setkey(unique_members, member_id, member_name)
unique_members <- unique(unique_members)
## Let's delete the NA's
setkey(unique_members, member_id)
unique_members <- unique_members[is.na(member_id) == FALSE, ]
## Let's save the unique_members file
export(unique_members, 'unique_members.RData')
write.table(unique_members, file = 'background_infos/unique_members.csv', sep = '¤', row.names = FALSE)
## Let's import unique_members table
# unique_members <- as.data.table(import('unique_members.RData'))

## Let's assign 'joined', 'country', 'city', 'lat', 'lon' and 'status' to each member
temp <- NULL
probe <- NULL
probe <- copy(unique_members[1:6])
probe[, member_created_date := as.POSIXct(1.372468e+12/1000, origin = '1970-01-01')]
probe[, member_country := 'oo'] 
probe[, member_city := 'oooo']
probe[, member_lat := 1.00]
probe[, member_lon := 1.00]
probe[, member_status := 'active']
probe[, member_stats_groups := 999]
probe[, member_stats_rsvps := 999]
for (i in probe[, member_id])
  {
    ## memberlink <- paste('https://api.meetup.com/members/', i, '?&sign=true&photo-host=public', sep = '')
    ## https://api.meetup.com/members/100000052?&sign=true&photo-host=public&fields=stats
    memberlink <- paste('https://api.meetup.com/members/', i, '?photo-host=public&', SigId, '&fields=stats&', Sig, sep = '')
    temp <- copy(fromJSON(memberlink, flatten = TRUE))   ## [, .(joined, country, city, lat, lon, status, stats)]
    wait((runif(1, 0, 1)/500), unit = 's') ## this delay helps pulling data without interruption
    probe[member_id == i, member_created_date := as.POSIXct(temp$joined/1000, origin = '1970-01-01')]
    probe[member_id == i, member_country := temp$country]
    probe[member_id == i, member_city := temp$city]
    probe[member_id == i, member_lat := temp$lat]
    probe[member_id == i, member_lon := temp$lon]
    probe[member_id == i, member_status := temp$status]
    probe[member_id == i, member_stats_groups := temp$stats$groups]
    probe[member_id == i, member_stats_rsvps := temp$stats$rsvps]
  }
## Let's save the modified unique_members file
export(unique_members, 'unique_members.RData')
## unique_members <- as.data.table(import('unique_members.RData'))


## Let's create an organizer_launched_group_mapping file
setkey(techgroups, organizer_member_id, id)
organizer_launched_group_mapping <- copy(techgroups[is.na(organizer_member_id) == FALSE, .(organizer_member_id, organizer_name, id, name, created_date, rating, country, city, lat, lon)]) 
setnames(organizer_launched_group_mapping, 'name', 'group_name')
setnames(organizer_launched_group_mapping, 'created_date', 'group_created_date')
setnames(organizer_launched_group_mapping, 'rating', 'group_rating')
setnames(organizer_launched_group_mapping, 'country', 'group_country')
setnames(organizer_launched_group_mapping, 'city', 'group_city')
setnames(organizer_launched_group_mapping, 'lat', 'group_lat')
setnames(organizer_launched_group_mapping, 'lon', 'group_lon')
setnames(organizer_launched_group_mapping, 'id', 'group_id')
organizer_launched_group_mapping <- organizer_launched_group_mapping[order(organizer_member_id, group_id)]
## Let's save the organizer_launched_group_mapping file
export(organizer_launched_group_mapping, 'organizer_launched_group_mapping.RData')
## organizer_launched_group_mapping <- as.data.table(import('organizer_launched_group_mapping.RData'))


## Let's create a unique_organizer_list file
unique_organizer_list <- copy(organizer_launched_group_mapping)
setkey(unique_organizer_list, organizer_member_id)
unique_organizer_list[, sum_of_organized_groups := .N, by=organizer_member_id]
members[, member_id := as.integer(member_id)]
setkey(members, member_id)
unique_organizer_list <- members[unique_organizer_list, nomatch=0]
unique_organizer_list[, sum_of_memberships := .N, by=member_id]
setnames(unique_organizer_list, 'i.group_id', 'organized_group_id')
setnames(unique_organizer_list, 'group_name', 'organized_group_name')
setnames(unique_organizer_list, 'group_created_date', 'organized_group_created_date')
setnames(unique_organizer_list, 'group_rating', 'organized_group_rating')
setnames(unique_organizer_list, 'group_country', 'organized_group_country')
setnames(unique_organizer_list, 'group_city', 'organized_group_city')
setnames(unique_organizer_list, 'group_lat', 'organized_group_lat')
setnames(unique_organizer_list, 'group_lon', 'organized_group_lon')
setnames(unique_organizer_list, 'group_id', 'group_id_as_member')
setnames(unique_organizer_list, 'topic_text_list', 'organizer_topic_text_list')
setnames(unique_organizer_list, 'member_id', 'organizer_id')
unique_organizer_list[, first_membership_date := min(joined_date), by=organizer_id]
unique_organizer_list[, first_organized_group_created_date := min(organized_group_created_date), by=organizer_id]
unique_organizer_list[, avg_organized_group_rating := mean(organized_group_rating), by=organizer_id]
unique_organizer_list[, joined_date := NULL]
unique_organizer_list[, organized_group_created_date := NULL]
unique_organizer_list[, organized_group_rating := NULL]
unique_organizer_list[, topic_list := NULL]
unique_organizer_list[, visited_date := NULL]
unique_organizer_list[, member_name := NULL]
unique_organizer_list[, member_created_date := NULL]
unique_organizer_list[, member_country := NULL]
unique_organizer_list[, member_city := NULL]
unique_organizer_list[, member_lat := NULL]
unique_organizer_list[, member_lon := NULL]
unique_organizer_list[, member_status := NULL]
unique_organizer_list[, organized_group_country := NULL]
unique_organizer_list[, organized_group_city := NULL]
unique_organizer_list[, organized_group_lat := NULL]
unique_organizer_list[, organized_group_lon := NULL]
unique_organizer_list[, group_id_as_member := NULL]
unique_organizer_list[, organized_group_id := NULL]
unique_organizer_list[, organized_group_name := NULL]
unique_organizer_list <- unique(unique_organizer_list)
unique_organizer_list <- unique_organizer_list[order(-sum_of_organized_groups, -sum_of_memberships, -avg_organized_group_rating)]
unique_organizer_list[, interest_ratio := sum_of_memberships/sum_of_organized_groups]
## Let's save the unique_organizer_list file
export(unique_organizer_list, 'unique_organizer_list.RData')
## unique_organizer_list <- as.data.table(import('unique_organizer_list.RData'))

# temp <- copy(as.data.table(techgroups[, .N, by = city]))
# temp <- temp[N > 1, ]
# and then join ...

#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
only_organizer <- copy(unique_organizer_list[sum_of_memberships == 0, ]) ## 0
erroneous <- copy(unique_organizer_list[sum_of_memberships < sum_of_organized_groups, ]) ## 0
not_interested_in_other_topics <- copy(unique_organizer_list[sum_of_memberships == sum_of_organized_groups, ]) ## 883
interested_in_other_topics <- copy(unique_organizer_list[sum_of_memberships > sum_of_organized_groups, ]) ## 7020
interested_in_other_topics[, interest_ratio := sum_of_memberships/sum_of_organized_groups]
unique_active_organizer_list <- copy(unique_organizer_list[sum_of_organized_groups > 1, ]) ## 1194
## Let's save the unique_active_organizer_list file
export(unique_active_organizer_list, 'unique_active_organizer_list.RData')
## unique_active_organizer_list <- as.data.table(import('unique_active_organizer_list.RData'))
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


# node - techgroup
# edge - mutual organizer_id