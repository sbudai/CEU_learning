require(jsonlite)
require(data.table)
require(sqldf)
require(rio)

creds <- import('creds.csv')
Key <- paste('key=', creds[ProjectTitle == 'meetup_com' & AppName == 'network_science', Key], sep = '')


## The meetup.com API gives back maximum 200 rows per request, thats why we need to know, how many request shall we do.
## This kind of metadata can be requested with V2 type of API
Offsets <- ceiling(fromJSON(paste('https://api.meetup.com/2/groups?', Key, '&sign=true&lat=47.51&lon=19.08', sep =''))$meta$total_count/200)-1
## The V3 type API gives back more solvable format than the V2 type one. Therefore we use V3 type one.
linkchunk <- paste('https://api.meetup.com/find/groups?', Key, '&sign=true&lat=47.51&lon=19.08&format=json&order=members&desc=false&offset=', sep = '')

## Let's download the first offset. (literally it is named as 0th.)
groups <- data.table(fromJSON(paste(linkchunk, '0', sep=''), flatten=TRUE)) ## It gives back only the first 200 hits.
groups <- groups[FALSE,] ## empty the table, but the structure remainsprint(paste(k,'-',j)) 
## Let's download all the rows of groups. (These are divided into offsets.)
for (i in 0:(Offsets))
  {
  link <- paste(linkchunk, i, sep= '')
  groups <- rbind(groups, data.table(fromJSON(link, flatten=TRUE)))
  }
## Let's check the structure
str(groups)
## There is column which contains list and unnecesarry. Therefore we drop that column.
groups[, photos:=NULL]
## Let's create a max member offset counter for each row
groups[, member_offsets:=ceiling(members/200)-1]
## Let's check the column names
names(groups)
## Let's save the results. The folder should be set according to the current local machine. 
write.csv(groups, file='./NetworkScience/project/groups.csv', sep=';') 

## Let's create an empty data.table
members <- data.table(fromJSON(paste('https://api.meetup.com/2/members?', Key, '&sign=true&format=json&order=name&group_id=408089&offset=0', sep = ''), flatten=TRUE)[[1]])
members[, group:='99999999']
members <- members[, .(id, name, link, joined, visited, group)]
members <- members[FALSE,]
## Let's download all the members from all groups
for (j in groups[,id])
  {
  memberlink <- paste('https://api.meetup.com/2/members?', Key, '&sign=true&format=json&order=name&group_id=', j, '&offset=', sep='')
  for (k in 0:groups[id==j, member_offsets])
    {
    memberlink2 <- paste(memberlink, k, sep='')
    temp <- data.table(fromJSON(memberlink2, flatten=TRUE)[[1]])
    if (length(temp)!=0) 
      {
      temp[, group:=j]
      members <- rbind(members, temp[, .(id, name, link, joined, visited, group)], fill= TRUE)
      }
    }
  print(paste(k,'members in',j,'group'))
  }

## Let's check the column names
names(members)

## Let's save the results. The folder should be set according to the current local machine. 
write.csv(members, file='./NetworkScience/project/members.csv', sep=';') 

from_group <- members[, .(id, name, group)]
from_group <- from_group[order(rank(id),rank(group))]
to_group <- from_group[, .(id, group)]
setnames(from_group, 'group', 'SOURCE')
setnames(to_group, 'group', 'TARGET')

conn_raw <-    sqldf("SELECT DISTINCT  
                              CASE WHEN f.SOURCE < t.TARGET THEN f.SOURCE ELSE t.TARGET END AS SOURCE
                             ,CASE WHEN f.SOURCE > t.TARGET THEN f.SOURCE ELSE t.TARGET END AS TARGET
                             ,f.id AS LINK_ID
                             ,f.name as LINK_LABEL
                      FROM    from_group f
                              INNER JOIN to_group t ON t.id=f.id AND f.SOURCE!=t.TARGET")

setnames(groups, 'category.id', 'category_id')
setnames(groups, 'category.name', 'category_name')

conn_each <-   sqldf("SELECT  c.SOURCE
                             ,c.TARGET
                             ,g1.name AS SOURCE_NAME
                             ,g1.category_id AS SOURCE_CAT_ID
                             ,g1.category_name AS SOURCE_CAT_NAME
                             ,g2.name AS TARGET_NAME
                             ,g2.category_id AS TARGET_CAT_ID
                             ,g2.category_name AS TARGET_CAT_NAME
                             ,c.LINK_ID
                             ,c.LINK_LABEL
                      FROM    conn_raw c
                              LEFT JOIN groups g1 ON g1.id=c.SOURCE 
                              LEFT JOIN groups g2 ON g2.id=c.TARGET")
write.csv(conn_each, file='./NetworkScience/project/conn_each.csv', sep=';') 

conn_dens_in <-sqldf("SELECT  c.SOURCE
                             ,c.TARGET
                             ,c.SOURCE_NAME
                             ,c.SOURCE_CAT_ID
                             ,c.SOURCE_CAT_NAME
                             ,c.TARGET_NAME
                             ,c.TARGET_CAT_ID
                             ,c.TARGET_CAT_NAME
                             ,COUNT(c.LINK_ID) AS WEIGHTS
                      FROM    conn_each c
                      GROUP BY c.SOURCE
                              ,c.TARGET")

conn_full <-      sqldf("SELECT g.ID AS SOURCE
                               ,c.TARGET
                               ,g.name AS SOURCE_NAME
                               ,g.category_id AS SOURCE_CAT_ID
                               ,g.category_name AS SOURCE_CAT_NAME
                               ,c.TARGET_NAME
                               ,c.TARGET_CAT_ID
                               ,c.TARGET_CAT_NAME
                               ,c.WEIGHTS  
                        FROM    groups g
                                LEFT JOIN conn_dens_in c ON c.SOURCE=g.ID")

write.csv(conn_dens_full, file='./NetworkScience/project/conn_full.csv', sep=';') 


##prob <- fromJSON(paste('https://api.meetup.com/newtech-42/events/408089/attendance?', Key, '&sign=true&format=json&offset=0', sep = '')
