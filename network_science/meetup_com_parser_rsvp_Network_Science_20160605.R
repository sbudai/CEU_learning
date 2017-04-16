require(jsonlite)
require(data.table)
require(sqldf)
require(curl)
require(utils)
require(rio)

creds <- import('creds.csv')
Key <- creds[ProjectTitle == 'meetup_com' & AppName == 'network_science', Key]

## The meetup.com API gives back maximum 200 rows per request, thats why we need to know, how many request shall we do.
## This kind of metadata can be requested with V2 type of API
Offsets <- ceiling(fromJSON(paste('https://api.meetup.com/2/groups?key=', Key, '&sign=true&lat=47.51&lon=19.08', sep = ''))$meta$total_count/200)-1
## The V3 type API gives back more solvable format than the V2 type one. Therefore we use V3 type one.
linkchunk <- paste('https://api.meetup.com/find/groups?key=', Key, '&sign=true&lat=47.51&lon=19.08&format=json&order=members&desc=false&offset=', sep = '')

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
groups[, description:=NULL]
groups[, label:=name]
## Let's create a max member offset counter for each row
groups[, member_offsets:=ceiling(members/200)-1]
## Let's check the column names
names(groups)
groups <- groups[, .(id, label, link, city, join_mode, visibility, lat, lon, category.id, category.name, category.shortname, category.sort_name, member_offsets)]
## Let's save the results. The folder should be set according to the current local machine. 
write.table(groups, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/groups.csv', sep=',') 

## Let's create an empty data.table
members <- data.table(fromJSON('https://api.meetup.com/2/members?key=', Key, '&sign=true&format=json&order=name&group_id=408089&offset=0', flatten=TRUE)[[1]])
members[, group:='99999999']
members <- members[, .(id, name, link, joined, visited, group)]
members <- members[FALSE,]
## Let's download all the members from all groups
for (j in groups[,id])
    {
    memberlink <- paste('https://api.meetup.com/2/members?key=', Key, '&sign=true&format=json&order=name&group_id=', j, '&offset=', sep='')
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
    print(paste(k+1,'member offsets in',j,'group'))
    }

## Let's check the column names
names(members)
## There is a column which contains list and unnecesarry. Therefore we drop that column.

## Let's save the results. The folder should be set according to the current local machine. 
write.table(members, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/members.csv', sep=',') 

from_group <- members[, .(id, label, group)]
from_group <- from_group[order(rank(id),rank(group))]
to_group <- from_group[, .(id, group)]
setnames(from_group, 'group', 'SOURCE')
setnames(to_group, 'group', 'TARGET')
setnames(groups, 'category.id', 'category_id')
setnames(groups, 'category.name', 'category_name')



################################################################################################################


## Let's download all the events for all groups
events <- data.table(fromJSON('https://api.meetup.com/2/events?key=', Key, '&sign=true&format=json&limited_events=True&status=past&order=time&desc=false&group_id=408089', flatten=TRUE)[[1]])
events <- events[, event_offsets:=0]
events <- events[FALSE, .(group.id, id, event_offsets)]
for (l in groups[,id])
    {
    eventlink <- paste('https://api.meetup.com/2/events?key=', Key, '&sign=true&format=json&limited_events=True&status=past&order=time&desc=false&group_id=', l, sep='')
    temp <- data.table(fromJSON(eventlink, flatten=TRUE)[[1]])
    if (length(temp)!=0) 
        {
        temp <- temp[, event_offsets:=ceiling(yes_rsvp_count/200)-1]
        events <- rbind(events, temp[, .(group.id, id, event_offsets)], fill= TRUE)
        print(l)
        }
    }
events <- events[event_offsets >= 0, ]
events <- events[order(group.id, id)]
write.table(events, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/events.csv', sep=',') 

## Let's download all the "yes" rsvps for all groups' all events
rsvps <- data.table(fromJSON(paste('https://api.meetup.com/2/rsvps?offset=0&key=', Key, '&order=name&event_id=229648983', sep = ''), flatten=TRUE)[[1]])
rsvps <- rsvps[FALSE, .(rsvp_id, member.member_id, event.id, group.id)]
print('Let us collect all the yes rsvp-s for all these events:')
for (m in events[, id])
{
  for (n in events[id==m, event_offsets])
  {
    rsvplink <- paste('https://api.meetup.com/2/rsvps?offset=', n, '&key=', Key, '&order=name&event_id=', m, sep='')
    temp <- data.table(fromJSON(rsvplink, flatten=TRUE)[[1]])
    ##temp <- temp[response=='yes', .(rsvp_id, member.member_id, event.id, group.id)]
    if (length(temp)!=0) 
    {
      temp <- temp[rsvp_id!=-1 & response=='yes', .(rsvp_id, member.member_id, event.id, group.id)]
      rsvps <- rbind(rsvps, temp, fill= TRUE)
      ## without this print command it gives back 502 error, because of the frequent requests
      print(paste(m, n+1, sep=' - '))
    }
  }
}
setnames(rsvps, 'member.member_id','member_id')
setnames(rsvps, 'group.id','group_id')
setnames(rsvps, 'event.id','event_id')
setnames(rsvps, 'rsvp_id','rsvp_id')
write.table(rsvps, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/rsvps.csv', sep=',') 

members2 <- data.table(unique(members[, .(id, name)]))
rsvps2 <-   sqldf("SELECT  r.member_id 
                          ,m.name 
                          ,r.group_id 
                          ,COUNT(DISTINCT r.rsvp_id) AS evt
                   FROM    rsvps r 
                           INNER JOIN members2 m ON r.member_id=m.id
                   GROUP BY r.member_id ,m.name, r.group_id
                   ORDER BY r.member_id ,m.name, r.group_id")

write.table(rsvps2, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/rsvps2.csv', sep=',') 

from_group_rsvp <- data.table(rsvps2)
to_group_rsvp <- from_group_rsvp[, .(member_id, group_id, evt)]
setnames(from_group_rsvp, 'group_id', 'SOURCE')
setnames(to_group_rsvp, 'group_id', 'TARGET')

## This about how all the active members of the community belong to "from" and "to" groups
conn_raw_rsvp <- sqldf("SELECT DISTINCT  
                                 f.SOURCE
                                ,t.TARGET
                                ,f.member_id AS LINK_ID
                                ,f.label AS LABEL
                                ,CASE WHEN f.evt<t.evt THEN f.evt ELSE t.evt END AS WEIGHT
                        FROM    from_group_rsvp f
                                INNER JOIN to_group_rsvp t ON t.member_id=f.member_id 
                                                           AND f.SOURCE<t.TARGET
                        ORDER BY  f.member_id
                                 ,f.SOURCE
                                 ,t.TARGET")

write.table(conn_raw_rsvp, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/conn_raw_rsvp.csv', sep=',') 

conn_each_rsvp <- sqldf("SELECT   c.SOURCE
                                 ,c.TARGET
                                 ,g1.label AS SOURCE_NAME
                                 ,g1.category_id AS SOURCE_CAT_ID
                                 ,g1.category_name AS SOURCE_CAT_NAME
                                 ,g2.label AS TARGET_NAME
                                 ,g2.category_id AS TARGET_CAT_ID
                                 ,g2.category_name AS TARGET_CAT_NAME
                                 ,c.LINK_ID
                                 ,c.LABEL
                                 ,c.WEIGHT
                        FROM    conn_raw_rsvp c
                                LEFT JOIN groups g1 ON g1.id=c.SOURCE 
                                LEFT JOIN groups g2 ON g2.id=c.TARGET")
write.table(conn_each_rsvp, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/conn_each_rsvp.csv', sep=',') 

conn_dens_in_rsvp <-sqldf(" SELECT   c.SOURCE
                                    ,c.TARGET
                                    ,c.SOURCE_NAME
                                    ,c.SOURCE_CAT_ID
                                    ,c.SOURCE_CAT_NAME
                                    ,c.TARGET_NAME
                                    ,c.TARGET_CAT_ID
                                    ,c.TARGET_CAT_NAME
                                    ,SUM(c.WEIGHT) AS WEIGHTS
                            FROM    conn_each_rsvp c
                            GROUP BY c.SOURCE
                                    ,c.TARGET
                                    ,c.SOURCE_NAME
                                    ,c.SOURCE_CAT_ID
                                    ,c.SOURCE_CAT_NAME
                                    ,c.TARGET_NAME
                                    ,c.TARGET_CAT_ID
                                    ,c.TARGET_CAT_NAME
                            ORDER BY c.SOURCE
                                    ,c.TARGET
                                    ,c.SOURCE_NAME
                                    ,c.SOURCE_CAT_ID
                                    ,c.SOURCE_CAT_NAME
                                    ,c.TARGET_NAME
                                    ,c.TARGET_CAT_ID
                                    ,c.TARGET_CAT_NAME
                                    ,SUM(c.WEIGHT) DESC")
write.table(conn_dens_in_rsvp, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/conn_dens_in_rsvp.csv', sep=',') 


conn_dens_full_rsvp <- sqldf("SELECT  c.SOURCE
                                     ,c.TARGET
                                     ,'Undirected' AS TYPE
                                     ,'' AS ID
                                     ,'' AS LABEL
                                     ,'' AS INTERVAL
                                     ,c.WEIGHTS AS WEIGHT                       
                                     ,g.label AS SOURCE_NAME
                                     ,g.category_id AS SOURCE_CAT_ID
                                     ,g.category_name AS SOURCE_CAT_NAME
                                     ,c.TARGET_NAME
                                     ,c.TARGET_CAT_ID
                                     ,c.TARGET_CAT_NAME
                             FROM    conn_dens_in_rsvp c
                                     INNER JOIN groups g ON g.ID=c.SOURCE
                             ORDER BY  c.WEIGHTS DESC
                                      ,c.SOURCE
                                      ,c.TARGET")

write.table(conn_dens_full_rsvp, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/edges2.csv', sep=',') 


rsvps2 <- read.table(header = TRUE, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/rsvps2.csv', sep=',') 
groups <- read.csv(header = TRUE, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/groups.csv', sep=',') 
setnames(groups, 'category.id','category_id')
setnames(groups, 'category.name','category_name')
setnames(groups, 'category.shortname','category_shortname')
setnames(groups, 'category.sort_name','category_sort_name')

## Let's check out how many active members do the groups have.
groups2 <-   sqldf("SELECT  g.id
                           ,g.label
                           ,g.link
                           ,g.city
                           ,g.join_mode
                           ,g.visibility
                           ,g.lat
                           ,g.lon
                           ,g.category_id
                           ,g.category_name
                           ,g.category_shortname
                           ,g.category_sort_name
                           ,COUNT(DISTINCT r.member_id) AS active_membership
                   FROM     groups g 
                            LEFT JOIN rsvps2 r ON r.group_id=g.id
                   GROUP BY g.id
                           ,g.label
                           ,g.link
                           ,g.city
                           ,g.join_mode
                           ,g.visibility
                           ,g.lat
                           ,g.lon
                           ,g.category_id
                           ,g.category_name
                           ,g.category_shortname
                           ,g.category_sort_name
                   ORDER BY g.id
                           ,g.label
                           ,g.link
                           ,g.city
                           ,g.join_mode
                           ,g.visibility
                           ,g.lat
                           ,g.lon
                           ,g.category_id
                           ,g.category_name
                           ,g.category_shortname
                           ,g.category_sort_name")

write.table(groups2, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/groups2.csv', sep=',') 


rsvps2 <- read.csv(header = TRUE, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/rsvps2.csv', sep=',') 
groups2 <- read.csv(header = TRUE, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/groups2.csv', sep=',')
groups3 <-   sqldf("SELECT g.id
                          ,g.label
                          ,g.link
                          ,g.city
                          ,g.join_mode
                          ,g.visibility
                          ,g.lat
                          ,g.lon
                          ,g.category_name
                          ,g.category_shortname
                          ,g.category_sort_name
                          ,g.active_membership
                          ,SUM(r.evt) AS visits_nr
                          ,SUM(r.evt)*1.00 /g.active_membership AS average_visits_per_member
                   FROM    groups2 g 
                           LEFT JOIN rsvps2 r ON r.group_id=g.id
                   GROUP BY g.id
                          ,g.label
                          ,g.link
                          ,g.city
                          ,g.join_mode
                          ,g.visibility
                          ,g.lat
                          ,g.lon
                          ,g.category_name
                          ,g.category_shortname
                          ,g.category_sort_name
                          ,g.active_membership 
                   ORDER BY g.id")
write.table(groups3, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/groups3.csv', sep=',') 

edges3 <- sqldf("SELECT  e.SOURCE
                        ,e.TARGET
                        ,e.TYPE
                        ,e.ID
                        ,e.LABEL
                        ,e.INTERVAL
                        ,e.WEIGHT
                        ,CASE WHEN g1.visits_nr<g2.visits_nr THEN g1.visits_nr ELSE g2.visits_nr END AS VISITS_NR 
                        ,e.WEIGHT*1.00/(CASE WHEN g1.visits_nr<g2.visits_nr THEN g1.visits_nr ELSE g2.visits_nr END) AS REL_CONN_STRENGTH
                        ,e.SOURCE_NAME
                        ,e.SOURCE_CAT_ID
                        ,e.SOURCE_CAT_NAME
                        ,e.TARGET_NAME
                        ,e.TARGET_CAT_ID
                        ,e.TARGET_CAT_NAME
                 FROM    edges2 e
                         LEFT JOIN groups3 g1 ON g1.id=e.SOURCE
                         LEFT JOIN groups3 g2 ON g2.id=e.TARGET")

write.table(edges3, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/edges3.csv', sep=',') 
        


## Először megmutatjuk a teljes sokaságot
## Csak azokat vesszük figyelembe, akik rendelkeznek aktív tagokkal (ergo volt legalább egy seményük, amire valaki el is ment)
## Kiszűrjük a medián alatti éleket
## Minden egyes groupra kiszámolni, hogy az eseményeiken mennyien vettek részt, majd felszummázni group szintre (részvétel)
## Minden egyes edge-hez kell a kisebbik össz részvételhez tartozó értékkel el kell osztani >> relatív mutató két meetup közötti kapcsolat erősségére



members <- setDT(read.csv(header = TRUE, file='/home/sbudai/Dokumentumok/CEU/NetworkScience/project/csv/members.csv', sep=','))
members <- unique(members[, .(id, name, group)])
setnames(members,'group','gr')

s1 <- data.table(unique(members[, .(id, name)]))
## number of unique members: 27463

s2 <- sqldf("SELECT  m1.gr AS SOURCE
                    ,m2.gr AS TARGET 
                    ,COUNT(DISTINCT m1.id) AS WEIGTH
             FROM    members m1
                     INNER JOIN members m2 ON m2.id=m1.id AND m2.gr>m1.gr
             GROUP BY  m1.gr
                      ,m2.gr
             ORDER BY  m1.gr
                      ,m2.gr")
## 55280 edge based on at least one mutual membership out of 72390 possible ones

s3 <- sqldf("SELECT SUM(WEIGTH) FROM s2")
## 879262 is the sum of mutual memberships
