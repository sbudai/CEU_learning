## *** https://github.com/trendct/walkthroughs/blob/master/0315-meetup-analysis/meetups-analysis.R ***
## Meetup.com
## http://www.meetup.com/meetup_api/apps/

require(data.table)
require(sqldf)
require(rio)
require(ggplot2)
require(igraph)
require(rworldmap)
require(ggmap)
require(maps)   
require(mapdata) 
require(network)
require(diagram)  
require(plotrix)
require(geosphere)

options(StringAsFactor = FALSE)
options(datatable.verbose = TRUE)

creds <- import('creds.csv')
Key <- paste('key=', creds[ProjectTitle == 'meetup_com' & AppName == 'network_science', Key], sep = '')

getwd()
setwd('./CEU_final_project')
getwd()

## Let's import the ecc file (http://www.countrycallingcodes.com/iso-country-codes/europe-codes.php) ##
ecc <- read.csv(file = 'background_infos/European_country_codes.csv', header = TRUE, sep = ';')
setDT(ecc)
organizer.member_id

## Let's import the techgroups_raw file
# techgroups_raw <- as.data.table(import('techgroups_raw.RData'))


## Let's import the techgroups file
techgroups <- as.data.table(import('techgroups.RData'))

# Let's see the evolutions of tech meetups through years by countries
techgroups <- techgroups[order(year, country)]
sample <- copy(techgroups[1, country, year])
sample[, yielded_groups := 0]
yg_country <-copy(sample)
for (c in techgroups[, unique(country)]) {
  x <- 0
  for (y in (2002:2016)) {
    temp <- copy(sample)
    if (is.na(techgroups[country == c & year == y, .N]) == FALSE) {
      x<-x+techgroups[country == c & year == y, .N]
      temp[1, country := c]
      temp[1, year := y]
      temp[1, yielded_groups := x]
      yg_country <- rbind(yg_country, temp[1, ])
    }
  }
}

# Let's delete the empty rows
yg_country <- yg_country[yielded_groups != 0, ]

## Let's save the yg_city file
export(yg_country, 'yg_country.RData')

# yg_country <- as.data.table(import('yg_country.RData'))

# Let's plot the result
ggplot(data = yg_country, 
       aes(x = year, 
           y = country)) + 
  geom_point(data = yg_country, 
             aes(color = yg_country[, as.factor(country)], 
                 size = yielded_groups),
             shape = 1) +
  guides(colour = FALSE) +
  scale_size(range = c(1, 10), 
             guide = guide_legend(title = '# tech groups', 
                                  keywidth = 10, 
                                  keyheight = 40, 
                                  default.unit = 'point')) +
  ggtitle('Evolution by countries') 


## Let's create map on which tech meetup cities will be shown sized by number of groups
# First of all let's count the yielded number of techgroups for each city
techgroups <- techgroups[order(year, country, city)]
setkey(techgroups, year)
sample <- copy(techgroups[1, .(year, country, city)])
sample[, yielded_groups := 0]  
yg_city <-copy(sample)
for (s in techgroups[, unique(country)]) {
  for (c in techgroups[country == s, unique(city)]) {
    x <- 0
    for (y in (2002:2016)) {
      temp <- copy(sample)
      if (is.na(techgroups[year == y & country == s & city == c, .N]) == FALSE) {
        x <- x+techgroups[year == y & country == s & city == c, .N]
        temp[1, year := y]
        temp[1, country := s]
        temp[1, city := c]
        temp[1, yielded_groups := x]
        yg_city <- rbind(yg_city, temp[1, ])
      }
    }
  }
}
# Let's delete the empty rows
yg_city <- yg_city[yielded_groups != 0, ]

# # Let's check the data integrity
# temp <- unique(techgroups[, .(city, lon, lat), by = city])
# temp2 <- temp[, .N, by = city]
# temp2 <- temp2[N>1]
# setkey(temp, city)
# setkey(temp2, city)
# temp2 <- temp[temp2, nomatch=NA]
# temp2[, city.1 := NULL]
# temp2[, avg_lon := mean(lon), by = city]
# temp2[, avg_lat := mean(lat), by = city]
# temp2[, dif_lon := round((lon-avg_lon)*1000)/1000, by = city]
# temp2[, dif_lat := round((lat-avg_lat)*1000)/1000, by = city]
# temp3 <- temp2[dif_lon>0.05 | dif_lat>0.05, ]
# # Arona & Brest cities should be investigated
# techgroups[city == 'Arona' | city == 'Brest', .(country, city, lon, lat)]
# # Both of them are existing in 2 different countries

# Let's normalize the lon and lat for each city
lonlat <- unique(techgroups[, .(country, lon, lat), by = city])
lonlat[, lon := mean(lon), by = .(country, city)]
lonlat[, lat := mean(lat), by = .(country, city)]
lonlat <- unique(lonlat)
# Let's inner join the two
setkey(lonlat, country, city)
setkey(yg_city, country, city)
yg_city <- yg_city[lonlat, nomatch = 0]
setDT(yg_city)
## Let's save the yg_city file
export(yg_city, 'yg_city.RData')
# yg_city <- as.data.table(import('yg_city.RData'))

# Let's create files by year
Europe_gmap <- get_map(location='Europe', zoom=4)
yg_city <- yg_city[order(year, country, city)]
for (y in (2002:2016)) {
  name <- paste('yg_city', y, sep = '_')
  name_gmap <- paste('yg_city_gmap', y, sep = '_')
  temp <- data.frame(yg_city[year == y & yielded_groups>0, ])
  mapPoints <- ggmap(Europe_gmap) + 
                geom_point(aes(x = temp$lon, 
                               y = temp$lat), 
                           data = temp, 
                           alpha = 0.9, 
                           color = 'red', 
                           shape = 1, 
                           size = temp$yielded_groups/50) +
                ggtitle(paste('Location and number of tech meetup groups in', y, sep = ' '))
  print(mapPoints)
  
  ## Let's save mapPoints file
  ggsave(paste('/pictures/yg_city_gmap_', y, '.png', sep = ''), 
         plot = last_plot(), 
         device = 'png', 
         scale = 2, 
         dpi = 300, 
         limitsize = TRUE)
  assign(name, temp)
  temp_gmap <- mapPoints
  assign(name_gmap, temp_gmap)
}

# #################################
# Europe_map <- getMap(resolution='low')
# plot(Europe_map, xlim = c(-20, 59), ylim = c(35, 71), asp = 1) +
#   title('Location and number of tech meetup groups in 2016') +
#   points(yg_city_2016$lon, 
#          yg_city_2016$lat, 
#          col = 'red', 
#          pch = 1, 
#          cex = yg_city_2016$yielded_groups/120)
# #################################
# Europe_gmap <- get_map(location = 'Europe', zoom = 4)
# mapPoints <- ggmap(Europe_gmap) + 
#   geom_point(aes(x = yg_city_2016$lon, 
#                  y = yg_city_2016$lat), 
#              data = data.frame(yg_city_2016), 
#              alpha = 0.9, color = 'red', 
#              shape = 1, 
#              size = yg_city_2016$yielded_groups/50) +
#   ggtitle('Location and number of tech meetup groups in 2016') 
# yg_city_2016_gmap <- mapPoints
# #################################









# Let's see the evolutions of tech meetups through years
techgroups <- techgroups[order(year, country)]
sample <- copy(techgroups[1, country, year])
setnames(sample, 'country', 'continent')
sample[, continent := 'Europe']
sample[, yielded_groups := 0]
ygac <-copy(sample)
x <- 0
for (y in (techgroups[, unique(year)])) {
  temp <- copy(sample)
  if (is.na(techgroups[year == y, .N]) == FALSE) {
    x<-x+techgroups[year == y, .N]
    temp[1, year := y]
    temp[1, continent := 'Europe']
    temp[1, yielded_groups := x]
    ygac <- rbind(ygac, temp[1, ])
  }
}



final <- ygac[year == 2016, yielded_groups]
for (i in (2002:2016)) {
  ygac[year == i, ratio := 1-(yielded_groups/final)]
}
ygac[year == i, ratio := yielded_groups/ratio]
# Let's plot the result
qplot(x=year, y=continent, data=ygac, size=yielded_groups)



# techgroups <- as.data.table(import('techgroups.RData'))
## Let's extract unique nodes
topic_nodes <- techgroups[, .(id, name, urlname)]
setkey(topic_nodes, id)
topic_nodes <- unique(topic_nodes)
## Let's save the topic_nodes file
export(topic_nodes, 'topic_nodes.RData')
# topic_nodes <- as.data.table(import('topic_nodes.RData'))


## Let's import the group_id-topics mapping table
techgroups_topics <- as.data.table(import('techgroups_topics.RData'))


### Let's extract edges
## Let's create the from_topic file
from_topic <- techgroups_topics[, .(topic_id, topic_name, topic_urlkey, id, name)]
setnames(from_topic, 'id', 'group_id')
setnames(from_topic, 'name', 'group_name')
from_topic <- from_topic[order(rank(topic_id),rank(group_id))]
## Let's save the from_group file
export(from_topic, 'from_topic.RData')
# from_topic <- as.data.table(import('from_topic.RData'))

## Let's create the to_group file
to_topic <- from_topic[, .(topic_id, group_id, group_name)]
## Let's save the to_group file
export(to_topic, 'tp_topic.RData')
# to_topic <- as.data.table(import('to_topic.RData'))

## This about how all the groups belong to "from" and "to" topics
topic_edges_raw <-    sqldf("SELECT DISTINCT  
                                    f.topic_id AS SOURCE_topic_ID
                                   ,t.topic_id AS TARGET_topic_ID
                                   ,f.group_id AS LINK_ID
                                   ,f.group_name AS LABEL
                                   ,1 AS WEIGHT
                       FROM    from_topic f
                               INNER JOIN to_topic t ON f.group_id=t.group_id 
                                                     AND f.topic_id<t.topic_id
                       ORDER BY f.topic_id
                               ,t.topic_id
                               ,f.group_name")
setDT(topic_edges_raw)
## Let's save the topic_edges_raw file
export(topic_edges_raw, 'topic_edges_raw.RData')
# topic_edges_raw <- as.data.table(import('topic_edges_raw.RData'))

## Let's sum up the WEIGHTs
topic_edges <- topic_edges_raw[, sum(WEIGHT), by = .(SOURCE_topic_ID, TARGET_topic_ID)]
setDT(topic_edges)
setnames(topic_edges, 'V1', 'WEIGHT')
## Let's attach topic names
unique_topic_list <- as.data.table(import('unique_topic_list.RData'))
setkey(unique_topic_list, topic_id)
setkey(topic_edges, SOURCE_topic_ID)
topic_edges <- topic_edges[unique_topic_list, nomatch = 0]
setnames(topic_edges, 'topic_name', 'SOURCE_topic_NAME')
setnames(topic_edges, 'N', 'SOURCE_topic_N')
topic_edges[, topic_urlkey := NULL]
setkey(unique_topic_list, topic_id)
setkey(topic_edges, TARGET_topic_ID)
topic_edges <- topic_edges[unique_topic_list, nomatch = 0]
setnames(topic_edges, 'topic_name', 'TARGET_topic_NAME')
setnames(topic_edges, 'N', 'TARGET_topic_N')
topic_edges[, topic_urlkey := NULL]
## Let's order by WEIGHT descending
topic_edges <- topic_edges[order(-WEIGHT)]
## Let's save the topic_edges file
export(topic_edges, 'topic_edges.RData')
write.table(topic_edges, file = 'Gephi/topic_edges.csv', sep = ',', row.names = FALSE) 
# topic_edges <- as.data.table(import('topic_edges.RData'))


## Let's create a gephi_topic_nodes file
gephi_topic_nodes <- unique_topic_list[order(-N)]
## Let's save the gephi_topics_nodes file
write.table(gephi_topic_nodes, file = 'Gephi/gephi_topic_nodes.csv', sep = ',', row.names = FALSE)
export(gephi_topic_nodes, 'gephi_topic_nodes.RData')
# gephi_topic_nodes <- as.data.table(import('gephi_topic_nodes.RData'))

## Let's harmonize topic_edges with Gephi
gephi_topic_edges <- topic_edges[order(-WEIGHT, SOURCE_TOPIC_ID, TARGET_TOPIC_ID)]
gephi_topic_edges[, TYPE := 'Undirected']
gephi_topic_edges[, LABEL := '']
gephi_topic_edges[, SOURCE_TOPIC_N := NULL]
gephi_topic_edges[, TARGET_TOPIC_N := NULL]
setnames(gephi_topic_edges, 'SOURCE_TOPIC_ID', 'SOURCE')
setnames(gephi_topic_edges, 'TARGET_TOPIC_ID', 'TARGET')
# Let's save the gephi_topic_edges file
export(gephi_topic_edges,'gephi_topic_edges.RData')
write.table(gephi_topic_edges, file = 'Gephi/gephi_topic_edges.csv', sep = ',', row.names = FALSE) 
# gephi_topic_edges <- as.data.table(import('gephi_topic_edges.RData'))

## Let's define communities (leading.eigenvector.community)
G <- graph_from_data_frame(d=gephi_topic_edges, vertices=gephi_topic_nodes, directed=F)
# E(G)            # The edges of the G object
# V(G)            # The nodes of the G object
# E(G)$WEIGHT     # Edge attribute G
# V(G)$topic_name # Node attribute topic
## leading.eigenvector.community
lec <- leading.eigenvector.community(G)
# str(lec)
# lec$merges
# merges(lec)
# lec$membership
# membership(lec)
# lec$options
# print(options(lec))
# lec$modularity
# modularity(lec)
# length(lec)
# str(lec[1])
# str(lec[2])
# str(lec[3])
# str(lec[4])
# plot(lec, G)
temp <- list(topics=V(G), community=array(lec$membership), stringsAsFactors=FALSE)
temp <- NULL
temp <- data.table(cbind(temp, V(G)$name, array(lec$membership)))
temp[, V1 := as.integer(V1)]
setnames(temp, 'V2', 'domain')
setkey(temp, V1)
gephi_topic_nodes2 <- gephi_topic_nodes
setkey(gephi_topic_nodes2, topic_id)
gephi_topic_nodes2 <- gephi_topic_nodes2[temp, nomatch = 0]
gephi_topic_nodes2 <- gephi_topic_nodes2[order(domain, -N)]
gephi_topic_nodes2[, topic_urlkey := NULL]
setnames(gephi_topic_nodes2, 'topic_id', 'Id')
setnames(gephi_topic_nodes2, 'topic_name', 'Label')
gephi_topic_nodes2[, domain := as.integer(domain)]
gephi_topic_nodes2[, domain_name := as.character('not known yet')]
## Let's save the Gephi compatible gephi_topic_nodes2.csv
write.table(gephi_topic_nodes2, file = 'Gephi/gephi_topic_nodes2.csv', sep = ',', row.names = FALSE) 
export(gephi_topic_nodes2,'gephi_topic_nodes2.RData')
# gephi_topic_nodes2 <- as.data.table(import('gephi_topic_nodes2.RData'))

# Let's pick the first 20 from each domain
gephi_topic_nodes3 <- gephi_topic_nodes2[, head(.SD, 30), by = domain]
# Let's set domain names with filtering and eyeballing
gephi_topic_nodes3[, .N, by = domain]
gephi_topic_nodes3[, ctr := .N, by = domain]
gephi_topic_nodes3[ctr<30, domain := 8]
gephi_topic_nodes3[ctr<30, domain_name := 'misc']
gephi_topic_nodes3[, ctr := NULL]
gephi_topic_nodes3 <- gephi_topic_nodes3[order(domain, -N)]
gephi_topic_nodes3[domain == 1, domain_name := 'Data & Coding']
gephi_topic_nodes3[domain == 5, domain_name := 'Applied Innovations']
gephi_topic_nodes3[domain == 6, domain_name := 'WebDesign']
gephi_topic_nodes3[domain == 7, domain_name := 'Business & Operations']
gephi_topic_nodes3 <- gephi_topic_nodes3[order(domain, -N)]
## Let's save the Gephi compatible gephi_topic_nodes3.csv
write.table(gephi_topic_nodes3, file = 'Gephi/gephi_topic_nodes3.csv', sep = ',', row.names = FALSE) 
export(gephi_topic_nodes3,'gephi_topic_nodes3.RData')
# gephi_topic_nodes3 <- as.data.table(import('gephi_topic_nodes3.RData'))

topic_lst <- gephi_topic_nodes2[, head(.SD, 10), by = domain]
topic_lst <- topic_lst[,list(SUM=sum(N)), by = list(domain_name, Label)]

topic_lst1 <- topic_lst[domain_name == 'Data & Coding', ]
topic_lst2 <- topic_lst[domain_name == 'Applied Innovations', ]
topic_lst3 <- topic_lst[domain_name == 'WebDesign', ]
topic_lst4 <- topic_lst[domain_name == 'Business & Operations', ]


tt <- ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
tbl1 <- tableGrob(topic_lst1, rows=NULL, theme=tt)
tbl2 <- tableGrob(topic_lst2, rows=NULL, theme=tt)
tbl3 <- tableGrob(topic_lst3, rows=NULL, theme=tt)
tbl4 <- tableGrob(topic_lst4, rows=NULL, theme=tt)

grid.arrange(tbl1,
             tbl2,
             tbl3,
             tbl4, clip = "off",
             ncol=2, 
             nrow=2,
             name='arrange',
             as.table=TRUE)


# Let's write back the domains into gephi_topic_nodes2
gephi_topic_nodes2 <- gephi_topic_nodes2[order(domain, -N)]
gephi_topic_nodes2[domain == 1, domain_name := 'Data & Coding']
gephi_topic_nodes2[domain == 5, domain_name := 'Applied Innovations']
gephi_topic_nodes2[domain == 6, domain_name := 'WebDesign']
gephi_topic_nodes2[domain == 7, domain_name := 'Business & Operations']
gephi_topic_nodes2[domain>1 & domain<5, domain := 8]
gephi_topic_nodes2[domain == 8, domain_name := 'misc']
gephi_topic_nodes2 <- gephi_topic_nodes2[order(domain, -N)]
## Let's save the Gephi compatible gephi_topic_nodes2.csv
write.table(gephi_topic_nodes2, file = 'Gephi/gephi_topic_nodes2.csv', sep = ',', row.names = FALSE) 
export(gephi_topic_nodes2,'gephi_topic_nodes2.RData')
# gephi_topic_nodes2 <- as.data.table(import('gephi_topic_nodes2.RData'))


setkey(gephi_topic_nodes3, Id)
setkey(gephi_topic_edges, SOURCE)
gephi_topic_edges3 <- gephi_topic_edges[gephi_topic_nodes3, nomatch=NA]
gephi_topic_edges3[, domain_name := NULL]
gephi_topic_edges3[, N := NULL]
gephi_topic_edges3[, Label := NULL]
gephi_topic_edges3[, domain := NULL]
setkey(gephi_topic_edges3, TARGET)
gephi_topic_edges3 <- gephi_topic_edges3[gephi_topic_nodes3, nomatch=NA]
gephi_topic_edges3[, domain_name := NULL]
gephi_topic_edges3[, N := NULL]
gephi_topic_edges3[, Label := NULL]
gephi_topic_edges3[, domain := NULL]
gephi_topic_edges3 <- gephi_topic_edges3[is.na(SOURCE) == FALSE, ]
gephi_topic_edges3 <- gephi_topic_edges3[is.na(TARGET) == FALSE, ]
gephi_topic_edges3 <- setkeyv(gephi_topic_edges3, c('SOURCE', 'TARGET'))
# Let's save the gephi_topic_edges3 file
export(gephi_topic_edges3,'gephi_topic_edges3.RData')
write.table(gephi_topic_edges3, file = 'Gephi/gephi_topic_edges3.csv', sep = ';', row.names = FALSE) 
# gephi_topic_edges3 <- as.data.table(import('gephi_topic_edges3.RData'))


## Let's import the sums of techgroup topics by countries (not unique!)
techgroups_topics <- as.data.table(import('techgroups_topics.RData'))
setkey(techgroups_topics, topic_id)
setkey(gephi_topic_nodes2, Id)
temp <- techgroups_topics[gephi_topic_nodes2, nomatch=NA]
temp[, Label := NULL]
setnames(temp, 'N', 'topic_N')
setnames(temp, 'domain', 'topic_domain')
setnames(temp, 'domain_name', 'topic_domain_name')
techgroups_topics <- temp
temp <- NULL
## Let's save the techgroups_topics file
export(techgroups_topics, 'techgroups_topics.RData')
write.table(techgroups_topics, file = 'background_infos/techgroups_topic.csv', sep = ';', row.names = FALSE)
## techgroups_topics <- as.data.table(import('techgroups_topics.RData'))

topics_ratings <- techgroups_topics[rating>0, mean(rating), by = .(topic_id, topic_name, topic_domain, topic_domain_name)]
topics_ratings <- topics_ratings[order(-V1, topic_name)]
## Let's save the topics_ratings file
export(topics_ratings, 'topics_ratings.RData')
write.table(topics_ratings, file = 'background_infos/topics_ratings.csv', sep = ';', row.names = FALSE)
## topics_ratings <- as.data.table(import('topics_ratings.RData'))

topic_domains_ratings <- techgroups_topics[rating>0, mean(rating), by = .(topic_domain, topic_domain_name)]
topic_domains_ratings <- topic_domains_ratings[order(-V1)]
## Let's save the topic_domains_ratings file
export(topic_domains_ratings, 'topic_domains_ratings.RData')
write.table(topic_domains_ratings, file = 'background_infos/topic_domains_ratings.csv', sep = ';', row.names = FALSE)
## topic_domains_ratings <- as.data.table(import('topic_domains_ratings.RData'))


# Let's see the evolutions of tech topic domains through years by countries
techgroups_topics <- techgroups_topics[order(year, topic_domain)]
sample <- copy(techgroups_topics[1, .(year, topic_domain, topic_domain_name)])
sample[, yielded_topic_domain := 0]
yg_topic_domain <-copy(sample)
for (t in techgroups_topics[, unique(topic_domain)]) {
  x <- 0
  for (y in (2002:2016)) {
    temp <- copy(sample)
    if (is.na(techgroups_topics[topic_domain == t & year == y, .N]) == FALSE) {
      x <- x + techgroups_topics[topic_domain == t & year == y, .N]
      td <- unique(techgroups_topics[topic_domain == t, topic_domain_name])
      temp[1, year := y]
      temp[1, topic_domain := t]
      temp[1, topic_domain_name := td]
      temp[1, yielded_topic_domain := x]
      yg_topic_domain <- rbind(yg_topic_domain, temp[1, ])
    }
  }
}
# yg_topic_domain <- as.data.table(import('yg_topic_domain.RData')
# Let's delete the empty rows
yg_topic_domain <- yg_topic_domain[yielded_topic_domain != 0, ]
## Let's save the yg_topic_domain file
write.table(yg_topic_domain, 'background_infos/yg_topic_domain.csv', sep = ';', row.names = FALSE)
export(yg_topic_domain, 'yg_topic_domain.RData')
# yg_topic_domain <- as.data.table(import('yg_topic_domain.RData'))

yg_topic_domain[, ' := ' (topic_domain_name=as.factor(topic_domain_name), topic_domain=as.factor(topic_domain))]

# Let's plot the result
p1 <- ggplot() +
      geom_line(data = yg_topic_domain, 
                aes(x = yg_topic_domain$year, 
                    y = yg_topic_domain$yielded_topic_domain, 
                    colour = yg_topic_domain$topic_domain),
                position = position_dodge(0.15), 
                size = 1) + 
      scale_color_manual(values = c('#99cc66','#ff7f50','#9966cc','#0064AB','#cccccc'),
                         name = '', ## topic domains'
                         breaks = c('1', '5', '6', '7', '8'),
                         labels = c('Data & Coding', 'Applied Innovations', 'Web Design', 'Business & Operations', 'miscellaneous'))
      ##  ggtitle('Evolution of topic domains')  +
      ##  labs(x = 'year', 
      ##       y = 'yielded mentions') 
ggdraw(switch_axis_position(p1 +
                            theme(axis.line = element_blank(),
                                  axis.ticks.length = unit(0, 'cm'),
                                  panel.border = element_blank(),
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_text(size = 0),
                                  axis.text.y = element_text(hjust = 0, size = 11),
                                  legend.position = c(.3, .25)
                                  ), axis = 'y'))

unique_organizer_list <- as.data.table(import('unique_organizer_list.RData'))
setkey(unique_organizer_list, organizer_id)
unique_members <- as.data.table(import('unique_members.RData'))
unique_members[, member_id := as.integer(member_id)]
setkey(unique_members, member_id)
unique_members <- unique_organizer_list[unique_members]
setnames(unique_members, 'organizer_id', 'member_id')
unique_members[is.na(organizer_name) == FALSE, organizer := TRUE] 
unique_members <- unique_members[, .(member_id, member_name, organizer, sum_of_organized_groups)]
# Let's save the unique_members file
export(unique_members, 'unique_members.RData')
# unique_members <- as.data.table(import('unique_members.RData'))


active_members <- members[, .N, by = .(member_id)]
active_members <- active_members[N>1, ]
active_members[, member_id := as.integer(member_id)]
setkey(active_members, member_id)
setkey(unique_members, member_id)
active_members <- active_members[unique_members, nomatch = 0]
setnames(active_members, 'N', 'sum_of_memberships')
# Let's save the active_members file
export(active_members, 'active_members.RData')
# active_members <- as.data.table(import('active_members.RData'))


# These can be filters
active_members[, .N]  ## 426177 active members (with at least 2 memberships)
active_members[organizer == TRUE, .N]  ## 7020 active members who organize also
active_members[sum_of_organized_groups>1, .N]  ## 1192 active members who organize least 2 groups also

temp <- members[, .(group_id, member_id, member_name, joined_date)]
write.table(temp[1:5, ], 'background_infos/members_structure.csv', sep = ';', row.names = FALSE)
temp[, member_id := as.integer(member_id)]
setkey(temp, member_id)
setkey(active_members, member_id)
active_members_activity <- active_members[temp, nomatch = 0]
setnames(active_members_activity, 'i.member_name', 'member_name')

temp <- active_members_activity[, .(group_id, member_id)]
temp[, group_id := as.integer(group_id)]
setkey(temp, group_id)
temptemp <- as.data.table(import('techgroups.RData'))
temptemp <- temptemp[, .(country, city, id, name)]
setkey(temptemp, id)
temp <- temptemp[temp]
setnames(temp, 'country','group_country')
setnames(temp, 'city','group_city')
setnames(temp, 'id','group_id')
setnames(temp, 'name','group_name')
temp[, member_id := as.character(member_id)]
temp2 <- temp[, .N, by = .(member_id, group_country, group_city)] 
temp2 <- temp2[N>1, ] # same country, same city, different groups 426077 members
setnames(temp2, 'N', 'nr_of_groups')
temp3 <- temp2[, .N, by = .(member_id, group_country)] 
temp3 <- temp3[N>1, ] # same country, different city, different groups 20308-140=20168 members
setnames(temp3, 'N', 'nr_of_cities')
temp4 <- temp3[, .N, by = .(member_id)] 
temp4 <- temp4[N>1, ] # different country, different city, different groups 140 members
setnames(temp4, 'N', 'nr_of_countries')
temp4[, member_id := as.integer(member_id)]
setkey(temp4, member_id)
temp[, member_id := as.integer(member_id)]
setkey(temp, member_id)
weevel_members <- temp4[temp, nomatch = 0]
weevel_members[, nr_of_countries := NULL]
techgroups <- as.data.table(import('techgroups.RData'))
techgroups <- techgroups[, .(id, lat, lon)]
setkey(weevel_members, group_id)
setkey(techgroups, id)
weevel_members <- weevel_members[techgroups, nomatch = 0]
# Let's save the weevel_members file
export(weevel_members, 'weevel_members.RData')
# weevel_members <- as.data.table(import('weevel_members.RData'))

### Let's extract edges
## Let's create the from_city file
from_city <- weevel_members[, .(group_country, group_city, lat, lon, member_id)]
setnames(from_city, 'group_country', 'country')
setnames(from_city, 'group_city', 'city')
from_city <- from_city[order(rank(city),rank(member_id))]
## Let's save the from_city file
export(from_city, 'from_city.RData')
# from_city <- as.data.table(import('from_city.RData'))

## Let's create the to_city file
to_city <- from_city[, .(country, city, lat, lon, member_id)] 
## Let's save the to_city file
export(to_city, 'tp_city.RData')
# to_city <- as.data.table(import('to_city.RData'))

## This about how all the weevel mebers belong to "from" and "to" cities
city_edges_raw <-    sqldf("SELECT DISTINCT  
                                    f.country AS source_country
                                   ,f.city AS SOURCE
                                   ,t.country AS target_country
                                   ,t.city AS TARGET
                                   ,f.member_id AS LINK_ID
                                   ,1 AS WEIGHT
                           FROM     from_city f
                                    INNER JOIN to_city t ON f.member_id=t.member_id 
                                                        AND f.city<t.city
                           ORDER BY f.country
                                   ,f.city
                                   ,t.country
                                   ,t.city
                                   ,f.member_id")
setDT(city_edges_raw)
## Let's save the city_edges_raw file
export(city_edges_raw, 'city_edges_raw.RData')
# city_edges_raw <- as.data.table(import('city_edges_raw.RData'))
## Let's sum up the WEIGHTs
city_edges <- city_edges_raw[, sum(WEIGHT), by = .(source_country, SOURCE, target_country, TARGET)]
setDT(city_edges)
setnames(city_edges, 'V1', 'WEIGHT')
## Let's order by WEIGHT descending
city_edges <- city_edges[order(-WEIGHT)]
## Let's save the city_edges file
export(city_edges, 'city_edges.RData')
write.table(city_edges, file = 'background_infos/city_edges.csv', sep = ';', row.names = FALSE) 
# city_edges <- as.data.table(import('city_edges.RData'))

## Let's create a city_nodes file
t <- weevel_members[, .(.N, lat, lon), by = .(group_country, group_city)]
t[, lat := mean(lat), by = .(group_country, group_city, N)]
t[, lon := mean(lon), by = .(group_country, group_city, N)]
city_nodes <- unique(t[, .(group_country, lat, lon, N), by = group_city])

## Let's save the city_nodes file
export(city_nodes, 'city_nodes.RData')
write.table(city_nodes, file = 'background_infos/city_nodes.csv', sep = ';', row.names = FALSE) 
# city_nodes <- as.data.table(import('city_nodes.RData'))



Europe_gmap <- get_map(location = 'Europe', zoom = 4)

temp <- data.frame(city_nodes)

mapPoints <- ggmap(Europe_gmap) + 
              geom_point(aes(x = temp$lon, 
                             y = temp$lat), 
                         data = temp, 
                         alpha = 1, 
                         color = 'red', 
                         shape = 1, 
                         size = (temp$N/30)) +
              ggtitle('Most internationally interconnected meetup locations')
              print(mapPoints)
              
# Let's save mapPoints file
ggsave('/pictures/interconnected_locations.png', 
       plot = last_plot(), 
       device = 'png', 
       scale = 2, 
       dpi = 300, 
       limitsize = TRUE)

 
# map("europe", col="orange",  border="gray10", fill=TRUE, bg="gray30")

## Let's import the sums of techgroup membership by countries (not unique!)
# country_sum_members <- as.data.table(import('country_sum_members.RData'))


## Let's import the sum up of techgroup membership by cities
# city_sum_members <- as.data.table(import('city_sum_members.RData'))

## Let's import all the group_id-member_id mapping table
# members <- as.data.table(import('members.RData'))


## Let's compute the overall measures
temp <- copy(techgroups)
temp[, topics_list := NULL]
techgroups_table_features_1 <- sqldf('SELECT AVG(CASE WHEN rating>0 THEN rating ELSE NULL END) AS AVERAGE_RATINGS
                                            ,COUNT(DISTINCT id) AS NR_OF_TECHMEETUPS
                                            ,COUNT(DISTINCT country) AS NR_OF_COUNTRIES
                                            ,COUNT(DISTINCT city) AS NR_OF_CITIES
                                            ,MIN(yearmonth) AS FIRST_CREATED_DATE
                                            ,COUNT(DISTINCT organizer_member_id) AS NR_OF_ORGANIZERS
                                            ,AVG(DISTINCT members) AS AVERAGE_NR_OF_MEMBERS
                                      FROM  temp')
setDT(techgroups_table_features_1)
## Let's save the techgroups_table_features_1 file
export(techgroups_table_features_1, 'techgroups_table_features_1.RData')
# techgroups_table_features_1 <- as.data.table(import('techgroups_table_features_1.RData'))


## Let's compute the measures of countries
techgroups_table_features_2 <- sqldf('SELECT AVG(CASE WHEN rating>0 THEN rating ELSE NULL END) AS AVERAGE_RATINGS
                                            ,COUNT(DISTINCT t.id) AS NR_OF_TECHMEETUPS
                                            ,e.region AS REGION
                                    I checked all the available labels in advance and I applied the “tech” label among them        ,e.country_name AS COUNTRY
                                            ,COUNT(DISTINCT t.city) AS NR_OF_CITIES
                                            ,MIN(t.yearmonth) AS FIRST_CREATED_DATE
                                            ,COUNT(DISTINCT t.organizer_member_id) AS NR_OF_ORGANIZERS
                                            ,ROUND(AVG(DISTINCT t.members)) AS AVERAGE_NR_OF_MEMBERS
                                      FROM  temp t
                                            INNER JOIN ecc e ON e.country = t.country
                                      GROUP BY e.region
                                              ,e.country_name
                                      ORDER BY MIN(t.yearmonth)
                                              ,e.country_name
                                              ,COUNT(DISTINCT t.id)')
setDT(techgroups_table_features_2)
## Let's save the techgroups_table_features_2 file
export(techgroups_table_features_2, 'techgroups_table_features_2.RData')
# techgroups_table_features_2 <- as.data.table(import('techgroups_table_features_2.RData'))

## Let's compute yielded measures of countries
sample <- data.table(PERIOD = as.integer('200210'),
                     REGION = as.character('nowhere'),
                     COUNTRY = as.character('AD'), 
                     Y_YEAR = as.integer('2002'),
                     Y_AVERAGE_RATINGS = as.numeric('1.0'),
                     Y_NR_OF_TECHMEETUPS = as.integer('1'), 
                     Y_NR_OF_CITIES = as.integer('1'), 
                     Y_NR_OF_ORGANIZERS = as.integer('1'), 
                     Y_NR_OF_MEMBERS = as.integer('1'))
temp <- copy(sample)
techgroups_table_features_3 <- copy(sample)
techgroups_table_features_3 <- techgroups_table_features_3[0]
period <- seq(from = as.Date('2002-10-01'), to = as.Date('2016-06-30'), by = 'months')
for (i in ecc[, country]) {
  for (j in seq_along(period)) {
    p <- as.integer(format(period[j], format = '%Y%m', tz = '', usetz = FALSE))
    y <- as.integer(format(period[j], format = '%Y', tz = '', usetz = FALSE))
    temp[1, PERIOD := p]
    temp[1, REGION := ecc[country == i, region]]
    temp[1, COUNTRY := i]
    temp[1, Y_YEAR := y]
    temp[1, Y_AVERAGE_RATINGS := techgroups[country == i & yearmonth <= p, mean(rating, na.rm=TRUE)]]
    temp[1, Y_NR_OF_TECHMEETUPS := nrow(techgroups[country == i & yearmonth <= p, .N, by = id])]  
    temp[1, Y_NR_OF_CITIES := nrow(techgroups[country == i & yearmonth <= p, .N, by = city])]
    temp[1, Y_NR_OF_ORGANIZERS := nrow(techgroups[country == i & yearmonth <= p, .N, by = organizer_member_id])]
    temp[1, Y_NR_OF_MEMBERS := techgroups[country == i & yearmonth <= p, sum(members)]]
    techgroups_table_features_3 <-rbind(techgroups_table_features_3, temp[1, ])
    }
  }
setDT(techgroups_table_features_3)
## Let's save the techgroups_table_features_2 file
export(techgroups_table_features_3, 'techgroups_table_features_3.RData')
# techgroups_table_features_3 <- as.data.table(import('techgroups_table_features_3.RData'))


## Let's create a long table of yielded number of members by country and period
techgroups_table_features_4 <- techgroups_table_features_3[, .(PERIOD, REGION, COUNTRY, Y_NR_OF_MEMBERS)]
techgroups_table_features_4 <- melt(techgroups_table_features_4, c('PERIOD', 'REGION', 'COUNTRY'), 'Y_NR_OF_MEMBERS')
## Let's found the inactive countries
temp <- techgroups_table_features_3[, sum(Y_NR_OF_MEMBERS), by = COUNTRY]
temp[V1 == 0, COUNTRY]
## Let's delete the inactive countries
techgroups_table_features_4 <- techgroups_table_features_4[COUNTRY != 'FO', ]
techgroups_table_features_4 <- techgroups_table_features_4[COUNTRY != 'IM', ]
techgroups_table_features_4 <- techgroups_table_features_4[COUNTRY != 'MC', ]
techgroups_table_features_4 <- techgroups_table_features_4[COUNTRY != 'SM', ]
techgroups_table_features_4 <- techgroups_table_features_4[COUNTRY != 'VA', ]
## Let's save the techgroups_table_features_4 file
export(techgroups_table_features_4, 'techgroups_table_features_4.RData')
# techgroups_table_features_4 <- as.data.table(import('techgroups_table_features_4.RData'))


## Let's sort countries by starting date of activity
temp <- techgroups_table_features_4[value > 0, min(PERIOD), by = c('REGION', 'COUNTRY')]
temp <- temp[order(V1, REGION, COUNTRY)]
##
ggplot(data = temp, 
       aes(x = V1, 
           y = REGION)) + 
  geom_point(aes(x = V1, 
                 color = factor(COUNTRY),
                 shape = REGION)) +
  xlab('period') + 
  ylab('regions and countries') + 
  ggtitle('starting points of European tech groups memberships') 


setkey(temp, COUNTRY)
setkey(techgroups_table_features_4, COUNTRY)
temp_2 <- techgroups_table_features_4[temp, nomatch = 0]
setkey(ecc, country)
temp_2 <- temp_2[ecc, nomatch = 0]
temp_2 <- temp_2[order(V1, COUNTRY, PERIOD)]
setnames(temp_2, 'V1', 'STARTING_POINT')
temp_2[, ROUNDED_VALUE := round(value/1000, digits = 0)*1000]
temp_2[, prop := sum(value), by = list(PERIOD)]
temp_2[, prop := value/prop]
##
ggplot(data = temp_2, 
       aes(x = PERIOD, 
           y = ROUNDED_VALUE)) + 
  geom_area(aes(color = country_name, 
                fill = country_name, 
                stat = 'identity', 
                position = 'stack', 
                show.legend = TRUE), 
            size = .1, 
            alpha = .4) +
  ylab('sum of members') + 
  ggtitle('time series of European tech groups memberships') 


## Let's create a short table of yielded number of members by country and period
techgroups_table_features_5 <- as.data.table(dcast(techgroups_table_features_4, formula = PERIOD ~ COUNTRY))

## Let's save the techgroups_table_features_5 file
export(techgroups_table_features_5, 'techgroups_table_features_5.RData')
# techgroups_table_features_5 <- as.data.table(import('techgroups_table_features_5.RData'))



  ######################################################################################


## Let's decompose all the topics 
all_members <- as.data.table(import('members.RData'))
setkey(all_members, group_id)
techgroups[, group_id := as.character(group_id)] ## or as.numeric
setnames(techgroups, 'city', 'group_city')
setnames(techgroups, 'country', 'group_country')
setkey(techgroups, group_id)
all_members <- all_members[techgroups[, .(group_id, group_country, group_city)], nomatch = 0]

# ## Decomposing topics fields into the topics table
# members_raw_misc <- NULL
# for (i in (1:nrow(members_raw))) {
#   if (length(members_raw[i, V1][1]) > 0) {
#     temp <- cbind(setDT(unlist(members_raw[i, V1], recursive = FALSE, use.names = TRUE)), group_id = members_raw[i, group_id])
#     members_raw_misc <- rbind(members_raw_misc, temp)
#   }
# }

# ## Let's convert topic list into a string
for (i in (1:nrow(members_raw))) {
  if (length(members_raw[i, V1][1]) > 0) {
    members_raw[i, V1_string := paste0(' | ', unlist(members_raw[i, V1], recursive = TRUE, use.names = TRUE), collapse = '')]
  }
}
members_raw[, V1 := NULL]
setkey(members_raw, V1_string, group_id, group_country, group_city)
members_raw <- unique(members_raw)


## Let's collapse the members_raw into members
members <- members_raw[, .(group_ids = paste(group_id, collapse=', '), group_ids_list = list(group_id)), by = .(member_id, name, country, city, lat, lon)]
## Let's save the members file
export(members, 'members.RData')
## members <- import('members.RData')



#### members <- members_tarcsi
#### members <- members[, .(group_ids = paste(group_id, collapse=', '), group_ids_list = list(group_id)), by = .(member_id, name, country, city, lat, lon)]
sqldf('SELECT AVG(members) FROM techgroups')
sqldf('SELECT COUNT(DISTINCT group_id) FROM members_raw')
sqldf('SELECT COUNT(DISTINCT member_id) FROM members_raw')

from_group <- members_raw[, .(member_id, name, group_id)]
from_group <- from_group[order(rank(member_id), rank(group_id))]
to_group <- from_group[, .(member_id, group_id)]
setnames(from_group, 'group_id', 'SOURCE')
setnames(to_group, 'group_id', 'TARGET')

################################################################################################################


## Let's download all the events for all groups
events <- data.table(fromJSON(paste('https://api.meetup.com/2/events?', Key, '&sign=true&format=json&limited_events=True&status=past&order=time&desc=false&group_id=408089', sep = ''), flatten = TRUE)[[1]])
events <- events[, event_offsets := 0]
events <- events[FALSE, .(group.id, id, event_offsets)]
for (l in groups[,id]) {
  eventlink <- paste('https://api.meetup.com/2/events?', Key, '&sign=true&format=json&limited_events=True&status=past&order=time&desc=false&group_id=', l, sep = '')
  temp <- data.table(fromJSON(eventlink, flatten = TRUE)[[1]])
  if (length(temp) != 0) {
    temp <- temp[, event_offsets := ceiling(yes_rsvp_count/200)-1]
    events <- rbind(events, temp[, .(group.id, id, event_offsets)], fill = TRUE)
    print(l)
  }
}
events <- events[event_offsets >= 0, ]
events <- events[order(group.id, id)]
write.table(events, file = 'background_infos/events.csv', sep = '|', row.names = FALSE) 


## Let's download all the "yes" rsvps for all groups' all events
rsvps <- data.table(fromJSON(paste('https://api.meetup.com/2/rsvps?offset=0&', Key, '&order=name&event_id=229648983', sep = ''), flatten = TRUE)[[1]])
rsvps <- rsvps[FALSE, .(rsvp_id, member.member_id, event.id, group.id)]
print('Let us collect all the yes rsvp-s for all these events:')
for (m in events[, id]) {
  for (n in events[id == m, event_offsets]) {
    rsvplink <- paste('https://api.meetup.com/2/rsvps?offset=', n, '&', Key, '&order=name&event_id=', m, sep = '')
    temp <- data.table(fromJSON(rsvplink, flatten=TRUE)[[1]])
    ##temp <- temp[response == 'yes', .(rsvp_id, member.member_id, event.id, group.id)]
    if (length(temp) != 0) {
      temp <- temp[rsvp_id != -1 & response == 'yes', .(rsvp_id, member.member_id, event.id, group.id)]
      rsvps <- rbind(rsvps, temp, fill = TRUE)
      ## without this print command it gives back 502 error, because of the frequent requests
      print(paste(m, n+1, sep = ' - '))
    }
  }
}
setnames(rsvps, 'member.member_id', 'member_id')
setnames(rsvps, 'group.id', 'group_id')
setnames(rsvps, 'event.id', 'event_id')
setnames(rsvps, 'rsvp_id', 'rsvp_id')

write.table(rsvps, file = 'background_infos/rsvps.csv', sep = '|', row.names = FALSE) 



members2 <- data.table(unique(members[, .(id, name)]))
rsvps2 <-   sqldf("SELECT  r.member_id 
                          ,m.name 
                          ,r.group_id 
                          ,COUNT(DISTINCT r.rsvp_id) AS evt
                  FROM    rsvps r 
                          INNER JOIN members2 m ON r.member_id=m.id
                  GROUP BY r.member_id 
                          ,m.name
                          ,r.group_id
                  ORDER BY r.member_id 
                          ,m.name
                          ,r.group_id")

write.table(rsvps2, file = 'background_infos/rsvps2.csv', sep = '|', row.names = FALSE) 

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
                       ORDER BY f.member_id
                               ,f.SOURCE
                               ,t.TARGET")

write.table(conn_raw_rsvp, file = 'background_infos/conn_raw_rsvp.csv', sep = '|', row.names = FALSE) 


conn_each_rsvp <- sqldf("SELECT  c.SOURCE
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
                        FROM  conn_raw_rsvp c
                              LEFT JOIN groups g1 ON g1.id=c.SOURCE 
                              LEFT JOIN groups g2 ON g2.id=c.TARGET")
write.table(conn_each_rsvp, file = 'background_infos/conn_each_rsvp.csv', sep = '|', row.names = FALSE) 


conn_dens_in_rsvp <-sqldf(" SELECT c.SOURCE
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
write.table(conn_dens_in_rsvp, file = 'background_infos/conn_dens_in_rsvp.csv', sep = '|', row.names = FALSE) 

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
                             FROM  conn_dens_in_rsvp c
                                   INNER JOIN groups g ON g.ID=c.SOURCE
                             ORDER BY c.WEIGHTS DESC
                                     ,c.SOURCE
                                     ,c.TARGET")

write.table(conn_dens_full_rsvp, file = 'background_infos/edges2.csv', sep = '|', row.names = FALSE) 



rsvps2 <- read.csv(header = TRUE, file = 'background_infos/rsvps2.csv', sep = '|') 
groups <- read.csv(header = TRUE, file = 'background_infos/groups.csv', sep = '|') 
setnames(groups, 'category.id', 'category_id')
setnames(groups, 'category.name', 'category_name')
setnames(groups, 'category.shortname', 'category_shortname')
setnames(groups, 'category.sort_name', 'category_sort_name')

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
                   FROM  groups g 
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

write.table(groups2, file = 'background_infos/groups2.csv', sep = '|', row.names = FALSE) 


rsvps2 <- read.csv(header = TRUE, file = 'background_infos/rsvps2.csv', sep = '|') 
groups2 <- read.csv(header = TRUE, file = 'background_infos/groups2.csv', sep = '|')
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
                   FROM   groups2 g 
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

write.table(groups3, file = 'background_infos/groups3.csv', sep = '|', row.names = FALSE) 

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
                  FROM  edges2 e
                        EFT JOIN groups3 g1 ON g1.id=e.SOURCE
                        EFT JOIN groups3 g2 ON g2.id=e.TARGET")

write.table(edges3, file = 'background_infos/edges3.csv', sep = '|', row.names = FALSE) 


members <- setDT(read.csv(header = TRUE, file = 'background_infos/members.csv', sep = '|'))
members <- unique(members[, .(id, name, group)])
setnames(members, 'group', 'gr')

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



