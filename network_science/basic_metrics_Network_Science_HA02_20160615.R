############ Setting up ############
require(network)
require(igraph)
require(dplyr)
require(ggplot2)
require(data.table)
require(sqldf)
require(tcltk)
require(Rmisc)
require(reshape2)

options(stringsAsFactors = FALSE)
# setwd("...")

## Ch3.2.b
G4_nodes <- data.table(read.csv(file = 'G4_Nodes.csv', sep = ';'))
G4_degree_dist <- sqldf('SELECT Degree ,COUNT(Degree) AS CNT_4 FROM G4_nodes GROUP BY Degree')

G100_nodes <- data.table(read.csv(file = 'G100_4_Nodes.csv', sep = ';'))
G100_degree_dist <- sqldf('SELECT Degree ,COUNT(Degree) AS CNT_100 FROM G100_nodes GROUP BY Degree')

G1000_nodes <- data.table(read.csv(file = 'G1000_4_Nodes.csv', sep = ';'))
G1000_degree_dist <- sqldf('SELECT Degree ,COUNT(Degree) AS CNT_1000 FROM G1000_nodes GROUP BY Degree')

G5000_nodes <- data.table(read.csv(file = 'G5000_4_Nodes.csv', sep = ';'))
G5000_degree_dist <- sqldf('SELECT Degree ,COUNT(Degree) AS CNT_5000 FROM G5000_nodes GROUP BY Degree')

G10000_nodes <- data.table(read.csv(file = 'G10000_4_Nodes.csv', sep = ';'))
G10000_degree_dist <- sqldf('SELECT Degree ,COUNT(Degree) AS CNT_10000 FROM G10000_nodes GROUP BY Degree')

all_degree_dist <- merge(merge(merge(merge(G4_degree_dist, G100_degree_dist, all = TRUE), G1000_degree_dist, all = TRUE), G5000_degree_dist, all = TRUE), G10000_degree_dist, all = TRUE)
setnames(all_degree_dist, c('degree', '_4', '_100', '_1000', '_5000', '_10000'))
write.csv(all_degree_dist, file = "all_degree_distribution.csv", row.names = FALSE)

all_degree_dist <- melt(all_degree_dist, id.vars = 'degree')

dd <- ggplot(all_degree_dist)+
        geom_point(aes(x = degree, 
					   y = value, 
					   color = variable, 
					   group = variable), 
				   shape = 1)+
        xlab('degree')+
        ylab('count')+
        ggtitle('degree distribution') +
        scale_color_discrete(name = "network size")

ddls <- ggplot(all_degree_dist)+
          geom_point(aes(x = degree, 
						 y = value, 
						 color = variable, 
						 group = variable), 
					 shape = 1)+
          coord_trans(x = "log10", 
					  y = "log10")+
          xlab('degree')+
          ylab('count')+
          ggtitle('degree distribution on a log10-log10 scale') +
          scale_color_discrete(name = "network size")

multiplot(dd, ddls, cols = 2)


## Ch3.2.c
all_degree_dist_rev <- data.table(merge(merge(merge(merge(G4_degree_dist, G100_degree_dist, all = TRUE), G1000_degree_dist, all = TRUE), G5000_degree_dist, all = TRUE), G10000_degree_dist, all = TRUE))
all_degree_dist_rev <- all_degree_dist_rev[,lapply(.SD,function(x){ifelse(is.na(x), 0, x)})]
all_degree_dist_rev$CNT_4rev = rev(cumsum(rev(all_degree_dist_rev$CNT_4)))/4
all_degree_dist_rev$CNT_100rev = rev(cumsum(rev(all_degree_dist_rev$CNT_100)))/100
all_degree_dist_rev$CNT_1000rev = rev(cumsum(rev(all_degree_dist_rev$CNT_1000)))/1000
all_degree_dist_rev$CNT_5000rev = rev(cumsum(rev(all_degree_dist_rev$CNT_5000)))/5000
all_degree_dist_rev$CNT_10000rev = rev(cumsum(rev(all_degree_dist_rev$CNT_10000)))/10000
all_degree_dist_rev <- all_degree_dist_rev[,lapply(.SD,function(x){ifelse(x == 0, NA, x)})]
all_degree_dist_rev[, CNT_4 := NULL]
all_degree_dist_rev[, CNT_100 := NULL]
all_degree_dist_rev[, CNT_1000 := NULL]
all_degree_dist_rev[, CNT_5000 := NULL]
all_degree_dist_rev[, CNT_10000 := NULL]
write.csv(all_degree_dist_rev, file = "all_cumulative_degree_distribution.csv", row.names = FALSE)

## all_degree_dist_rev[, 'CNT_100rev' <- '100'] ---
setnames(all_degree_dist_rev, c('degree','_4','_100','_1000','_5000','_10000'))
all_degree_dist_rev <- melt(all_degree_dist_rev, id.vars = 'degree')

ggplot(all_degree_dist_rev) + 
   geom_point(aes(x = degree, 
				  y = value, 
				  color = variable, 
				  group = variable), 
			  shape = 1) + 
   coord_trans(x = 'log10', 
			   y = 'log10') + 
   ylab('ratio') + 
   ggtitle('cumulative degree distribution on a log-log10 scale') + 
   scale_color_discrete(name = 'network size')


## Ch3.2.d
G10000_acc <- G10000_nodes[, Id, Clustering.Coefficient]
setorder(G10000_acc, Id)
G10000_acc$acc <- cumsum(G10000_acc$Clustering.Coefficient)/G10000_acc$Id
G10000_acc[, Clustering.Coefficient := NULL]

acc <- ggplot(G10000_acc)+
          geom_point(aes(x = Id, 
						 y = Clustering.Coefficient, 
						 color = 'brown'), 
					 shape = 1)+
          xlab('number of nodes')+
          ylab('avg. clustering coeff.')+
          ggtitle('average clustering coefficient in function of N')

accls <- ggplot(G10000_acc)+
            geom_point(aes(x = Id, 
						   y = Clustering.Coefficient, 
						   color = 'brown'), 
					   shape = 1)+
            coord_trans(x = 'log10', 
						y = 'log10')+
            xlab('number of nodes')+
            ylab('avg. clustering coeff.') +
            ggtitle('average clustering coefficient in function of N on a log10-log10 scale')

multiplot(acc, accls, cols = 2)





## Ch3.2.e
G10000_edges <- data.table(read.csv(file = 'G10000_4_Edges_v2.csv', sep = ';'))
norm_G10000_edges <- sqldf('SELECT nr_of_nodes
                                  ,MAX(N1) AS N1_edges
                                  ,MAX(N5) AS N5_edges
                                  ,MAX(N101) AS N101_edges
                                  ,MAX(N1001) AS N1001_edges
                                  ,MAX(N5001) AS N5001_edges
                            FROM   G10000_edges
                            GROUP BY nr_of_nodes')

norm_G10000_edges$N5_edges <- as.numeric(norm_G10000_edges$N5_edges)
norm_G10000_edges$N101_edges <- as.numeric(norm_G10000_edges$N101_edges)
norm_G10000_edges$N1001_edges <- as.numeric(norm_G10000_edges$N1001_edges)
norm_G10000_edges$N5001_edges <- as.numeric(norm_G10000_edges$N5001_edges)

setnames(norm_G10000_edges, c('nr_of_nodes','N1','N5','N101','N1001','N5001'))
write.csv(norm_G10000_edges, file = 'norm_G10000_4_Edges.csv')
norm_G10000_edges <- melt(norm_G10000_edges, id.vars = 'nr_of_nodes')

ddy <- ggplot(norm_G10000_edges)+
         geom_point(aes(x = nr_of_nodes, 
						y = value, 
						color = variable, 
						group = variable), 
					shape = 1)+
         xlab('number of nodes')+
         ylab('weight')+
         ggtitle('degree dynamics') +
         scale_color_discrete(name = 'node id')

ddyls <- ggplot(norm_G10000_edges)+
            geom_point(aes(x = nr_of_nodes, 
						   y = value, 
						   color = variable, 
						   group = variable), 
					   shape = 1)+
            coord_trans(x = 'log10', 
						y = 'log10')+
            xlab('number of nodes')+
            ylab('weight')+
            ggtitle('degree dynamics on a log10-log10 scale') +
            scale_color_discrete(name = 'node id')

multiplot(ddy, ddyls, cols = 2)
