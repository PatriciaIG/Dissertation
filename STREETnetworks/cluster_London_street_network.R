#Libraries

library(gplots)
library(igraph)
library(reshape2)
library(plyr)
library(rgdal)
library(Polychrome)
library(spatstat)
library(here)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(stringr)
library(fs)
library(tidyverse)
library(stringr)
library(raster)
library(janitor)
library(wesanderson)
library(lsa)
library(igraph)
library(reshape2)
library(plyr)
library(dplyr)
library(rgdal)
library(Polychrome)
library(deldir)
library(interp)
library(rlist)
library(collapsibleTree)
library(raster)
library(sp)
library(expss)


#London street network

dir_data <- "London_st_network/"
file_network <- paste0(dir_data,"London_edgelist_clean.txt")

#file with the coordinates is only used to visualise the network, it is not needed for the analysis since the distance between intersections has already been computed. Otherwise one could generate the weights of the network computing the distance between them.
file_coords <- paste0(dir_data,"london_coordinates_clean.csv")

#let us read the file that contains the list of nodes
all_table <- read.table(file_network,header=T,sep=",")
head(all_table)

#choose the column that contains the weight that you which to use for the percolation
colnames(all_table)

#In this case it's column 3, "length".
p_col=3

#let us construct the network first and do the percolation afterwards
node1=all_table$start_point
node2=all_table$end_point
weight=all_table$length
df=data.frame(node1,node2,weight)

#Build the network
G=graph.data.frame(df,directed=FALSE)

#List the Edges
head(E(G))

# Let us see if there are self loops
sum(which_loop(G, eids = E(G)))

# yes, let us remove them
G=simplify(G)
sum(which_loop(G, eids = E(G)))


#Is it a connected network or does it have disconnected parts?

components(G)$no
summary(components(G))

#There are 135 disconnected components. 
#This operation gives you the membership of each point, the cluster size of 
#each component is obtained by typing \$csize and the number of clusters \$no.

dist_sizes=components(G)$csize
sort(dist_sizes,decreasing=T)[1:20]

#The largest connected component has most points
#Let us compute the size of largest component
largest_cpt=max(dist_sizes)
tot_nodes=length(V(G))
largest_cpt/tot_nodes

#Although there are disconnected bits, 99% of the nodes belong to the largest component.

#Let us take only the largest connected component
gclust<-components(G, mode='weak')
largestConnectedComponent<-induced.subgraph(G, V(G)[which(gclust$membership == which.max(gclust$csize))])
G=largestConnectedComponent
summary(G)

all_weights=(E(G)$weight)
size_net=length(V(G))

#--> let us assign coordinates to the nodes
data_coords <- read.csv(file_coords,sep=',',header=TRUE)
head(data_coords)

##Relevant thresholds


#---------- BEGINNING: Number of intersections inside LSOAS---------

#Loading shapefile of LSOAs
lsoas <- st_read(here::here("Data", "2011_london_boundaries","LSOAs","LSOAs.shp"))
boroughs <- st_read(here::here("Data", "2011_london_boundaries","Boroughs.gpkg"))
msoas <- st_read(here::here("Data","2011_london_boundaries", "London_MSOAs.gpkg"))

#Adjust projections of LsOAs
lsoas <- lsoas %>%
  dplyr::filter(str_detect(LAD11CD, "^E09"))%>%
  st_transform(., 27700)
  

boroughs <- boroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

msoas <- msoas %>%
  st_transform(., 27700)

qtm(lsoas)
qtm(boroughs)
qtm(msoas)



#Converting coordinates data frame into geometry type

intersections <- read_csv(file_coords) %>%
  st_as_sf(., coords = c("x", "y"), 
           crs = 27700)

#plot the intersections in the city
tmap_mode("view")

tm_shape(lsoas) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(intersections) +
  tm_dots(col = "blue")

# Group number of points by LSOA

## By LSOA
numb <- data_coords_plot %>%
  group_by(LSOA11CD) %>%
  summarise(count=n())%>%
  as.data.frame(.)%>%
  left_join(.,lsoas[,c(1,5)],by=c("LSOA11CD"="LSOA11CD"))%>%
  left_join(.,lsoas[,c(1,3)],by=c("LSOA11CD"="LSOA11CD"))


## By Borough
numb1 <- numb %>%
  group_by(LAD11CD) %>%
  summarise(count=sum(count))
##minimun number of points by borough is 936

hist(numb1$count,breaks=20)
median(numb$count)
mean(numb$count)


## By MSOA  180 points
numb2 <- numb %>%
  group_by(MSOA11CD) %>%
  summarise(count=sum(count))
##minimun number of points by borough is 936

hist(numb2$count,breaks=20)
median(numb2$count)
mean(numb2$count)


#Plot the histogram
library(ggplot2)

# Add mean line to single histogram
ggplot(numb, aes(x = count)) +
  geom_histogram(binwidth = 10, color = "grey30", fill = "white") +
  geom_vline(xintercept = mean(numb$count), color = "red", linetype = "dashed")+
  geom_text( mapping=aes(x=mean(numb$count), y=900, label="mean = 37.4"), size=4, angle=90, vjust=+1.2, hjust=0, col="red") +
  geom_vline(xintercept = median(numb$count), color = "blue", linetype = "dashed")+
  geom_text( mapping=aes(x=median(numb$count), y=900, label="median = 32"), size=4, angle=90, vjust=-1.2, hjust=0, col="blue") +
  labs(x="Street Intersections in LSOAs",y="Count")

median(numb$count)
mean(numb$count)

#---------- END: Number of intersections inside LSOAS--------------

#---------- BEGINNING: SAMPLE OF THE FIRST COMPONENT --------------
#let us read the file that contains the list of nodes
#all_table <- read.table("Data/g_test.txt",header=T,sep=" ")
#head(all_table)

#choose the column that contains the weight that you which to use for the percolation
#colnames(all_table)

#In this case it's column 3, "length".
#p_col=3
#
#let us construct the network first and do the percolation afterwards
#node1=all_table$from
#node2=all_table$to
#weight=all_table$distance
#df=data.frame(node1,node2,weight)

#Build the network
#g=graph.data.frame(df,directed=FALSE)
#components(g)$no
#summary(components(g))


##Plot network of the selected component
lay_test <- data.frame(id_point=as.integer(as_ids(V(G))))

lay_test <- lay_test %>%
  left_join(.,data_coords,by=c("id_point"="id")) 

lay_test <- lay_test[,c(1,3,4)] %>%
  distinct()

lay_plot <- as.matrix(lay_test[,c(2,3)])

plot(G, layout = lay_plot,vertex.size=0.01,edge.color=NA,vertex.color="gray",vertex.label="")

lsoas_points <- data_coords%>%
  group_by(LSOA11CD)%>%
  summarise(count=n())
lsoas_points <- lsoas_points%>%
  mutate(color=c(rep(top36,11),top36[1:92]))

data_coords_plot <- data_coords%>%
  left_join(.,lsoas_points,by=c("LSOA11CD"="LSOA11CD")) 
data_coords_plot <- data_coords_plot%>%
  st_as_sf(., coords = c("x", "y"), 
         crs = 27700)

par(bg= "white")
plot(data_coords_plot$x,data_coords_plot$y,xlab=NA,ylab=NA,pch=15,cex=0.2,axes=FALSE, frame.plot=F,col=rep("gray",4833))
title(main = "London Street Network",col.main= "black")

# tmap mode set to interactive viewing
  tm_shape(data_coords_plot) +
  tm_dots(col="pink1",title = "London Street Network")+ 
  tm_shape(lsoas) +
  tm_borders(col = "black",alpha=0.25,lwd=1)+
  tm_scale_bar(position=c("right", "top"))+
  tm_layout(title = "London Street Network",
            title.color = "black")
 


##Core distances --Not calculated due to capacity limit

n_min <- 220

##No calculated
b <- shortest.paths(g)
core_dist <- c()
for (i in c(1:length(V(g)))) {
  core_dist <- c(core_dist, sort(b[i,],decreasing = FALSE)[n_min])
}

df <- data.frame(core_dist)

ggplot(df, aes(x = core_dist)) +
  geom_histogram(binwidth = 10, color = "grey30", fill = "white") +
  geom_vline(xintercept = mean(df$core_dist), color = "red", linetype = "dashed")

median(df$core_dist)
mean(df$core_dist)

length(unique(df$core_dist))

##Results 295/3520 for 10 points minimun to compose a cluster median coredis 133m 
##Results 407/3520 for 20 points minimun to compose a cluster median coredis 217m 
##Results 411/3520 for 25 points minimun to compose a cluster median coredis 252m
##Results 432/3520 for 30 points minimun to compose a cluster median coredis 283m
##Results 454/3520 for 35 points minimun to compose a cluster median coredis 315m


## --------Not calculated due to capacity limit

## Label each cluster 
g <- G

r1=40  ##threshold where the last cluster is seen (maximum d possible) 21,30 -32,50
r2=865  ##threshold where the whole cluster appears (minimum d possible) 865,30-865,50
c(seq(r1, 125,by=5),seq(135, 315,by=10),seq(340, r2,by=25))

r0=sort(c(40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,135,145,155,165,175,185,195,205,215,225,235,245,255,265,275,285,295,305,315,340,365,390,415,440,465,515,565,615,690,765,840,865)
,decreasing = TRUE)

for (i_t in r0)
{
  #find subgraph such that all weights <= threshold r0
  g_h <- subgraph.edges(g, E(g)[weight<=i_t],delete.vertices = TRUE)
  #take subcomponents
  membclusters <- clusters(g_h, mode="weak")$membership
  head(membclusters)
  m <- (cbind(V(g_h)$name,membclusters))
  colnames(m) <- c("id_point",paste("id_cluster",i_t,sep="_"))
  
  M_data <- as.data.frame(m)
  head(M_data)
  M_data[,2] <- as.integer(M_data[,2])
  
  
  table_data <- table(M_data$id_cluster)
  n_cluster <- length(table_data)
  table_data <- as.data.frame(table_data)
  colnames(table_data) <- c("id_cluster", "points")
  head(table_data)
  
  id_clu_non_sig <- table_data %>%
    filter(points<n_min)
  
  id_clu_non_sig <- as.vector(as.integer(id_clu_non_sig[,1]))
  
  M_data[,2]  <-  mapvalues(M_data[,2],id_clu_non_sig,rep(NA, length(id_clu_non_sig)))
  
  if(i_t==r2)
  {
    clusters <- M_data
  }else{
    clusters <- clusters %>%
      left_join(.,M_data,by=c("id_point"="id_point"))
  }
}
##---1--SAVE-CSV: RAW CLUSTERS FROM PERCOLATION
write.csv(clusters,"rawclusters_london_220.csv")

##Rename clusters labels
ini <- 1
for (i in c(3:ncol(clusters))) {
  n_c <- length(sort(unique(clusters[,i])))
  end <- ini+n_c-1
  clusters[,i]  <-  mapvalues(clusters[,i],sort(unique(clusters[,i])),c(ini:end))
  ini <- end+1
}

##---2--SAVE-CSV: RAW CLUSTERS RELABELED NUMBERS
write.csv(clusters,"clusters_1_london_220.csv")


##Tree table
tree <- clusters%>%
  #Get the names
  #str_c(colnames(clusters[,c(3:ncol(clusters))]),collapse=",")
  group_by(id_cluster_840,id_cluster_765,id_cluster_690,id_cluster_615,id_cluster_565,id_cluster_515,id_cluster_465,id_cluster_440,id_cluster_415,id_cluster_390,id_cluster_365,id_cluster_340,id_cluster_315,id_cluster_305,id_cluster_295,id_cluster_285,id_cluster_275,id_cluster_265,id_cluster_255,id_cluster_245,id_cluster_235,id_cluster_225,id_cluster_215,id_cluster_205,id_cluster_195,id_cluster_185,id_cluster_175,id_cluster_165,id_cluster_155,id_cluster_145,id_cluster_135,id_cluster_125,id_cluster_120,id_cluster_115,id_cluster_110,id_cluster_105,id_cluster_100,id_cluster_95,id_cluster_90,id_cluster_85,id_cluster_80,id_cluster_75,id_cluster_70,id_cluster_65,id_cluster_60,id_cluster_55,id_cluster_50,id_cluster_45,id_cluster_40)%>%
  summarise(count=n())

##---3--SAVE-CSV: RAW TREE
write.csv(tree,"rawtree_london_220.csv")


##Clean the tree to relevant leafs

A <- vector()
B <- vector()

for (j in sort(c(2:(ncol(tree)-1)),decreasing =TRUE)){
  subset <- tree[!(is.na(tree[,(j)])), c((j-1),j)]
  for (i in as.vector(unlist(unique(subset[,1])))) {
    if (length(as.vector(unlist(unique(subset[(subset[,1]==i),2]))))==1){
      tree[(tree[,(j-1)]==i)&!(is.na(tree[,j])), j] <- NA
      k <- as.vector(unlist(unique(subset[(subset[,1]==i),2])))[1]
      A <- append(A,i )
      B <- append(B,k)
    }
  }
} 


tree <- tree[,c(1:(ncol(tree)-1))]%>%
  distinct()

tree <- tree[rowSums(is.na(tree)) != ncol(tree), ]  

map_clusters <- data.frame(id_1=A,id_2=B)

##---4--SAVE-CSV: RAW TREE AND MAPPING
write.csv(tree,"tree_1_london_220.csv")
write.csv(map_clusters,"deleted_cluster_london_220.csv")



##Truncate the tree with columns without meaningful clusters
end <- ncol(tree)
while (colSums(is.na(tree[,end])) == nrow(tree))
{
  end <- end-1
}

tree <- tree[,c(1:end)]

branches <- tree

##---5--SAVE-CSV: PROCESSED TREE
write.csv(tree,"tree_2_london_220.csv")

##Calculation of stability

## Max Epsilon

A <- vector()
B <- vector()
for (i in c(1:ncol(tree))) {
  k <- as.integer(strsplit(colnames(tree[,i]),"_")[[1]][3])
  for (j in sort(as.vector(unlist(unique(tree[,i]))))) {
    #colname and treshold
    A <- append(A,j )
    B <- append(B,k)
  }
}

eps_max <- data.frame(id_cluster=A,eps_max=B)

##---6--SAVE-CSV: RELEVANT THRESHOLD
write.csv(eps_max,"threshold_london_220.csv")


## Min Epsilon for nodes
##Mapping dictionary of significant and non-significant clusters
i <- 3
end <- max(sort(clusters[,ncol(clusters)]))
d <- eps_max%>%
  merge(.,map_clusters,by.x=1,by.y=1,all.x=TRUE)
while (max(sort(d[,i]))!=end){
  colnames(map_clusters)[2] <- paste("id",i,sep="_") 
  d<- d %>%
    merge(.,map_clusters,by.x=i,by.y=1,all.x=TRUE)
  i <- i+1
}

seq_cluster <- d[,c((ncol(d)-2),sort(1:(ncol(d)-3),decreasing = TRUE),ncol(d))]

##---7--SAVE-CSV: RELEVANT THRESHOLD
write.csv(seq_cluster,"leafs_london_220.csv")

##Creating list of replacement values

A <- vector()
B <- vector()   
for(i in 1:nrow(seq_cluster)) {       # for-loop over rows
  B <- append(B,sort(as.vector(unlist(seq_cluster[i,c(2:ncol(seq_cluster))]))))
  A <- append(A,rep(seq_cluster[i,1 ],length(sort(as.vector(unlist(seq_cluster[i,c(2:ncol(seq_cluster))]))))))
}

for(i in 2:ncol(clusters)) {
  clusters[,i]  <-  mapvalues(clusters[,i],B,A)
}

##---8--SAVE-CSV: RELEVANT THRESHOLD CLUSTERS
write.csv(clusters,"clusters_2_london_220.csv")



##Calculate min epsilon for each point and cluster

#g <- clusters
clusters <- clusters[,-2]
clusters[is.na(clusters)] <- 0
summary(clusters)


##Replace non-significant clusters

for (i in 2:ncol(clusters)){
  clusters[,i]  <-  mapvalues(clusters[,i],map_clusters[,2],rep(0, length(map_clusters[,2])))
} 

clusters2 <- clusters

for (i in c(1:nrow(clusters2))){
  for (j in c(2:(ncol(clusters2)-1))){
  
    subset <- clusters2[i, c(j,j+1)]
    if (subset[1]==subset[2]){
      clusters2[i,j] <- 0
    }
  }
}

clusters3 <- clusters2
colnames(clusters3) <- c("id_point",r0[2:ncol(clusters3)])

clusters3 <- clusters3 %>% 
  pivot_longer(!id_point, names_to = "threshold", values_to = "cluster")


clusters3 <- clusters3[,c(3,2)]

clusters3 <- clusters3%>%
  filter(.,cluster!=0)

clusters3$threshold <- as.numeric(clusters3$threshold)

clusters3 <- clusters3%>%
  mutate(threshold=1/threshold)

eps_min <- clusters3%>%
  group_by(cluster)%>%
  summarise(eps_min=sum(threshold),count=n())

eps_max <- eps_max%>%
  mutate(eps_max=1/eps_max)

stability <- eps_min%>%
  left_join(.,eps_max,c("cluster"="id_cluster"))

stability <- stability%>%
  mutate(stability=eps_min-count*eps_max)

##---10--SAVE-CSV: CLUSTER STABILITY
write.csv(stability,"cluster_stab_london_220.csv")


##Identify significant clusters

#Clean branches with repetitive values of repeated clusters 
clean_branches <- branches
clean_branches[is.na(clean_branches)] <- 0
for (i in 1:nrow(clean_branches)){
  end <- max((sort(clean_branches[i,])))
  j <- 1
  while(clean_branches[i,j]!=end){
    if (clean_branches[i,j+1]==0){
      clean_branches[i,j+1] <- clean_branches[i,j]
    }
    j <- j+1
  }
}


##---11--SAVE-CSV: PROCESSED TREE
write.csv(clean_branches,"tree_3_london_220.csv")



##Copy to the model
##Analyse the stability between parents and children
clean_branches[clean_branches[,]==0] <- NA

stability[stability[,5]<0,5] <- 0

stab_clu <- stability[,c(1,5)]
stab_clu <- stab_clu%>%
  mutate(significance="significant")

#origin <- sort(as.vector(unlist(unique(clean_branches[,ncol(clean_branches)]))))
#stab_clu[stab_clu[,1]%in%origin,3] <- "significant"


stab_clu$cluster <- as.numeric(stab_clu$cluster)
stab_clu$stability <- as.numeric(stab_clu$stability)


for (i in sort(1:(ncol(clean_branches)-1),decreasing = TRUE)){
  #i <- i+1 ##Clean this line
  subset <- clean_branches[,c(i,i+1)]%>%
    distinct()
  
  ##In some rows the subset can be empty, pay attention to
  subset <- subset[(!is.na(subset[,2])&(subset[,1]!=subset[,2])),]
  if (nrow(subset)!=0){
    for (j in as.vector(unlist(unique(subset[,1])))){
      parent <- stab_clu[ stab_clu[,1]==j,2]
      children <- sum(stab_clu[ stab_clu$cluster %in% as.vector(unlist(subset[subset[,1]==j,2])),2])
      
      if (parent>children){
        
        stab_clu[ stab_clu[,1]==j,3] <- "significant"
        stab_clu[ stab_clu[,1] %in% as.vector(unlist(subset[subset[,1]==j,2])),3] <- "non-significant"
      }else{
        
        stab_clu[ stab_clu[,1]==j,3] <- "non-significant"
        stab_clu[ stab_clu[,1]==j,2] <- children
        for (k in as.vector(unlist(subset[subset[,1]==j,2]))){
          if (stab_clu[stab_clu[,1]==k,3] %in% c("significant")){
            
            stab_clu[stab_clu[,1]==k,3] <- "significant"
          }
        }
      }
    }
  }
}

clean_branches_sig <- branches
clean_branches_sig[] <- lapply(clean_branches_sig, as.character)
clean_branches_sig[is.na(clean_branches_sig)] <- "0"

key <- as.character(as.vector(unlist(stab_clu[,1])))
val <- as.vector(unlist(stab_clu[,3]))

lapply(1:length(key),FUN = function(i){clean_branches_sig[clean_branches_sig == key[i]] <<- val[i]})


#Verificar class of first column
#summary(stab_clu)
#stab_clu[,1] <- as.numeric(stab_clu[,1])

for(i in 1:nrow(clean_branches_sig)) {
  j <- ncol(clean_branches_sig)
  k <- sum(clean_branches_sig[i,]=="significant")
  if (k!=1){
    while(k>1){
      if (clean_branches_sig[i,j]=="significant"){
        print("Hola")
        print(i)
        print(j)
        stab_clu[stab_clu[,1]==as.integer(branches[i,j]),3] <- "non-significant"  
        k <- k-1
      }
      j <- j-1
    }
  }
}


##Plot the different significant clusters

##Create the summarised information of all nodes
points_info <- data.frame(clusters[,1])
colnames(points_info) <- "id_point"
points_info <- points_info%>%
  mutate(id_cluster=NA)

##List of significant clusters
sig_clusters <- as.vector(unlist(stab_clu[stab_clu[,3]=="significant",1]))

for (i in 1:nrow(points_info)){
  a <-  unique(as.vector(unlist((clusters[i,-1]))))
  cluster_id <- a[a%in%sig_clusters]
  if(length(cluster_id)==1){
    points_info[i,2]<- cluster_id
  }
  if (length(cluster_id)>1){
    points_info[i,2]<- "error"
  }
  if (length(cluster_id)==0){
    points_info[i,2]<- NA
  }
}



sig_clus <- points_info%>%
  group_by(id_cluster)%>%
  summarise(count=n())

sig_clus <- sig_clus%>%
  mutate(color=NA,size=NA)

##Asign colors and size

#top36 <-  as.vector(palette36.colors(36))
#barplot(c(1:36), col=top36)

#Palette of 433 colors
top36 <- grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][c(-1,-24)]
top36_hex <- vector()

for (i in top36){
  top36_hex <- append(top36_hex,col2hex(i))
}

##modify colors in case there are more than 34 clusters

color <- c(sample(top36_hex, nrow(sig_clus)-1, replace = FALSE, prob = NULL),"black")

color <- c(sample(rainbow(200,s=0.6),200), "black")

pie(rep(1,nrow(sig_clus)), col=color)


size <- c(rep(2,(nrow(sig_clus)-1)),1)

sig_clus[,3] <- color #color
sig_clus[,4] <- size  #size


##Assign and match to the point set

points_info <- points_info%>%
  left_join(.,sig_clus,c("id_cluster"="id_cluster"))

points_info[,1] <- as.numeric(points_info[,1])
data_coords[,1] <- as.integer(data_coords[,1])
points_info <- points_info%>%
  left_join(.,data_coords,c("id_point"="id"))

##Plot

##Plot all the clusters

par(bg= "white")
plot(points_info$x,points_info$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info$size),
     axes=FALSE, frame.plot=F,col=points_info$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "Flat Representation Stability Optimisation",col.main= "black")

##Plot all the clusters
points_info_clu <- points_info[points_info[,2] %in% sig_clusters,]

par(bg= "white")
plot(points_info_clu$x,points_info_clu$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_clu$size),
     axes=FALSE, frame.plot=F,col=points_info_clu$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "Flat Representation Stability Optimisation",col.main= "black")

##Plot only the noisy points
points_info_noi <- points_info[is.na(points_info[,2]),]

par(bg= "white")
plot(points_info_noi$x,points_info_noi$y,xlab=NA,ylab=NA,pch=20,cex=0.1,
     axes=FALSE, frame.plot=F,col="#C3B0A7",xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "Unclustered Street Intersection Points",col.main= "black")

##Plot only the 40 first clusters
points_info_40 <- points_info[points_info[,2] %in% sig_clusters[1:40],]

par(bg= "white")
plot(points_info_40$x,points_info_40$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_40$size),
     axes=FALSE, frame.plot=F,col=points_info_40$color)
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "40 Clusters that appear first",col.main= "black")

##Plot only the 40-80 clusters
points_info_80 <- points_info[points_info[,2] %in% sig_clusters[41:80],]

par(bg= "white")
plot(points_info_80$x,points_info_80$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_80$size),
     axes=FALSE, frame.plot=F,col=points_info_80$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "40 Clusters that appear second",col.main= "black")

##Plot only the 81-120 clusters
points_info_120 <- points_info[points_info[,2] %in% sig_clusters[81:120],]

par(bg= "white")
plot(points_info_120$x,points_info_120$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_120$size),
     axes=FALSE, frame.plot=F,col=points_info_120$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "40 Clusters that appear third",col.main= "black")

##Plot only the 121-160 clusters
points_info_160 <- points_info[points_info[,2] %in% sig_clusters[121:160],]

par(bg= "white")
plot(points_info_160$x,points_info_160$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_160$size),
     axes=FALSE, frame.plot=F,col=points_info_160$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "40 Clusters that appear fourth",col.main= "black")

##Plot only the 161-200 clusters
points_info_200 <- points_info[points_info[,2] %in% sig_clusters[161:200],]

par(bg= "white")
plot(points_info_200$x,points_info_200$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_200$size),
     axes=FALSE, frame.plot=F,col=points_info_200$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "40 Clusters that appear fifth",col.main= "black")


##Plot only the biggest clusters
sig_clus_big <- sig_clus %>%
  arrange(desc(count))
sig_clus_big <- as.vector(unlist(sig_clus_big[-1,1]))

points_info_40b <- points_info[points_info[,2] %in% sig_clus_big[1:40],]

par(bg= "white")
plot(points_info_40b$x,points_info_40b$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_40b$size),
     axes=FALSE, frame.plot=F,col=points_info_40b$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = "Largest Cluster - Stability Optimisation",col.main= "black")

##Plot only the 41-80th clusters
points_info_80b <- points_info[points_info[,2] %in% sig_clus_big[41:80],]

par(bg= "white")
plot(points_info_80b$x,points_info_80b$y,xlab=NA,ylab=NA,pch=20,cex=0.2*as.numeric(points_info_80b$size),
     axes=FALSE, frame.plot=F,col=points_info_80b$color,xlim=c(500000,565000),ylim=c(155000,205000))
plot(msoas$geom,border="gray",lwd=1,add=TRUE)
title(main = " 40 Second Largest Cluster - Stability Optimisation",col.main= "black")


##Plot Hierarchical Tree

tree_network <- data.frame(parent=c(NA,0,0),child=c(0,1,2))

for (j in c(1:(ncol(clean_branches)-1))){
  subset <- clean_branches[!(is.na(clean_branches[,(j)]))&!(is.na(clean_branches[,(j+1)])), c(j,j+1)]%>%
    distinct()
  colnames(subset) <- c("parent","child")
  subset <- subset[(subset[,1]!=subset[,2]),]
  tree_network <- rbind(tree_network,subset)
}

tree_network <- tree_network%>%
  mutate(color=NA)

for (i in 1:nrow(tree_network)){
  if (tree_network[i,2] %in% as.vector(unlist(sig_clus[-nrow(sig_clus),1]))){
    tree_network[i,3] <- sig_clus[(sig_clus[,1]==tree_network[i,2])&(!is.na(sig_clus[,1])),3]
  }
}

tree_network <- tree_network%>%
  mutate(color_u=ifelse(child %in% sig_clusters,"#C3B0A7",NA))


##Head of tree
collapsibleTreeNetwork(tree_network[c(1:100),],  fill = "color_u",collapsed = FALSE)



##Core distances
core_d <- eps_max%>%
  mutate(dcore=1/eps_max)

quantile(core_d$dcore,probs=c(0.50,0.60,0.70,0.75,0.8,0.9,0.95))

ggplot(core_d, aes(x = dcore)) +
  geom_histogram(binwidth = 10, color = "grey30", fill = "white") +
  geom_vline(xintercept = 145, color = "red", linetype = "dashed")+
  geom_text( mapping=aes(x=145, y=60, label="50 %"), size=4, angle=90, vjust=+1.2, hjust=0.9, col="red") +
  geom_vline(xintercept = 165, color = "red", linetype = "dashed")+
  geom_text( mapping=aes(x=165, y=60, label="75 %"), size=4, angle=90, vjust=+1.2, hjust=0.9, col="red") +
  geom_vline(xintercept = 258, color = "red", linetype = "dashed")+
  geom_text( mapping=aes(x=258, y=60, label="95 %"), size=4, angle=90, vjust=+1.2, hjust=0.9, col="red")+
  labs(x="Cluster Thresholds",y="Count")


##Largest clusters in layout
points_info_sp <- points_info%>%
  st_as_sf(., coords = c("x", "y"), 
           crs = 27700)

tm_shape(points_info_sp) +
  tm_dots(col="color",title = "London Street Network")+ 
  tm_shape(boroughs) +
  tm_borders(col = "black",alpha=0.25,lwd=1)+
  tm_scale_bar(position=c("right", "top"))
  #+tm_layout(title = "London Street Network",
           #title.color = "black")


write.csv(points_info,"london_final_cluster_220.csv")


##Cophenetic distance in the street network

tree_network_coph <- tree_network

tree_network_coph <- tree_network_coph%>%
  left_join(.,core_d[,-2],by=c("parent"="id_cluster"))

tree_network_coph <- tree_network_coph[-1,]
tree_network_coph[c(1,2),4] <- 865

leafs <- sig_clus[-nrow(sig_clus),1]

##Build the matrix with all the street intersection clusters
matrix_c <- matrix( 1,nrow = nrow(leafs) , ncol = nrow(leafs))
diag(matrix_c) <- 0
rownames(matrix_c) <- leafs$id_cluster
colnames(matrix_c) <- leafs$id_cluster

G_clus <- graph_from_adjacency_matrix(matrix_c, mode ="undirected", weighted = TRUE)
coph_dist_st <-  as_long_data_frame(G_clus)
coph_dist_st <- coph_dist_st[,c(4,5)]
colnames(coph_dist_st) <-c("from","to") ##Verify the number of rows n*n-1/2 of number of elements

##ReBuild paths of significant clusters


##DELETE non-significant clusters at the end
clean_branches_sig1 <- branches
clean_branches_sig1 <- clean_branches_sig1%>%
  mutate(index=0)

for (i in 1:(nrow(clean_branches_sig1))){
   j <- 1
   end <- 1
   while(end==1){
     if(clean_branches_sig1[i,j]%in%sig_clusters){
       end <- end-1
       clean_branches_sig1[i,49] <- clean_branches_sig1[i,j]
     }
     if(j==48){
       end <- 0
     }
     if(j<48){
       j <- j+1
     }
   }
}

##Significant clusters
sig_cluster_comprobar <- unique(unlist(clean_branches_sig1[,49]))

clean_branches_sig1 <- clean_branches_sig1%>%
  distinct(index, .keep_all = TRUE)

clean_branches_sig2 <- clean_branches_sig1[clean_branches_sig1[,49]!=0,]

clean_branches_sig2 <- clean_branches_sig2[!duplicated(clean_branches_sig2$index), ]

##Coph Distance in the street
coph_dist_st <- coph_dist_st%>%
  mutate(coph_dist_st =0)

for (i in 1:nrow(coph_dist_st)){
  a <- coph_dist_st[i,1]
  b <- coph_dist_st[i,2]
  A <- unique(unlist(clean_branches_sig2[clean_branches_sig2[,49]==a,-49]))
  B <- unique(unlist(clean_branches_sig2[clean_branches_sig2[,49]==b,-49]))
  if (length(sort(intersect(A,B)))!=0){
    coph_dist_st[i,3] <- max(sort(intersect(A,B)))
  }
}

coph_dist_st[] <- lapply(coph_dist_st, as.numeric)
#coph_dist_st[,1] <- as.numeric(coph_dist_st[,1])
#coph_dist_st[,2] <- as.numeric(coph_dist_st[,2])

coph_dist_st <- data.frame(t(apply(coph_dist_st[,c(1,2)], 1, sort)),coph_dist_st[,3])
colnames(coph_dist_st) <- c("from","to","coph_dist_st")


##Match Heights with coph_dist_ 
coph_dist_st <- coph_dist_st%>%
  left_join(.,tree_network_coph[,c(1,4)],by=c("coph_dist_st"="parent"))

coph_dist_st_final <- coph_dist_st[,-3]

coph_dist_st_final <- coph_dist_st_final%>%
  distinct()

colnames(coph_dist_st_final) <- c("from","to","coph_dist_st")

write.csv(coph_dist_st_final,"coph_dist_st_final.csv")


##TEST: new dataset for number of steps

c <- clean_branches_sig2%>%
  mutate(id_cluster_865=0)
c <- c[,c(49,50,1:48)]
c <- c%>%
  mutate(length=0)
c <- c[,c(1,51,2:50)]

for (i in 1:nrow(c)){
  c[i,2] <- length(sort(unique(unlist(c[i,c(3:51)]))))
}

d <- c

for (i in 1:nrow(d)){ 
  d[i,c(3:51)] <-  as.list(c(sort(unique(unlist(d[i,c(3:51)]))),rep(NA,49-d[i,2])))
  }
d <- d[,c(1:33)] ##Colnames are not validate

a <- coph_dist_st_final%>%
  left_join(.,d[,-2],by=c("from"="index"))%>%
  left_join(.,d[,-2],by=c("to"="index"))

a <- a%>%
  mutate(clust_mer=NA)

for (i in 1:nrow(a)){
  a[i,66] <-max(intersect(sort(a[i,c(4:34)]), sort(a[i,c(35:65)])))
}


a <- a%>%
  mutate(di1=NA,di2=NA,df1=NA,df2=NA)

for (i in 1:nrow(a)){
  a[i,67] <- match_col(a[i,66],unlist(a[i,c(4:34)]))
  a[i,68] <- match_col(a[i,66],unlist(a[i,c(35:65)]))
  a[i,69] <- match_col(a[i,1],unlist(a[i,c(4:34)]))
  a[i,70] <- match_col(a[i,2],unlist(a[i,c(35:65)]))
}



a <- a%>%
  mutate(dif1=df1-di1,dif2=df2-di2)

a$coph_dist_st <- apply(a[,c(71:72)], 1, max)

write.csv(a[,c(1,2,3)],"Data/step_distance_st.csv")

##TEST

#---------- END: SAMPLE OF THE FIRST COMPONENT --------------------

###----COPHENETIC DISTANCES
head(data_coords) ###180,667 ids of points
length(V(G)) ### 179,744 points that belong to the whole of the connected component
nrow(clusters) ###178,810 points that are not noise and belong to one whole cluster


##Update the tree
##Plot Hierarchical Tree
clean_branches_sig4 <- d[,c(3:33)]

f <-as.vector(unlist(d[,1])) 

for (i in 1:nrow(clean_branches_sig4)){
  for (j in 1:ncol(clean_branches_sig4)){
    if(!is.na(clean_branches_sig4[i,j])&clean_branches_sig4[i,j]>f[i]){
      clean_branches_sig4[i,j] <- NA
    }
  }
}

clean_branches_sig[is.na(clean_branches_sig)] <- "0"


tree_network_clean <- data.frame(parent=c(NA),child=c(0))

for (j in c(1:(ncol(clean_branches_sig4)-1))){
  subset <- clean_branches_sig4[!(is.na(clean_branches_sig4[,(j)]))&!(is.na(clean_branches_sig4[,(j+1)])), c(j,j+1)]%>%
    distinct()
  colnames(subset) <- c("parent","child")
  subset <- subset[(subset[,1]!=subset[,2]),]
  tree_network_clean <- rbind(tree_network_clean,subset)
}

tree_network_clean <- tree_network_clean%>%
  mutate(color=NA)

for (i in 1:nrow(tree_network_clean)){
  if (tree_network_clean[i,2] %in% as.vector(unlist(sig_clus[-nrow(sig_clus),1]))){
    tree_network_clean[i,3] <- sig_clus[(sig_clus[,1]==tree_network_clean[i,2])&(!is.na(sig_clus[,1])),3]
  }
}

tree_network_clean <- tree_network_clean%>%
  mutate(color_u=ifelse(child %in% sig_clusters,"#C3B0A7",NA))


##Head of tree
collapsibleTreeNetwork(tree_network_clean[,],  fill = "color",collapsed = FALSE)

