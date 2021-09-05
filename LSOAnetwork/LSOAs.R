#Loading a few packages I will use during my project
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
library(dendextend)

coph_street <- read_csv("Data/step_distance_st.csv")
coph_street <- coph_street[,-1]

coph_street <- data.frame(t(apply(coph_street[-3], 1, sort)),coph_dist=coph_street$coph_dist_st)
colnames(coph_street) <- c("from","to","coph_dist_st")


d_core <- read_csv("Data/core_distances_final.csv")
d_core <- d_core[,-1]



###--------> BEGGINING: LONDON BOROUGHS MAP AND LSOAS

#Loading London Boroughs boundaries
LondonBoroughs <- st_read(here::here("Data", "statistical-gis-boundaries-london",
                                     "ESRI", "London_Borough_Excluding_MHW.shp"))
#Project the map

LondonBoroughs <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(., 27700)

qtm(LondonBoroughs)

#Loading shapefile of LSOAs
lsoas <- st_read(here::here("Data", "2011_london_boundaries","LSOAs","LSOAs.shp"))

#Adjust projections of LsOAs
lsoas <- lsoas %>%
  dplyr::filter(str_detect(LAD11CD, "^E09"))%>%
  st_transform(., 27700)

qtm(lsoas)

###--------> END: LONDON BOROUGHS MAP

###--------> BEGGINING: LSOAS NETWORK WITH TRIANGULATION

puntos <- lsoas%>%
  st_centroid()

tmap_mode("view")
## tmap mode set to interactive viewing
tmap_mode("view")
tm_shape(lsoas) +
  tm_polygons(col = NA, alpha = 0.5) +
  tm_shape(puntos) +
  tm_dots(col = "red",size = 0.01,shape = 22)

##Delaunay triangulation

puntos <- puntos %>%
  st_transform(., 27700)

puntos %>%
  st_geometry(.)

coords <- puntos %>%
  st_coordinates(.)

coords <- data.frame(coords)

coords <- coords %>%
  mutate(indices=c(1:4835))%>%
  mutate(LSOASCODE=lsoas$LSOA11CD)

triangulation <- tri.mesh(coords[,1], coords[,2], duplicate = "error")
interp::plot.triSht(triangulation,col = "blue", pch = 20 ,cex=0.05, lwd=0.1)

###--------> END: LSOAS NETWORK WITH TRIANGULATION

###--------> BEGGINING: SMALL QUADRANT TO TEST

coords_test <- coords %>%
  filter(X>545000,X<555000,Y>170000,Y<185000)

tri_test <- tri.mesh(coords_test[,1], coords_test[,2], duplicate = "error")
plot(tri_test)

###--------> END: SMALL QUADRANT TO TEST


###--------> BEGGINING:  LSOAS PRINCIPAL COMPONENT OF CENSUS

# Vector of distances with loading census data and paste it to lsoas geometry

census_lsoas <- read_csv(here::here("Data","census","pca_census.csv"),
                         locale = locale(encoding = "latin1"),
                         na = "n/a")

#Examine the file, variables, columns and cleaning
Datatypelist3 <- census_lsoas %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist3

census_lsoas%>%
  colnames()

census_lsoas <-census_lsoas %>%
  distinct()

census_lsoas

lsoas <- lsoas %>%
  left_join(.,census_lsoas,by=c("LSOA11CD"="LSOA11CD"))

###--------> END: LSOAS PRINCIPAL COMPONENT OF CENSUS



###--------> BEGGINING: SMALL QUADRANT TO TEST

coords_test <- coords %>%
  filter(X>545000,X<555000,Y>170000,Y<185000)

tri_test <- tri.mesh(coords_test[,1], coords_test[,2], duplicate = "error")
plot(tri_test)

###--------> END: SMALL QUADRANT TO TEST


###--------> BEGGINING: LSOAS VARIABLES OF CENSUS

# Vector of distances with loading census data and paste it to lsoas geometry

census_lsoas <- read_csv(here::here("Data","census","census_lsoas.csv"),
                   locale = locale(encoding = "latin1"),
                   na = "n/a")

#Examine the file, variables, columns and cleaning
Datatypelist2 <- census_lsoas %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist2

census_lsoas%>%
  colnames()

census_lsoas <-census_lsoas %>%
  distinct()

census_lsoas

lsoas <- lsoas %>%
  left_join(.,census_lsoas,by=c("LSOA11CD"="GeographyCode"))

###--------> END: LSOAS VARIABLES OF CENSUS


###--------> BEGGINING: WEIGHTED NETWORK OF LSOAS

##Files to build the network
nodes <- data.frame(id_point=c(1:4835),LSOACD11=lsoas$LSOA11CD,x=coords$X,y=coords$Y)
write.csv(nodes,"Data/nodes_lsoas.csv")


##Files to build the edges

relations_lsoas <- data.frame(from=triangulation$arcs[,1],to=triangulation$arcs[,2])

relations_lsoas <- relations_lsoas %>%
  distinct()

lsoas$X1 <- c(1:4835)

relations_lsoas <- relations_lsoas %>%
  left_join(.,nodes,by=c("from"="id_point"))%>%
  left_join(.,nodes,by=c("to"="id_point"))

colnames(relations_lsoas) <- c("from","to","LSOACD11_from","x_from","y_from","LSOACD11_to","x_to","y_to")

relations_lsoas <- relations_lsoas%>%
  mutate(distance=((x_from-x_to)^2+(y_from-y_to)^2)^0.5)

distances <- relations_lsoas[order(relations_lsoas$distance),]
relations_lsoas <- distances

relations_lsoas <-  relations_lsoas[,c(1:2)]

relations_lsoas <- relations_lsoas %>%
  left_join(.,st_set_geometry(lsoas[,c(1,17:29,30)], NULL),by=c("from"="X1"))%>%
  left_join(.,st_set_geometry(lsoas[,c(1,17:29,30)], NULL),by=c("to"="X1"))


relations_lsoas <- relations_lsoas %>%
  mutate(cosdistance=1)

for(i in 1:nrow(relations_lsoas)) {       # for-loop over rows
  relations_lsoas[i,31] <- 1-cosine( as.numeric(relations_lsoas[i,4:16]),as.numeric(relations_lsoas[i,18:30]))
}

relations_lsoas <- relations_lsoas %>%
  dplyr::select(c(1:3,17,31))

relations_lsoas <- relations_lsoas %>%
  distinct()

hist(relations$cosdistance, col="red", main ="Cosine distance")

#write.csv(relations_lsoas,"Data/relations_pca_lsoas.csv")

###--------> END: WEIGHTED NETWORK OF LSOAS



###--------> BEGGINING: SET UP NETWORK OF LSOAS

### to build the network

relations <- read.table("Data/relations_pca_lsoas.csv",header=T,sep=",")
colnames(relations)

node1 <- relations$LSOA11CD.x
node2 <- relations$LSOA11CD.y
weight <- relations$cosdistance
df <- data.frame(node1,node2,weight)

G_lsoas=graph.data.frame(df,directed=FALSE)
edge_attr(G_lsoas)

#List the Edges
head(E(G_lsoas))

# Let us see if there are self loops
sum(which_loop(G_lsoas, eids = E(G_lsoas)))

#Is it a connected network or does it have disconnected parts?

components(G_lsoas)$no
summary(components(G_lsoas))


lay_test <- data.frame(id_point=as.vector(as_ids(V(G_lsoas))))

lay_test <- lay_test %>%
  left_join(.,coords[,-3],by=c("id_point"="LSOASCODE")) 

lay_test <- lay_test[,c(2,3)] %>%
  distinct()

lay_plot <- as.matrix(lay_test)

plot(G_lsoas, layout = lay_plot,vertex.size=1,vertex.label="") #edge.color=NA



###--------> END: SET UP NETWORK OF OAS

###--------> BEGGINING: COMMUNITY DETECTION

# ----- 1. Girvan-Newman algorithm    20 communities and 0.83 of modularity
alg_name='Girvan-Newman'
girvan <- edge.betweenness.community(G_lsoas)
#look at result of algorithm
print(girvan)

modularity(girvan)
nc=length(girvan)
sizes(girvan)

mem_vec=membership(girvan)
#visualise the results
plot(girvan,G_lsoas,layout=lay_plot,vertex.label="",vertex.size=1)
title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))

is_hierarchical(girvan)
dendPlot(girvan,use.modularity = TRUE)
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))

plot(c(1:4835), girvan$modularity, type = "b", pch = 19, 
     col = "red", xlab = "k", ylab = "Modularity")

dend_girvan <-as.dendrogram(girvan,use.modularity = TRUE)
get_branches_heights(dend_girvan)
get_subdendrograms(dend_girvan,length(sizes(girvan)))


# ----- 2.Fast greedy modularity optimisation: Clauset-Newman-Moore 34 communities and 0.9154 of modularity
alg_name='Clauset-Newman-Moore'
greedy_c <- fastgreedy.community(G_lsoas)
#look at result of algorithm
print(greedy_c)

modularity(greedy_c)
nc=length(greedy_c)
sizes(greedy_c)

mem_vec=membership(greedy_c)
#visualise the results
plot(greedy_c,G_lsoas, layout=lay_plot,vertex.label="",vertex.size=1)
title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))

is_hierarchical(greedy_c)
dendPlot(greedy_c,use.modularity = TRUE)
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))


plot(c(1:4835), greedy_c$modularity, type = "b", pch = 19, 
     col = "red", xlab = "k", ylab = "Modularity")

dend_greedy <-as.dendrogram(greedy_c,use.modularity = TRUE)
get_branches_heights(dend_greedy)

# ----- 3. Community strucure via short random walks: Pons&Latapy  168 communities and 0.847 of modularity
alg_name='Random walks'
wtrap_c <- walktrap.community(G_lsoas)

#look at result of algorithm
print(wtrap_c)
modularity(wtrap_c)
nc=length(wtrap_c)
sizes(wtrap_c)

mem_vec=membership(wtrap_c)
#visualise the results
plot(wtrap_c,G_lsoas,layout=lay_plot,vertex.label="",vertex.size=1)
title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))

is_hierarchical(wtrap_c)
dendPlot(wtrap_c,use.modularity = TRUE)
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))

plot(c(1:4835), wtrap_c$modularity, type = "b", pch = 19, 
     col = "red", xlab = "k", ylab = "Modularity")

dend_wtrap <-as.dendrogram(wtrap_c,use.modularity = TRUE)
get_branches_heights(dend_wtrap)


# ----- 4. Leading eigenvector: Newman spectral approach 10 communities and 0.272 of modularity
alg_name='Spectral'
spectral_c <- leading.eigenvector.community(G_lsoas)

#look at result of algorithm
print(spectral_c)
modularity(spectral_c)
nc=length(spectral_c)
sizes(spectral_c)

#visualise the results
plot(spectral_c,G_lsoas,layout=lay_plot,vertex.label="",vertex.size=1)
title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))

is_hierarchical(spectral_c)
dendPlot(spectral_c,mode="hclust" ) #use.modularity = TRUE)
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))


# ----- 5. Louvain method: Blondel et al, modularity optimization 35 communities and 0.918 of modularity
alg_name='Louvain'
louv_c <- multilevel.community(G_lsoas)

#look at result of algorithm
print(louv_c)

modularity(louv_c)
nc=length(louv_c)
sizes(louv_c)

#visualise the results
plot(louv_c,G_lsoas, layout=lay_plot,vertex.label="",vertex.size=1)
title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))

is_hierarchical(louv_c)
dendPlot(louv_c,use.modularity = TRUE)
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))

# ----- 6. Infomap method: Rosvall and Bergstrom 320 communities and 0.821 of modularity
alg_name='Infomap'
info_c <- infomap.community(G_lsoas)

#look at result of algorithm
print(info_c)
modularity(info_c)

nc=length(info_c)
sizes(info_c)
mem_vec=membership(info_c)
#visualise the results
plot(info_c,G_lsoas,layout=lay_plot,layout=lay_plot,vertex.label="",vertex.size=1)
title(paste(alg_name,' algorithm \n',nc,' communities for the karate club',sep=''))

is_hierarchical(info_c)
dendPlot(info_c,use.modularity = TRUE)
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the karate club',sep=''))

##----BEGINNING: DIFFERENTIATED COMMUNITIES BY ALGORITHMS
comm_algorithms <- c("girvan","greedy_c","wtrap_c","spectral_c","louv_c","info_c")


A <- data.frame("id_lsoa"=as.vector(V(G_lsoas)$name),"girvan"=girvan$membership)
lsoas <- lsoas %>%
  left_join(.,A,by=c("LSOA11CD"="id_lsoa"))

A <- data.frame("id_lsoa"=as.vector(V(G_lsoas)$name),"greedy"=greedy_c$membership)
lsoas <- lsoas %>%
  left_join(.,A,by=c("LSOA11CD"="id_lsoa"))

A <- data.frame("id_lsoa"=as.vector(V(G_lsoas)$name),"wtrap"=wtrap_c$membership)
lsoas <- lsoas %>%
  left_join(.,A,by=c("LSOA11CD"="id_lsoa"))

A <- data.frame("id_lsoa"=as.vector(V(G_lsoas)$name),"spectral"=spectral_c$membership)
lsoas <- lsoas %>%
  left_join(.,A,by=c("LSOA11CD"="id_lsoa"))

A <- data.frame("id_lsoa"=as.vector(V(G_lsoas)$name),"louvain"=louv_c$membership)
lsoas <- lsoas %>%
  left_join(.,A,by=c("LSOA11CD"="id_lsoa"))

A <- data.frame("id_lsoa"=as.vector(V(G_lsoas)$name),"infomap"=info_c$membership)
lsoas <- lsoas %>%
  left_join(.,A,by=c("LSOA11CD"="id_lsoa"))

## Save new geopackage
st_write(lsoas, " loas_communities.gpkg", driver="GPKG")


# Plots of cluster
ggplot(data = lsoas) +
  geom_sf(aes(fill = girvan))+
  scale_fill_distiller(name="girvan", palette = "Paired" )

ggplot(data = lsoas) +
  geom_sf(aes(fill = greedy))+
  scale_fill_distiller(name="greedy", palette = "Paired" )

ggplot(data = lsoas) +
  geom_sf(aes(fill = wtrap))+
  scale_fill_distiller(name="wtrap", palette = "Paired" )

ggplot(data = lsoas) +
  geom_sf(aes(fill = spectral))+
  scale_fill_distiller(name="spectral", palette = "Paired" )

ggplot(data = lsoas) +
  geom_sf(aes(fill = louvain))+ #color = "white"
  scale_fill_distiller(name="louvain", palette = "RdYlBu" )

ggplot(data = lsoas) +
  geom_sf(aes(fill = infomap))+
  scale_fill_distiller(name="infomap", palette = "Paired" )

##----END: DIFFERENTIATED COMMUNITIES BY ALGORITHMS

##----BEGGINING COPHENETIC DISTANCES FROM ALGORITHMS
### 1. Girvan

##--------Veify certain conditions
length(get_branches_heights(dend_girvan)) ##n-1 4834
length(girvan$modularity) ##n 4835

##Core distances
index <- match(max(girvan$modularity),rev(girvan$modularity))

core_dist_girvan <- rev(get_branches_heights(dend_girvan))[1:index]

##Calculate cophenetic distances
C_girvan <- as.matrix(cophenetic(dend_girvan))

C_girvan[C_girvan<(core_dist_girvan[length(core_dist_girvan)-1])] <- 0

G_distances_girvan <- graph_from_adjacency_matrix(C_girvan, mode ="undirected", weighted = TRUE)
coph_dist_girvan <-  as_long_data_frame(G_distances_girvan)
coph_dist_girvan <- coph_dist_girvan[,c(4,5,3)]
colnames(coph_dist_girvan) <-c("from","to","coph_dist") 

##Verify results
unique(coph_dist_girvan[,3]) ##must be in core dist 
core_dist_girvan[-length(core_dist_girvan)]
length(unique(coph_dist_girvan[,3]))  #m elements, triple union reduces in 1 the number = communities-1
length (core_dist_girvan[-length(core_dist_girvan)]) #m elements=communities-1

plot(dend_girvan, ylim = c(core_dist_girvan[length(core_dist_girvan)-1],core_dist_girvan[1]),
     xlab = "Communities",ylab="Distances")
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the LSOAs network',sep=''))

### 2. Greedy

##--------Veify certain conditions
length(get_branches_heights(dend_greedy)) ##n-1 4834
length(greedy_c$modularity) ##n 4835

##Core distances
index <- match(max(greedy_c$modularity),rev(greedy_c$modularity))

core_dist_greedy <- rev(get_branches_heights(dend_greedy))[1:index]

##Calculate cophenetic distances
C_greedy <- as.matrix(cophenetic(dend_greedy))

C_greedy[C_greedy<(core_dist_greedy[length(core_dist_greedy)-1])] <- 0

G_distances_greedy <- graph_from_adjacency_matrix(C_greedy, mode ="undirected", weighted = TRUE)
coph_dist_greedy <-  as_long_data_frame(G_distances_greedy)
coph_dist_greedy <- coph_dist_greedy[,c(4,5,3)]
colnames(coph_dist_greedy) <-c("from","to","coph_dist") 

##Verify results
unique(coph_dist_greedy[,3]) ##must be in core dist 
core_dist_greedy[-length(core_dist_greedy)]
length(unique(coph_dist_greedy[,3]))  #m elements, triple union reduces in 1 the number = communities-1
length (core_dist_greedy[-length(core_dist_greedy)]) #m elements=communities-1

plot(dend_greedy, ylim = c(2999,core_dist_greedy[1]),
     xlab = "Communities",ylab="Distances")
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the LSOAs network',sep=''))

### 3. Random

##--------Veify certain conditions
length(get_branches_heights(dend_wtrap)) ##n-1 4834
length(wtrap_c$modularity) ##n 4835

##Core distances
index <- match(max(wtrap_c$modularity),rev(wtrap_c$modularity))

core_dist_wtrap <- rev(get_branches_heights(dend_wtrap))[1:index]

##Calculate cophenetic distances
C_wtrap <- as.matrix(cophenetic(dend_wtrap))

C_wtrap[C_wtrap<(core_dist_wtrap[length(core_dist_wtrap)-1])] <- 0

G_distances_wtrap <- graph_from_adjacency_matrix(C_wtrap, mode ="undirected", weighted = TRUE)
coph_dist_wtrap <-  as_long_data_frame(G_distances_wtrap)
coph_dist_wtrap <- coph_dist_wtrap[,c(4,5,3)]
colnames(coph_dist_wtrap) <-c("from","to","coph_dist") 

##Verify results
unique(coph_dist_wtrap[,3]) ##must be in core dist 
core_dist_wtrap[-length(core_dist_wtrap)]
length(unique(coph_dist_wtrap[,3]))  #m elements, triple union reduces in 1 the number = communities-1
length (core_dist_wtrap[-length(core_dist_wtrap)]) #m elements=communities-1

plot(dend_wtrap, ylim = c(1925,2035),
     xlab = "Communities",ylab="Distances")
title(paste('Community structure dendrogram for ',alg_name,' method \n',nc,' communities for the LSOAs network',sep=''))

##  FINAL: Save files
write.csv(coph_dist_girvan,"coph_dist_girvan.csv")
write.csv(coph_dist_greedy,"coph_dist_greedy.csv")
write.csv(coph_dist_wtrap,"coph_dist_wtrap.csv")

##----END COPHENETIC DISTANCES FROM ALGORITHMS

##----BEGGINING Create community identity for each algorithm

### 1. Girvan
##File to compile the cluster transition

memb_list <- cutree(dend_girvan, h =core_dist_girvan[length(core_dist_girvan)])
members_com_girvan <- as.data.frame(cbind(memb_list))
members_com_girvan$names <- rownames(members_com_girvan)
rownames(members_com_girvan) <- c(1:girvan$vcount)
members_com_girvan <- members_com_girvan[,c(2,1)]

coph_dist_girvan <- coph_dist_girvan %>%
  left_join(.,members_com_girvan,by=c("from"="names"))

coph_dist_girvan <- coph_dist_girvan %>%
  left_join(.,members_com_girvan,by=c("to"="names"))

coph_dist_girvan_code <- coph_dist_girvan[,c(4,5,3)]%>%
  distinct()

coph_dist_girvan_code <- data.frame(t(apply(coph_dist_girvan_code[-3], 1, sort)),coph_dist=coph_dist_girvan_code$coph_dist)
colnames(coph_dist_girvan_code) <- c("from","to","coph_dist")

write.csv(coph_dist_girvan_code,"Data/coph_dist_girvan_code.csv")

### 2. Greedy
##File to compile the cluster transition
memb_list <- cutree(dend_greedy, h =core_dist_greedy[length(core_dist_greedy)])
members_com_greedy <- as.data.frame(cbind(memb_list))
members_com_greedy$names <- rownames(members_com_greedy)
rownames(members_com_greedy) <- c(1:greedy_c$vcount)
members_com_greedy <- members_com_greedy[,c(2,1)]

coph_dist_greedy <- coph_dist_greedy %>%
  left_join(.,members_com_greedy,by=c("from"="names"))

coph_dist_greedy <- coph_dist_greedy %>%
  left_join(.,members_com_greedy,by=c("to"="names"))

coph_dist_greedy_code <- coph_dist_greedy[,c(4,5,3)]%>%
  distinct()

coph_dist_greedy_code <- data.frame(t(apply(coph_dist_greedy_code[-3], 1, sort)),coph_dist=coph_dist_greedy_code$coph_dist)
colnames(coph_dist_greedy_code) <- c("from","to","coph_dist")

write.csv(coph_dist_greedy_code,"Data/coph_dist_greedy_code.csv")


### 3. Random
##File to compile the cluster transition
memb_list <- cutree(dend_wtrap, h =core_dist_wtrap[length(core_dist_wtrap)])
members_com_wtrap <- as.data.frame(cbind(memb_list))
members_com_wtrap$names <- rownames(members_com_wtrap)
rownames(members_com_wtrap) <- c(1:wtrap_c$vcount)
members_com_wtrap <- members_com_wtrap[,c(2,1)]

coph_dist_wtrap <- coph_dist_wtrap %>%
  left_join(.,members_com_wtrap,by=c("from"="names"))

coph_dist_wtrap <- coph_dist_wtrap %>%
  left_join(.,members_com_wtrap,by=c("to"="names"))

coph_dist_wtrap_code <- coph_dist_wtrap[,c(4,5,3)]%>%
  distinct()

coph_dist_wtrap_code <- data.frame(t(apply(coph_dist_wtrap_code[-3], 1, sort)),coph_dist=coph_dist_wtrap_code$coph_dist)
colnames(coph_dist_wtrap_code) <- c("from","to","coph_dist")


write.csv(coph_dist_wtrap_code,"Data/coph_dist_wtrap_code.csv")

##----END Create community identity for each algorithm

##----BEGGINING MATCHING DISTANCES FROM INTERSECTION POINTS

##Loading points that belongs to clusters in the street percolation
points_hdbscan <- read_csv(here::here("Data","id_point_cluster.csv"),
                         locale = locale(encoding = "latin1"),
                         na = "n/a")
points_hdbscan <- as.data.frame(points_hdbscan)


##Loading all the information from the original dataset
original_info <- read_csv(here::here("Data","london_coordinates_clean.csv"),
                           locale = locale(encoding = "latin1"),
                           na = "n/a")
original_info <- as.data.frame(original_info)

original_info <- original_info%>%
  distinct(id, .keep_all = TRUE)


points_hdbscan <- points_hdbscan %>%
  left_join(.,original_info,by=c("id_point"="id"))


###Match with community codes

points_hdbscan <- points_hdbscan%>%
  left_join(.,members_com_girvan,by=c("LSOA11CD"="names"))%>%
  left_join(.,members_com_greedy,by=c("LSOA11CD"="names"))%>%
  left_join(.,members_com_wtrap,by=c("LSOA11CD"="names"))


write.csv(points_hdbscan,"Data/final.csv")

##Explore types between clusters from both methodologies

## 1. GIRVAN

girvan_corr <- points_hdbscan%>%
  group_by(street_cluster,girvan)%>%
  dplyr::summarise(count=n())

girvan_corr$element <- c(1:nrow(girvan_corr))
girvan_corr <- girvan_corr[,c(4,3,1,2)]

##Build the matrix with all the street intersection points
matrix_coph_1 <- matrix( 1,nrow = nrow(girvan_corr) , ncol = nrow(girvan_corr))
#diag(matrix_coph_1) <- 0
rownames(matrix_coph_1) <- girvan_corr$element
colnames(matrix_coph_1) <- girvan_corr$element

G_distances <- graph_from_adjacency_matrix(matrix_coph_1, mode ="undirected", weighted = TRUE)
coph_dist_gir <-  as_long_data_frame(G_distances)
coph_dist_gir <- coph_dist_gir[,c(4,5)]
colnames(coph_dist_gir) <-c("from","to") ##Verify the number of rows (n2+n)/2 of number of elements

##Match with the correlate variables
coph_dist_gir[,1] <- as.numeric(coph_dist_gir[,1])
coph_dist_gir[,2] <- as.numeric(coph_dist_gir[,2])

coph_dist_gir <- coph_dist_gir %>%
  left_join(.,girvan_corr,by=c("from"="element"))

coph_dist_gir <- coph_dist_gir %>%
  left_join(.,girvan_corr,by=c("to"="element"))

#Verify that this matches the 8,317,726,731 possible combinations of street intersections
coph_dist_gir <- coph_dist_gir%>%
  mutate(total=ifelse(from==to,count.x*(count.x-1)/2,count.x*count.y))

sum(coph_dist_gir$total) #8,317,726,731

#Reorder columns 
coph_dist_gir <- coph_dist_gir[,c(1,2,9,4,7,5,8)]

coph_dist_gir <- data.frame(coph_dist_gir[,c(1:3)],t(apply(coph_dist_gir[,c(6,7)], 1, sort)),t(apply(coph_dist_gir[,c(4,5)], 1, sort)))
colnames(coph_dist_gir) <- c("from","to","total","girvan.x","girvan.y","street_cluster.x","street_cluster.y")

##Compute the values to plot of correlation
coph_dist_gir <- coph_dist_gir%>%
  left_join(., coph_dist_girvan_code, by = c("girvan.x" = "from", "girvan.y" = "to"))

#Falta
coph_dist_gir <- coph_dist_gir%>%
  left_join(.,coph_street , by = c("street_cluster.x" = "from", "street_cluster.y" = "to"))

coph_dist_gir[is.na(coph_dist_gir)] <- 0

## 2. GREEDY

greedy_corr <- points_hdbscan%>%
  group_by(street_cluster,greedy)%>%
  dplyr::summarise(count=n())

greedy_corr$element <- c(1:nrow(greedy_corr))
greedy_corr <- greedy_corr[,c(4,3,1,2)]

##Build the matrix with all the street intersection points
matrix_coph_2 <- matrix( 1,nrow = nrow(greedy_corr) , ncol = nrow(greedy_corr))

rownames(matrix_coph_2) <- greedy_corr$element
colnames(matrix_coph_2) <- greedy_corr$element

G_distances <- graph_from_adjacency_matrix(matrix_coph_2, mode ="undirected", weighted = TRUE)
coph_dist_gre <-  as_long_data_frame(G_distances)
coph_dist_gre <- coph_dist_gre[,c(4,5)]
colnames(coph_dist_gre) <-c("from","to") ##Verify the number of rows (n2*+n)/2 of number of elements

##Match with the correlate variables
coph_dist_gre[,1] <- as.numeric(coph_dist_gre[,1])
coph_dist_gre[,2] <- as.numeric(coph_dist_gre[,2])

coph_dist_gre <- coph_dist_gre %>%
  left_join(.,greedy_corr,by=c("from"="element"))

coph_dist_gre <- coph_dist_gre %>%
  left_join(.,greedy_corr,by=c("to"="element"))

#Verify that this matches the 8,317,726,731 possible combinations of street intersections
coph_dist_gre <- coph_dist_gre%>%
  mutate(total=ifelse(from==to,count.x*(count.x-1)/2,count.x*count.y))

sum(coph_dist_gre$total) #6,386,590,671

#Reorder columns 
coph_dist_gre <- coph_dist_gre[,c(1,2,9,4,7,5,8)]

coph_dist_gre <- data.frame(coph_dist_gre[,c(1:3)],t(apply(coph_dist_gre[,c(6,7)], 1, sort)),t(apply(coph_dist_gre[,c(4,5)], 1, sort)))
colnames(coph_dist_gre) <- c("from","to","total","greedy.x","greedy.y","street_cluster.x","street_cluster.y")

##Compute the values to plot of correlation
coph_dist_gre <- coph_dist_gre%>%
  left_join(., coph_dist_greedy_code, by = c("greedy.x" = "from", "greedy.y" = "to"))


#Falta
coph_dist_gre <- coph_dist_gre%>%
  left_join(.,coph_street , by = c("street_cluster.x" = "from", "street_cluster.y" = "to"))

coph_dist_gre[is.na(coph_dist_gre)] <- 0


## 3. RANDOM

wtrap_corr <- points_hdbscan%>%
  group_by(street_cluster,wtrap)%>%
  dplyr::summarise(count=n())

wtrap_corr$element <- c(1:nrow(wtrap_corr))
wtrap_corr <- wtrap_corr[,c(4,3,1,2)]

##Build the matrix with all the street intersection points
matrix_coph_3 <- matrix( 1,nrow = nrow(wtrap_corr) , ncol = nrow(wtrap_corr))
rownames(matrix_coph_3) <- wtrap_corr$element
colnames(matrix_coph_3) <- wtrap_corr$element

G_distances <- graph_from_adjacency_matrix(matrix_coph_3, mode ="undirected", weighted = TRUE)
coph_dist_wtr <-  as_long_data_frame(G_distances)
coph_dist_wtr <- coph_dist_wtr[,c(4,5)]
colnames(coph_dist_wtr) <-c("from","to") ##Verify the number of rows (n2+n)/2 of number of elements

##Match with the correlate variables
coph_dist_wtr[,1] <- as.numeric(coph_dist_wtr[,1])
coph_dist_wtr[,2] <- as.numeric(coph_dist_wtr[,2])

coph_dist_wtr <- coph_dist_wtr %>%
  left_join(.,wtrap_corr,by=c("from"="element"))

coph_dist_wtr <- coph_dist_wtr %>%
  left_join(.,wtrap_corr,by=c("to"="element"))

#Verify that this matches the 8,317,726,731 possible combinations of street intersections
coph_dist_wtr <- coph_dist_wtr%>%
  mutate(total=ifelse(from==to,count.x*(count.x-1)/2,count.x*count.y))

sum(coph_dist_wtr$total) ##6,386,590,671

#Reorder columns 
coph_dist_wtr <- coph_dist_wtr[,c(1,2,9,4,7,5,8)]

coph_dist_wtr <- data.frame(coph_dist_wtr[,c(1:3)],t(apply(coph_dist_wtr[,c(6,7)], 1, sort)),t(apply(coph_dist_wtr[,c(4,5)], 1, sort)))
colnames(coph_dist_wtr) <- c("from","to","total","wtrap.x","wtrap.y","street_cluster.x","street_cluster.y")

##Compute the values to plot of correlation
coph_dist_wtr <- coph_dist_wtr%>%
  left_join(., coph_dist_wtrap_code, by = c("wtrap.x" = "from", "wtrap.y" = "to"))


#Falta
coph_dist_wtr <- coph_dist_wtr%>%
  left_join(.,coph_street, by = c("street_cluster.x" = "from", "street_cluster.y" = "to"))

coph_dist_wtr[is.na(coph_dist_wtr)] <- 0

##end

boxplot_df <- data.frame(method=rep("Random Walk",nrow(coph_dist_wtr)),distances=coph_dist_wtr[,8],total=coph_dist_wtr[,3])
boxplot_df <- rbind(boxplot_df,data.frame(method=rep("Girvan-Newman",nrow(coph_dist_gir)),distances=coph_dist_gir[,8],total=coph_dist_gir[,3]))
boxplot_df <- rbind(boxplot_df,data.frame(method=rep("Clauset-Newman-Moore",nrow(coph_dist_gre)),distances=coph_dist_gre[,8],total=coph_dist_gre[,3]))

boxplot_st <- data.frame(distances=coph_dist_wtr[,9],total=coph_dist_wtr[,3])


write.csv(coph_dist_wtr,"Data/wtrap_final.csv")
write.csv(coph_dist_gir,"Data/girvan_final.csv")
write.csv(coph_dist_gre,"Data/greedy_final.csv")
write.csv(boxplot_df,"Data/boxplot.csv")
write.csv(boxplot_st,"Data/boxplot_st.csv")



##Step distances

## 1. GIRVAN
##TEST
member1 <- as.data.frame(cbind(cutree(dend_girvan, h =core_dist_girvan[1])))
names <- rownames(member1)
rownames(member1) <- NULL
member1 <- cbind( names,member1)

for (i in 2:length(core_dist_girvan)) {
  new <- as.data.frame(cbind(cutree(dend_girvan, h =core_dist_girvan[i])))
  member1 <- cbind( member1,new)
}

rownames(member1) <- NULL

##Rename labels of clusters
ini <- 1
for (i in c(2:ncol(member1))) {
  n_c <- length(sort(unique(member1[,i])))
  end <- ini+n_c-1
  member1[,i]  <-  mapvalues(member1[,i],sort(unique(member1[,i])),c(ini:end))
  ini <- end+1
}
colnames(member1) <- c("member1",'V1','V1.1','V1.2','V1.3','V1.4','V1.5','V1.6','V1.7','V1.8','V1.9','V1.10','V1.11','V1.12','V1.13','V1.14','V1.15','V1.16','V1.17','V1.18','V1.19')


##Tree table
tree1 <- member1%>%
  #Get the names
  #str_c(colnames(member1[,c(2:ncol(member1))]),collapse="\',\'")
  group_by( V1,V1.1,V1.2,V1.3,V1.4,V1.5,V1.6,V1.7,V1.8,V1.9,V1.10,V1.11,V1.12,V1.13,V1.14,V1.15,V1.16,V1.17,V1.18,V1.19
    )%>%
  dplyr::summarise(count=n())

A <- vector()
B <- vector()

for (j in sort(c(2:(ncol(tree1)-1)),decreasing =TRUE)){
  subset <- tree1[!(is.na(tree1[,(j)])), c((j-1),j)]
  for (i in as.vector(unlist(unique(subset[,1])))) {
    if (length(as.vector(unlist(unique(subset[(subset[,1]==i),2]))))==1){
      tree1[(tree1[,(j-1)]==i)&!(is.na(tree1[,j])), j] <- NA
      k <- as.vector(unlist(unique(subset[(subset[,1]==i),2])))[1]
      A <- append(A,i )
      B <- append(B,k)
    }
  }
} 


tree1 <- tree1[,c(1:(ncol(tree1)-1))]%>%
  distinct()

tree1 <- tree1[rowSums(is.na(tree1)) != ncol(tree1), ]  

map_clusters1 <- data.frame(id_1=A,id_2=B)

#Verify number of cluster 2*C-1
length(sort(unique(unlist(tree1))))
nrow(tree1) ##number of communities

tree1 <- tree1%>%
  mutate(cluster=0)

for (i in 1:nrow(tree1)){
  tree1[i,ncol(tree1)] <- max(unique(sort(tree1[i,c(1:(ncol(tree1)-1))])))
}

##Create the combination between leafs

m1 <- matrix( 1,nrow = nrow(tree1) , ncol = nrow(tree1))
rownames(m1) <- tree1$cluster
colnames(m1) <- tree1$cluster

G_distances <- graph_from_adjacency_matrix(m1, mode ="undirected", weighted = TRUE)
coph_dist1 <-  as_long_data_frame(G_distances)
coph_dist1 <- coph_dist1[,c(4,5)]
colnames(coph_dist1) <-c("from","to") ##Verify the number of rows (n2+n)/2 of number of elements


coph_dist1[,1] <- as.numeric(coph_dist1[,1])
coph_dist1[,2] <- as.numeric(coph_dist1[,2])

##Reorder the tree

for (i in 1:nrow(tree1)){ 
  tree1[i,c(1:(ncol(tree1)-1))] <-  as.list(c(sort(unique(unlist(tree1[i,c(1:(ncol(tree1)-1))])))
                                              ,rep(NA,ncol(tree1)-1-length(sort(unique(unlist(tree1[i,c(1:(ncol(tree1)-1))])))))))
}

##Match to calculate step distances
coph_dist1 <- coph_dist1%>%
  left_join(.,tree1,by=c("from"="cluster"))%>%
  left_join(.,tree1,by=c("to"="cluster"))

##Calculate the cluster in which they merge
coph_dist1 <- coph_dist1%>%
  mutate(clust_mer=NA)

for (i in 1:nrow(coph_dist1)){
  coph_dist1[i,ncol(coph_dist1)] <-max(intersect(sort(coph_dist1[i,c(1:nrow(tree1)+2)]), 
                                                 sort(coph_dist1[i,c((nrow(tree1)+3):(ncol(coph_dist1)-1))])))
}

##Set the distance of the bottom ##Verify number ofCOLUMNS IMPORTAR
coph_dist1 <- coph_dist1%>%
  mutate(di1=NA,di2=NA,df1=NA,df2=NA)

for (i in 1:nrow(coph_dist1)){
  coph_dist1[i,44] <- match_col(coph_dist1[i,43],unlist(coph_dist1[i,c(3:22)]))
  coph_dist1[i,45] <- match_col(coph_dist1[i,43],unlist(coph_dist1[i,c(23:42)]))
  coph_dist1[i,46] <- match_col(coph_dist1[i,1],unlist(coph_dist1[i,c(3:22)]))
  coph_dist1[i,47] <- match_col(coph_dist1[i,2],unlist(coph_dist1[i,c(23:42)]))
}

coph_dist1 <- coph_dist1%>%
  mutate(dif1=df1-di1,dif2=df2-di2)

coph_dist1$coph_dist <- apply(coph_dist1[,c(48:49)], 1, max)

coph_dist_girvan_code <- coph_dist1[,c(1,2,ncol(coph_dist1))]
coph_dist_girvan_code <- data.frame(t(apply(coph_dist_girvan_code[-3], 1, sort)),coph_dist=coph_dist_girvan_code$coph_dist)
colnames(coph_dist_girvan_code) <- c("from","to","coph_dist")


##Create dataset with LSOAS and the cluster they belong
member1 <- member1%>%
  mutate(girvan=0)

for (i in 1:nrow(member1)){
  member1[i,ncol(member1)] <- intersect(member1[i,-ncol(member1)],tree1$cluster)
}

##Verify unique number of communities
length(unique(member1$girvan))

members_com_girvan <- member1[,c(1,ncol(member1))]
colnames(members_com_girvan) <- c("names","girvan")
  
## 2. GREEDY
##TEST
member2 <- as.data.frame(cbind(cutree(dend_greedy, h =core_dist_greedy[1])))
names <- rownames(member2)
rownames(member2) <- NULL
member2 <- cbind( names,member2)

for (i in 2:34) {
  new <- as.data.frame(cbind(cutree(dend_greedy, k =i)))
  member2 <- cbind( member2,new)
}

rownames(member2) <- NULL

##Rename labels of clusters
ini <- 1
for (i in c(2:ncol(member2))) {
  n_c <- length(sort(unique(member2[,i])))
  end <- ini+n_c-1
  member2[,i]  <-  mapvalues(member2[,i],sort(unique(member2[,i])),c(ini:end))
  ini <- end+1
}
colnames(member2) <- c("member2",'V1','V1.1','V1.2','V1.3','V1.4','V1.5','V1.6','V1.7','V1.8','V1.9','V1.10','V1.11','V1.12','V1.13','V1.14','V1.15','V1.16','V1.17','V1.18','V1.19','V1.20','V1.21','V1.22','V1.23','V1.24','V1.25','V1.26','V1.27','V1.28','V1.29','V1.30','V1.31','V1.32','V1.33')


##Tree table
tree2 <- member2%>%
  #Get the names
  #str_c(colnames(member2[,c(2:ncol(member2))]),collapse="\',\'")
  group_by( V1,V1.1,V1.2,V1.3,V1.4,V1.5,V1.6,V1.7,V1.8,V1.9,V1.10,V1.11,V1.12,V1.13,V1.14,V1.15,V1.16,V1.17,V1.18,V1.19,V1.20,V1.21,V1.22,V1.23,V1.24,V1.25,V1.26,V1.27,V1.28,V1.29,V1.30,V1.31,V1.32,V1.33
  )%>%
  dplyr::summarise(count=n())

A <- vector()
B <- vector()

for (j in sort(c(2:(ncol(tree2)-1)),decreasing =TRUE)){
  subset <- tree2[!(is.na(tree2[,(j)])), c((j-1),j)]
  for (i in as.vector(unlist(unique(subset[,1])))) {
    if (length(as.vector(unlist(unique(subset[(subset[,1]==i),2]))))==1){
      tree2[(tree2[,(j-1)]==i)&!(is.na(tree2[,j])), j] <- NA
      k <- as.vector(unlist(unique(subset[(subset[,1]==i),2])))[1]
      A <- append(A,i )
      B <- append(B,k)
    }
  }
} 


tree2 <- tree2[,c(1:(ncol(tree2)-1))]%>%
  distinct()

tree2 <- tree2[rowSums(is.na(tree2)) != ncol(tree2), ]  

map_clusters2 <- data.frame(id_1=A,id_2=B)

#Verify number of cluster 2*C-1
length(sort(unique(unlist(tree2))))
nrow(tree2) ##number of communities

tree2 <- tree2%>%
  mutate(cluster=0)

for (i in 1:nrow(tree2)){
  tree2[i,ncol(tree2)] <- max(unique(sort(tree2[i,c(1:(ncol(tree2)-1))])))
}

##Create the combination between leafs

m2 <- matrix( 1,nrow = nrow(tree2) , ncol = nrow(tree2))
rownames(m2) <- tree2$cluster
colnames(m2) <- tree2$cluster

G_distances <- graph_from_adjacency_matrix(m2, mode ="undirected", weighted = TRUE)
coph_dist2 <-  as_long_data_frame(G_distances)
coph_dist2 <- coph_dist2[,c(4,5)]
colnames(coph_dist2) <-c("from","to") ##Verify the number of rows (n2+n)/2 of number of elements


coph_dist2[,1] <- as.numeric(coph_dist2[,1])
coph_dist2[,2] <- as.numeric(coph_dist2[,2])

##Reorder the tree

for (i in 1:nrow(tree2)){ 
  tree2[i,c(1:(ncol(tree2)-1))] <-  as.list(c(sort(unique(unlist(tree2[i,c(1:(ncol(tree2)-1))])))
                                              ,rep(NA,ncol(tree2)-1-length(sort(unique(unlist(tree2[i,c(1:(ncol(tree2)-1))])))))))
}

##Match to calculate step distances
coph_dist2 <- coph_dist2%>%
  left_join(.,tree2,by=c("from"="cluster"))%>%
  left_join(.,tree2,by=c("to"="cluster"))

##Calculate the cluster in which they merge
coph_dist2 <- coph_dist2%>%
  mutate(clust_mer=NA)

for (i in 1:nrow(coph_dist2)){
  coph_dist2[i,ncol(coph_dist2)] <-max(intersect(sort(coph_dist2[i,c(1:nrow(tree2)+2)]), 
                                                 sort(coph_dist2[i,c((nrow(tree2)+3):(ncol(coph_dist2)-1))])))
}

##Set the distance of the bottom ##Verify number ofCOLUMNS IMPORTAR
coph_dist2 <- coph_dist2%>%
  mutate(di1=NA,di2=NA,df1=NA,df2=NA)

for (i in 1:nrow(coph_dist2)){
  coph_dist2[i,72] <- match_col(coph_dist2[i,71],unlist(coph_dist2[i,c(3:36)]))
  coph_dist2[i,73] <- match_col(coph_dist2[i,71],unlist(coph_dist2[i,c(37:70)]))
  coph_dist2[i,74] <- match_col(coph_dist2[i,1],unlist(coph_dist2[i,c(3:36)]))
  coph_dist2[i,75] <- match_col(coph_dist2[i,2],unlist(coph_dist2[i,c(37:70)]))
}

coph_dist2 <- coph_dist2%>%
  mutate(dif1=df1-di1,dif2=df2-di2)

coph_dist2$coph_dist <- apply(coph_dist2[,c(76:77)], 1, max)

coph_dist_greedy_code <- coph_dist2[,c(1,2,ncol(coph_dist2))]
coph_dist_greedy_code <- data.frame(t(apply(coph_dist_greedy_code[-3], 1, sort)),coph_dist=coph_dist_greedy_code$coph_dist)
colnames(coph_dist_greedy_code) <- c("from","to","coph_dist")


##Create dataset with LSOAS and the cluster they belong
member2 <- member2%>%
  mutate(greedy=0)

for (i in 1:nrow(member2)){
  member2[i,ncol(member2)] <- intersect(member2[i,-ncol(member2)],tree2$cluster)
}

##Verify unique number of communities
length(unique(member2$greedy))

members_com_greedy <- member2[,c(1,ncol(member2))]
colnames(members_com_greedy) <- c("names","greedy")

## 3. WTRAP
##TEST
member3 <- as.data.frame(cbind(cutree(dend_wtrap, h =core_dist_wtrap[1])))
names <- rownames(member3)
rownames(member3) <- NULL
member3 <- cbind( names,member3)

for (i in 2:168) {
  new <- as.data.frame(cbind(cutree(dend_wtrap, k =i)))
  member3 <- cbind( member3,new)
}

rownames(member3) <- NULL

##Rename labels of clusters
ini <- 1
for (i in c(2:ncol(member3))) {
  n_c <- length(sort(unique(member3[,i])))
  end <- ini+n_c-1
  member3[,i]  <-  mapvalues(member3[,i],sort(unique(member3[,i])),c(ini:end))
  ini <- end+1
}
colnames(member3) <- c("member3",'V1','V1.1','V1.2','V1.3','V1.4','V1.5','V1.6','V1.7','V1.8','V1.9','V1.10','V1.11','V1.12','V1.13','V1.14','V1.15','V1.16','V1.17','V1.18','V1.19','V1.20','V1.21','V1.22','V1.23','V1.24','V1.25','V1.26','V1.27','V1.28','V1.29','V1.30','V1.31','V1.32','V1.33','V1.34','V1.35','V1.36','V1.37','V1.38','V1.39','V1.40','V1.41','V1.42','V1.43','V1.44','V1.45','V1.46','V1.47','V1.48','V1.49','V1.50','V1.51','V1.52','V1.53','V1.54','V1.55','V1.56','V1.57','V1.58','V1.59','V1.60','V1.61','V1.62','V1.63','V1.64','V1.65','V1.66','V1.67','V1.68','V1.69','V1.70','V1.71','V1.72','V1.73','V1.74','V1.75','V1.76','V1.77','V1.78','V1.79','V1.80','V1.81','V1.82','V1.83','V1.84','V1.85','V1.86','V1.87','V1.88','V1.89','V1.90','V1.91','V1.92','V1.93','V1.94','V1.95','V1.96','V1.97','V1.98','V1.99','V1.100','V1.101','V1.102','V1.103','V1.104','V1.105','V1.106','V1.107','V1.108','V1.109','V1.110','V1.111','V1.112','V1.113','V1.114','V1.115','V1.116','V1.117','V1.118','V1.119','V1.120','V1.121','V1.122','V1.123','V1.124','V1.125','V1.126','V1.127','V1.128','V1.129','V1.130','V1.131','V1.132','V1.133','V1.134','V1.135','V1.136','V1.137','V1.138','V1.139','V1.140','V1.141','V1.142','V1.143','V1.144','V1.145','V1.146','V1.147','V1.148','V1.149','V1.150','V1.151','V1.152','V1.153','V1.154','V1.155','V1.156','V1.157','V1.158','V1.159','V1.160','V1.161','V1.162','V1.163','V1.164','V1.165','V1.166','V1.167')


##Tree table
tree3 <- member3%>%
  #Get the names
  #str_c(colnames(member3[,c(2:ncol(member3))]),collapse="\',\'")
  group_by( V1,V1.1,V1.2,V1.3,V1.4,V1.5,V1.6,V1.7,V1.8,V1.9,V1.10,V1.11,V1.12,V1.13,V1.14,V1.15,V1.16,V1.17,V1.18,V1.19,V1.20,V1.21,V1.22,V1.23,V1.24,V1.25,V1.26,V1.27,V1.28,V1.29,V1.30,V1.31,V1.32,V1.33,V1.34,V1.35,V1.36,V1.37,V1.38,V1.39,V1.40,V1.41,V1.42,V1.43,V1.44,V1.45,V1.46,V1.47,V1.48,V1.49,V1.50,V1.51,V1.52,V1.53,V1.54,V1.55,V1.56,V1.57,V1.58,V1.59,V1.60,V1.61,V1.62,V1.63,V1.64,V1.65,V1.66,V1.67,V1.68,V1.69,V1.70,V1.71,V1.72,V1.73,V1.74,V1.75,V1.76,V1.77,V1.78,V1.79,V1.80,V1.81,V1.82,V1.83,V1.84,V1.85,V1.86,V1.87,V1.88,V1.89,V1.90,V1.91,V1.92,V1.93,V1.94,V1.95,V1.96,V1.97,V1.98,V1.99,V1.100,V1.101,V1.102,V1.103,V1.104,V1.105,V1.106,V1.107,V1.108,V1.109,V1.110,V1.111,V1.112,V1.113,V1.114,V1.115,V1.116,V1.117,V1.118,V1.119,V1.120,V1.121,V1.122,V1.123,V1.124,V1.125,V1.126,V1.127,V1.128,V1.129,V1.130,V1.131,V1.132,V1.133,V1.134,V1.135,V1.136,V1.137,V1.138,V1.139,V1.140,V1.141,V1.142,V1.143,V1.144,V1.145,V1.146,V1.147,V1.148,V1.149,V1.150,V1.151,V1.152,V1.153,V1.154,V1.155,V1.156,V1.157,V1.158,V1.159,V1.160,V1.161,V1.162,V1.163,V1.164,V1.165,V1.166,V1.167
    )%>%
  dplyr::summarise(count=n())

A <- vector()
B <- vector()

for (j in sort(c(2:(ncol(tree3)-1)),decreasing =TRUE)){
  subset <- tree3[!(is.na(tree3[,(j)])), c((j-1),j)]
  for (i in as.vector(unlist(unique(subset[,1])))) {
    if (length(as.vector(unlist(unique(subset[(subset[,1]==i),2]))))==1){
      tree3[(tree3[,(j-1)]==i)&!(is.na(tree3[,j])), j] <- NA
      k <- as.vector(unlist(unique(subset[(subset[,1]==i),2])))[1]
      A <- append(A,i )
      B <- append(B,k)
    }
  }
} 


tree3 <- tree3[,c(1:(ncol(tree3)-1))]%>%
  distinct()

tree3 <- tree3[rowSums(is.na(tree3)) != ncol(tree3), ]  

map_clusters3 <- data.frame(id_1=A,id_2=B)

#Verify number of cluster 2*C-1
length(sort(unique(unlist(tree3))))
nrow(tree3) ##number of communities

tree3 <- tree3%>%
  mutate(cluster=0)

for (i in 1:nrow(tree3)){
  tree3[i,ncol(tree3)] <- max(unique(sort(tree3[i,c(1:(ncol(tree3)-1))])))
}

##Create the combination between leafs

m3 <- matrix( 1,nrow = nrow(tree3) , ncol = nrow(tree3))
rownames(m3) <- tree3$cluster
colnames(m3) <- tree3$cluster

G_distances <- graph_from_adjacency_matrix(m3, mode ="undirected", weighted = TRUE)
coph_dist3 <-  as_long_data_frame(G_distances)
coph_dist3 <- coph_dist3[,c(4,5)]
colnames(coph_dist3) <-c("from","to") ##Verify the number of rows (n2+n)/2 of number of elements


coph_dist3[,1] <- as.numeric(coph_dist3[,1])
coph_dist3[,2] <- as.numeric(coph_dist3[,2])

##Reorder the tree

for (i in 1:nrow(tree3)){ 
  tree3[i,c(1:(ncol(tree3)-1))] <-  as.list(c(sort(unique(unlist(tree3[i,c(1:(ncol(tree3)-1))])))
                                              ,rep(NA,ncol(tree3)-1-length(sort(unique(unlist(tree3[i,c(1:(ncol(tree3)-1))])))))))
}

##Match to calculate step distances
coph_dist3 <- coph_dist3%>%
  left_join(.,tree3,by=c("from"="cluster"))%>%
  left_join(.,tree3,by=c("to"="cluster"))

##Calculate the cluster in which they merge
coph_dist3 <- coph_dist3%>%
  mutate(clust_mer=NA)

for (i in 1:nrow(coph_dist3)){
  coph_dist3[i,ncol(coph_dist3)] <-max(intersect(sort(coph_dist3[i,c(1:nrow(tree3)+2)]), 
                                                 sort(coph_dist3[i,c((nrow(tree3)+3):(ncol(coph_dist3)-1))])))
}

##Set the distance of the bottom ##Verify number ofCOLUMNS IMPORTAR
coph_dist3 <- coph_dist3%>%
  mutate(di1=NA,di2=NA,df1=NA,df2=NA)

for (i in 1:nrow(coph_dist3)){
  coph_dist3[i,340] <- match_col(coph_dist3[i,339],unlist(coph_dist3[i,c(3:170)]))
  coph_dist3[i,341] <- match_col(coph_dist3[i,339],unlist(coph_dist3[i,c(171:338)]))
  coph_dist3[i,342] <- match_col(coph_dist3[i,1],unlist(coph_dist3[i,c(3:170)]))
  coph_dist3[i,343] <- match_col(coph_dist3[i,2],unlist(coph_dist3[i,c(171:338)]))
}

coph_dist3 <- coph_dist3%>%
  mutate(dif1=df1-di1,dif2=df2-di2)

coph_dist3$coph_dist <- apply(coph_dist3[,c(344:345)], 1, max)

coph_dist_wtrap_code <- coph_dist3[,c(1,2,ncol(coph_dist3))]
coph_dist_wtrap_code <- data.frame(t(apply(coph_dist_wtrap_code[-3], 1, sort)),coph_dist=coph_dist_wtrap_code$coph_dist)
colnames(coph_dist_wtrap_code) <- c("from","to","coph_dist")

##Create dataset with LSOAS and the cluster they belong
member3 <- member3%>%
  mutate(wtrap=0)

for (i in 1:nrow(member3)){
  member3[i,ncol(member3)] <- intersect(member3[i,-ncol(member3)],tree3$cluster)
}

##Verify unique number of communities
length(unique(member3$wtrap))

members_com_wtrap <- member3[,c(1,ncol(member3))]
colnames(members_com_wtrap) <- c("names","wtrap")

##TEST


memb_list <- cutree(dend_girvan, h =core_dist_girvan[length(core_dist_girvan)])
members_com_girvan <- as.data.frame(cbind(memb_list))
members_com_girvan$names <- rownames(members_com_girvan)
rownames(members_com_girvan) <- c(1:girvan$vcount)
members_com_girvan <- members_com_girvan[,c(2,1)]

##DATA FOR SILHOUETTE
silhouette <- points_hdbscan[,c(1:3)]%>%
  left_join(.,census_lsoas,by=c("LSOA11CD"="LSOA11CD"))

silhouette <- silhouette %>% 
  # Base the removal on the "Age" column
  distinct(id_point, .keep_all = TRUE)

silhouette <- silhouette %>% 
  # Base the removal on the "Age" column
  distinct(street_cluster,LSOA11CD, .keep_all = TRUE)

silhouette <- silhouette[,c(1,4:16,2)]
colnames(silhouette) <-c( "id_point","comp_1","comp_2","comp_3","comp_4" ,
                          "comp_5","comp_6","comp_7","comp_8","comp_9",
                          "comp_10","comp_11","comp_12","comp_13","street_cluster")
##Rename labels of clusters from 1 to 200
silhouette[,15]  <-  mapvalues(silhouette[,15],sort(unique(silhouette[,15])),c(1:200))

write.csv(silhouette,"data_silhouette_red.csv",row.names = FALSE)

##Silhouette analysis social

rownames(silhouette) <- silhouette[,1]
labels <- as.vector(unlist(silhouette[,15]))
silhouette <- silhouette[,c(-1,-15)]

##Scores

library("cluster")
sil <- silhouette(labels, dist(silhouette))
head(sil[, 1:3], 10)

plot(sil, main ="Silhouette plot")

si.sum <- summary(sil)
# Average silhouette width of each cluster
si.sum$clus.avg.widths

si.sum$avg.width

##Silhouette analysis physical

silhouette1 <- points_hdbscan[,c(1,2,4,5)]

silhouette1 <- silhouette1 %>% 
  # Base the removal on the "Age" column
  distinct(id_point, .keep_all = TRUE)

silhouette1[,2]  <-  mapvalues(silhouette1[,2],sort(unique(silhouette1[,2])),c(1:200))

labels1 <- as.vector(unlist(silhouette[,2]))
rownames(silhouette1) <- silhouette1[,1]
silhouette1 <- scale(silhouette1[,c(3,4)])


write.csv(silhouette1,"data_silhouette_physical.csv",row.names = FALSE)


##Scores

library("cluster")
sil <- silhouette(labels, dist(silhouette1))
head(sil[, 1:3], 10)

plot(sil, main ="Silhouette plot")

si.sum <- summary(sil)
# Average silhouette width of each cluster
si.sum$clus.avg.widths

si.sum$avg.width
