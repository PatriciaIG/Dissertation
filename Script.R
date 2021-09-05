#Libraries

library(igraph)
library(reshape2)
library(plyr)
library(rgdal)
library(Polychrome)
library(dplyr)

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
G=simplify(G, remove.loops = TRUE)
sum(which_loop(G, eids = E(G)))

#Is it a connected network or does it have disconnected parts?

components(G)$no
summary(components(G))


lay_test <- data.frame(id_point=as.integer(as_ids(V(G))))

lay_test <- lay_test %>%
  left_join(.,data_coords,by=c("id_point"="id")) 

lay_test <- lay_test[,c(1,3,4)] %>%
  distinct()

lay_plot <- as.matrix(lay_test[,c(2,3)])

plot(G, layout = lay_plot,vertex.size=0.1,vertex.color="red",vertex.label="") #edge.color=NA


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

#Directories for results
dir_res <- "Results/"
dir.create(dir_res)
#and the directory where you will create your membership tables
dir_memb <- paste0(dir_res,"membTables/")
dir.create(dir_memb)

file_n_clust <- paste0(dir_res,"n_clusters_p")

#file for the largest cluster size
file_clust_size <- paste0(dir_res,"clust_p_size.txt")

#Now a directory for maps
dir_maps <- paste0(dir_res,"maps/")
dir.create(dir_maps)

rmin=10
rmax=501
r0=c(40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,135,145,155,165,175,185,195,205,215,225,235,245,255,265,275,285,295,305,315,340,365,390,415,440,465,515,565,615,690,765,840,865)
n_loops=length(r0)
write('threshold\t size',file=file_clust_size,append = FALSE)

#define the colours for the 10 top clusters
top10 = palette36.colors(10)
top20 = palette36.colors(20)
top30 = palette36.colors(30)
top40 = palette36.colors(40)
#visualise the colours
barplot(c(1:10), col=top10)
barplot(c(1:20), col=top20)
barplot(c(1:30), col=top30)
barplot(c(1:40), col=top40)

jumps_2plot=c(40,45,50,55,60,65,70,75,80,85,90,95,100,105,110,115,120,125,135,145,155,165,175,185,195,205,215,225,235,245,255,265,275,285,295,305,315,340,365,390,415,440,465,515,565,615,690,765,840,865)

##write_graph(g_test, "g_test.txt", "ncol",weights="weight")

for (i_t in r0)
{
  #find subgraph such that all weights <= threshold r0
  g <- subgraph.edges(G, E(G)[weight<=i_t],delete.vertices = TRUE)
  #take subcomponents
  membclusters <- clusters(g, mode="weak")$membership
  head(membclusters)
  m <- cbind(V(g)$name,membclusters)
  colnames(m) <- c("id_point","id_cluster")
  head(m)
  #file for membership table for each threshold
  file_name <- paste0(dir_memb,"London_p")
  
  file_memb <- paste0(file_name,i_t,".txt")
  write.table(m,file_memb,col.names = TRUE, sep=",",row.names=FALSE)
  
  M_data <- as.data.frame(m)
  head(M_data)
  table_data <- table(M_data$id_cluster)
  head(table_data)
  n_cluster <- length(table_data)
  
  #Minimum number of points to define clusters n_min
  n_min <- 35
  sig_cluster <- as.data.frame(sort(table_data,decreasing = TRUE)) %>%
                  dplyr::filter(Freq>=n_min)
  NSC <- nrow(sig_cluster)
  NSC_r <- NSC/n_cluster
  NSP <- sum(sig_cluster$Freq)
  NSP_r=NSP/size_net
  
  SC=c(i_t,NSC,NSC_r,NSP,NSP_r)
  
  if(i_t==rmin)
  {
    SC_t=SC
  }else{
    SC_t=rbind(SC_t,SC)
  }
  
  #Largest connected component
  LCC=unname(sort(table_data,decreasing = TRUE)[1])
  LCC_p=LCC/size_net
  v_LCC=c(i_t,LCC,LCC_p,n_cluster)
  if(i_t==rmin)
  {
    v_LCC_t=v_LCC
  }else{
    v_LCC_t=rbind(v_LCC_t,v_LCC)
  }
  ##--
  #Second Largest connected component
  SLC=unname(sort(table_data,decreasing = TRUE)[2])
  SLC_p=LCC/size_net
  v_SLC=c(i_t,SLC,SLC_p)
  if(i_t==rmin)
  {
    v_SLC_t=v_SLC
  }else{
    v_SLC_t=rbind(v_SLC_t,v_SLC)
  }
  ##--
  #Third Largest connected component
  TLC=unname(sort(table_data,decreasing = TRUE)[3])
  TLC_p=LCC/size_net
  v_TLC=c(i_t,TLC,TLC_p)
  if(i_t==rmin)
  {
    v_TLC_t=v_TLC
  }else{
    v_TLC_t=rbind(v_TLC_t,v_TLC)
  }
  ##--
  sorted_table <- sort(table_data,decreasing = T)
  head(sorted_table)
  file_out <- paste(file_n_clust,i_t,".txt",sep="")
  write.table(sorted_table,file_out,row.names=FALSE,col.names=c('id_cluster','n_points'))
  
  #let us construct at the same time the file with the largest cluster size, i.e. connected component.
  write(c(i_t,LCC),file=file_clust_size,append = TRUE)
  
  if(i_t %in% jumps_2plot)
  {
    #----------- assign colours
    #Let us get the top 40 clusters
    list_clusts <- as.data.frame(sorted_table)
    colnames(list_clusts) <- c("id_cluster","n_points")
    head(list_clusts)
    list_clusts$colour = "grey"
    list_clusts$colour[1:40] = top40
    list_clusts$size = 0.1
    list_clusts$size[1:40] = 0.2
    list_clusts[1:40,]
    head(m)
    total_list <- merge(list_clusts,m,by="id_cluster")
    head(total_list)
    colnames(data_coords) <- c("id_point","LSOA11CD","x","y")
    points_coords_cols <-  merge(total_list,data_coords,by="id_point")
    head(points_coords_cols)
    file_map <- paste0(dir_maps,"London_d",i_t,".png")
    
    #remove the # sign in the next line whrn you rin it in your computer to safe the maps
    #png(file_map,height=850,width=1000)
    par(bg= "white")
    plot(points_coords_cols$x,points_coords_cols$y,xlab=NA,ylab=NA,pch=16,cex=as.numeric(points_coords_cols$size),axes=FALSE, frame.plot=F,col=points_coords_cols$colour)
    title(main = paste0("London at d=",i_t,"m"),col.main= "black")
    #remove the # sign in the next line whrn you rin it in your computer to safe the maps
    #dev.off()
    
  }
}

#Evolution of the largest component
par(bg= "white")
plot(v_LCC_t[,1],v_LCC_t[,2],xlab="distance",ylab="size",pch=16,cex=0.5,panel.first=grid())
title("Evolution of Largest Connected Component")

#Evolution of the largest component normalised
plot(v_LCC_t[,1],v_LCC_t[,3],xlab="distance",ylab="size",pch=16,cex=0.5,panel.first=grid())
title("Evolution of Largest Connected Component normalised")

#Evolution of the second largest component
par(bg= "white")
plot(v_SLC_t[,1],v_SLC_t[,2],xlab="distance",ylab="size",pch=16,cex=0.5,panel.first=grid())
title("Evolution of the Second Largest Connected Component")

#Evolution of the second largest component normalised
plot(v_SLC_t[,1],v_SLC_t[,3],xlab="distance",ylab="size",pch=16,cex=0.5,panel.first=grid())
title("Evolution of the Second Largest Connected Component normalised")

#Evolution of the third largest component
par(bg= "white")
plot(v_TLC_t[,1],v_TLC_t[,2],xlab="distance",ylab="size",pch=16,cex=0.5,panel.first=grid())
title("Evolution of the Third Largest Connected Component")

#Evolution of the third largest component normalised
plot(v_TLC_t[,1],v_TLC_t[,3],xlab="distance",ylab="size",pch=16,cex=0.5,panel.first=grid())
title("Evolution of the Third Largest Connected Component normalised")


##----------Counts of connected components through the thresholds
#Evolution of the number of components
plot(v_LCC_t[,1],v_LCC_t[,4],xlab="distance",ylab="number of components",pch=16,cex=0.5,panel.first=grid())
title("Evolution of the number of Components")

plot(SC_t[,1],SC_t[,2],xlab="distance",ylab="number of significant components",pch=16,cex=0.5,panel.first=grid())
title("Evolution of the significant number of Components")


##----------Task 1: Significative size of clusters
#Evolution of the number of components
plot(SC_t[,1],SC_t[,2],xlab="distance",ylab="number of significative components",pch=16,cex=0.5,panel.first=grid())
title("Evolution of SC with minimum size of 30 points")

##----------Task 1: Significative size of clusters
#Evolution of the number of components
plot(SC_t[,1],SC_t[,3],xlab="distance",ylab="% significative components",pch=16,cex=0.5,panel.first=grid())
title("Evolution of SC with minimum size of 30 points")

##----------Task 1: Significative size of clusters
#Evolution of the number of components
plot(SC_t[,1],SC_t[,4],xlab="distance",ylab="% significative components",pch=16,cex=0.5,panel.first=grid())
title("Evolution of SC with minimum size of 30 points")


lay_test <- data.frame(id_point=as.integer(as_ids(V(G))))

lay_test <- lay_test %>%
  left_join(.,data_coords,by=c("id_point"="id")) 

lay_plot <- as.matrix(lay_test[,c(3,4)])

plot(G, layout = lay_plot,vertex.size=1,vertex.color="black",vertex.label="")


louvain <- multilevel.community(G)

layout<- as.matrix(lay_plot[,-1])
dendPlot(louvain, mode="hclust")
plot(G_lsoas,layout = layout,vertex.size=0.1,edge.color=NA,vertex.label="",mark.groups=c("E01000748","E01000749","E01000680"))
