
###to be completed.
##LA-ICPMS analysis 
###Processed data from code modified from TERMITE code Mischel SA, Mertz-Kraus R, Jochum KP & Scholz D (2017) TERMITE: An R Script for Fast Reduction of Laser Ablation Inductively Coupled Plasma Mass Spectrometry Data and its Application to Trace Element Measurements. Rapid Communications in Mass Spectrometry: RCM 31: 1079–1087
##Note that files were split into sherds with LAtools ##Then TERMITE used to get background subtracted values then calibrated with standards see linescan.R files

#want to compare the element concentrations of Vao and Teouma iron oxides
#make matrix of ppm 


library(tidyverse)
library(dplyr)
library(tidyheatmap)

path			            <- "~/TERMITE-master/your_main_directory/"	
path.corrData.results	<- "Results/" 
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

place="Agarabi"
sherd <-"NIE328"
sample.name <- "NIE328"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep(sample.name, length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NIE328 <- data_tidy
sample.name <- "NIE328_01"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep(sample.name, length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NIE328_01 <- data_tidy
sherd <-"NNF2"
sample.name <- "PNG16"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NNF2", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NNF2 <- data_tidy
sample.name <- "PNG16_01"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NNF2_01", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NNF2_01 <- data_tidy
sherd <-"NOU1"
sample.name <- "PNG18"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NOU1", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NOU1 <- data_tidy
sample.name <- "PNG18_01"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NOU1_01", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NOU1_01 <- data_tidy
place="Adzera"
sherd <-"NKD44"
sample.name <- "NKD44"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep(sample.name, length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NKD44 <- data_tidy
sample.name <- "NKD44_01"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep(sample.name, length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NKD44_01 <- data_tidy
sherd <-"NME2"
sample.name <- "PNG36"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NME2", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NME2 <- data_tidy
sample.name <- "PNG36_01"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NME2_01", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NME2_01 <- data_tidy
sherd <-"NLQ15"
sample.name <- "PNG37"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NLQ15", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NLQ15 <- data_tidy
sample.name <- "PNG37_01"
df <- as.data.frame(loadRData(file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name)))
data_tidy = df %>% as_tibble(rownames="Length") %>% gather(Element, Value, -`Length`)
data_tidy$sample <- rep("NLQ15_01", length(data_tidy[,1]))
data_tidy$sherd <- rep(sherd, length(data_tidy[,1]))
data_tidy$type <- rep(place, length(data_tidy[,1]))
NLQ15_01 <- data_tidy

all_1 <- dplyr::bind_rows(NIE328,NNF2_01,NOU1,NKD44,NME2,NLQ15)
all_1$uni <- paste(all_1$sample, all_1$Length, sep="_")
ann_colors <- list(sherd = c(NIE328 = "purple", NNF2 = "light blue",NOU1 = "blue",NKD44 = "yellow", NME2 = "orange", NLQ15="red"))
#tidy_heatmap(all_1, columns=Element,rows=uni,values=Value, annotation_row = sherd,colors=c("dark blue", "blue","white", "red","dark red"), scale="column",annotation_colors = ann_colors,cluster_cols = TRUE, cluster_rows=FALSE, color_legend_n=20,angle_col=45,annotation_legend=TRUE ,color_legend_min = -3,
#             color_legend_max = 3,filename = "PNG_sherd_outside_in.pdf", show_rownames = FALSE)
all_2 <- dplyr::bind_rows(NIE328_01,NNF2,NOU1_01,NKD44_01,NME2_01,NLQ15_01)
all_2$uni <- paste(all_2$sample, all_2$Length, sep="_")
#tidy_heatmap(all_2, columns=Element,rows=uni,values=Value, annotation_row = sherd,colors=c("dark blue", "blue","white", "red","dark red"), scale="column",annotation_colors = ann_colors,cluster_cols = TRUE, cluster_rows=FALSE, color_legend_n=20,angle_col=45,annotation_legend=TRUE ,color_legend_min = -3,
#             color_legend_max = 3,filename = "PNG_sherd_inside_out.pdf", show_rownames = FALSE)
all <- dplyr::bind_rows(all_1,all_2)
#get median and sd of each run

grouped <- group_by(all,  sample, Element)
grouped <- na.omit(grouped)
sorting_Z <- summarise(grouped, median=median(Value), sd=sd(Value), mean=mean(Value))
sorting_Z$sample <- factor(sorting_Z$sample,levels=c("NIE328","NIE328_01","NNF2","NNF2_01","NOU1","NOU1_01","NKD44","NKD44_01","NME2","NME2_01","NLQ15","NLQ15_01"))
test <- arrange(sorting_Z, sample)
test$group <- rep("Agarabi", length(test[,1]))
n <- test$sample%in%c("NKD44","NKD44_01","NME2","NME2_01","NLQ15","NLQ15_01")
test$group[n] <- "Adzera"
tidy_heatmap(test, columns=Element,rows=sample,values=median,colors=c("dark blue", "blue","white", "red","dark red"), scale="column",cluster_cols = TRUE, cluster_rows=FALSE, color_legend_n=20,angle_col=45 ,annotation_colors = list(group=c(Agarabi="purple", Adzera="orange")),color_legend_min = -3,color_legend_max = 3,show_rownames = TRUE,annotation_row = group)

all$sherd <- factor(all$sherd, levels= c("NIE328", "NNF2","NOU1" ,"NKD44", "NME2", "NLQ15"))
element <- "U238"
n <- which(all$Element==element)
ggplot(all[n,], aes(x=sherd,y=Value, fill=type)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = c(0, 8))+ scale_fill_manual(values=c("orange","purple")) +theme_classic()

element <- "Mg24"
n <- which(all$Element==element)
ggplot(all[n,], aes(x=sherd,y=Value, fill=type)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = c(0, 60000))+ scale_fill_manual(values=c("orange","purple")) +theme_classic()

element <- "Co59"
n <- which(all$Element==element)
ggplot(all[n,], aes(x=sherd,y=Value, fill=type)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = c(0, 125))+ scale_fill_manual(values=c("orange","purple")) +theme_classic()

element <- "Nb93"
n <- which(all$Element==element)
ggplot(all[n,], aes(x=sherd,y=Value, fill=type)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = c(0, 20))+ scale_fill_manual(values=c("orange","purple")) +theme_classic()

element <- "Th232"
n <- which(all$Element==element)
ggplot(all[n,], aes(x=sherd,y=Value, fill=type)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = c(0, 25))+ scale_fill_manual(values=c("orange","purple")) +theme_classic()

element <- "Ce140"
n <- which(all$Element==element)
ggplot(all[n,], aes(x=sherd,y=Value, fill=type)) + geom_boxplot(outlier.shape = NA)+coord_cartesian(ylim = c(0, 150))+ scale_fill_manual(values=c("orange","purple")) +theme_classic()


