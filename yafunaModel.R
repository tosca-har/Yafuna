
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidymodels)
library(Hmisc)
tidymodels_prefer()
#just using tidymodels to make testing, training, assessment etc sets.

sherds <- read.delim("Sherds.txt", stringsAsFactors=FALSE)
t <- table(sherds$Site) # total sherd numbers
locations <- read.delim("locations.txt")

AgarabiOrder <- c("NIB","NIC","NIE", "NNF","NNQ","NNR","NNE","NVM","NVW","NMI","NOT","NOU","NOV","NOX","NWO","NID")
yonkiOrder <- c("NKC", "NKD","NNH", "NKE", "NNI", "NNG", "NKF", "NNJ", "NNX", "NTQ", "NVF")
southOrder <- c("NDO", "NGM" ,"NIO" ,"NMK")
aronaOrder <- c("NME","NMJ")
eastOrder <- c("NOQ","NLO","NMT" ,"NMN" )
binOrder <- c("NLQ" ,"NLX", "NML" )
pun <- "NMH"
siteOrder <- c("NNA", AgarabiOrder, yonkiOrder,southOrder, aronaOrder,eastOrder,pun,binOrder)

sherds$Group <- locations$Group[match(sherds$Site, locations$Symbol)]
sherds$Group <- recode(sherds$Group, purple="NNA",blue = "North", green="Central", orange="South", red="East" )
sherds$Group <- factor(sherds$Group, levels=c("NNA","North", "Central","South", "East"))

### clean up variables and make dataframe
outcome <- recode(sherds$Agarabi_Adzera,Agarabi="not_Adzera",notAdzera = "not_Adzera", AMR="not_Adzera") # don't distinguish Agaragi, AMR or not Adzera
inclusions <- recode(sherds$Inclusions, Sparse=-1, Moderate=0, Abundant=1)
burnish_outside <- recode(sherds$Burnished_outside, N=-1, Y=1, .default=0)
burnish_inside <- recode(sherds$Burnished_inside, N=-1, Y=1, .default=0)
burnish_either <- pmax(burnish_outside, burnish_inside) #burnished either side, take max value
thickness <- sherds$max.thickness
colour_outside <- recode(sherds$exterior.colour, tan= "tan", tan1="tan1",sienna1="sienna1",salmon2="salmon2", sienna3="sienna3", salmon3="salmon3",sienna4="sienna4", salmon4="salmon4", tan4="tan4", grey="grey", "dark grey"="dark grey", FIRE="NA")
colour_inside <- recode(sherds$interior, tan= "tan", tan1="tan1",sienna1="sienna1",salmon2="salmon2", sienna3="sienna3", salmon3="salmon3",sienna4="sienna4", salmon4="salmon4", tan4="tan4", grey="grey", "dark grey"="dark grey", FIRE="NA")
colour_core <- recode(sherds$core, tan= "tan", tan1="tan1",sienna1="sienna1",salmon2="salmon2", sienna3="sienna3", salmon3="salmon3",sienna4="sienna4", salmon4="salmon4", tan4="tan4", grey="grey", "dark grey"="dark grey", FIRE="NA", U="NA", ND="NA")
site <- sherds$Site

df <- data.frame(outcome=outcome, inclusion=inclusions,burnish_outside=burnish_outside,burnish_inside=burnish_inside,burnish_either=burnish_either,thickness=thickness, colour_outside=colour_outside,colour_inside=colour_inside,colour_core=colour_core, site=site)
#remove fire damaged
filtered <- df %>% filter(colour_outside !="NA",colour_inside !="NA",colour_core !="NA",inclusion != "NA")

filtered_known <- filter(filtered, outcome != "")
filtered_unknown <- filter(filtered, outcome == "")
df_known <-  filter(df, outcome != "")
summar2 <- filtered_known %>% summarise(n = n(),outcome=sum(outcome=="Adzera")/n()*100) # finds % of Adzera in diagnostic
av=summar2$outcome #% of Adzera in diagnostic
m <- 100 #ylim and for printing total number of sherds up top of plot

#Plots % Adzera with Sparse, Moderate, Abundant inclusions. Prints total number of all sherds up top.
incl <- filtered_known %>% group_by(inclusion) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
incl$inclusion <- incl$inclusion %>% recode( "-1"="Sparse", "0"="Moderate" ,"1"= "Abundant") %>% factor(levels=c("Sparse","Moderate","Abundant"))
c <- seq(from=1, to=length(incl$n))
ggplot(data=incl, aes(x=inclusion, y=Adzera)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) + scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Inclusions")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(incl$n), size=2, vjust=1)
ggsave("inclu2.png", width = 3, height = 5, units = "cm")

#Plots % Adzera with burnished exerior. Prints total number of all sherds up top.
b_o <- filtered_known %>% group_by(burnish_outside) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
b_o$burnish_outside <- b_o$burnish_outside %>% recode( "-1"="Rough", "0"="Inbetween" ,"1"= "Burnished") %>% factor(levels=c("Rough","Inbetween","Burnished"))
c <- seq(from=1, to=length(b_o$n))
ggplot(data=b_o, aes(x=burnish_outside, y=Adzera)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) + scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Burnish exterior")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(b_o$n), size=2, vjust=1)
ggsave("b_o2.png", width = 3, height = 5, units = "cm")

#Plots % Adzera with burnished interior. Prints total number of all sherds up top.
b_i <- filtered_known %>% group_by(burnish_inside) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
b_i$burnish_inside <- b_i$burnish_inside %>% recode( "-1"="Rough", "0"="Inbetween" ,"1"= "Burnished") %>% factor(levels=c("Rough","Inbetween","Burnished"))
c <- seq(from=1, to=length(b_i$n))
ggplot(data=b_i, aes(x=burnish_inside, y=Adzera)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) + scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Burnish interior")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(b_i$n), size=2, vjust=1)
ggsave("b_i2.png", width = 3, height = 5, units = "cm")

#Plots % Adzera with burnished interior or exterior. Prints total number of all sherds up top.
b_e <- filtered_known %>% group_by(burnish_either) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
b_e$burnish_either <- b_e$burnish_either %>% recode( "-1"="Rough", "0"="Inbetween" ,"1"= "Burnished") %>% factor(levels=c("Rough","Inbetween","Burnished"))
c <- seq(from=1, to=length(b_e$n))
ggplot(data=b_e, aes(x=burnish_either, y=Adzera)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) + scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Burnish either")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(b_e$n), size=2, vjust=1)
ggsave("b_e2.png", width = 3, height = 5, units = "cm")

#Plots % Adzera by colour outside. Prints total number of all sherds up top.
c_o <- filtered_known %>% group_by(colour_outside) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
c_o$colour_outside <- c_o$colour_outside %>% factor(levels=c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey","dark grey"))
c <- seq(from=1, to=length(c_o$n))
cols <- c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey","dark grey")
c_o <- arrange(c_o, factor(colour_outside, levels = cols)) 
cols <- c("tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey58","grey36")
ggplot(data=c_o, aes(x=colour_outside, y=Adzera, fill=colour_outside)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) +scale_fill_manual(values=cols)+ scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Colour exterior")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(c_o$n), size=2, vjust=1)
ggsave("c_o2.png", width = 5, height = 5, units = "cm")

#Plots % Adzera by colour inside. Prints total number of all sherds up top.
c_i <- filtered_known %>% group_by(colour_inside) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
c_i$colour_inside <- c_i$colour_inside %>% factor(levels=c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey","dark grey"))
c <- seq(from=1, to=length(c_i$n))
cols <- c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey","dark grey")
c_i<- arrange(c_i, factor(colour_inside, levels = cols)) 
cols <- c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey58","grey36")
ggplot(data=c_i, aes(x=colour_inside, y=Adzera, fill=colour_inside)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) +scale_fill_manual(values=cols)+ scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Colour interior")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(c_i$n), size=2, vjust=1)
ggsave("c_i2.png", width = 5, height = 5, units = "cm")

#Plots % Adzera by core colour. Prints total number of all sherds up top.
c_c <- filtered_known %>% group_by(colour_core) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
c_c$colour_core <- c_c$colour_core %>% factor(levels=c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey","dark grey"))
c <- seq(from=1, to=length(c_c$n))
cols <- c("tan","tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey","dark grey")
c_c<- arrange(c_c, factor(colour_core, levels = cols)) 
cols <- c("tan1","sienna1","salmon2","sienna3", "salmon3","sienna4","salmon4","tan4", "grey58","grey36")
ggplot(data=c_c, aes(x=colour_core, y=Adzera, fill=colour_core)) +geom_bar(stat="identity", color="black", size=0.3,show.legend = FALSE)+ labs(x=" ", y = "Adzera (%)")+
  geom_hline(yintercept = av, col="red")+coord_cartesian(ylim=c(0, m), expand=FALSE) +scale_fill_manual(values=cols)+ scale_y_continuous(breaks=seq(from=0, to=m, by=20))+ labs(title="Colour core")+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = m, label = as.vector(c_c$n), size=2, vjust=1)
ggsave("c_c2.png", width = 5, height = 5, units = "cm")

ggplot(data=filtered_known, aes(x=outcome, y=thickness)) +geom_dotplot(binwidth=0.5,binaxis='y',stackdir="center",dotsize=.2, position=position_dodge(0.8))+ labs(x=" ", y="Thickness (mm)")+coord_cartesian(ylim=c(0, 25), expand=FALSE) + scale_y_continuous(breaks=seq(from=0, to=25, by=5))+ labs(title="Thickness")+ scale_x_discrete(labels=c("Adzera" = "Adzera", "not_Adzera" = "Not Adzera"))+stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),geom="pointrange",color="red",size=.1, shape=2)+theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 8),axis.text.x = element_text(angle =90,vjust=0.5, size=8),axis.ticks = element_line(colour = "black", size = 0.2))
ggsave("thick2.png", width =2.2, height = 5, units = "cm")
medianA <- median(filtered_known$thickness[filtered_known$outcome=="Adzera"])

## grouping of like colours ("dark grey", "sienna4", "salmon4") ("sienna3", "sienna1", "tan1","tan") ("grey", "salmon2", "salmon3", "tan4")

filtered_known$colour_outside_B <- filtered_known$colour_outside %>% recode("dark grey"= -1, sienna4=-1, salmon4=-1,"sienna3"=0, "sienna1"=0, "tan1"=0,"tan"=0, "grey"=1, "salmon2"=1, "salmon3"=1, "tan4"=1 )
filtered_known$colour_inside_B <- filtered_known$colour_inside %>% recode("dark grey"= -1, sienna4=-1, salmon4=-1,"sienna3"=0, "sienna1"=0, "tan1"=0,"tan"=0, "grey"=1, "salmon2"=1, "salmon3"=1, "tan4"=1 )
filtered_known$colour_core_B <- filtered_known$colour_core %>% recode("dark grey"= -1, sienna4=-1, salmon4=-1,"sienna3"=0, "sienna1"=0, "tan1"=0,"tan"=0, "grey"=1, "salmon2"=1, "salmon3"=1, "tan4"=1 )
filtered_unknown$colour_outside_B <- filtered_unknown$colour_outside %>% recode("dark grey"= -1, sienna4=-1, salmon4=-1,"sienna3"=0, "sienna1"=0, "tan1"=0,"tan"=0, "grey"=1, "salmon2"=1, "salmon3"=1, "tan4"=1 )
filtered_unknown$colour_inside_B <- filtered_unknown$colour_inside %>% recode("dark grey"= -1, sienna4=-1, salmon4=-1,"sienna3"=0, "sienna1"=0, "tan1"=0,"tan"=0, "grey"=1, "salmon2"=1, "salmon3"=1, "tan4"=1 )
filtered_unknown$colour_core_B <- filtered_unknown$colour_core %>% recode("dark grey"= -1, sienna4=-1, salmon4=-1,"sienna3"=0, "sienna1"=0, "tan1"=0,"tan"=0, "grey"=1, "salmon2"=1, "salmon3"=1, "tan4"=1 )


##split diagnostic sherds into training and test set, using tidymodels functions. Use 80% training. Use 10 replacement folds for analysis and assessment.
set.seed(1501)
known_split <- initial_split(filtered_known,prop = 8/10)
known_train <- training(known_split)
known_test  <- testing(known_split)
fo <-10
known_folds <- vfold_cv(known_train, v = fo) #makes fo lists of Analysis, Assess (and total) sherds.

#make 9 coefficients for each model (and each fold)- except just inclusions only 3 coefficients, and the last model gets 12 coefficents.
#use these coefficients to predict the assessment set (x10)

## gets the proportion of sherds with the different variables from analysis set and uses them as coefficients in assessment set to get predicted proportion of Adzera sherds in assessment set
justInclusions <- function(i){
  aly <- known_folds$splits[[i]] %>% analysis()
  coe <- aly %>% group_by(inclusion) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()) # get coefficients
  ass <- known_folds$splits[[i]] %>% assessment()
  cos <- ass %>% group_by(inclusion) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n())
  result <- ((coe$Adzera[1]*cos$n[1]) +  (coe$Adzera[2]*cos$n[2]) + (coe$Adzera[3]*cos$n[3])) / sum(cos$n)*100 #predicted % Adzera
  return(result)}

actualf <- function(i){
  result <- known_folds$splits[[i]] %>% assessment() %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)
  return(result$Adzera)
}
## gets the proportion of sherds with the different variables from analysis set and uses them as coefficients in assessment set to get predicted proportion of Adzera sherds in assessment set
inclusions_burnisheither <- function(i, inc = c(-1,0,1), be = c(-1,0,1), param = "burnish_either"){
  comb <- length(inc) *length(be)#how many combinations
  grid <- expand_grid(inc,be)
  result <- 0
  aly <- known_folds$splits[[i]] %>% analysis()
  ass <- known_folds$splits[[i]] %>% assessment()
  for(j in 1:comb){
    adz <- aly %>% filter(outcome=="Adzera", inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim #how many are Adzera in aly
    all <- aly %>% filter(inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim 
    coef <- adz[1]/all[1]
    inAss <- ass %>% filter(inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim 
    result <- (inAss[1]*coef) + result
     }
  return(result/dim(ass)[1]*100)}


#model using inclusion and a burnishing factor and colour factor
#inc inclusions states, be burnish states, ci colour states
model_3_factors <- function(i, inc = c(-1,0,1), be = c(-1,0,1),ci = c(-1,0,1), paramA = "burnish_either",paramB = "colour_inside_B"){
comb <- length(inc) *length(be)*length(ci)#how many combinations
grid <- expand_grid(inc,be,ci)
result <- 0
aly <- known_folds$splits[[i]] %>% analysis()
ass <- known_folds$splits[[i]] %>% assessment()
for(j in 1:comb){
  adz <- aly %>% filter(outcome=="Adzera", inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]), get(paramB)==as.character(grid[j,3]))  %>% dim #how many are Adzera in aly
  all <- aly %>% filter(inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]), get(paramB)==as.character(grid[j,3]))  %>% dim 
  if(all[1]>0) {coef <- adz[1]/all[1]
  inAss <- ass %>% filter(inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]), get(paramB)==as.character(grid[j,3]))  %>% dim 
  result <- (inAss[1]*coef) + result}
}
return(result/dim(ass)[1]*100)}

actual <- sapply(seq(from=1, to=fo), actualf)
inclusionModel <- sapply(seq(from=1, to=fo), justInclusions)
incl_be <- sapply(seq(from=1, to=fo), inclusions_burnisheither)
incl_out <- sapply(seq(from=1, to=fo), inclusions_burnisheither,  param = "burnish_outside")
incl_in <- sapply(seq(from=1, to=fo), inclusions_burnisheither,  param = "burnish_inside")
incl_col_in <- sapply(seq(from=1, to=fo), inclusions_burnisheither,  param = "colour_inside_B")
incl_col_out <- sapply(seq(from=1, to=fo), inclusions_burnisheither,  param = "colour_outside_B")
incl_col_core <- sapply(seq(from=1, to=fo), inclusions_burnisheither,  param = "colour_core_B")
incl_be_col_in <- sapply(seq(from=1, to=fo), model_3_factors,  paramA="burnish_either",paramB = "colour_inside_B")

foldValues <- data.frame(Actual=actual, ModelI=inclusionModel, ModelIBE=incl_be, ModelIBO=incl_out, ModelIBI=incl_in, ModelICI=incl_col_in, ModelICO=incl_col_out, ModelICC=incl_col_core, ModelIBECI=incl_be_col_in)
toplot <- foldValues %>% pivot_longer(!Actual, names_to = "Model", values_to = "Predicted")
#plot the actual versus the predicted values for the different models and assessment fold sets
ggplot(toplot, aes(x=Actual, y=Predicted,color=Model))+ geom_abline(intercept = 0, slope = 1)+geom_point(size=1, aes(shape=Model),alpha=0.8)+ scale_shape_manual(values=c(3, 1, 18,1,1,2,2,2))+scale_color_manual(values=c('red','green', 'black','blue','orange', 'green','blue','orange'))+coord_cartesian(ylim=c(0, 35), xlim=c(0,35),expand=FALSE)+ labs(x="Actual Adzera (%)", y = "Predicted Adzera (%)") +theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 6),axis.ticks = element_line(colour = "black", size = 0.2),legend.title = element_blank(),legend.key.height = unit(0.5, "cm"),legend.key.width = unit(0.2, "cm"),legend.position="top")
ggsave("foldModel2.png", width = 6, height = 7, units = "cm")

## Root mean squared errors.
RMSE <- function(observeds, predicteds){mean((observeds - predicteds)^2) %>% sqrt() }
RMSE(actual, inclusionModel)
RMSE(actual, incl_be)
RMSE(actual, incl_out)
RMSE(actual, incl_in)
RMSE(actual, incl_col_out)
RMSE(actual, incl_col_in)
RMSE(actual, incl_col_core)
RMSE(actual, incl_be_col_in)

#Coefficient of determination
1 - (sum((inclusionModel - actual)^2)/sum((actual - mean(actual))^2)) 
1 - (sum((incl_be - actual)^2)/sum((actual - mean(actual))^2)) 
1 - (sum((incl_out - actual)^2)/sum((actual - mean(actual))^2)) 
1 - (sum((incl_in - actual)^2)/sum((actual - mean(actual))^2)) 
1 - (sum((incl_col_out - actual)^2)/sum((actual - mean(actual))^2))
1 - (sum((incl_col_in - actual)^2)/sum((actual - mean(actual))^2)) 
1 - (sum((incl_col_core - actual)^2)/sum((actual - mean(actual))^2)) 
1 - (sum((incl_be_col_in - actual)^2)/sum((actual - mean(actual))^2)) 


#Looking at test set with just inclusion model
coe <- known_train %>% group_by(inclusion) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n())
cos <- known_test %>% group_by(inclusion) %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n())
result <- ((coe$Adzera[1]*cos$n[1]) +  (coe$Adzera[2]*cos$n[2]) + (coe$Adzera[3]*cos$n[3])) / sum(cos$n)*100#predicted % Adzera
act_test <- known_test  %>% summarise(n = n(),Adzera=sum(outcome=="Adzera")/n()*100)

#get coefficients from inclusion and another factor.
getceof <- function(aknown_train, inc = c(-1,0,1),be = c(-1,0,1),param = "burnish_either"){
  comb <- length(inc) *length(be)#how many combinations
  grid <- expand_grid(inc,be)
  grid$coef <- 0
  result <- 0
  for(j in 1:comb){
  adz <- aknown_train %>% filter(outcome=="Adzera", inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim #how many are Adzera in aly
  all <- aknown_train %>% filter(inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim 
  coef <- adz[1]/all[1]
  grid$coef[j] <- coef
  }
  return(grid)
}
#get coefficients from inclusion and two other factors.
getceof_3 <- function(aknown_train, inc = c(-1,0,1),be = c(-1,0,1),ci = c(-1,0,1), paramA = "burnish_either",paramB = "colour_inside_B"){
  comb <- length(inc) *length(be)*length(ci)#how many combinations
  grid <- expand_grid(inc,be,ci)
  grid$coef <- 0
  result <- 0
  for(j in 1:comb){
    adz <- aknown_train %>% filter(outcome=="Adzera", inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]),get(paramB)==as.character(grid[j,3]))  %>% dim #how many are Adzera in aly
    all <- aknown_train %>% filter(inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]),get(paramB)==as.character(grid[j,3]))  %>% dim 
    if(all[1]>0) {coef <- adz[1]/all[1]
    grid$coef[j] <- coef}
  }
  return(grid)
}

b_e_c_i_coeff <- getceof_3(aknown_train=known_train, paramA = "burnish_either",paramB = "colour_inside_B")
b_e_coeff <- getceof(aknown_train=known_train, param = "burnish_either")
b_o_coeff <- getceof(aknown_train=known_train, param = "burnish_outside")
b_i_coeff <- getceof(aknown_train=known_train, param = "burnish_inside")
c_o_coeff <- getceof(aknown_train=known_train, param = "colour_outside_B")
c_i_coeff <- getceof(aknown_train=known_train, param = "colour_inside_B")
c_c_coeff <- getceof(aknown_train=known_train, param = "colour_core_B")

predictSITE <- function(asite = "NKC",grid = b_e_coeff, inc = c(-1,0,1),be = c(-1,0,1), param = "burnish_either"){
  comb <- length(inc) *length(be)#how many combinations
  gridP <- expand_grid(inc,be)
  gridP$num <- 0
  for(j in 1:comb){
    all <- filtered_unknown %>% filter(site==asite, inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim 
    gridP$num[j] <- all[1]
  }
  total <- sum(gridP$num)
  adz <- sum(gridP$num*grid$coef)
  return(adz/total*100)}

predictSITE_3 <- function(asite = "NKC",grid = b_e_coeff, inc = c(-1,0,1),be = c(-1,0,1), paramA = "burnish_either",paramB = "colour_inside_B"){
  ci <- c(-1,0,1)
  comb <- length(inc) *length(be)*length(ci)#how many combinations
  gridP <- expand_grid(inc,be,ci)
  gridP$num <- 0
  for(j in 1:comb){
    all <- filtered_unknown %>% filter(site==asite, inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]),get(paramB)==as.character(grid[j,3]))  %>% dim 
    gridP$num[j] <- all[1]
  }
  total <- sum(gridP$num)
  adz <- sum(gridP$num*grid$coef)
  return(adz/total*100)}

predictTRAIN <- function(set=known_train,grid = b_e_coeff, inc = c(-1,0,1),be = c(-1,0,1), param = "burnish_either"){
  comb <- length(inc) *length(be)#how many combinations
  gridP <- expand_grid(inc,be)
  gridP$num <- 0
  for(j in 1:comb){
    all <- set %>% filter(inclusion==as.character(grid[j,1]), get(param)==as.character(grid[j,2]))  %>% dim 
    gridP$num[j] <- all[1]
  }
  total <- sum(gridP$num)
  adz <- sum(gridP$num*grid$coef)
  return(adz/total*100)}

#
predictTRAIN_3 <- function(set=known_train,grid = b_e_coeff, inc = c(-1,0,1),be = c(-1,0,1), paramA = "burnish_either",paramB = "colour_inside_B"){
  ci <- c(-1,0,1)
  comb <- length(inc) *length(be)*length(ci)#how many combinations
  gridP <- expand_grid(inc,be,ci)
  gridP$num <- 0
  for(j in 1:comb){
    all <- set %>% filter(inclusion==as.character(grid[j,1]), get(paramA)==as.character(grid[j,2]),get(paramB)==as.character(grid[j,3]))  %>% dim 
    gridP$num[j] <- all[1]
  }
  total <- sum(gridP$num)
  adz <- sum(gridP$num*grid$coef)
  return(adz/total*100)}

#predict Adzera proportion in test sherds
predictTRAIN(known_test, grid=b_e_coeff,param = "burnish_either")
predictTRAIN(known_test, grid=c_i_coeff,param = "colour_inside_B")
predictTRAIN_3(known_test, grid=b_e_c_i_coeff,paramA = "burnish_either",paramB = "colour_inside_B")
train <- known_test %>% filter(outcome=="Adzera") %>% dim #numbers of Adzera in test set
train[1]/dim(known_test)[1]*100 #percent of Adzera in test set

diag <- df_known %>% group_by(site) %>% summarise(n = n(),AdzeraKNOWN=sum(outcome=="Adzera")/n()*100)# include fire damaged
diag$num <- diag$n*diag$AdzeraKNOWN/100
match2 <- diag$num[match(siteOrder, as.character(diag$site))]
match3 <- diag$AdzeraKNOWN[match(siteOrder, as.character(diag$site))]
match4 <- diag$n[match(siteOrder, as.character(diag$site))]

unknown <- filtered_unknown %>% group_by(site) %>% summarise(n = n()) 
match <- unknown$n[match(siteOrder, as.character(unknown$site))]
adzeraNonDiagnostic <- sapply(siteOrder, predictSITE_3, grid=b_e_c_i_coeff,paramA = "burnish_either",paramB = "colour_inside_B")
adzeraNonDiagnosticN <- match*adzeraNonDiagnostic/100
group <- factor(c("purple", rep("blue",16), rep("dark green", 11), rep("orange4",4), rep("brown",2), rep("red", 5), rep("magenta",3)), levels=c("purple","blue","dark green","orange4","brown","red","magenta"))
thcol <- group
siteAdzera <- data.frame(site=factor(siteOrder,levels=siteOrder), AdzeraDiagnostic =match3, AdzeraNonDiagnostic=adzeraNonDiagnostic , AdzeraDiagnosticN =match2, AdzeraNonDiagnosticN=adzeraNonDiagnosticN,TotalDiagnosticN =match4, TotalNonDiagnosticN=match, group= group)
siteAdzera <- siteAdzera %>% arrange(site)

c <- seq(from=1.2, to=dim(siteAdzera)[1], by=2) 
d <- c+1 
ggplot(data=siteAdzera, aes(x=site, y=AdzeraDiagnostic, fill=group)) +geom_bar(stat="identity", color="black", size=.3)+scale_fill_manual(values=c("purple","blue","green","orange","brown","red","magenta"))+ labs(title="Diagnostic sherds",x=" ", y = "Adzera Proportion (%)")+coord_cartesian(ylim=c(0, 115), expand=FALSE)+scale_y_continuous(breaks=seq(from=0, to=100, by=20))+theme_classic()+ theme(axis.line.x=element_blank() ,text = element_text(size = 5),legend.position="none",axis.text.x = element_text(angle = 90,vjust=0.5,color=as.character(thcol)),axis.ticks = element_line(colour = "black", size = 0.2 )) + annotate("text", x = c, y = 112, label = as.vector(siteAdzera$TotalDiagnosticN)[c], color=thcol[c], size=1.2, vjust=1)+ annotate("text", x = d, y = 107, label = as.vector(siteAdzera$TotalDiagnosticN)[d], color=thcol[d], size=1.2,vjust=1)
ggsave("diagnosticPer.png", width = 9, height = 5, units = "cm")

ggplot(data=siteAdzera, aes(x=site, y=AdzeraNonDiagnostic, fill=group)) +geom_bar(stat="identity", color="black", size=.3)+scale_fill_manual(values=c("purple","blue","green","orange","brown","red","magenta"))+ labs(title="Non-Diagnostic sherds",x=" ", y = "Adzera Proportion (%)")+coord_cartesian(ylim=c(0, 115), expand=FALSE)+scale_y_continuous(breaks=seq(from=0, to=100, by=20))+theme_classic()+ theme(axis.line.x=element_blank() ,text = element_text(size = 5),legend.position="none",axis.text.x = element_text(angle = 90,vjust=0.5,color=as.character(thcol)),axis.ticks = element_line(colour = "black", size = 0.2 )) + annotate("text", x = c, y = 112, label = as.vector(siteAdzera$TotalNonDiagnosticN)[c], color=thcol[c], size=1.2, vjust=1)+ annotate("text", x = d, y = 107, label = as.vector(siteAdzera$TotalNonDiagnosticN)[d], color=thcol[d], size=1.2,vjust=1)
ggsave("NondiagnosticPer.png", width = 9, height = 5, units = "cm")

match4[is.na(match4)] <- 0 
match[is.na(match)] <- 0 
siteAdzera_cut <- data.frame(site=factor(siteOrder,levels=siteOrder),Diagnostic =match2, Non_Diagnostic=adzeraNonDiagnosticN,Total =match4+match)
siteAdzera_cut[is.na(siteAdzera_cut)] <- 0 
toplot <- siteAdzera_cut %>% pivot_longer(!c(site,Total), names_to = "Sherd", values_to = "Number")
e <- seq(from=1.2, to=dim(toplot)[1], by=4) 
f <- e+2 
ggplot(data=toplot, aes(x=site, y=Number, fill=Sherd)) +geom_bar(stat="identity", color="black", size=.3)+scale_fill_manual(values=c('light blue','pink'))+ labs(title="Number of Adzera Sherds",x=" ", y = "Number of Adzera Sherds")+coord_cartesian(ylim=c(0, 45), expand=FALSE)+scale_y_continuous(breaks=seq(from=0, to=45, by=10))+theme_classic()+ theme(text = element_text(size = 5),legend.title = element_blank(),legend.key.size = unit(0.3, "cm"),legend.position="bottom",axis.text.x = element_text(angle = 90,vjust=0.5,color=as.character(thcol)),axis.ticks = element_line(colour = "black", size = 0.2))+ annotate("text", x = c, y = 44, label = as.vector(toplot$Total)[e], color=thcol[c], size=1.2, vjust=1)+ annotate("text", x = d, y = 45, label = as.vector(toplot$Total)[f], color=thcol[d], size=1.2,vjust=1)

ggsave("adzeraNum.png", width = 9, height = 6, units = "cm")

#for map, should do with x y co-ordinates
siteAdzera0 <- siteAdzera
siteAdzera0[is.na(siteAdzera0)] <- 0 
siteAdzera0$avP <- (siteAdzera0$AdzeraDiagnosticN + siteAdzera0$AdzeraNonDiagnosticN) /(siteAdzera0$TotalDiagnosticN + siteAdzera0$TotalNonDiagnosticN)*100
siteAdzera0$siteNum <- seq(from=1, to=dim(siteAdzera0)[1]) #cause sites are in order
siteAdzera0$xcoord <- c(16.5,15,16,16,14.5,14,13.5,14.5,26,11.5,14.5,16.5,16,16.4,12,17,15.5,16.5,17.5,17.6,17,17.5,18,17,18,18,18,17.5,16,3,17,14,28,24,25.5,26,22.4,22,25,28.5,30,30.5)
siteAdzera0$ycoord <- c(36.5,29,27,24.5,28,27.5,26.4,28.5,26,26,25,27.3,26,26,26,23.5,25.5,17,17,15.5,15,15,16,14,14,19,18,17.5,6,4,5.5,5.5,20,21.5,11,14.5,9,7.5,13.5,15,16,13)

ggplot(siteAdzera0, aes(x=xcoord, y=ycoord,color=avP))+geom_point(size=.3, shape=19)+scale_colour_gradient(low = "blue",high = "red",limits=c(0,100))+coord_cartesian(xlim=c(0,45), ylim=c(0,45),expand=TRUE)+ labs(x=" ", y = " ") +theme_classic()+ theme(plot.title = element_text(size=7),text = element_text(size = 6),axis.ticks = element_line(colour = "black", size = 0.2),legend.title = element_blank(),legend.key.height = unit(0.5, "cm"),legend.key.width = unit(0.2, "cm"),legend.position="right", panel.background = element_rect(fill = "transparent"), plot.background = element_rect(fill = "transparent", color = NA),  panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),  legend.background = element_rect(fill = "transparent"),  legend.box.background = element_rect(fill = "transparent") 
)
ggsave("forMAP.png", width = 9, height = 6, units = "cm",bg = "transparent")

###to be completed.
##LA-ICPMS analysis 
###Modified from TERMITE code Mischel SA, Mertz-Kraus R, Jochum KP & Scholz D (2017) TERMITE: An R Script for Fast Reduction of Laser Ablation Inductively Coupled Plasma Mass Spectrometry Data and its Application to Trace Element Measurements. Rapid Communications in Mass Spectrometry: RCM 31: 1079–1087

