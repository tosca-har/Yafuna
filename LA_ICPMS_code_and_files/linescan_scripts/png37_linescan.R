#modified by Kris Hardy from TERMITE.
#to use ANU LA-ICPMS files that have been divided into sample by LATOOLS.
# ######################################################################### #
# ######################################################################### #
#									                                                          #
#			                  	TERMITE                                          	#
#								                                                      	    #
#			An R-Script for fast reduction of LA_ICPMS data and                   #
#       its application to trace element measurements                       #
#                                                                           #
#             			      Version 1_rev                                     #
#                                                                           #
#			                	Simon Mischel				                                #
#									                                                          #
#		Speleothem Research Group, University Mainz		                          #
#									                                                          #
#			eMail: simon.mischel@uni-mainz.de		                                  #
#									                                                          #
# ######################################################################### #
# ######################################################################### #
### find . -name "*.csv" -type f -exec cp {} ~/your_folder \;

# installs all required packages
rm(list=ls(all=TRUE))                                                             # delete the internal R memory to avoid duplicates
if (!require("miscTools"))   install.packages("miscTools", dependencies = TRUE)	  # on first run package will be installed if not available
if (!require("matrixStats")) install.packages("matrixStats", dependencies = TRUE)	# on first run package will be installed if not available
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)	# on first run package will be installed if not available
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)	# on first run package will be installed if not available

# Directory section 
path			            <- "~/TERMITE-master/your_main_directory/"	                                # defining the working-directory of the script
path.rawData.samples	<- "Rawdata_linescan"   	      	                          # defining the directory of the raw-data-files
path.rawData.RefMat 	<- "ReferenceMaterial_linescan"                             # defining the directory of the reference material
path.corrData.results	<- "Results/"          		                                  # defining the directory for the overview-plots

sample.name	        	<- "PNG37"	                            # is printed in the file-names
  
##### before starting the script check the HelpValue Sections !! #####

##### general HelpValue Section #####
Mode.of.Scan                <- "line.scan"        # spot.scan or line.scan 
Line.of.Header              <- 7                  # the line where the measured isotopes are listed- 
Line.of.Signal              <- 8                 # the line where the raw data start- 
measured.isotopes	          <- 34			          	# number of analysed isotopes
column.IS		                <- 5				          # column of internal standard in the rawdata-file. Si is 5
IS 		          	          <- 342447	        # [µg/g] internal standard. Making equal to ANU2000 for Si
outlier.Test                <- "Y"                # "Y" or "N"
m              		          <- 30 				        # percentage for the range of the outlier test
#number.sweeps.sample        <- 470				        # No. of sweeps (e.g. overall number of rows)
number.sweeps.Refs          <- 102                # No. of sweeps (e.g. overall number of rows)#KH but my refs have diff lengths
resolution		              <- "(LR)"		        	# Resolution "(LR)""(MR)""(HR)" (needed for Thermo Element2 ICP-MS)
machine                     <- "Agilent"          # Element2 or Agilent

background.correction	      <- "median"		  	    # method for background.correction (mean <-> median)


##### HelpValue for Linescans Section #####
first.blankValue.linescan   <- 8			            # first Blank-Value (e.g. line number) used for background-correction
last.blankValue.linescan		<- 35				        # last  Blank-Value (e.g. line number) used for background-correction

first.sampleValue.linescan  <- 39				        # first Laser-on-sample-Value (e.g. line number) used for analysis
last.sampleValue.linescan	  <- 371			          # last  Laser-on-sample-Value (e.g. line number) used for analysis

laser.speed                 <- 50                  # laser scan speed in µm/s

##### HelpValues for spot scans and/or reference material Section #####
first.blankValue	          <- 8			            # first Blank-Value (e.g. line number) used for background-correction
last.blankValue		          <- 30				        # last  Blank-Value (e.g. line number) used for background-correction
first.sampleValue	          <- 37				        # first Laser-on-sample-Value (e.g. line number) used for analysis
last.sampleValue	          <- 72		        		# last  Laser-on-sample-Value (e.g. line number) used for analysis

#need to make these a vector as more than 1, need to make sure match the order established later.
first.blankValue <- c(8,8,200,8,8,8)
last.blankValue <- c(30,30,323,30,30,36)
first.sampleValue <- c(37, 37,333,37,37,43)			        # first Laser-on-sample-Value (e.g. line number) used for analysis
last.sampleValue <- c(72,75,396,97,83,91)



##### Reference material Section

##### Values from the GeoReM database (www.http://georem.mpch-mainz.gwdg.de)
##### MPI-DING ATHO-G (ATHO), MPI-DING KL2-G (KL2-G)
##### MPI-DING StHs6/80-G (StHs), MPI-DING T1-G (T1-G)
##### BAM-S005B (BAM-B)
##### USGS MACS-1 (MACS1), USGS MACS-3 (MACS3), USGS GSD-G1 (GSD)
##### NIST SRM 610 (NIST610), NIST SRM 612 (NIST612)

RefMat1     <- "NIST612"	; Reference.Material <- c(RefMat1) # 1. referencematerial
RefMat2  <- "NIST610" 	; Reference.Material <- c(Reference.Material,RefMat2)	# 2. referencematerial
RefMat3     <- "ANU2000"	  ; Reference.Material <- c(Reference.Material,RefMat3)	# 2. referencematerial
#RefMat4    <- "NIST679"	  ; Reference.Material <- c(Reference.Material,RefMat4)	# 4. referencematerial

##### End HelpValues #####


##### do not change #####
sampleValues		<- (last.sampleValue-first.sampleValue)+1			# ???
sampleValue.all.linescan <- (last.sampleValue.linescan-first.sampleValue.linescan)+1
path.notChange		<- sprintf("%s%s",path,"TERMITEScriptFolder/")				# the directory containing the script-files

setwd(path)

# truncate time in rawData
column.IS       <- column.IS - 1
Line.of.Header  <- Line.of.Header - 1
Line.of.Signal  <- Line.of.Signal - 1

# subtract line.of.Signal due to header of file
number.sweeps.Refs   <- number.sweeps.Refs   - Line.of.Signal
#  number.sweeps.sample <- number.sweeps.sample - Line.of.Signal


first.blankValue <- first.blankValue - Line.of.Signal
last.blankValue  <- last.blankValue - Line.of.Signal

first.sampleValue <- first.sampleValue - Line.of.Signal
last.sampleValue  <- last.sampleValue - Line.of.Signal

first.blankValue.linescan   <- first.blankValue.linescan - Line.of.Signal
last.blankValue.linescan    <- last.blankValue.linescan - Line.of.Signal

first.sampleValue.linescan  <- first.sampleValue.linescan - Line.of.Signal
last.sampleValue.linescan   <- last.sampleValue.linescan - Line.of.Signal

m <- m/100

########################################################################
#### INPUT SECTION for Data files line scan section reference material #
########################################################################
# all variables in the INPUT SECTION should be formated ################
# like the original ones to fit the data from other MASS SPECTROMETERS #
########################################################################

#################################################
# check the second input further down this file #
#################################################

# Read data from the reference material files (long version)
alleDatenRefs <- list()

for (i in seq(along=Reference.Material)) {
  alleDatenRefs[[i]] <- list.files(path.rawData.RefMat,full.names=TRUE,pattern=Reference.Material[i])
}


# read data from the reference material files (only filenames)
alleDatenRefsCount <- matrix()

for (i in seq(along=Reference.Material)) {
  alleDatenRefsCount <- matrix(c(alleDatenRefsCount,list.files(path.rawData.RefMat,full.names=FALSE,pattern=Reference.Material[i])))
  
}

alleDatenRefsCount <- alleDatenRefsCount[-1, ] 


#########################
#### READ Data files ####
#########################
# check seperator for other MASS SPECTROMETER

# fill LISTE with all the datafiles
LISTE<-list() #will put same refs together ie 

if (machine=="Element2") {
  for (j in seq(along=Reference.Material)) {
    for (i in 1:length(alleDatenRefs[[j]])) {
      LISTE[length(LISTE)+1] <- list(read.table(alleDatenRefs[[j]][i],header=FALSE,skip=Line.of.Signal,sep="\t",nrow=number.sweeps.Refs)[,2:(measured.isotopes+1)])
    }
  }
}  else {
  for (j in seq(along=Reference.Material)) {
    for (i in 1:length(alleDatenRefs[[j]])) {
      #LISTE[length(LISTE)+1] <- list(read.table(alleDatenRefs[[j]][i],header=FALSE,skip=Line.of.Signal,sep=",",nrow=number.sweeps.Refs)[,2:(measured.isotopes+1)])
      LISTE[length(LISTE)+1] <- list(read.table(alleDatenRefs[[j]][i],header=FALSE,skip=Line.of.Signal,sep=",")[,2:(measured.isotopes+1)])
    }
  }
}


#####################
#### READ Header ####
#####################
# check seperator for other MASS SPECTROMETERS

# Header einlesen
if (machine=="Element2") {
  Kopf <- as.matrix(read.table(alleDatenRefs[[1]][1],skip=Line.of.Header,header=FALSE,sep="\t")[1,2:(measured.isotopes+1)])
  Kopf <- gsub(resolution,"",Kopf,fixed=TRUE)
} else {
  Kopf <- as.matrix(read.table(alleDatenRefs[[1]][1],header=FALSE,skip=Line.of.Header,sep=",")[1,2:(measured.isotopes+1)])
}


# Einlesen der Standards nach der GeoReM-Database
Standards_all <- read.table(sprintf("%s%s",path.notChange,"Standards_GeoReM.csv"),header=TRUE,sep=",",row.names=1)


# Einlesen der Atomgewichte und der Isotopenhäufigkeit
IsotopWerte_all <- read.table(sprintf("%s%s",path.notChange,"AtomGewIsoAbund_NIST.csv"),header=TRUE,sep="\t")


##########################################
#### END INPUT SECTION for Data files ####
##########################################


Kopf.rows <- c(alleDatenRefsCount)

r <- 3				# MPI Standard				# number of referenceglass-meassurements per referenceglass-meassurement


# extract elementname from isotope  
Element <- sub("^([[:alpha:]]*).*", "\\1", Kopf)


# extraction of isotopic values and reference materials
IsotopWerte <- subset(IsotopWerte_all,select=Kopf)
Standards <- subset(Standards_all,select=Kopf)


# calculation of the medianGasBlank MAKES matrix of the values
medianGasBlank <- list()


if (background.correction=="mean") {
  for (i in seq(along=LISTE)) {
    mat <- as.matrix(LISTE[[i]][first.blankValue[i]:last.blankValue[i],])
    medianGasBlank[[i]] <- matrix(c(colMeans(mat,na.rm=TRUE)),ncol=measured.isotopes,nrow=last.sampleValue[i],byrow=TRUE)
  }
} else {
  for (i in seq(along=LISTE)) {
    mat <- as.matrix(LISTE[[i]][first.blankValue[i]:last.blankValue[i],])
    medianGasBlank[[i]] <- matrix(c(colMedians(mat,na.rm=TRUE)),ncol=measured.isotopes,nrow=last.sampleValue[i],byrow=TRUE)
  }
}

Y <- list()

for( i in seq(along=LISTE)) {
  X <- matrix(0,nrow=last.sampleValue[i],ncol=measured.isotopes)
  X[first.sampleValue[i]:last.sampleValue[i],] <- as.matrix(LISTE[[i]][first.sampleValue[i]:last.sampleValue[i],] - medianGasBlank[[i]][first.sampleValue[i]:last.sampleValue[i],])
  Y[[i]] <- as.data.frame(X[first.sampleValue[i]:last.sampleValue[i],])
}


# calculate the LoD 
LoD <- list()
for (i in seq(along=LISTE)) {
  mat <- as.matrix(LISTE[[i]][first.blankValue[i]:last.blankValue[i],])
  LoD[[i]] <- matrix(c(
    ifelse(3*colSds(mat,na.rm=TRUE)==0,1,3*colSds(mat,na.rm=TRUE))
    /
      as.numeric(colMeans(Y[[i]]))
    * 
      as.numeric((matrix(Standards[eval(RefMat1), ],nrow=1,byrow=TRUE)))
  )
  ,ncol=measured.isotopes,nrow=last.sampleValue[i],byrow=TRUE)
}
# extract the Limit of Detection for the reference material (RefMat1) only  
w.LoD <- matrix(NA,nrow=(length(Kopf.rows)-length(alleDatenRefs)),ncol=measured.isotopes)

for (i in   (length(Kopf.rows) - (length(Kopf.rows) - length(alleDatenRefs))+1):length(Kopf.rows)) {
  w.LoD[i - (length(Kopf.rows) - (length(Kopf.rows) - length(alleDatenRefs))),] <- LoD[[i]][1, ]
}
# plot the Limit of Detection    
pdf(sprintf("%s%s%s.%s",path.corrData.results,"LoD_ReferenceMaterial1_",sample.name,"pdf"),width=12)
par(las=1)
plot(colMedians(w.LoD,na.rm=TRUE),type="p",log="y",ylab="LoD",xlab="Element",xaxt="n")
axis(1, at=seq(1,measured.isotopes,1),labels=Element)
dev.off()
# save the Limit of Detection    
write.table(round(colMedians(w.LoD,na.rm=TRUE),digits=5),sprintf("%s%s%s.%s",path.corrData.results,"LoD_ReferenceMaterial_",sample.name,"csv"),sep="\t",row.names=Element,col.names="LoD")
#detach("package:matrixStats", unload=TRUE)

#changed for diff sample values
# calculates values relative to IS
ISNormSample <- list()

for (i in seq(along=LISTE)) {
  ISNormSample[[i]] <- Y[[i]] / (LISTE[[i]][first.sampleValue[i]:last.sampleValue[i],column.IS] - medianGasBlank[[i]][first.sampleValue[i]:last.sampleValue[i],column.IS])
}
ISN <- ISNormSample   


# is just used if outlier.Test == "N"
ISN.noOutlier <- ISNormSample


# calculates the median of values relative to IS
medianISNormSample <- list()
for (i in seq(along=ISNormSample)) {
  medianISNormSample[[i]] <- sapply(ISNormSample[[i]],median,na.rm=TRUE)
}

#changed for diff sample values
# defines the upper boundary of the outlier test
FilterA <- list()
for (i in seq(along=medianISNormSample)) {
  FilterA[length(FilterA)+1] <- list(matrix(c(medianISNormSample[[i]] + medianISNormSample[[i]] * m),ncol=measured.isotopes,nrow=last.sampleValue[i],byrow=TRUE))
}

#changed for diff sample values
# defines the lower boundary of the outlier test
FilterB <- list()

for (i in seq(along=medianISNormSample)) {
  FilterB[length(FilterB)+1] <- list(matrix(c(medianISNormSample[[i]] - medianISNormSample[[i]] * m),ncol=measured.isotopes,nrow=last.sampleValue[i],byrow=TRUE))
}

#changed for diff sample values
# eliminates outliers

gefilterteWerte <- list()    

for (i in seq(along=ISNormSample)) {
  gef <- matrix(0,ncol=measured.isotopes,nrow=last.sampleValue[i]-first.sampleValue[i]+1)
  
  gef[1:(last.sampleValue[i]-first.sampleValue[i]+1),] <- as.matrix({ISNormSample[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),][ISNormSample[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),] > FilterA[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),]] <- NA;
  ISNormSample[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),][ISNormSample[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),] < FilterB[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),]] <- NA;
  ISNormSample[[i]][1:(last.sampleValue[i]-first.sampleValue[i]+1),]})
  gefilterteWerte[length(gefilterteWerte)+1]<- list(as.data.frame(gef))
}


ISF <- gefilterteWerte
# override the  outlier Test if intended
if (outlier.Test=="N") { 
  gefilterteWerte <- ISN.noOutlier
} else {
  gefilterteWerte <- gefilterteWerte
}


# plot outlier test
pdf(sprintf("%s%s%s.%s",path.corrData.results,"Outlier_Test_",sample.name,"pdf"))
par(mfrow=c(2,2),mar=c(5.5,5,1,1),las=1,mgp=c(4,1,0))

for (j in seq(along=ISN))  {
  for (i in 1:measured.isotopes)    {
    plot(ISN[[j]][ ,i],type="b",ylab=c(paste(Kopf[i],"rel_to_IS")),xlab=Kopf.rows[j])
    abline(h=(median(ISN[[j]][ ,i],na.rm=TRUE)+median(ISN[[j]][ ,i],na.rm=TRUE)*m),col="black")
    abline(h=(median(ISN[[j]][ ,i],na.rm=TRUE)-median(ISN[[j]][ ,i],na.rm=TRUE)*m),col="black")
    
    points(ISF[[j]][ ,i],type="p",col="red",pch=16)
    legend("topleft",inset=0.02,c(paste("boundary outlier test. m=",m),"incorporated values"),col=c("black","red"),pch=c(NA,16),lty=c(1,1,1))
  }
}
dev.off()



# mean of the filtered values
Auswertung <- list()

for (i in seq(along=gefilterteWerte)) {
  Auswertung[length(Auswertung)+1] <- list(colMeans(gefilterteWerte[[i]],na.rm=TRUE))
}


# (isotopAbundance(Ca)/isotopAbundace(Element))*(isotopWeigth(Element)/IsotopWeigth(Ca)))
IsotopBerechnung <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,1] * IsotopWerte[1,1] / IsotopWerte[1,column.IS])

for (i in 2:measured.isotopes) {
  IsotopBerechnung[length(IsotopBerechnung)+1] <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,i] * IsotopWerte[1,i] / IsotopWerte[1,column.IS])
}


# isotopic abundance relative to IS * measured values
AbundanzAuswertung <- matrix(0,ncol=measured.isotopes,nrow=length(LISTE),byrow=TRUE)

for (i in seq(along=Auswertung)) {
  AbundanzAuswertung[i,] <- Auswertung[[i]] * IsotopBerechnung
}


##### Referenzgläser und mean RSF 
# helpvalues
helpValue.1 <- matrix(0,ncol=measured.isotopes)

#if(machine=="Element2") {
for ( i in seq(along=alleDatenRefs)) {
  helpValue.1 <- rbind(helpValue.1,matrix(Standards[eval(parse(text=paste("RefMat",i,sep=""))),column.IS],ncol=measured.isotopes,nrow=length(alleDatenRefs[[i]]),byrow=TRUE))
}

helpValue.1 <- helpValue.1[-1, ]

# uncorrected concentration
unCorrPpm.all <- AbundanzAuswertung * helpValue.1
unCorrPpm.Refs <- unCorrPpm.all #unCorrPpm.all[-1:-(length(alleDaten)),]	# Auftrennen nach Referenzgläser


# helpvalues for the reference materials 
helpValue.2<-matrix(0,ncol=measured.isotopes,nrow=1)

for (i in seq(along=alleDatenRefs)) {
  helpValue.2 <- rbind(helpValue.2,matrix(as.matrix(Standards[eval(parse(text=paste("RefMat",i,sep=""))),]),ncol=measured.isotopes,nrow=length(alleDatenRefs[[i]]),byrow=TRUE))
}

helpValue.2 <- helpValue.2[-1,]


# calcualation for corrected reference materials
CorrPpm.Refs <- ifelse((unCorrPpm.Refs / helpValue.2)==0,NA, (unCorrPpm.Refs / helpValue.2))


# plot RSF
if (length(alleDatenRefs)>1)  {
  helpValue.RSF <- matrix(length(alleDatenRefs[[1]]),nrow=length(alleDatenRefs))
  for (i in 2:length(alleDatenRefs)) {
    helpValue.RSF[i,] <- helpValue.RSF[i-1,]+length(alleDatenRefs[[i]])
  }; helpValue.RSF <- helpValue.RSF[-length(alleDatenRefs),]
} else {
  helpValue.RSF <- matrix(length(alleDatenRefs[[1]]),nrow=(length(alleDatenRefs)))
}


pdf(sprintf("%s%s%s.%s",path.corrData.results,"RSFused_",sample.name,"pdf"))
par(mfrow=c(2,2),mar=c(4,4,1,1),las=1)
for (i in 1:measured.isotopes) if (all(is.na(CorrPpm.Refs[,i]))==FALSE)
{
  plot(CorrPpm.Refs[,i],type="b",xlab=paste(paste(Reference.Material,collapse=" | ")),ylab=paste("mean RSF",Element[i]))
  abline(h=mean(CorrPpm.Refs[,i],na.rm=TRUE),lty=3,lwd=2)
  abline(v=helpValue.RSF + 0.5,lty=2)
} else {next}
dev.off()
#  }


# calculate the mean of the RSF of all reference materials
RSFused <- colMeans(CorrPpm.Refs,na.rm=TRUE)


write.table(round(RSFused,digits=2),(sprintf("%s%s%s.%s",path.corrData.results,"RSFused_",sample.name,"csv")),sep="\t",row.names=Element)


## read the RSFused for linescans calculated
RSFused.LS <- as.matrix(RSFused)


########################################################
#### INPUT SECTION for Data files line scan section ####
########################################################################
# all variables in the INPUT SECTION should be formated ################
# like the original ones to fit the data from other MASS SPECTROMETERS #
########################################################################


Daten.linescan <- list.files(path.rawData.samples,full.names=TRUE) #KH mod

if (machine=="Element2"){
  sample.linescan <- read.table(Daten.linescan,sep="\t",skip=Line.of.Signal)[,2:(measured.isotopes+1)]
  sample.time <- as.numeric(as.character(read.table(Daten.linescan,sep="\t",skip=Line.of.Signal)[1:(last.sampleValue.linescan-first.sampleValue.linescan+1),1]))
} else{
  sample.linescan <- read.table(Daten.linescan,sep=",",skip=Line.of.Signal)[,2:(measured.isotopes+1)]
  sample.time <- as.numeric(as.character(read.table(Daten.linescan,sep=",",skip=Line.of.Signal)[1:(last.sampleValue.linescan-first.sampleValue.linescan+1),1]))
}

line.length <- sample.time*laser.speed/1000


# read Header
if (machine=="Element2"){
  Kopf <- as.matrix(read.table(Daten.linescan,header=FALSE,skip=Line.of.Header,sep="\t")[1,2:(measured.isotopes+1)])
} else {
  Kopf <- as.matrix(read.table(Daten.linescan,header=FALSE,skip=Line.of.Header,sep=",")[1,2:(measured.isotopes+1)])
} 

Kopf <- gsub("(LR)","",Kopf,fixed=TRUE)


##########################################
#### END INPUT SECTION for Data files ####
##########################################


# change names according to the Elements
names(sample.linescan) <- Kopf


# eliminate potential E-values
EWerte <- list()
EWerte[[1]] <- sapply(sample.linescan,function(x) {as.numeric(gsub("[^E0-9.0-9]", "NA", x))})
sample.linescan <- as.data.frame(EWerte[[1]])


## plot cps raw Data
pdf(sprintf("%s%s%s.%s",path.corrData.results,"rawCountrate_",sample.name,"pdf"))

df.liste <- as.data.frame(sample.linescan)
af <- rep(1:length(sample.linescan[,1]),measured.isotopes)
names(df.liste) <- c(Kopf)
df.liste.long <- melt(df.liste)
df_l <- cbind(af,df.liste.long)

p <- ggplot(df_l,aes(x=af,y=value))+
  geom_line() +
  scale_y_log10()+
  facet_wrap(~variable)+labs(x=paste("No. of sweeps - linescan:",sample.name),y="cps")+
  theme_bw()

print(p)


for(i in seq(along=LISTE)){
  
  df.liste <- as.data.frame(LISTE[[i]])
  af <- rep(1:length(LISTE[[i]][,1]),measured.isotopes)
  names(df.liste) <- c(Kopf)
  df.liste.long <- melt(df.liste)
  df_l <- cbind(af,df.liste.long)
  
  p <- ggplot(df_l,aes(x=af,y=value))+
    geom_line() +
    scale_y_log10()+
    facet_wrap(~variable)+labs(x=paste("No. of sweeps - spot:",Kopf.rows[i]),y="cps")+
    theme_bw()
  
  print(p) 
}
dev.off()


## background Median
fb <- as.matrix(sample.linescan[first.blankValue.linescan:last.blankValue.linescan,])
background <- matrix(colMedians(fb),nrow=sampleValue.all.linescan,ncol=measured.isotopes,byrow=TRUE)
colnames(background) <- Kopf


## background corrected cps  
sample.linescan.bg <- sample.linescan[first.sampleValue.linescan:last.sampleValue.linescan,]-background
names(sample.linescan.bg) <- Kopf

sample.linescan.Ca <- sample.linescan.bg / sample.linescan.bg[,column.IS]  
names(sample.linescan.Ca) <- Kopf


IsotopBerechnung <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,1] * IsotopWerte[1,1] / IsotopWerte[1,column.IS])
for (i in 2:measured.isotopes){
  IsotopBerechnung[length(IsotopBerechnung)+1] <- matrix(IsotopWerte[2,column.IS] / IsotopWerte[2,i] * IsotopWerte[1,i] / IsotopWerte[1,column.IS])
}


## matrix for Abundance of isotopes and weight
IsoB <- matrix(IsotopBerechnung,nrow=sampleValue.all.linescan,ncol=measured.isotopes,byrow=TRUE)
colnames(IsoB) <- Kopf


## IS-nomalized sample multiplicate with isotopic abundances
sample.linescan.CaIsoNorm <- sample.linescan.Ca * IsoB


## RSFused calculations for line scans
RSFused.sample.linescan <- matrix(RSFused.LS[,1],nrow=sampleValue.all.linescan,ncol=measured.isotopes,byrow=TRUE)
colnames(RSFused.sample.linescan) <- Kopf


## uncorrected concentration / RSFused
CorrConc <- sample.linescan.CaIsoNorm*IS/RSFused.sample.linescan

###Don't use? KH
# apply Limit of Detection
limit <- matrix(colMedians(w.LoD,na.rm=TRUE),nrow=length(CorrConc[,1]),ncol=measured.isotopes,byrow=TRUE)
CorrConc.limit <- ifelse(as.matrix(CorrConc)>=limit,as.matrix(CorrConc),NA)
CorrConc.limit <- ifelse(CorrConc.limit>=0,CorrConc.limit,NA)

#save(CorrConc.limit, file="PNG16_CorrConc")	
#save(CorrConc, file="PNG16_Conc")		

# write the concentrations of the linescan
write.table(cbind(line.length,CorrConc.limit),(sprintf("%s%s%s.%s",path.corrData.results,"ResultsLIMITED_",sample.name,"csv")),sep="\t",row.names=FALSE,col.names=as.matrix(cbind("length",Element)))
write.table(cbind(line.length,CorrConc),(sprintf("%s%s%s.%s",path.corrData.results,"Results_",sample.name,"csv")),sep="\t",row.names=FALSE,col.names=as.matrix(cbind("length",Element)))

# plot the concentrations of the linescan  
pdf(sprintf("%s%s%s.%s",path.corrData.results,"Results_",sample.name,"pdf"),useDingbats=FALSE)
par(mfrow=c(2,2),mar=c(5,5,1,1),las=1,mgp=c(3.5,1,0))
for (i in 1:measured.isotopes)
  if(i != column.IS) { 
    if (all(is.na(CorrConc[,i]))==FALSE)
      if (all(is.na(CorrConc.limit[,i]))==FALSE){
        plot(CorrConc.limit[,i]~line.length,type="l",xlab=paste("Distance - ",sample.name),ylab=paste(Element[i]," [µg/g]"))
      } else {next}
  } else {next}
dev.off()

##### End of the script TERMITE #####

#get ratios and make log2
logRatio <- log(CorrConc/IS,2)
#to ratio to another element minus that column from all.
#plot elements in line form

#heatmap elements.


#https://www.jcu.edu.au/advanced-analytical-centre/resources/element-to-stoichiometric-oxide-conversion-factors
oxide <- read.table(sprintf("%s%s",path.notChange,"oxide_multipler.txt"),header=TRUE,sep="\t")
#multiple each MATCHINGratio by oxideM

ratios <- sample.linescan.CaIsoNorm/RSFused.sample.linescan
oxideM <- oxide[match(colnames(ratios), oxide[,1]),2]

ratiosXoxides <- ratios
for(i in 1:length(oxideM)){
  ratiosXoxides[,i] <- ratios[,i]*oxideM[i] 
}

#sum each row, inverse and multiple by million to get ug/g of Silicon 
siConc <- apply(ratiosXoxides, MARGIN=1, function(x) 1000000/sum(x))
SiCorrConc <- ratios
for(i in 1:length(siConc)){
  SiCorrConc[i,] <- ratios[i,]*siConc[i] 
}
pdf(sprintf("%s%s%s.%s",path.corrData.results,"Results_SIcorr_",sample.name,"pdf"),useDingbats=FALSE)
par(mfrow=c(2,2),mar=c(5,5,1,1),las=1,mgp=c(3.5,1,0))
for (i in 1:measured.isotopes)
  if (all(is.na(SiCorrConc[,i]))==FALSE)
    if (all(is.na(SiCorrConc[,i]))==FALSE){
      plot(SiCorrConc[,i]~line.length,type="l",xlab=paste("Distance - ",sample.name),ylab=paste(Element[i]," [µg/g]"))
    } else {next}

dev.off()

write.table(cbind(line.length,SiCorrConc),(sprintf("%s%s%s.%s",path.corrData.results,"Results_Si1000000_",sample.name,"txt")),sep="\t",row.names=FALSE,col.names=as.matrix(cbind("length",Element)))
save(SiCorrConc,file=sprintf("%s%s%s",path.corrData.results,"Results_Si1000000_",sample.name))
