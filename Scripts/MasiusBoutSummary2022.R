# MasiusBoutSummary2022.R
#
#    Takes the 312-bout datafile "CoreColsMFC8Feb20.csv" from folder "Masius ntrpy cmprss Jaro "
#      with 15 variables: {UID, ObsDate, Log, Time, Total.length, Male1ID, FemID, Bird2ID, 
#          FemUpDown, Behavior, JulianWeek, OrigLength, Type, CopM, CodedBehav} and 
#      22,019 observations and creates tibbles with which to summarize the data
# 
#  Output will be 
#   1) Julian timing of bouts table                  See lines 55-61
#   2) Fff variability in Fem vs. Cop bouts.         See lines 62-77
#   3) Fem UpDown BowNeck 2-way table.               See lines 78-96
#   4) Bow, Neck twist, ALAD tibbles                 See lines 97-130
#   5) 11 major (Mal, Fem, Cop) behav elements       See lines 131-149
#   6) Tabulation of Male1IDs for Table 4 of MS      See lines 153-204
#   7) Fig 4 of Anim Behav M296 Cmprs~SampleDay      See lines 265-273 
#   8) Cmprss ~ Bfr vs. After copulation             See lines 275-296
#   9) Bow v Neck twists pre- v post-copulation      See lines 298-338
#   10) Fem ID (single- & multi-bout FemID)          See lines 339-403
#   11) Fem biogs {289,294,295,935,936,959,981,990}  See lines 405-468
#   12) Miscellaneous Male1ID exploration            See lines 469-503 
#   13) NEW BwNk proportion boxplots and tibbles     See lines 505-601
#   14) New variation in pre-cop Bows by Male ID     See lines 602-678
#   15) Distinct element count for 312 MFC bouts     See lines 678-769
#   16) Algorithmic complexity (local)               See lines 770-END

#    Created by D.B. McDonald 2-May-19; modified 16-May-19
#    8-Jun-19 Added a portion to analyze Bow and Neck repetition across MAL, FEM, COP bouts
#    11-Feb-20 Redone with new 312-bout dataset
#    29-May-20 moved Fem, Julian and other summary analyses from "MasiusEntrpyCmprss.R"
#    31-Jul-20 redid Fig. 4. M296 {Mal, Fem, Cop} comprss ~ sample day ll. 265-273
#    29-Sep Big2P (drop ALAD)
#    30-Sep-20 boxplots of proportion Bow, ALAD Neck for {Mal, Fem, Cop}
#    19-Oct-20 new date span analyses 
#    6-Feb-21 Pre-cop Bow by Male ID (see line 602)
#    10-Oct-22 Distinct elem count boxplot (see lines 733-)
#    11-Oct-22 Local algorithmic complexity for MCF bouts (see lines 770-END)
##########################################################################
library(tidyverse)
library(entropy)   ## for log-Base2 entropy of bout sequences
library(ggplot2)
library(dplyr)
library(lubridate)
library(stats)
library(MASS) ## for the polr() function for ordinal regression of lines 
library(acss) ## Algorithmic complexity (Kolmogorov complexity) of element strings

setwd("~/Desktop/Lab members & res/a R related items/McProjects/Masius video analysis R")

  ###### IMPORT  "CoreColsMFC8Feb20.csv" from folder "Masius ntrpy cmprss Jaro "
CoreMFCFile <- file.choose()
  CoreMFC  <- read.csv(CoreMFCFile, header = TRUE, check.names=FALSE, stringsAsFactors=F) 
  CoreColsMFC <- as_tibble(CoreMFC)
  ## length(CoreColsMFC$UID) N = 22,019 observations fr 312 bouts 29-May-20 length(unique(CoreColsMFC$UID)) 
  ## Make dates calculable in tidyverse
CoreColsMFC$ObsDate <- mdy(CoreColsMFC$ObsDate)      ## unique(CoreColsMFC$CodedBehav) still has t & h

CoreColsMFClean <- filter(CoreColsMFC, CodedBehav != "t" & CodedBehav != "h") ## 21395 - 22019  312*2
  ## A tibble: 21,395 x 15  [ no "t" or "h" "Start" and "End" in $CodedBehav ]

########## Import "CmprssNoStEndEntrp312Bouts8Feb20.csv" fr ƒ "Masius ntrpy cmprss Jaro "
Cmprssfile <- file.choose()
 CmprsEntrp  <- read.csv(Cmprssfile, header = TRUE, check.names=FALSE, stringsAsFactors=F) 
 CmprsEntrpTibl <- as_tibble(CmprsEntrp)

  ################## Julian analyses of 4-Feb-2020
JulianCore <- dplyr::select(CoreColsMFClean, UID, JulianWeek, Type)
JulianCoreUID <- unique(JulianCore)
JulianTable <- with(JulianCoreUID,
                    table(Type,JulianWeek))  ## str(JulianTable)
write.csv(JulianTable,"./Outputs/JulianTable8Feb20.csv")

  ################## Fff variability of 5-Feb-20 
FemCore <- filter(CoreColsMFClean, Type == "Fem", Behavior == "Fff")
FemCore %>%
  group_by(UID) %>%
  summarise(Num= n()) %>%
  summarise(Mean = mean(Num), SD = sd(Num), Max = max(Num), Min = min(Num), Cov = SD/Mean)
CopCore <- filter(CoreColsMFClean, Type == "Cop", Behavior == "Fff")
CopCore %>%
  group_by(UID) %>%
  summarise(Num= n()) %>%
  summarise(Mean = mean(Num), SD = sd(Num), Max = max(Num), Min = min(Num), Cov = SD/Mean)
  ## Fff summary
  ##               Mean    SD     Max   Min   Cov
  ## Fem bouts     2.6   2.80    23     1    1.08 
  ## Cop buts      2.14  1.10     5     1    0.513

  ##################### Fem Up Down analysis 8-Feb-20
unique(CoreColsMFC$FemUpDown)  ##   NA, "FemDown", "FemUp", "Na"  
FemUpDwnTibl <- filter(CoreColsMFC, FemUpDown == "FemDown" | FemUpDown == "FemUp")
  FemUpDwnTibl <- filter(CoreColsMFC, Behavior == "Bow" | Behavior == "Neck")
  BowFem <- filter(FemUpDwnTibl, Behavior == "Bow")
  BowFemTable <- table(BowFem$FemUpDown)              # Down  = 4,540   ;  Up = 62   unname (BowFemTable)
NeckFem <- filter(FemUpDwnTibl, Behavior == "Neck")     ## 4540+62
  NeckFemTable <- table(NeckFem$FemUpDown)            #  Down = 54      ;  Up = 553
ContMatrix <- matrix(nrow=2,ncol=2)
  colnames(ContMatrix) <- c("Down", "Up")
  rownames(ContMatrix) <- c("Bow", "Neck")
  ContMatrix[1,] <- unname(BowFemTable)
  ContMatrix[2,] <- unname(NeckFemTable)
chisq.test(ContMatrix)    ##   Pearson's Chi-squared test with Yates' continuity correction
##   X-squared = 4140.5, df = 1, p-value < 2.2e-16     P < 0.0001
##  ContMatrix:          Down    Up
#                 Bow   4540    62
#                 Neck    54   553 

  ############## Bow & Neck twist rate per minute
RateTable <- as.data.frame(matrix(ncol=6,nrow=9))
  colnames(RateTable) <- c("Type", "Mean","s.d.","Min","Max","Bhv")
  Behav <- c("Bow","Neck","ALAD")
  LngthB <- length(Behav)
  Typ <- c("Mal","Fem","Cop")
  LngthT <- length(Typ)
  RateTable$Type <- c("Mal","Fem","Cop","Mal","Fem","Cop","Mal","Fem","Cop")
  RateTable$Bhv <- c("Bow","Bow","Bow","Neck","Neck","Neck","ALAD","ALAD","ALAD")
  RowPlcr <- 0
  for (i in 1:LngthB) {   ## i <- 1
    Bhv <- Behav[i]
    for (j in 1:LngthT) { # j <- 1
      RowPlcr <- RowPlcr + 1
      BhvData <- filter(CoreColsMFC, Type == Typ[j], Behavior == Behav[i])
      BoutTimes <- BhvData %>% 
        dplyr::select(UID, Total.length) %>%
         distinct(UID, .keep_all = TRUE) %>%
          mutate(BoutMins = Total.length/60) 
      BhvNum <- BhvData %>%       
        dplyr::select(UID, Behavior) %>%
         group_by(UID) %>%
          count(Behavior)  
      BhvJoin <- left_join(BoutTimes,BhvNum) %>%
        mutate(BhvRate = n/BoutMins) %>%
          dplyr::select(UID, BoutMins, n, BhvRate)
      RateTable[RowPlcr, 2] <- mean(BhvJoin$BhvRate)    
      RateTable[RowPlcr, 3] <- sd(BhvJoin$BhvRate)      
      RateTable[RowPlcr, 4] <- min(BhvJoin$BhvRate)
      RateTable[RowPlcr, 5] <- max(BhvJoin$BhvRate)
    }
}
write.csv(RateTable,"./Outputs/Big3RatePerMinTable1Jun20.csv",row.names=F) 

  ################ 11 major behav elements by bout type {Mal, Fem, Cop}
Big11Count <- as.data.frame(matrix(ncol=6,nrow=11))
  colnames(Big11Count) <- c("Bhv", "Count","UIDcount","UIDcntM","UIDcntF","UIDcntC")
  Big11Count <- as_tibble(Big11Count)
Big11Count$Bhv <- c("ALAD","Bow","Cop","HdBw","Metr","Mix","Neck","SLAD","Swtc","Taf","Zro")
NumBhvs <- length(Big11Count$Bhv)

for (i in 1:NumBhvs){
  Bhv <- Big11Count$Bhv[i]
  Big11Count$Count[i] <- length(which(CoreColsMFC$Behavior == Bhv))
  BhvBit <- filter(CoreColsMFC,Behavior == Bhv)
  Big11Count$UIDcount[i] <- length(unique(BhvBit$UID))  
  Big11Count$UIDcntM[i] <- length(unique(filter(BhvBit, Type == "Mal")$UID)) 
  Big11Count$UIDcntF[i] <- length(unique(filter(BhvBit, Type == "Fem")$UID))
  Big11Count$UIDcntC[i] <- length(unique(filter(BhvBit, Type == "Cop")$UID))
}

write.csv(Big11Count, "./Outputs/Big11CountTbl1Jun20.csv") 

######## Male1ID tabulation

UIDList <- sort(unique(CoreColsMFC$Male1ID))
    ## 112  113  291  296  299  940  948  965 976  978  980 982 8000
CopBouts <- filter(CoreColsMFC, Type == "Cop")
CopMales <- unique(CopBouts$Male1ID)   ## 296 940 980   

CopM980Bouts <- filter(CopBouts, Male1ID == "980")
  CopM980UIDset <- unique(CopM980Bouts$UID) ## N = 3
  ## UID = {"989" "991" "992"}
CopM296Bouts <- filter(CopBouts, Male1ID == "296")
  CopM296UIDset <- unique(CopM296Bouts$UID) ## N = 10
  ## UID = {"1455" "1533" "1558" "1817" "1824" "1878" "1920" "1987" "2017" "2020"}
CopM940Bouts <- filter(CopBouts, Male1ID == "940")
  CopM940UIDset <- unique(CopM940Bouts$UID)   ## UID = {"5005"}

MaleIDtbl <- as.data.frame(matrix(ncol=8,nrow=13))
  colnames(MaleIDtbl) <- c("MaleID", "Total","Mal","Fem","Cop","FirstDate","LastDate","Span")

MaleIDtbl$MaleID <- UIDList
NumMls <- length(UIDList)
for (i in 1:NumMls) {   ## i <- 1
  CurrMale <- filter(CoreColsMFC,Male1ID==UIDList[i])
  MaleIDtbl$Total[i] <- length(unique(CurrMale$UID))
  MaleIDtbl$Mal[i] <- length(unique(filter(CurrMale,Type=="Mal")$UID))
  MaleIDtbl$Fem[i] <- length(unique(filter(CurrMale,Type=="Fem")$UID)) 
  MaleIDtbl$Cop[i] <- length(unique(filter(CurrMale,Type=="Cop")$UID))
  MaleIDtbl$FirstDate[i] <- min(CurrMale$ObsDate)
  MaleIDtbl$LastDate[i]  <- max(CurrMale$ObsDate)
  MaleIDtbl$Span[i]      <- MaleIDtbl$LastDate[i] - MaleIDtbl$FirstDate[i] + 1
}

MaleIDtbl$FirstDate <- as.Date(MaleIDtbl$FirstDate,"1970-01-01")
MaleIDtbl$LastDate  <- as.Date(MaleIDtbl$LastDate,"1970-01-01")
MaleIDtbl$FirstDate <- format(MaleIDtbl$FirstDate, "%d-%b-%y")
MaleIDtbl$LastDate <- format(MaleIDtbl$LastDate, "%d-%b-%y")
write.csv(MaleIDtbl,"./Outputs/MaleIDTable1Jun20.csv")

  ## Analyze "MasiusTable4DateSpanInput.csv" for date span
MalDateSpanFile <- file.choose()
MalDateSpan  <- read.csv(MalDateSpanFile, header = TRUE, check.names=FALSE, stringsAsFactors=F) 
  MalDateSpan <- as_tibble(MalDateSpan)
    ## Make dates calculable in tidyverse
    MalDateSpan$FirstDate <- mdy(MalDateSpan$FirstDate)      
    MalDateSpan$LastDate <- mdy(MalDateSpan$LastDate)
  MaleDateSpanTbl <- as.data.frame(matrix(ncol=8,nrow=length(MalDateSpan$MaleID)))
    colnames(MaleDateSpanTbl) <- names(MalDateSpan)
  MaleDateSpanTbl[,1:7] <- MalDateSpan[,1:7]
  for (i in 1:length(MalDateSpan$MaleID)) {   ## i <- 1
    MaleDateSpanTbl$Span[i]      <- MalDateSpan$LastDate[i] - MalDateSpan$FirstDate[i] + 1
  }
  write.csv(MaleDateSpanTbl,"./Outputs/MaleDateSpanTable19Oct20.csv")  
  
  ######################## CopMal, CopFem, CopCop by Male1ID and date
DateIDfile <- filter(CoreColsMFC, CopM=="Y" & Behavior == "Start")
  DateIDfile <- dplyr::select(DateIDfile, UID, ObsDate, Male1ID, FemID)

ComboFile <- inner_join(DateIDfile, CmprsEntrpTibl) %>%
  dplyr::select(-CompressLength,-CopM,-Big3P,-Entrop)

M296Set <- filter(ComboFile, Male1ID == "296") ## N = 153  length(M296Set$UID)
M940Set <- filter(ComboFile, Male1ID == "940") ## N = 7  length(M940Set$UID)
M980Set <- filter(ComboFile, Male1ID == "980") ## N = 78  length(M980Set$UID)
  M296SetDateOrd <- arrange(M296Set,ObsDate)
  M940SetDateOrd <- arrange(M940Set,ObsDate)
  M980SetDateOrd <- arrange(M980Set,ObsDate)

  ### Regressions for ablines of Fig. 4  
    M296SetDateOrdM <- filter(M296SetDateOrd, Type=="Mal")
  summary(lm(M296SetDateOrdM$CmprsRatio~M296SetDateOrdM$Day)) ## Slope = -0.001 Y-int = 1.18
    M296SetDateOrdF <- filter(M296SetDateOrd, Type=="Fem")
  summary(lm(M296SetDateOrdF$CmprsRatio~M296SetDateOrdF$Day)) ## Slope =  0.001 Y-int = 2.54
    M296SetDateOrdC <- filter(M296SetDateOrd, Type=="Cop")
  summary(lm(M296SetDateOrdC$CmprsRatio~M296SetDateOrdC$Day)) ## Slope =  0.0005 Y-int = 3.74
  
   ## Using https://stats.idre.ucla.edu/r/dae/ordinal-logistic-regression/    
M296SetDateOrd$Type <- as_factor(M296SetDateOrd$Type) 
  M296ObsMFC <- polr(Type ~ CmprsRatio, data = M296SetDateOrd); summary(M296ObsMFC)
  ##             Value    Std. Error t value
  ## CmprsRatio  2.02      0.285     7.091
  ## Intercepts:
  ##          Value    Std. Error   t value
  ##  Mal|Fem 4.0217    0.5056       7.9539 
  ##  Fem|Cop 8.4017    1.0856       7.7395 
  ##  Residual Deviance: 150.6056  AIC: 156.6056 
  CI <- confint(M296ObsMFC)  ## 2.5% to  97.5%      1.505631 to 2.630495 
  OddsRatio <- exp(coef(M296ObsMFC)) ## 7.54195 

## Mal means for M296
  MalM296Mean <- mean(filter(M296SetDateOrd, Type=="Mal")$CmprsRatio)  ## mean = 1.1
  FemM296Mean <- mean(filter(M296SetDateOrd, Type=="Fem")$CmprsRatio)  ## mean = 2.6
  CopM296Mean <- mean(filter(M296SetDateOrd, Type=="Cop")$CmprsRatio)  ## mean = 3.8

  DaySet <- M296SetDateOrd$ObsDate - M296SetDateOrd$ObsDate[1]  ## str(DaySet)
  DaySet <- as.numeric(DaySet)
  DayRank <- rank(DaySet)
  M296SetDateOrd <- mutate(M296SetDateOrd, Day = DayRank)

  FirstCop <- M296SetDateOrd$Day[which(M296SetDateOrd$Type == "Cop")[1]] ## Day 57.5
  LaterMalMean <- mean(filter(M296SetDateOrd, Type=="Mal" & Day > 57.5)$CmprsRatio)  ## mean = 1.07
  TypeColors <- as.character(M296SetDateOrd$Type)
  TypeColors <- gsub("Mal", "gray68",TypeColors)
  TypeColors <- gsub("Fem", "gray39",TypeColors)
  TypeColors <- gsub("Cop", "black",TypeColors)
  TypeShapes <- as.character(M296SetDateOrd$Type)
  TypeShapes <- gsub("Mal", "17",TypeShapes)
  TypeShapes <- gsub("Fem", "19",TypeShapes)
  TypeShapes <- gsub("Cop", "15",TypeShapes)
  TypeShapes <- as.numeric(TypeShapes)

  ####################### Fig. 4 of Masius MS
  par(mar=c(4.8,4.6,1,1), cex=1.4, font=2, font.axis=2, font.lab=2)
plot(M296SetDateOrd$CmprsRatio ~ M296SetDateOrd$Day, type="p", xlab = "Sample day for Male 296", ylab="Compression ratio", bty="l", pch=TypeShapes, col=TypeColors)
  text(60,7,"Means: Mal = 1.1, Fem = 2.6, Cop = 3.8")
      ##  Regressions for ablines on ll. 201-207 
  abline(1.18, -0.001, lwd=2, col="gray68")         ## Mal Slope = -0.001 Y-int = 1.18
  abline(2.54, 0.001, lwd=2, col="gray39", lty= 2) ## Fem Slope =  0.001 Y-int = 2.54
  abline(3.74, 0.0005, lwd=2, col="black")          ## Cop Slope =  0.0005 Y-int = 3.74

    ####################### Bfr/AfterCop ~ Compressibility 
which(M296SetDateOrd$Type == "Cop")
  BfrAftr <- as_factor(c(rep(0,58),rep(1, 95)))    ## 153-58
  Cmprss <- M296SetDateOrd$CmprsRatio
  M296CmprBfAf <- glm(BfrAftr ~ Cmprss, family = "binomial"); summary(M296CmprBfAf)
    ## Deviance Residuals: 
    ##    Min       1Q     Median       3Q       Max  
    ##  -1.9478  -1.2795   0.7862      1.0444   1.1211  
    ##              Estimate   Std. Error z value   Pr(>|z|)  
    ## (Intercept)  -0.2140     0.3372     -0.635   0.5258  
    ##  Cmprss       0.4347     0.1903      2.284   0.0224
    ##  Null deviance: 203.07  on 152  degrees of freedom
    ##  Residual deviance: 196.51  on 151  degrees of freedom
    ##   AIC: 200.51   Number of Fisher Scoring iterations: 4

 LineFitM296ObsCmpr <- lm(M296SetDateOrd$CmprsRatio ~ M296SetDateOrd$Day); summary(LineFitM296ObsCmpr)
   ## Residuals:
   ##   Min      1Q    Median   3Q      Max 
   ## -1.0733 -0.7645 -0.4915  0.4880  6.8111  
   ##               Estimate    Std. Error t value  Pr(>|t|)
   ## (Intercept)   1.550067   0.192049    8.071    2.02e-13 ***
   ## Slope         0.002237   0.002164    1.034    0.303 
   ## Resid stndrd error: 1.182 on 151 degrees of freedom
   ## Mltpl R-squared:  0.007031,	Adj R-squared:  0.0004546 
   ## F-statistic: 1.069 on 1 and 151 DF,  p-value: 0.3028

######################## Before and after 1st Copulation 
      ##################### analysis Bow-Neck

CopData <- filter(CoreColsMFC, Type == "Cop") %>%
  dplyr::select(UID, Time, Behavior)

CopTimes <- filter(CopData, Behavior == "Cop") %>%
  distinct(UID, .keep_all = TRUE) 
  NumBouts <- length(CopTimes$Time)
  Mean1stCopTime <- mean(CopTimes$Time) ## 129.1246 as before (seconds fr start of bout)
  sd1stCopTime   <- sd(CopTimes$Time)   ## 41.32241 as before
  range(CopTimes$Time) ## 70.428 212.724

CopBowNckData <- filter(CopData, Behavior == "Bow" | Behavior == "Neck")

PrePostCopCountTable <- as.data.frame(matrix(ncol=5,nrow=NumBouts))
  colnames(PrePostCopCountTable) <- c("Bout","PreBowCnt","PostBowCnt", "PreNeckCnt", "PostNeckCnt")
for (i in 1:length(CopTimes$Time)) {
  PrePostCopCountTable[i,1] <- CopTimes$UID[i]
  CurrBout <- filter(CopBowNckData, UID == CopTimes$UID[i])
  PreCopBow <- filter(CurrBout, Time < CopTimes$Time[i] & Behavior == "Bow")
  PrePostCopCountTable[i,2] <- length(PreCopBow$Behavior)
  PreCopNck <- filter(CurrBout, Time < CopTimes$Time[i] & Behavior == "Neck")
  PrePostCopCountTable[i,3] <- length(PreCopNck$Behavior) 
  
  PostCopBow <- filter(CurrBout, Time > CopTimes$Time[i] & Behavior == "Bow")
  PrePostCopCountTable[i,4] <- length(PostCopBow$Behavior)
  PostCopNck <- filter(CurrBout, Time > CopTimes$Time[i] & Behavior == "Neck")
  PrePostCopCountTable[i,5] <- length(PostCopNck$Behavior) 
}

PrePostTbl <- as_tibble(PrePostCopCountTable) %>%
  summarize_at(vars(PreBowCnt:PostNeckCnt),c(mean, sd))
PrePostCopTable <- as.data.frame(matrix(ncol=2,nrow=4))
  colnames(PrePostCopTable) <- c("Mean","sd")
  rownames(PrePostCopTable) <- c("Pre-cop Bows","Post-cop Bows", "Pre-cop Necks", "Post-cop Necks")
PrePostCopTable[,1] <- as_vector(PrePostTbl[1, 1:4])
PrePostCopTable[,2] <- as_vector(PrePostTbl[1, 5:8])

write.csv(PrePostCopTable, "./Outputs/PrePostCopBowNckTable1Jun20.csv")

######################## Female identity

## Proportion of Fem + Cop bouts where fem was 8200-8400
FemPres <- filter(CoreColsMFClean, Type=="Fem" | Type=="Cop")
  NumFCbouts <- length(unique(FemPres$UID)) ## N = 114
  FCbtSet <- unique(FemPres$UID)
  NumBandedFems <- rep(NA,NumFCbouts)
  Num8200Fems <- rep(NA,NumFCbouts)
  Num8300Fems <- rep(NA,NumFCbouts)
  Num8400Fems <- rep(NA,NumFCbouts)

for (i in 1:NumFCbouts) { ## i <- 1
  CurrFCbout <- filter(FemPres, UID == FCbtSet[i])
  ifelse(CurrFCbout$FemID < 8000, NumBandedFems[i] <- 1,NumBandedFems[i] <- 0)
  ifelse(CurrFCbout$FemID == 8200, Num8200Fems[i] <- 1,Num8200Fems[i] <- 0)
  ifelse(CurrFCbout$FemID == 8300, Num8300Fems[i] <- 1,Num8300Fems[i] <- 0)
  ifelse(CurrFCbout$FemID == 8400, Num8400Fems[i] <- 1,Num8400Fems[i] <- 0)
  }  
sum(NumBandedFems)  ## 42 of 114 bouts were for banded females
sum(Num8200Fems)    ## 35 of 114 bouts were for 8200 (suspected females)
sum(Num8300Fems)    ## 15 of 114 bouts were for 8300 (unknown sex)
sum(Num8400Fems)    ## 22 of 114 bouts were for 8400 (suspected pre-def males)

FullIDfile <- filter(CoreColsMFC, Behavior == "Start")
  FullIDfile <- dplyr::select(FullIDfile, UID, ObsDate, Log, Total.length, Male1ID, FemID, JulianWeek)
  ComboIDFile <- inner_join(FullIDfile, CmprsEntrpTibl)
  ComboIDFile <- dplyr::select(ComboIDFile,-CompressLength,-Big3P,-Entrop,-NumCodes)
  ComboIDFile <- filter(ComboIDFile, FemID < 8000) ## length(ComboFile$UID) N = 238
TotalFem <- sort(unique(ComboIDFile$FemID))  
  NumFems <- length(TotalFem)
  ## FemID 118  289  292  294  295  935  936  959  972  977  981  984  988  990
  ##  N     1    7    1    7    4    3    2    1    1    1    8    2    1    3

FemIDTable <- as.data.frame(matrix(ncol = 8,nrow = NumFems))
  colnames(FemIDTable) <- c("FemID", "NumBouts","FirstObsDate","LastObsDate","Span", "Cop","NumMales", "NumLogs")  
  FemIDTable <- as_tibble(FemIDTable)
  FemIDTable$FemID    <- TotalFem
  FemIDTable$NumBouts <- unname(table(ComboIDFile$FemID))
  FemIDTable$Cop <- "N"
  
CopFile <- filter(ComboIDFile, Type == "Cop") 
 CopFem <- sort(unique(CopFile$FemID))  ## N = 4
 FemIDTable$Cop[which(FemIDTable$FemID %in% CopFem)] <- "Y"

for (i in 1:NumFems) { ## i <- 1
  CurrFemFile <- filter(ComboIDFile, FemID == TotalFem[i])
  FemIDTable$FirstObsDate[i] <- as.character(min(CurrFemFile$ObsDate))
  FemIDTable$LastObsDate[i]  <- as.character(max(CurrFemFile$ObsDate))
  FemIDTable$Span[i]         <- max(CurrFemFile$ObsDate) - min(CurrFemFile$ObsDate) + 1
  FemIDTable$NumMales[i]     <- length(unique(CurrFemFile$Male1ID))
  FemIDTable$NumLogs[i]      <- length(unique(CurrFemFile$Log))
}
FemIDTable$FirstObsDate <- as_date(FemIDTable$FirstObsDate)  ## str(FemIDTable$FirstObsDate)
FemIDTable$LastObsDate  <- as_date(FemIDTable$LastObsDate)
################## IMPORT file of dates when females were banded 
   ## ƒ "Masius video9 analysis R/Data" file "FemID1Jun20.csv"
 FemBandDateFile <- file.choose()
     FemBandDate <- read.csv(FemBandDateFile, header = TRUE, check.names=FALSE, stringsAsFactors=F) 
## Make dates calculable in tidyverse
     FemBandDate$DateBanded <- mdy(FemBandDate$DateBanded) 
     
 FemIDTableFull <-    left_join(FemIDTable,FemBandDate) %>%
   mutate(FullSpan = as.numeric(LastObsDate - DateBanded + 1))
   FemIDTableFull$FirstObsDate <- format(FemIDTableFull$FirstObsDate,"%d-%b-%y") 
   FemIDTableFull$LastObsDate <- format(FemIDTableFull$LastObsDate,"%d-%b-%y")
   FemIDTableFull$DateBanded <- format(FemIDTableFull$DateBanded,"%d-%b-%y")
 
write.csv(FemIDTableFull,"./Outputs/AllFemIDTable1Jun20.csv") 

OneBoutFemID <- FemIDTableFull$FemID[which(FemIDTableFull$NumBouts == 1)]
Num1BtFems <- length(OneBoutFemID)
     
SingleBoutFems <- as.data.frame(matrix(ncol = 6,nrow = Num1BtFems))
  colnames(SingleBoutFems) <- c("FemID", "UID", "Date", "Log","MaleID","Type")
  SingleBoutFems$FemID <- OneBoutFemID
for (i in 1:Num1BtFems) {
  CurrFem <- filter(CoreColsMFC, FemID == OneBoutFemID[i]) 
  SingleBoutFems$UID[i]    <- unique(CurrFem$UID)
  SingleBoutFems$Date[i]   <- format(unique(CurrFem$ObsDate),"%d-%b-%y")
  SingleBoutFems$Log[i]    <- unique(CurrFem$Log)
  SingleBoutFems$MaleID[i] <- unique(CurrFem$Male1ID)
  SingleBoutFems$Type[i]   <- unique(CurrFem$Type)
}
     
write.csv(SingleBoutFems,"./Outputs/SinglBtFems1Jun20.csv")

#######################  Histories for Fems {289, 294, 295,935,936,959,981,990}

Fem981Hist <- filter(CoreColsMFC, FemID == "981") ## No copulation, banded 24-Jul-14
  Fem981Hist <- dplyr::select(Fem981Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
   unique(Fem981Hist$Bird2ID)        ## {NA  107 a/LW®-LW/LW©  banded 15-Aug-16 pre-def}
Fem981HstShrt <- filter(Fem981Hist, Behavior == "ALAD" | Behavior == "AttC")
  Fem981HstShrt <- arrange(Fem981HstShrt,ObsDate)
  Fem981HstShrt$ObsDate <- format(Fem981HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem981HstShrt,"./Outputs/Fem981Hist2Jun20.csv")  
   
Fem295Hist <- filter(CoreColsMFC, FemID == "295") ## Cop w/ M, banded 14-Jul-15
 Fem295Hist <- dplyr::select(Fem295Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
unique(Fem295Hist$Bird2ID)        ## {NA  8000}
Fem295HstShrt <- filter(Fem295Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem295HstShrt <- arrange(Fem295HstShrt,ObsDate)
  Fem295HstShrt$ObsDate <- format(Fem295HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem295HstShrt,"./Outputs/Fem295Hist2Jun20.csv")   
   
Fem990Hist <- filter(CoreColsMFC, FemID == "990") ## Cop w/ M, banded 14-Jul-15
  Fem990Hist <- dplyr::select(Fem990Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
unique(Fem990Hist$Bird2ID)        ## {NA  8300}
Fem990HstShrt <- filter(Fem990Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem990HstShrt <- arrange(Fem990HstShrt,ObsDate)
  Fem990HstShrt$ObsDate <- format(Fem990HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem990HstShrt,"./Outputs/Fem990Hist2Jun20.csv")    
 
Fem935Hist <- filter(CoreColsMFC, FemID == "935") 
  Fem935Hist <- dplyr::select(Fem935Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
  unique(Fem935Hist$Bird2ID)        ## {NA}
Fem935HstShrt <- filter(Fem935Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem935HstShrt <- arrange(Fem935HstShrt,ObsDate)
  Fem935HstShrt$ObsDate <- format(Fem935HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem935HstShrt,"./Outputs/Fem935Hist2Jun20.csv") 

Fem294Hist <- filter(CoreColsMFC, FemID == "294") 
  Fem294Hist <- dplyr::select(Fem294Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
  unique(Fem294Hist$Bird2ID)        ## {NA}
Fem294HstShrt <- filter(Fem294Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem294HstShrt <- arrange(Fem294HstShrt,ObsDate)
  Fem294HstShrt$ObsDate <- format(Fem294HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem294HstShrt,"./Outputs/Fem294Hist2Jun20.csv") 

Fem289Hist <- filter(CoreColsMFC, FemID == "289") 
  Fem289Hist <- dplyr::select(Fem289Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
  unique(Fem289Hist$Bird2ID)        ## {NA}
Fem289HstShrt <- filter(Fem289Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem289HstShrt <- arrange(Fem289HstShrt,ObsDate)
  Fem289HstShrt$ObsDate <- format(Fem289HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem289HstShrt,"./Outputs/Fem289Hist2Jun20.csv") 

Fem936Hist <- filter(CoreColsMFC, FemID == "936") 
  Fem936Hist <- dplyr::select(Fem936Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
  unique(Fem936Hist$Bird2ID)        ## {NA}
Fem936HstShrt <- filter(Fem936Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem936HstShrt <- arrange(Fem936HstShrt,ObsDate)
  Fem936HstShrt$ObsDate <- format(Fem936HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem936HstShrt,"./Outputs/Fem936Hist2Jun20.csv") 

Fem959Hist <- filter(CoreColsMFC, FemID == "959") 
  Fem959Hist <- dplyr::select(Fem959Hist, -FemID, -Bird2ID, -FemUpDown, -JulianWeek, -CopM, -CodedBehav)   
  unique(Fem959Hist$Bird2ID)        ## {NA}
Fem959HstShrt <- filter(Fem959Hist, Behavior == "ALAD" | Behavior == "AttC" | Behavior == "Cop")
  Fem959HstShrt <- arrange(Fem959HstShrt,ObsDate)
  Fem959HstShrt$ObsDate <- format(Fem959HstShrt$ObsDate, "%d-%b-%y") 
write.csv(Fem959HstShrt,"./Outputs/Fem959Hist2Jun20.csv") 

#######################  Older miscellaneous
  
Fem296 <- sort(unique(M296SetDateOrd$FemID))
table(M296SetDateOrd$FemID)  
## FemID    294  295  935  936  959  981  984  990
## ObsNum    5    1    1    2    1    3    2    2 
## Type      F5   C    F   C2    C  FFF   F2   FC   For Male 296

Fem980 <- sort(unique(M980SetDateOrd$FemID))
table(M980SetDateOrd$FemID)   
## FemID    289  295  972  977  981   (Cop w/8200 UID 989, 991, 992) 
## ObsNum    2    1    1    1    1   
## Type     FF    F    F    F    F          For Male 980

M112Hist <- filter(ComboIDFile, Male1ID == "112")
  ## 20 UID 3-Oct-17 to 20-Dec-17; 1st 8 all Mal, last 12 all Fem

#############################################################################
## NEW BowP, ALADP, NeckP tabulation for Mal, Fem Cop bouts

MalLines <- filter(CoreColsMFClean, Type == "Mal")  ## 9,184 rows
  MalBouts <- unique(MalLines$UID)  ## LIST OF 100 Mal BOUTS
  NumMbouts <- length(MalBouts)
MalBig3Tbl <- as.data.frame(matrix(ncol=10,nrow=NumMbouts))
  colnames(MalBig3Tbl) <- c("UID","ElemCnt","BowP","ALADP","NeckP","Big3P","Big2P","Bn","An","Nn")
  MalBig3Tbl$UID <- MalBouts

for (i in 1:NumMbouts) {     ## i <- 1
  CurrBout <- filter(MalLines, UID == MalBouts[i])
  NumElems <- length(CurrBout$UID)
  MalBig3Tbl$ElemCnt[i] <- NumElems
  MalBig3Tbl$Bn[i] <- length(which(CurrBout$Behavior == "Bow"))
  MalBig3Tbl$An[i] <- length(which(CurrBout$Behavior == "ALAD"))
  MalBig3Tbl$Nn[i] <- length(which(CurrBout$Behavior == "Neck"))
  MalBig3Tbl$BowP[i]  <- length(which(CurrBout$Behavior == "Bow"))/NumElems
  MalBig3Tbl$ALADP[i] <- length(which(CurrBout$Behavior == "ALAD"))/NumElems
  MalBig3Tbl$NeckP[i] <- length(which(CurrBout$Behavior == "Neck"))/NumElems
  MalBig3Tbl$Big3P[i] <- (MalBig3Tbl$Bn[i] + MalBig3Tbl$An[i] + MalBig3Tbl$Nn[i])/NumElems
  MalBig3Tbl$Big2P[i] <- (MalBig3Tbl$Bn[i] + MalBig3Tbl$Nn[i])/NumElems
  }

FemLines <- filter(CoreColsMFClean, Type == "Fem")  ## 10,900 rows
 FemBouts <- unique(FemLines$UID)  ## LIST OF 100 FEM BOUTS
 NumFbouts <- length(FemBouts)
FemBig3Tbl <- as.data.frame(matrix(ncol=10,nrow=NumFbouts))
  colnames(FemBig3Tbl) <- c("UID","ElemCnt","BowP","ALADP","NeckP","Big3P","Big2P","Bn","An","Nn")
  FemBig3Tbl$UID <- FemBouts
  
for (i in 1:NumFbouts) {     ## i <- 1
  CurrBout <- filter(FemLines, UID == FemBouts[i])
  NumElems <- length(CurrBout$UID)
  FemBig3Tbl$ElemCnt[i] <- NumElems
  FemBig3Tbl$Bn[i] <- length(which(CurrBout$Behavior == "Bow"))
  FemBig3Tbl$An[i] <- length(which(CurrBout$Behavior == "ALAD"))
  FemBig3Tbl$Nn[i] <- length(which(CurrBout$Behavior == "Neck"))
  FemBig3Tbl$BowP[i]  <- length(which(CurrBout$Behavior == "Bow"))/NumElems
  FemBig3Tbl$ALADP[i] <- length(which(CurrBout$Behavior == "ALAD"))/NumElems
  FemBig3Tbl$NeckP[i] <- length(which(CurrBout$Behavior == "Neck"))/NumElems
  FemBig3Tbl$Big3P[i] <- (FemBig3Tbl$Bn[i] + FemBig3Tbl$An[i] + FemBig3Tbl$Nn[i])/NumElems
  FemBig3Tbl$Big2P[i] <- (FemBig3Tbl$Bn[i] + FemBig3Tbl$Nn[i])/NumElems
  }

CopLines <- filter(CoreColsMFClean, Type == "Cop")  ## 10,900 rows
  CopBouts <- unique(CopLines$UID)  ## LIST OF 100 Cop BOUTS
  NumCbouts <- length(CopBouts)
CopBig3Tbl <- as.data.frame(matrix(ncol=9,nrow=NumCbouts))
  colnames(CopBig3Tbl) <- c("UID","ElemCnt","BowP","ALADP","NeckP","Big3P","Big2P","Bn","An","Nn")
  CopBig3Tbl$UID <- CopBouts
  
for (i in 1:NumCbouts) {     ## i <- 1
    CurrBout <- filter(CopLines, UID == CopBouts[i])
    NumElems <- length(CurrBout$UID)
    CopBig3Tbl$ElemCnt[i] <- NumElems
    CopBig3Tbl$Bn[i] <- length(which(CurrBout$Behavior == "Bow"))
    CopBig3Tbl$An[i] <- length(which(CurrBout$Behavior == "ALAD"))
    CopBig3Tbl$Nn[i] <- length(which(CurrBout$Behavior == "Neck"))
    CopBig3Tbl$BowP[i]  <- length(which(CurrBout$Behavior == "Bow"))/NumElems
    CopBig3Tbl$ALADP[i] <- length(which(CurrBout$Behavior == "ALAD"))/NumElems
    CopBig3Tbl$NeckP[i] <- length(which(CurrBout$Behavior == "Neck"))/NumElems
    CopBig3Tbl$Big3P[i] <- (CopBig3Tbl$Bn[i] + CopBig3Tbl$An[i] + CopBig3Tbl$Nn[i])/NumElems
    CopBig3Tbl$Big2P[i] <- (CopBig3Tbl$Bn[i] + CopBig3Tbl$Nn[i])/NumElems
    }  
  
boxplot(MalBig3Tbl$BowP,MalBig3Tbl$ALADP,MalBig3Tbl$NeckP,
          FemBig3Tbl$BowP,FemBig3Tbl$ALADP,FemBig3Tbl$NeckP,
          CopBig3Tbl$BowP,CopBig3Tbl$ALADP,CopBig3Tbl$NeckP,
        names = c("MalBowP", "MalALADP", "MalNeckP",
                  "FemBowP", "FemALADP", "FemNeckP",
                  "CopBowP", "CopALADP", "CopNeckP"))

boxplot(MalBig3Tbl$Big3P,FemBig3Tbl$Big3P,CopBig3Tbl$Big3P,
        names = c("MalBig3P", "FemBig3P", "CopBig3P"))

boxplot(MalBig3Tbl$Big2P,FemBig3Tbl$Big2P,CopBig3Tbl$Big2P,
        names = c("MalBwNkP", "FemBwNkP", "CopBwNkP"))

write.csv(MalBig3Tbl,"./Outputs/MalBwNkTble30Sep20.csv",row.names=F)
write.csv(FemBig3Tbl,"./Outputs/FemBwNkTble30Sep20.csv",row.names=F)
write.csv(CopBig3Tbl,"./Outputs/CopBwNkTble30Sep20.csv",row.names=F)

########### Number of Mal & Fem bouts that have Neck twists 
CoreColsMNeck <- filter(CoreColsMFC, Behavior == "Neck" & Type == "Mal")
length(unique(CoreColsMNeck$UID)) ## 30 of 198 Mal bouts have Neck twists
   ## unique(CoreColsMFC$Behavior)
CoreColsFNeck <- filter(CoreColsMFC, Behavior == "Neck" & Type == "Fem")
 length(unique(CoreColsFNeck$UID)) ## 96 of 100 Mal bouts have Neck twists
CoreColsCNeck <- filter(CoreColsMFC, Behavior == "Neck" & Type == "Cop")
 length(unique(CoreColsCNeck$UID)) ## 14 of 14 Mal bouts have Neck twists
 
 ########### Variation in pre-cop Bows by Male ID
 
 CopSet <- filter(CoreColsMFClean, Type == "Cop")
 UIDset <- unique(FirstCopSet$UID)
 FirstCopTimeID <- as.data.frame(matrix(ncol=11,nrow=14))
   colnames(FirstCopTimeID) <- c("UID", "MalID", "Time1stCop","EvntNmCp","Time1stBow","EvntNmBw1","nBow","sdBow","nBreaks", "sdBreaks","PreALAD")
   FirstCopTimeID$UID <- UIDset
 for (i in 1:length(UIDset)) {   ## i <- 1
   CurrUID <- UIDset[i]
     CurrCopSet <- filter(CopSet, UID == CurrUID) ## CurrCopSet$Behavior
     Cops <- filter(CurrCopSet, Behavior == "Cop")
   FirstCopTimeID$Time1stCop[i] <- min(Cops$Time)
   FirstCopTimeID$MalID[i] <- unique(Cops$Male1ID)
     PreCopSet <- filter(CurrCopSet, Time <= min(Cops$Time))
     print(CurrUID)
     print(PreCopSet$Behavior)
   FirstCopTimeID$EvntNmCp[i] <- length(PreCopSet$UID)
   FirstCopTimeID$nBow[i] <- length(filter(PreCopSet, Behavior == "Bow"))
     BowEventNumSet <- which(PreCopSet$Behavior == "Bow")
   FirstCopTimeID$EvntNmBw1[i] <-  min(BowEventNumSet)
   FirstCopTimeID$nBow[i] <- length(BowEventNumSet)
     NonBowEventNumSet <- which(PreCopSet$Behavior != "Bow")
     NonBowEventNumSet <- NonBowEventNumSet[which(NonBowEventNumSet > min(BowEventNumSet))]
     BreakSet <- rep(NA, length(NonBowEventNumSet))
     for (j in 1:length(NonBowEventNumSet)) {    ## j <- 1
       ifelse(j==1,   BreakSet[j] <- NonBowEventNumSet[j],
           ifelse(NonBowEventNumSet[j] != NonBowEventNumSet[j-1]+1, 
               BreakSet[j] <- NonBowEventNumSet[j], BreakSet[j] <- NA)) 
     }
     BreakSet <- BreakSet[which(BreakSet>0)]
   FirstCopTimeID$nBreaks[i] <- length(BreakSet)
     Bows <- filter(CurrCopSet, Behavior == "Bow")
   FirstCopTimeID$Time1stBow[i] <- min(Bows$Time)
     ## Does a Bow precede the pre-Cop ALAD?
   FirstCopTimeID$PreALAD[i] <- PreCopSet$Behavior[length(PreCopSet$UID)-2] 
 }
range(FirstCopTimeID$nBow) ## 25 to 76 Bows pre-Cop
mean(FirstCopTimeID$nBow)   # mean = 54
sd(FirstCopTimeID$nBow)     # sd = 16.875
range(FirstCopTimeID$EvntNmCp)    ## 37 to 87 Event number for 1st Cop
mean(FirstCopTimeID$EvntNmCp)     ##      mean = 65.571
sd(FirstCopTimeID$EvntNmCp)       ##        sd = 16.346

range(FirstCopTimeID$nBow[1:3]) ## MAL 980    51 to 67 Bows pre-Cop
mean(FirstCopTimeID$nBow[1:3])  ## MAL 980       mean = 60.333
sd(FirstCopTimeID$nBow[1:3])    ## MAL 980       sd = 8.33
range(FirstCopTimeID$EvntNmCp[1:3])    ## MAL 980    56 to 71 Event number for 1st Cop
mean(FirstCopTimeID$EvntNmCp[1:3])     ## MAL 980       mean = 66
sd(FirstCopTimeID$EvntNmCp[1:3])       ## MAL 980       sd = 8.66

range(FirstCopTimeID$nBow[4:13]) ## MAL 296    25 to 76 Bows pre-Cop
mean(FirstCopTimeID$nBow[4:13])  ## MAL 296       mean = 54.2
sd(FirstCopTimeID$nBow[4:13])    ## MAL 296       sd = 18.26
range(FirstCopTimeID$EvntNmCp[4:13])    ## MAL 296    43 to 87 Event number for 1st Cop
mean(FirstCopTimeID$EvntNmCp[4:13])     ## MAL 296       mean = 68.3
sd(FirstCopTimeID$EvntNmCp[4:13])       ## MAL 296       sd = 16.439

TtestNBows2Coppers <- t.test(FirstCopTimeID$nBow[1:3], FirstCopTimeID$nBow[4:13])
  ## Welch Two Sample t-test t = 0.81624, df = 8.1607, p-value = 0.4375
EffectSize <- (mean(FirstCopTimeID$nBow[1:3]) - mean(FirstCopTimeID$nBow[4:13]))/sd(FirstCopTimeID$nBow[1:13])
   ## 0.3740094 for NumBows Mal 980 bs. NumBows Mal 296
range(FirstCopTimeID$nBow[14])     ## MAL 940      33 Bows pre-Cop
range(FirstCopTimeID$EvntNmCp[14]) ## MAL 940      37 Event number for 1st Cop

   ## See "14 Cop sets.docx" for analysis
   ## Analyses below excludes unusual UID 1824 and 1987) 
PreBowBehavs <- c(4, 6, 2, 5, 1, 2, 3, 13, 2, 17, 3, 3) ## length(PreBowBehavs) = 12
range(PreBowBehavs)  ## 1 to 17
mean(PreBowBehavs)
sd(PreBowBehavs)   ## 5.08 ± 4.91 

########### Count of distinct element types in the 312 bouts (for Liam's barplot of MCF element count)
  ## Created 10-Oct-22 CodedBhvMFC has 21 distinct CodedBhv for the 312 bouts (UIDs)
CodedBhvMFC <- dplyr::select(CoreColsMFClean, UID, Type, CodedBehav)
UID312set <- unique(CodedBhvMFC$UID)
UIDnum <- length(UID312set)

ElemCountTable <- as.data.frame(matrix(ncol=5,nrow=UIDnum))
  colnames(ElemCountTable) <- c("UID", "Type", "Count", "Length", "CntLngth")
  ElemCountTable$UID <- UID312set
  ElemCountTable$Type <- "typ"
  ElemCountTable$Count <- 0
  ElemCountTable <- as_tibble(ElemCountTable)
for (i in 1:UIDnum)   { ## i <- 1
  CurrBout <- filter(CodedBhvMFC, UID == UID312set[i])
  ElemCountTable$Type[i] <- CurrBout$Type[1]
  ElemCountTable$Count[i] <- length(unique(CurrBout$CodedBehav)) 
  ElemCountTable$Length[i] <- length(CurrBout$CodedBehav) 
  ElemCountTable$CntLngth[i] <- ElemCountTable$Count[i]/ElemCountTable$Length[i]
}
  ggplot(ElemCountTable, aes(x=factor(Type), y=Count)) +
    geom_boxplot()
  ggplot(ElemCountTable, aes(x=factor(Type), y=CntLngth)) +
    geom_boxplot()
  ggplot(ElemCountTable, aes(x=factor(Type), y=Length)) +
    geom_boxplot()
  
ElmSumCntTable <- as.data.frame(matrix(ncol=9,nrow=3))
  colnames(ElmSumCntTable) <- c("Type", "Mean", "Min", "Max", "Var", "sd", "Median", "2ndQuart", "3rdQuart")  
  ElmSumCntTable$Type <- c("Mal", "Fem", "Cop")
MalCount <- filter(ElemCountTable, Type == "Mal") 
  ElmSumCntTable[1,2] <- mean(MalCount$Count) 
  ElmSumCntTable[1,3] <- min(MalCount$Count) ## 0% bottom of whisker
  ElmSumCntTable[1,4] <- max(MalCount$Count) ## 100% top of whisker
  ElmSumCntTable[1,5] <- var(MalCount$Count)
  ElmSumCntTable[1,6] <- sd(MalCount$Count)
  ElmSumCntTable[1,7] <- median(MalCount$Count) ## 50% middle of box
  ElmSumCntTable[1,8] <- quantile(MalCount$Count)[[2]] ## 25% bottom of box
  ElmSumCntTable[1,9] <- quantile(MalCount$Count)[[4]] ## 75% top of box
FemCount <- filter(ElemCountTable, Type == "Fem") 
  ElmSumCntTable[2,2] <- mean(FemCount$Count)
  ElmSumCntTable[2,3] <- min(FemCount$Count) ## 0% bottom of whisker
  ElmSumCntTable[2,4] <- max(FemCount$Count) ## 100% top of whisker
  ElmSumCntTable[2,5] <- var(FemCount$Count)
  ElmSumCntTable[2,6] <- sd(FemCount$Count)
  ElmSumCntTable[2,7] <- median(FemCount$Count) ## 50% middle of box
  ElmSumCntTable[2,8] <- quantile(FemCount$Count)[[2]] ## 25% bottom of box
  ElmSumCntTable[2,9] <- quantile(FemCount$Count)[[4]] ## 75% top of box
CopCount <- filter(ElemCountTable, Type == "Cop") 
  ElmSumCntTable[3,2] <- mean(CopCount$Count)
  ElmSumCntTable[3,3] <- min(CopCount$Count) ## 0% bottom of whisker
  ElmSumCntTable[3,4] <- max(CopCount$Count) ## 100% top of whisker
  ElmSumCntTable[3,5] <- var(CopCount$Count)
  ElmSumCntTable[3,6] <- sd(CopCount$Count)
  ElmSumCntTable[3,7] <- median(CopCount$Count) ## 50% middle of box
  ElmSumCntTable[3,8] <- quantile(CopCount$Count)[[2]] ## 25% bottom of box
  ElmSumCntTable[3,9] <- quantile(CopCount$Count)[[4]] ## 75% top of box
  
  write.csv(ElmSumCntTable,"./Outputs/ElmSumCntTbl10Oct22.csv")
 
ElmCntLngthRatioTable <- as.data.frame(matrix(ncol=9,nrow=3))
  colnames(ElmCntLngthRatioTable) <- c("Type", "Mean", "Min", "Max", "Var", "sd", "Median", "2ndQuart", "3rdQuart")  
  ElmCntLngthRatioTable$Type <- c("Mal", "Fem", "Cop")
  MalCount <- filter(ElemCountTable, Type == "Mal") 
  ElmCntLngthRatioTable[1,2] <- mean(MalCount$CntLngth) 
  ElmCntLngthRatioTable[1,3] <- min(MalCount$CntLngth) ## 0% bottom of whisker
  ElmCntLngthRatioTable[1,4] <- max(MalCount$CntLngth) ## 100% top of whisker
  ElmCntLngthRatioTable[1,5] <- var(MalCount$CntLngth)
  ElmCntLngthRatioTable[1,6] <- sd(MalCount$CntLngth)
  ElmCntLngthRatioTable[1,7] <- median(MalCount$CntLngth) ## 50% middle of box
  ElmCntLngthRatioTable[1,8] <- quantile(MalCount$CntLngth)[[2]] ## 25% bottom of box
  ElmCntLngthRatioTable[1,9] <- quantile(MalCount$CntLngth)[[4]] ## 75% top of box
  FemCount <- filter(ElemCountTable, Type == "Fem") 
  ElmCntLngthRatioTable[2,2] <- mean(FemCount$CntLngth)
  ElmCntLngthRatioTable[2,3] <- min(FemCount$CntLngth) ## 0% bottom of whisker
  ElmCntLngthRatioTable[2,4] <- max(FemCount$CntLngth) ## 100% top of whisker
  ElmCntLngthRatioTable[2,5] <- var(FemCount$CntLngth)
  ElmCntLngthRatioTable[2,6] <- sd(FemCount$CntLngth)
  ElmCntLngthRatioTable[2,7] <- median(FemCount$CntLngth) ## 50% middle of box
  ElmCntLngthRatioTable[2,8] <- quantile(FemCount$CntLngth)[[2]] ## 25% bottom of box
  ElmCntLngthRatioTable[2,9] <- quantile(FemCount$CntLngth)[[4]] ## 75% top of box
  CopCount <- filter(ElemCountTable, Type == "Cop") 
  ElmCntLngthRatioTable[3,2] <- mean(CopCount$CntLngth)
  ElmCntLngthRatioTable[3,3] <- min(CopCount$CntLngth) ## 0% bottom of whisker
  ElmCntLngthRatioTable[3,4] <- max(CopCount$CntLngth) ## 100% top of whisker
  ElmCntLngthRatioTable[3,5] <- var(CopCount$CntLngth)
  ElmCntLngthRatioTable[3,6] <- sd(CopCount$CntLngth)
  ElmCntLngthRatioTable[3,7] <- median(CopCount$CntLngth) ## 50% middle of box
  ElmCntLngthRatioTable[3,8] <- quantile(CopCount$CntLngth)[[2]] ## 25% bottom of box
  ElmCntLngthRatioTable[3,9] <- quantile(CopCount$CntLngth)[[4]] ## 75% top of box 

  write.csv(ElmCntLngthRatioTable,"./Outputs/ElmCntLngthRatio10Oct22.csv")
 
############## Algoritmic complexity Max elements = 9, max substring length = 12
    ## must use local_complexity
ElemTblCmplx <- add_column(ElemCountTable, LcLCmlx= 0.00)

BigAlphBouts <- which(ElemCountTable$Count > 9)  ## str(BigAlphBouts)
  BoutList22 <- ElemCountTable$UID[BigAlphBouts]

 ## Extract bigAlph UIDs  str(CodedBhvMFC$UID)
BigAlphTbl <- filter(CodedBhvMFC, UID %in% BoutList22) ## Has 3,810 rows
BigUIDlist <- unique(BigAlphTbl$UID) ## length(BigUIDlist)
SmallCntTbl <- filter(ElemCountTable, UID %in% BoutList22) # tibble with just the 22 bouts 
    ## with >9 distinct elements
for (i in 1:length(BigUIDlist)) {  ## i <- 1
  CurrUID <- BigUIDlist[i] 
  CurrentBout <- filter(BigAlphTbl,UID == CurrUID)
  ShrtStrng <- CurrentBout$CodedBehav  ## length(ShrtStrng)
  DelNum <- length(unique(ShrtStrng)) - 9
  ExtraNum <- as_tibble(sort(table(ShrtStrng)))  ## class(ExtraNum) = table sort(ExtraNum)
  AlphList <- ExtraNum[,1][[1]] ## extracts the ranked alphabet list
  AlphList[1:DelNum]
  Deleters <- as.numeric(which(ShrtStrng %in% c(AlphList[1:DelNum]))) ## str(Deleters)
  Clean <- ShrtStrng[-c(Deleters)] ## length(Clean) = 109 unique(Clean) n=9
  ConcatShrt <- str_c(Clean, sep = "", collapse = "")
  CmplxTbl <- local_complexity(ConcatShrt, alphabet = 9, span=12) ## str(CmplxTbl)
  Vals <- unlist(CmplxTbl) 
  MeanVal <- mean(Vals, na.rm=T)
  Index <- which(ElemTblCmplx$UID == CurrUID)   ## ElemTblCmplx[13,]
  ElemTblCmplx$LcLCmlx[Index] <- MeanVal
}   

## Extract smallAlph UIDs 
SmallAlphBouts <- which(ElemCountTable$Count < 10)  ## str(BigAlphBouts)
BoutList190 <- ElemCountTable$UID[SmallAlphBouts]
SmallAlphTbl <- filter(CodedBhvMFC, UID %in% BoutList190) ## Has 17,585 rows
SmallUIDlist <- unique(SmallAlphTbl$UID) ## length(BigUIDlist)
SmallCntTbl <- filter(ElemCountTable, UID %in% BoutList190) # tibble with just the 190 bouts 
   ## with <10 distinct elements (i.e., manageable under local_complexity() )
for (i in 1:length(SmallUIDlist)) {  ## i <- 1
  CurrUID <- SmallUIDlist[i] 
  CurrentBout <- filter(SmallAlphTbl,UID == CurrUID)
  ShrtStrng <- CurrentBout$CodedBehav  ## length(ShrtStrng)
  ConcatShrt <- str_c(ShrtStrng, sep = "", collapse = "")
  CmplxTbl <- local_complexity(ConcatShrt, alphabet = 9, span=12) ## str(CmplxTbl)
  Vals <- unlist(CmplxTbl) 
  MeanVal <- mean(Vals, na.rm=T)
  Index <- which(ElemTblCmplx$UID == CurrUID)   ## ElemTblCmplx[13,]
  ElemTblCmplx$LcLCmlx[Index] <- MeanVal
}   
MalCmplx <- mean(ElemTblCmplx$LcLCmlx[1:198])   ## Mean 45.65071
   unique(ElemTblCmplx$Type[1:198])
FemCmplx <- mean(ElemTblCmplx$LcLCmlx[199:297]) ## Mean 35.8381
   unique(ElemTblCmplx$Type[199:297])
CopCmplx <- mean(ElemTblCmplx$LcLCmlx[298:312]) ## Mean 32.93926
   unique(ElemTblCmplx$Type[298:312])
   
   ggplot(ElemTblCmplx, aes(x=factor(Type), y=LcLCmlx)) +
     geom_boxplot()
   
write.csv(ElemTblCmplx,"./Outputs/ElemTblCmplx11Oct22.csv")
############################### END