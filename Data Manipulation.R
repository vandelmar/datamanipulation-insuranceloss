getwd()
setwd("D:/Courses/Data Science/Dibimbing/Day 6")
getwd()

## 1. Import Insurance Loss Data set
insurance <- read.table("Insurance_Loss.csv", header=TRUE, sep=",")
head(insurance)
str(insurance)
summary(insurance)
dim(insurance)

insurance

library(dplyr)
library(datasets)

#creating a local dataframe.
#now data will be in tabular structure
il <- tbl_df(insurance)
il

# Summary Information
prop.table(table(il$Number.of.Vehicles))
summary(il)

#rename variabel
il1 <- il %>% rename (pn = Policy.Number, driveExp = Years.of.Driving.Experience,
               noVehic = Number.of.Vehicles)
il1 %>% print(n = 25)


## 2. Check outliers, missing data and dummies variables

library(tidyr)    

#check column that has NA
str(il)
summary(il) #no column has NA

il1

#check IQR
x = insurance["Losses"]
x

Q1 = quantile(x, 0.25, na.rm = TRUE)
Q1
Q3 = quantile(x, 0.75, na.rm = TRUE)
Q3
IQR = Q3-Q1
IQR

Lower_Whisker = Q1-(1.5*IQR)
Lower_Whisker

Upper_Whisker = Q3+(1.5*IQR)
Upper_Whisker

##check outliers data 
out = subset(insurance, x > Upper_Whisker | x < Lower_Whisker)
out

dim(out)
summary(out)

##check non outliers data
df <- x[!(x > Upper_Whisker | x < Lower_Whisker),]
df

nonout = subset(insurance, x < Upper_Whisker & x > Lower_Whisker)
nonout

dim(nonout)
summary(nonout)

#Create simple Boxplot
str(insurance)
boxplot(insurance$Losses, col = "orange", border = "brown")

#Return value of Boxplot
b <- boxplot(insurance$Losses)
b

##check missing data
library(mice)

summary(nonout)

md.pattern(nonout) #No need for mice. This data set is completely observed

##dummy variable
head(nonout)

nonout$Male <- ifelse(nonout$Gender == 'M', 1, 0) # 1=M, 0=F
nonout$Female <- ifelse(nonout$Gender == 'F', 1, 0) # 1=F, 0=M
head(nonout)

library(fastDummies)
nonout1 <- dummy_cols(nonout, select_columns = 'Gender')
head(nonout1)

#Remove columns
nonout2 <- dummy_cols(nonout, select_columns = "Gender", 
                        remove_selected_columns = TRUE)
head(nonout2)

#remove column Male & Female
nonout <- nonout[-(9:10)]
head(nonout)


## 3. Define business question of data In PPT

### Profile Vehicle

#rename variabel
nonout3 <- nonout %>% rename (pn = Policy.Number, driveExp = Years.of.Driving.Experience,
                      noVehic = Number.of.Vehicles, VehicAge = Vehicle.Age, Fuel = Fuel.Type)
nonout3

# Check FuelType dan SumLosses
ftl <- nonout3 %>%
    group_by(Fuel) %>%
    summarise(SumLosses = sum(Losses))
ftl

# Check % SummLosses based on FuelType
pft = transform(ftl, Percent = round(ftl$SumLosses/sum(ftl$SumLosses)*100, 2))
pft


#ggplot
library(ggplot2)

# Create plot
ppft = pft$Percent
barplot(ppft, ylim=c(0,100), main="Percentase Losses based on FuelType",
        names.arg = pft$Fuel, ylab = "Percentase", xlab = "Fuel Type",
        cex.names = 0.8, col = c("orange", "red"))


# check vehicle age dan sumlosses
val <- nonout3 %>%
  group_by(VehicAge) %>%
  summarise(SumLosses = sum(Losses))
val

# Check % SummLosses based on Vehicle Age
pva = transform(val, Percent = round(val$SumLosses/sum(val$SumLosses)*100, 2))
pva

# Create plot
ppva = pva$Percent
barplot(ppva, ylim=c(0,10), main="Percentase Losses based on Vehicle Age",
        names.arg = pva$VehicAge, ylab = "Percentase", xlab = "Vehicle Age",
        cex.names = 0.8, col = heat.colors(16))

# check profile no.vehic : variable no.vehicle, vehicle age, fuel type, dan sum losses
fnv <- nonout3 %>%
  group_by(noVehic) %>%
  summarise(SumLosses= sum(Losses))
fnv

# Check % SummLosses based on noVehic
pnv = transform(fnv, Percent = round(fnv$SumLosses/sum(fnv$SumLosses)*100, 2))
pnv

# Create plot
ppnv = pnv$Percent
barplot(ppnv, ylim=c(0,50), main="Percentase Losses based on Number Vehicle",
        names.arg = pnv$noVehic, ylab = "Percentase", xlab = "Number Vehicle",
        cex.names = 0.8, col = heat.colors(4))

### Profile Customer

head(nonout3)
summary(nonout)

# check profile Age dan sum losses
fag <- nonout3 %>%
  group_by(Age) %>%
  summarise(SumLosses= sum(Losses)) %>%
  print(n = 60)
fag

#Age Group
x <- nonout3$Age
x[x<"71" & x>"60"]="61-70"
x
x[x<"61" & x>"50"]="51-60"
x
x[x<"51" & x>"40"]="41-50"
x[x<"41" & x>"30"]="31-40"
x[x<"31" & x>"20"]="21-30"
x[x<"21" & x>"10"]="11-20"
x

#Menambah kolom Age Group
nonout4 = transform(nonout3, AgeGroup = x)
head(nonout4)
summary(nonout4)

# Check Sum Losses based on Profile Customer : AgeGroup
lag <- nonout4 %>%
  group_by(AgeGroup) %>%
  summarise(SumLosses= sum(Losses))
lag

# Check % SummLosses based on AgeGroup
pag = transform(lag, Percent = round(lag$SumLosses/sum(lag$SumLosses)*100, 2))
pag

# Create plot SummLosses based on AgeGroup
ppag = pag$Percent
barplot(ppag, ylim=c(0,30), main="Percentase Losses based on Age Group",
        names.arg = pag$AgeGroup, ylab = "Percentase", xlab = "Age",
        cex.names = 0.8, col = heat.colors(6))


# Check Sum Losses based on Profile Customer : Gender
summary(nonout1)
head(nonout1)

lg <- nonout1 %>%
  group_by(Gender) %>%
  summarise(SumLosses= sum(Losses))
lg

# Check % SummLosses based on Gender
pg = transform(lg, Percent = round(lg$SumLosses/sum(lg$SumLosses)*100, 2))
pg

# Create plot SummLosses based on Gender
pgen = pg$Percent
pgen = paste(pgen,"%",sep = "")
pie(pg$SumLosses, labels = pgen, main="Percentage Losses based on Gender",
    col = c("red", "blue"))
colors= c("red", "blue")
legend(1,0.5, c(pg$Gender), cex=0.8, fill = colors)


# Check Sum Losses based on Profile Customer : Married
summary(nonout3)
head(nonout3)

lm <- nonout3 %>%
  group_by(Married) %>%
  summarise(SumLosses= sum(Losses))
lm

# Check % SummLosses based on Married
pm = transform(lm, Percent = round(lm$SumLosses/sum(lm$SumLosses)*100, 2))
pm

# Create plot SummLosses based on Married
pmr = pm$Percent
pmr = paste(pmr,"%",sep = "")
pie(pm$SumLosses, labels = pmr, main="Percentage Losses based on Married",
    col = c("red", "white"))
colors= c("red", "white")
legend(1,0.5, c(pm$Married), cex=0.8, fill = colors)

# Check Sum Losses based on Profile Customer : Drive Experience
summary(nonout3)
head(nonout3)

nonout3$driveExp
#DriveExp Group
y <- nonout3$driveExp
y[y=="0"]="0-10"
y
y[y=="1"]="0-10"
y
y[y=="2"]="0-10"
y
y[y=="3"]="0-10"
y
y[y=="4"]="0-10"
y
y[y=="5"]="0-10"
y
y[y=="6"]="0-10"
y
y[y=="7"]="0-10"
y
y[y=="8"]="0-10"
y
y[y=="9"]="0-10"
y
y[y=="10"]="0-10"
y
y[y>"10" & y<"21"]="11-20"
y
y[y>"20" & y<"31"]="21-30"
y
y[y>"30" & y<"41"]="31-40"
y
y[y>"40" & y<"51"]="41-50"
y
y[y>"50" & y<"61"]="51-60"
y

#Menambah kolom DriveExp Group
nonout5 = transform(nonout4, DriveExpGroup = y)
head(nonout5)
summary(nonout5)
prop.table(table(nonout5$DriveExpGroup)) #proporsi isi tabel DriveExpGroup

# Check Sum Losses based on Profile Customer : DriveExp
lde <- nonout5 %>%
  group_by(DriveExpGroup) %>%
  summarise(SumLosses= sum(Losses))
lde

# Check % SummLosses based on Drive Experience
pld = transform(lde, Percent = round(lde$SumLosses/sum(lde$SumLosses)*100, 2))
pld

# Create plot SummLosses based on Drive Experience
ppld = pld$Percent
barplot(ppld, ylim=c(0,50), main="Percentage Losses based on Drive Experience",
        names.arg = pld$DriveExpGroup, ylab = "Percentage", xlab = "Year",
        cex.names = 0.8, col = heat.colors(6))


#2)Profile customer yang klaim diatas rata-rata : AgeGroup, DriveExpGroup, Gender, Married


##AgeGroup

pc <- nonout5 %>%
  group_by(AgeGroup) %>%
  select(AgeGroup, Losses) %>%
  filter(Losses > mean(Losses)) %>%
  arrange(desc(Losses))
pc

#menampilkan frekuensi dari AgeGroup yang nilai Losses diatas rata-rata
fpc = table(pc[,1])
fpc

#check type data fpc
class(fpc)

#diubah menjadi data frame
fpc = as.data.frame(fpc)
fpc

#rename
names(fpc)[1]='AgeGroup'
fpc

# Check % Freq based on AgeGroup yg losses di atas rata2
pfa = transform(fpc, Percent = round(fpc$Freq/sum(fpc$Freq)*100, 2))
pfa

# Create simple plot % Freq based on AgeGroup yg losses di atas rata2
plot(pfa$AgeGroup,pfa$Percent, xlab = "Age", ylab = "Percentage", ylim = c(0,30))


## DriveExperience

pd <- nonout5 %>%
  group_by(DriveExpGroup) %>%
  select(DriveExpGroup, Losses) %>%
  filter(Losses > mean(Losses)) %>%
  arrange(desc(Losses))
pd

#menampilkan frekuensi dari DriveExperience yang nilai Losses diatas rata-rata
fpd = table(pd[,1])
fpd

#check type data fpd
class(fpd)

#diubah menjadi data frame
fpd = as.data.frame(fpd)
fpd

#rename
names(fpd)[1]='DriveExpGroup'
fpd

# Check % Freq based on DriveExperience yg losses di atas rata2
pfd = transform(fpd, Percent = round(fpd$Freq/sum(fpd$Freq)*100, 2))
pfd

# Create simple plot % Freq based on DriveExperience yg losses di atas rata2
plot(pfd$DriveExpGroup,pfd$Percent, xlab = "Year", ylab = "Percentage", ylim = c(0,50))


## Gender
head(nonout5)

pg <- nonout5 %>%
  group_by(Gender) %>%
  select(Gender, Losses) %>%
  filter(Losses > mean(Losses)) %>%
  arrange(desc(Losses))
pg

#menampilkan frekuensi dari DriveExperience yang nilai Losses diatas rata-rata
fpg = table(pg[,1])
fpg

#check type data fpg
class(fpg)

#diubah menjadi data frame
fpg = as.data.frame(fpg)
fpg

#rename
names(fpg)[1]='Gender'
fpg

# Check % Freq based on Gender yg losses di atas rata2
pfg = transform(fpg, Percent = round(fpg$Freq/sum(fpg$Freq)*100, 2))
pfg

# Create simple plot % Freq based on Gender yg losses di atas rata2
plot(pfg$Gender,pfg$Percent, xlab = "Gender", ylab = "Percentage", ylim = c(0,60))


## Married
head(nonout5)

pm <- nonout5 %>%
  group_by(Married) %>%
  select(Married, Losses) %>%
  filter(Losses > mean(Losses)) %>%
  arrange(desc(Losses))
pm

#menampilkan frekuensi dari Married yang nilai Losses diatas rata-rata
fpm = table(pm[,1])
fpm

#check type data fpg
class(fpm)

#diubah menjadi data frame
fpm = as.data.frame(fpm)
fpm

#rename
names(fpm)[1]='Married'
fpm

# Check % Freq based on Married yg losses di atas rata2
pfm = transform(fpm, Percent = round(fpm$Freq/sum(fpm$Freq)*100, 2))
pfm

# Create simple plot % Freq based on Married yg losses di atas rata2
plot(pfm$Married,pfm$Percent, xlab = "Status", ylab = "Percentage", ylim = c(0,60))


## 4. Give recommendation to business to manage insurance in PPT


##############################################################################



  
  
  
  