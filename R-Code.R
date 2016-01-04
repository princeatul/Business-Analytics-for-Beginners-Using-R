getwd()
setwd("D:/Crowd/Learnign-Startup")
setwd(choose.dir())

##Importing data
data_1 <- read.csv("CAX_Startup_Data.csv", header = TRUE, as.is=T)
View(data_1)
summary(data_1)
##Cehcking Structure of data
str(data_1)

##Checking for NA data
table(is.na(data_1))
##678 NA new-8063

# R code for replacing 'No Info' and 'blanks' with NA
data_1[data_1=="No Info"]<- NA
data_1[data_1==""]<- NA

# R code for converting column as date
data_1$Est..Founding.Date <- as.Date(data_1$Est..Founding.Date, "%m/%d/%Y")
data_1$Last.Funding.Date <- as.Date(data_1$Last.Funding.Date, "%m/%d/%Y")

# display column header of data
colnames(data_1)
# display row names of data
rownames(data_1)

# R code for converting character vector to numeric
# noting columns that needs to be converted to numeric
col<- c(3:5,10,11,18:23,25,61,66,68:70,72,74,88,92,94:96,98,99,102:116)

# using for loop for converting column as numeric
for(i in col)
{
  data_1[,i]<-as.numeric(data_1[,i])
}

# Percent missing value for each variable
mis_val<-sapply(data_1, function(x) sum(is.na(x)))
percent_mis<-as.data.frame(round((mis_val/nrow(data_1))*100,1))

# making data frame with variable and missing value percent for filtering
name<-row.names(percent_mis)
pcnt_mis_var<-cbind(name,percent_mis)
row.names(pcnt_mis_var)<-NULL
colnames(pcnt_mis_var)<-c("variable","Percent.Missing")

View(pcnt_mis_var)
dim(pcnt_mis_var)

# keeping only variables with less than 40% missing
new_var<-pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing<=40)]
new_startup<-data_1[new_var]

# separate data frame for more than 40% missing
other_var<-pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing>40)]
other_data<-data_1[other_var]



# Separate data frame for numeric variables

type_col <- cbind(seq(from = 1, to = 111),(sapply(new_startup, typeof)))
row.names(type_col)<-NULL
colnames(type_col)<-c("Column_Number","Data_Type")
dim(type_col)
type_col <- as.data.frame(type_col)

cnt_df_name <- type_col$Column_Number[which(type_col$Data_Type != "character")]
char_df_name <- type_col$Column_Number[which(type_col$Data_Type == "character")]
cnt_df <- new_startup[,cnt_df_name]
char_df <- new_startup[,char_df_name]
str(cnt_df_name)
class(cnt_df_name)
char_df_name
View(char_df_name)
cnt_df_name<-as.numeric(as.character(cnt_df_name))
char_df_name<-as.numeric(as.character(char_df_name))
str(new_startup[,cnt_df_name])
cnt_df$Team.size.all.employees
colnames(new_startup)[35]
######################Exploring Variables##################

# checking distribution of continuous variable for outlier detection and missing values
summary(cnt_df$Team.size.all.employees)
quantile(cnt_df$Team.size.all.employees, probs = seq(0, 1, by= 0.05),na.rm=T)

# further exploration to determine cutoff for capping
quantile(cnt_df$Team.size.all.employees, probs = seq(.9, 1, by= 0.01),na.rm=T)

# capping values
cnt_df$Team.size.all.employees[cnt_df$Team.size.all.employees>103.8]<-103.8

View(cnt_df)
#preparing percent skill investment column
summary(cnt_df$Percent_skill_Investment)
quantile(cnt_df$Percent_skill_Investment, probs=seq(0,1,by=0.05),na.rm=T)
quantile(cnt_df$Percent_skill_Investment, probs=seq(0.95,1,by=0.01),na.rm=T)
cnt_df$Percent_skill_Investment[cnt_df$Percent_skill_Investment>12.5]<-12.5

#preparing Number of investors in seed column
summary(cnt_df$Number.of.Investors.in.Seed)
quantile(cnt_df$Number.of.Investors.in.Seed, probs=seq(0,1,by=0.05),na.rm=T)
quantile(cnt_df$Number.of.Investors.in.Seed, probs=seq(0.95,1,by=0.01),na.rm=T)
cnt_df$Number.of.Investors.in.Seed[cnt_df$Number.of.Investors.in.Seed>12.78]<-12.78

#preparing renowed in professional circle
summary(cnt_df$Renowned.in.professional.circle)
quantile(cnt_df$Renowned.in.professional.circle, probs=seq(0,1,by=0.05),na.rm=T)
quantile(cnt_df$Renowned.in.professional.circle, probs=seq(0,0.05,by=0.01),na.rm=T)
cnt_df$Number.of.Investors.in.Seed[cnt_df$Number.of.Investors.in.Seed<144]<-144

# preparing experiecne in fortune 100

#################continuous variable#####################################
# checking distribution of categorical variable
table(char_df$Local.or.global.player,useNA="always")

# convert a variable to uppercase
char_df$Local.or.global.player<-toupper(char_df$Local.or.global.player)

# trimming whitespaces
char_df$Local.or.global.player<-trimws(char_df$Local.or.global.player)
?trimws

# Recoding variable levels and converting to factor variable
char_df$Local.or.global.player[char_df$Local.or.global.player=='LOCAL']<-0
char_df$Local.or.global.player[char_df$Local.or.global.player=='GLOBAL']<-1
char_df$Local.or.global.player<- as.factor(char_df$Local.or.global.player)



