setwd("/home/yoon/mnt/SSD1T/HIRA-NPS_2011/NPS_2011")
file.list<-dir()
file.list.20<-file.list[grep("_20_",file.list)]
file.list.30<-file.list[grep("_30_",file.list)]
file.list.40<-file.list[grep("_40_",file.list)]
file.list.53<-file.list[grep("_53_",file.list)]

#20 table
file.path=file.list.20[1]
Temp.data <- read.csv(file.path,nrows=10) #첫 10개 행만 가져오기
head(Temp.data)
colnames(Temp.data) #전체 열 구조 파악 

req.col<-c("FOM_CD", "PAT_AGE", "SEX_TP_CD", "INSUP_CD", "RECU_FR_DD", "RVD_INSUP_BRAMT"
           , "RVD_SLF_BRAMT", "RVD_RPE_TAMT", "YNO", "MSICK_CD", "key", "NO")
#필요한 열만 선택 
#FOM_CD 서식코드 
#PAT_AGE 나이 (2개월 이하:0.2, 3-12개월:0.9 ) 
#SEX_TP_CD 성별 (9:기타, 1:남, 2:여) 
#INSUP_CD 보험자코드 / 14
#RECU_FR_DD 요양개시일 (방문의 고유 키) / 한번 방문에 여러 서식 발생 가능하기 때문
#RVD_INSUP_BRAMT 심결보험자부담금  
#RVD_SLF_BRAMT 심결본인부담금
#RVD_RPE_TAMT 심결요양급여비용총액
#YNO 요양기관 고유번호 (1~2자리: 지역코드)
#MSICK_CD 주상병코드
#Key 명세서조인키
#NO 수진자고유번호
req.col.idx<-which(colnames(Temp.data)%in%req.col)
length(req.col)==length(req.col.idx)
col.select<-rep("NULL",length(Temp.data))
col.select[req.col.idx]<-NA #선택할 열만 입력

Data.20<-Temp.data[0,req.col]
for(i in 1:length(file.list.20)){
  Data.20 <- rbind(Data.20,read.csv(file.list.20[i], as.is=TRUE, colClasses=col.select))
}
gc() 

nrow(Data.20)
head(Data.20)

#40 table
file.path=file.list.40[1]
Temp.data <- read.csv(file.path,nrows=10)
head(Temp.data)
colnames(Temp.data)

req.col<-c("key", "SNO", "DMD_SICK_SYM")
#key
#SNO
#DMD_SICK_SYM
req.col.idx<-which(colnames(Temp.data)%in%req.col)
length(req.col)==length(req.col.idx)
col.select<-rep("NULL",length(Temp.data))
col.select[req.col.idx]<-NA #선택할 열만 입력

Data.40<-Temp.data[0,req.col]
for(i in 1:length(file.list.40)){
  Data.40 <- rbind(Data.40,read.csv(file.list.40[i], as.is=TRUE, colClasses=col.select))
}
gc() 

nrow(Data.40)
head(Data.40)
L20.key<-Data.40[substring(Data.40$DMD_SICK_SYM,1,3)=="L20","key"]

#Data preprocessing
str(Data.20)
hist(Data.20$PAT_AGE)
hist(Data.20$SEX_TP_CD)

Data.20<-Data.20[Data.20$PAT_AGE<150,]
Data.20<-Data.20[Data.20$SEX_TP_CD<3,]
gc()


from=0
to=89
by=5
age.group.seq<-seq(from=from,to=to,by=by)
Data.20$age_group<-NA
for(i in 1:length(age.group.seq)){
  Data.20[Data.20$PAT_AGE>=age.group.seq[i]&Data.20$PAT_AGE<(age.group.seq[i]+(by)),"age_group"]<-age.group.seq[i]
}
head(Data.20)
Data.20.L20<-Data.20[Data.20$key%in%L20.key,]
head(Data.20.L20)

#유병률
uniq.length<-function(a){
  length(unique(a))
}

Data.age<-read.csv("C:/HIRA-NPS_2011/age_statistics.csv",head=TRUE) #연령대별 인구조사데이터
Data.age.korea<-Data.age[Data.age$local==0, 2:5] #연령대별 인구조사데이터_전국
n.korea.male<-as.integer(Data.age.korea$male) #연령대별 인구_전국_남자
n.korea.female<-as.integer(Data.age.korea$female) #연령대별 인구_전국_여자

Data.20.L20.male<-Data.20.L20[Data.20.L20$SEX_TP_CD%in%1,] #아토피_남자
n.L20.male<-tapply(Data.20.L20.male$NO, Data.20.L20.male$age_group, uniq.length) #연령대별 아토피_남자
n.L20.male.year<-tapply(Data.20.L20.male$NO, Data.20.L20.male$PAT_AGE, uniq.length) #나이별 아토피_남자
Data.20.L20.female<-Data.20.L20[Data.20.L20$SEX_TP_CD%in%2,] #아토피_여자
n.L20.female<-tapply(Data.20.L20.female$NO, Data.20.L20.female$age_group, uniq.length) #연령대별 아토피_여자
n.L20.female.year<-tapply(Data.20.L20.female$NO, Data.20.L20.female$PAT_AGE, uniq.length) #나이별 아토피_여자

#연령별 성별유병률
prev.male <- round(n.L20.male/n.korea.male*100, digits=3)
prev.female <- round(n.L20.female/n.korea.female*100, digits=3)
prev.tot <-round((n.L20.male+n.L20.female)/(n.korea.male+n.korea.female)*100, digits=3)
prev <- cbind(prev.male, prev.female, prev.tot)
colnames(prev) = c("남자", "여자", "전체")
rownames(prev) = c("0-4세","5-9세","10-14세","15-19세","20-24세","25-29세","30-34세","35-39세","40-44세","45-49세","50-54세",
                   "55-59세","60-64세","65-69세","70-74세","75-79세","80-84세","85-89세")
prev

# Create a table plot 
install.packages("htmlTable")
library(htmlTable)
htmlTable(prev, rowlabel='연령',caption="연령별 성별유병률(%)" )

#전체유병률 
prev.all<-sum(n.L20.male, n.L20.female)/sum(n.korea.male, n.korea.female) *100
print(c('전체유병률(%) : ', prev.all))


#아토피 환자의 연령/성별/지역 분포
local.names<-c(11,21,22,23,24,25,26,31,32,33,34,35,36,37,38,39)
#11=서울, 21=부산, 22=인천, 23=대구, 24=광주, 25=대전, 26=울산, 31=경기, 32=강원, 33=충북, 34=충남, 35=전북, 36=전남, 37=경북, 38=경남, 39=제주
#Sample 데이터는 임의의 번호 사용 

local.n.L20<-table(substring(Data.20.L20$YNO,1,2)) #지역별 총환자수
local.n.L20<-local.n.L20[names(local.n.L20)%in%local.names]

Data.20.L20.male.young<-Data.20.L20.male[Data.20.L20.male$PAT_AGE%in%0:19,] #아토피_남자_소아
Data.20.L20.male.adult<-Data.20.L20.male[Data.20.L20.male$PAT_AGE%in%20:89,] #아토피_남자_성인
Data.20.L20.female.young<-Data.20.L20.female[Data.20.L20.female$PAT_AGE%in%0:19,] #아토피_여자_소아
Data.20.L20.female.adult<-Data.20.L20.female[Data.20.L20.female$PAT_AGE%in%20:89,] #아토피_여자_성인

local.n.L20.male.young<-table(substring(Data.20.L20.male.young$YNO,1,2)) #지역별_남자/여자_소아/성인
local.n.L20.female.young<-table(substring(Data.20.L20.female.young$YNO,1,2))
local.n.L20.male.adult<-table(substring(Data.20.L20.male.adult$YNO,1,2))
local.n.L20.female.adult<-table(substring(Data.20.L20.female.adult$YNO,1,2))

local.n.L20.male.young<-local.n.L20.male.young[names(local.n.L20.male.young)%in%local.names]
local.n.L20.female.young<-local.n.L20.female.young[names(local.n.L20.female.young)%in%local.names]
local.n.L20.male.adult<-local.n.L20.male.adult[names(local.n.L20.male.adult)%in%local.names]
local.n.L20.female.adult<-local.n.L20.female.adult[names(local.n.L20.female.adult)%in%local.names]

atopy<-cbind(local.n.L20.male.young, local.n.L20.female.young, local.n.L20.male.adult, local.n.L20.female.adult)

local.n.tot<- tapply(Data.age$total, Data.age$local, FUN=sum) #지역별_총인구
local.n.tot<-local.n.tot[names(local.n.tot)%in%local.names]

Data.age.young<-Data.age[Data.age$age%in%0:19, 1:5] #지역별_소아/성인 총인구
Data.age.adult<-Data.age[Data.age$age%in%20:89, 1:5]

local.n.tot.male.young<- tapply(Data.age.young$male, Data.age.young$local, FUN=sum) #지역별_소아/성인_남자/여자총인구
local.n.tot.female.young<- tapply(Data.age.young$female, Data.age.young$local, FUN=sum)
local.n.tot.male.adult<- tapply(Data.age.adult$male, Data.age.adult$local, FUN=sum)
local.n.tot.female.adult<- tapply(Data.age.adult$female, Data.age.adult$local, FUN=sum)

local.n.tot.male.young<-local.n.tot.male.young[names(local.n.tot.male.young)%in%local.names]
local.n.tot.female.young<-local.n.tot.female.young[names(local.n.tot.female.young)%in%local.names]
local.n.tot.male.adult<-local.n.tot.male.adult[names(local.n.tot.male.adult)%in%local.names]
local.n.tot.female.adult<-local.n.tot.female.adult[names(local.n.tot.female.adult)%in%local.names]

local<-cbind(local.n.tot.male.young, local.n.tot.female.young, local.n.tot.male.adult, local.n.tot.female.adult)

local.prev <- round(atopy/local*100, digits = 3) #지역별_소아/성인_남자/여자_유병률
local.prev<-cbind(local.n.L20, local.prev)
colnames(local.prev) = c("총환자수", "남자","여자", "남자","여자")
rownames(local.prev)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
htmlTable(local.prev, rowlabel='지역',n.cgroup=c(1,2,2), cgroup = c('','소아','성인'),caption="지역/연령/성별별 유병률(%)" )


#age group 0~19세:1년단위 / 20~89세:10년단위
age.group.seq<-c(0:19, seq(from=20,to=90,by=10))
for(i in 1:(length(age.group.seq)-1)){
  Data.20.L20[Data.20.L20$PAT_AGE>=age.group.seq[i]&Data.20.L20$PAT_AGE<age.group.seq[i+1],"age_group"]<-age.group.seq[i]
}
head(Data.20.L20)


#연령/성별/지역별 심결요양급여비용총액 비교
Age.cost.total<-round(tapply(Data.20.L20$RVD_RPE_TAMT,Data.20.L20$age_group,mean),digits=0)
Sex.cost.total<-round(tapply(Data.20.L20$RVD_RPE_TAMT,Data.20.L20$SEX_TP_CD,mean),digits=0)
local.cost.total<-round(tapply(Data.20.L20$RVD_RPE_TAMT,substring(Data.20.L20$YNO,1,2),mean),digits=0)
local.cost.total<-local.cost.total[names(local.cost.total)%in%local.names]
names(local.cost.total)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

#연령/성별/지역별 심결보험자부담금 비교
Age.cost.insurance<-round(tapply(Data.20.L20$RVD_INSUP_BRAMT,Data.20.L20$age_group,mean),digits=0)
Sex.cost.insurance<-round(tapply(Data.20.L20$RVD_INSUP_BRAMT,Data.20.L20$SEX_TP_CD,mean),digits=0)
local.cost.insurance<-round(tapply(Data.20.L20$RVD_INSUP_BRAMT,substring(Data.20.L20$YNO,1,2),mean),digits=0)
local.cost.insurance<-local.cost.insurance[names(local.cost.insurance)%in%local.names]
names(local.cost.insurance)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

#연령/성별/지역별 심결본인부담금 비교
Age.cost.self<-round(tapply(Data.20.L20$RVD_SLF_BRAMT,Data.20.L20$age_group,mean),digits=0)
Sex.cost.self<-round(tapply(Data.20.L20$RVD_SLF_BRAMT,Data.20.L20$SEX_TP_CD,mean),digits=0)
local.cost.self<-round(tapply(Data.20.L20$RVD_SLF_BRAMT,substring(Data.20.L20$YNO,1,2),mean),digits=0)
local.cost.self<-local.cost.self[names(local.cost.self)%in%local.names]
names(local.cost.self)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

Age.cost<-cbind(Age.cost.total,Age.cost.insurance,Age.cost.self)
colnames(Age.cost) = c("심결요양급여비용총액", "심결보험자부담금","심결본인부담금")
rownames(Age.cost) = c("0-1세","1-2세","2-3세","3-4세","4-5세","5-6세","6-7세","7-8세","8-9세","9-10세",
                       "10-11세","11-12세","12-13세","13-14세","14-15세","15-16세","16-17세","17-18세","18-19세","19-20세",
                       "20-29세","30-39세","40-49세","50-59세","60-69세","70-79세","80-89세")
htmlTable(Age.cost, rowlabel='연령대' ,caption = "나이별 진료비 특성(원/1인)")

Sex.cost<-cbind(Sex.cost.total,Sex.cost.insurance,Sex.cost.self)
colnames(Sex.cost) = c("심결요양급여비용총액", "심결보험자부담금","심결본인부담금")
rownames(Sex.cost) = c("남자", "여자")
htmlTable(Sex.cost, rowlabel='성별' ,caption = "성별별 진료비 특성(원/1인)")

local.cost<-cbind(local.cost.total,local.cost.insurance,local.cost.self)
colnames(local.cost) = c("심결요양급여비용총액", "심결보험자부담금","심결본인부담금")
rownames(local.cost)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
htmlTable(local.cost, rowlabel='지역' ,caption = "지역별 진료비 특성(원/1인)")


#방문 건수
Y_CD<-c(10,20,28,29,30,40,50,60,71,72,73,74,75,81,92,93)
Y_CD.names<-c("전문요양기관/종합병원", "병원", "요양기관", "정신요양기관", "의원" , "치과병원", "치과의원", "조산원", "보건소", "보건지소", "보건진료소", "모자보건센타", "보건의료원", "약국", "한방병원", "한방의원")
Y_CD.names.abb<-c("종합병원", "병원", "요양", "정신요양", "의원" , "치과병원", "치과의원", "조산원", "보건소", "보건지소", "진료소", "모자보건", "의료원", "약국", "한방병원", "한방의원")
#요양기관번호 3-4번째 자리: 요양기관종별
#10:전문요양기관/종합병원
#20:병원
#28:요양기관
#29:정신요양기관
#30:의원
#40:치과병원
#50:치과의원
#60:조산원
#71:보건소 
#72:보건지소 
#73:보건진료소 
#74:모자보건센타 
#75:보건의료원 
#81:약국
#92:한방병원
#93:한방의원
visit.count<-tapply(Data.20.L20$RECU_FR_DD, substring(Data.20.L20$YNO,3,4), uniq.length)
visit.count<-visit.count[names(visit.count)%in%Y_CD]
names(visit.count)<-c("전문요양기관/종합병원", "병원", "요양기관", "정신요양기관", "의원"
                      , "치과병원", "치과의원", "조산원", "보건소", "보건지소", "보건진료소"
                      , "모자보건센타", "보건의료원", "약국", "한방병원", "한방의원")
#연령별 방문 건수
visit.age<-rep(NA,length(Y_CD))
for(i in age.group.seq){
  Temp.data <- Data.20.L20[Data.20.L20$age_group==i, 9:10]
  visit.age.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.age.temp<-visit.age.temp[names(visit.age.temp)%in%Y_CD]
  visit.age <- rbind(visit.age, visit.age.temp)
}
visit.age<-visit.age[-1,]
colnames(visit.age)<-Y_CD.names.abb
rownames(visit.age) <- c("0-1세","1-2세","2-3세","3-4세","4-5세","5-6세","6-7세","7-8세","8-9세","9-10세",
                         "10-11세","11-12세","12-13세","13-14세","14-15세","15-16세","16-17세","17-18세","18-19세","19-20세",
                         "20-29세","30-39세","40-49세","50-59세","60-69세","70-79세","80-89세")
htmlTable(visit.age, rowlabel='연령대' ,caption = "연령별 요양기관별 방문 건수(회)")

#성별별 방문 건수
visit.sex<-NA
for(i in 1:2){
  Temp.data <- Data.20.L20[Data.20.L20$SEX_TP_CD==i, 9:10]
  visit.sex.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.sex.temp<-visit.sex.temp[names(visit.sex.temp)%in%Y_CD]
  visit.sex <- rbind(visit.sex, visit.sex.temp)
}
visit.sex<-visit.sex[-1,]
colnames(visit.sex)<-Y_CD.names.abb
rownames(visit.sex) <- c("남자","여자")
htmlTable(visit.sex, rowlabel='성별' ,caption = "성별별 요양기관별 방문 건수(회)")

#연도별 방문 건수
visit.year<-matrix(as.integer(0),5,16)
for(i in 2011:2015){
  Temp.data <- Data.20.L20[substring(Data.20.L20$RECU_FR_DD,1,4)==i, 9:10]
  visit.year.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.year.temp<-visit.year.temp[names(visit.year.temp)%in%Y_CD]
  if(length(visit.year.temp)!=0)visit.year[i-2010,] = visit.year.temp
}
colnames(visit.year)<-Y_CD.names.abb
rownames(visit.year) = c("2011년","2012년","2013년","2014년","2015년")
htmlTable(visit.year, rowlabel='연도' ,caption = "연도별 요양기관별 방문 건수(회)")

#지역별 방문 건수
visit.local<-NA
visit.local<-data.frame(names=Y_CD, visit.local)

for(i in local.names){
  Temp.data <- Data.20.L20[substring(Data.20.L20$YNO,1,2)==i, 9:10]
  visit.local.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.local.temp<-visit.local.temp[names(visit.local.temp)%in%Y_CD]
  visit.local.temp<-data.frame(names=names(visit.local.temp), visit.local.temp)
  visit.local<- merge(visit.local, visit.local.temp, by.x="names", by.y="names", all.x=TRUE)
}
visit.local[is.na(visit.local)] <- 0
visit.local<-t(visit.local)

visit.local<-visit.local[-1:-2,]
colnames(visit.local)<-Y_CD.names.abb
rownames(visit.local) <-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
htmlTable(visit.local, rowlabel='지역' ,caption = "지역별 요양기관별 방문 건수(회)")





#동반 피부질환이 없는 순수한 아토피피부염 환자
#L40:건선
#L71:장미진
#L21:지루피부염
#L80:백반증
#L43, K13, L66:편평태선

#Select data
L20.key<-Data.40[substring(Data.40$DMD_SICK_SYM,1,3)=="L20","key"]
L20.notKey<-Data.40[substring(Data.40$DMD_SICK_SYM,1,3)=="L40"
                    |substring(Data.40$DMD_SICK_SYM,1,3)=="L21"
                    |substring(Data.40$DMD_SICK_SYM,1,3)=="L80"
                    |substring(Data.40$DMD_SICK_SYM,1,3)=="L43"
                    |substring(Data.40$DMD_SICK_SYM,1,3)=="K13"
                    |substring(Data.40$DMD_SICK_SYM,1,3)=="L66","key"]

L20.key<-subset(L20.key, !(L20.key%in%L20.notKey))
Data.20.L20.new<-Data.20.L20[Data.20.L20$key%in%L20.key,]

#동일하게 재분석

#Data preprocessing

from=0
to=89
by=5
age.group.seq<-seq(from=from,to=to,by=by)
for(i in 1:length(age.group.seq)){
  Data.20.L20.new[Data.20.L20.new$PAT_AGE>=age.group.seq[i]&Data.20.L20.new$PAT_AGE<(age.group.seq[i]+(by)),"age_group"]<-age.group.seq[i]
}

#유병률
uniq.length<-function(a){
  length(unique(a))
}


Data.20.L20.new.male<-Data.20.L20.new[Data.20.L20.new$SEX_TP_CD%in%1,] #아토피_남자
n.L20.new.male<-tapply(Data.20.L20.new.male$NO, Data.20.L20.new.male$age_group, uniq.length) #연령대별 아토피_남자
n.L20new..male.year<-tapply(Data.20.L20.new.male$NO, Data.20.L20.new.male$PAT_AGE, uniq.length) #나이별 아토피_남자
Data.20.L20.new.female<-Data.20.L20.new[Data.20.L20.new$SEX_TP_CD%in%2,] #아토피_여자
n.L20.new.female<-tapply(Data.20.L20.new.female$NO, Data.20.L20.new.female$age_group, uniq.length) #연령대별 아토피_여자
n.L20.new.female.year<-tapply(Data.20.L20.new.female$NO, Data.20.L20.new.female$PAT_AGE, uniq.length) #나이별 아토피_여자

#연령별 성별유병률
prev.male.new <- round(n.L20.new.male/n.korea.male*100, digits=3)
prev.female.new <- round(n.L20.new.female/n.korea.female*100, digits=3)
prev.tot.new <-round((n.L20.new.male+n.L20.new.female)/(n.korea.male+n.korea.female)*100, digits=3)
prev.new <- cbind(prev.male.new, prev.female.new, prev.tot.new)
colnames(prev.new) = c("남자", "여자", "전체")
rownames(prev.new) = c("0-4세","5-9세","10-14세","15-19세","20-24세","25-29세","30-34세","35-39세","40-44세","45-49세","50-54세",
                   "55-59세","60-64세","65-69세","70-74세","75-79세","80-84세","85-89세")

htmlTable(prev.new, rowlabel='연령',caption="연령별 성별유병률(%)" )

#전체유병률 
prev.all.new<-sum(n.L20.new.male, n.L20.new.female)/sum(n.korea.male, n.korea.female) *100
print(c('전체유병률(%) : ', prev.all.new))


#아토피 환자의 연령/성별/지역 분포
local.names<-c(11,21,22,23,24,25,26,31,32,33,34,35,36,37,38,39)

local.n.L20.new<-table(substring(Data.20.L20.new$YNO,1,2)) #지역별 총환자수
local.n.L20.new<-local.n.L20.new[names(local.n.L20.new)%in%local.names]

Data.20.L20.new.male.young<-Data.20.L20.new.male[Data.20.L20.new.male$PAT_AGE%in%0:19,] #아토피_남자_소아
Data.20.L20.new.male.adult<-Data.20.L20.new.male[Data.20.L20.new.male$PAT_AGE%in%20:89,] #아토피_남자_성인
Data.20.L20.new.female.young<-Data.20.L20.new.female[Data.20.L20.new.female$PAT_AGE%in%0:19,] #아토피_여자_소아
Data.20.L20.new.female.adult<-Data.20.L20.new.female[Data.20.L20.new.female$PAT_AGE%in%20:89,] #아토피_여자_성인

local.n.L20.new.male.young<-table(substring(Data.20.L20.new.male.young$YNO,1,2)) #지역별_남자/여자_소아/성인
local.n.L20.new.female.young<-table(substring(Data.20.L20.new.female.young$YNO,1,2))
local.n.L20.new.male.adult<-table(substring(Data.20.L20.new.male.adult$YNO,1,2))
local.n.L20.new.female.adult<-table(substring(Data.20.L20.new.female.adult$YNO,1,2))

local.n.L20.new.male.young<-local.n.L20.new.male.young[names(local.n.L20.new.male.young)%in%local.names]
local.n.L20.new.female.young<-local.n.L20.new.female.young[names(local.n.L20.new.female.young)%in%local.names]
local.n.L20.new.male.adult<-local.n.L20.new.male.adult[names(local.n.L20.new.male.adult)%in%local.names]
local.n.L20.new.female.adult<-local.n.L20.new.female.adult[names(local.n.L20.new.female.adult)%in%local.names]

atopy.new<-cbind(local.n.L20.new.male.young, local.n.L20.new.female.young, local.n.L20.new.male.adult, local.n.L20.new.female.adult)

local.n.tot.new<- tapply(Data.age$total, Data.age$local, FUN=sum) #지역별_총인구
local.n.tot.new<-local.n.tot.new[names(local.n.tot.new)%in%local.names]

Data.age.young.new<-Data.age[Data.age$age%in%0:19, 1:5] #지역별_소아/성인 총인구
Data.age.adult.new<-Data.age[Data.age$age%in%20:89, 1:5]

local.n.tot.male.young.new<- tapply(Data.age.young.new$male, Data.age.young.new$local, FUN=sum) #지역별_소아/성인_남자/여자총인구
local.n.tot.female.young.new<- tapply(Data.age.young.new$female, Data.age.young.new$local, FUN=sum)
local.n.tot.male.adult.new<- tapply(Data.age.adult.new$male, Data.age.adult.new$local, FUN=sum)
local.n.tot.female.adult.new<- tapply(Data.age.adult.new$female, Data.age.adult.new$local, FUN=sum)

local.n.tot.male.young.new<-local.n.tot.male.young.new[names(local.n.tot.male.young.new)%in%local.names]
local.n.tot.female.young.new<-local.n.tot.female.young.new[names(local.n.tot.female.young.new)%in%local.names]
local.n.tot.male.adult.new<-local.n.tot.male.adult.new[names(local.n.tot.male.adult.new)%in%local.names]
local.n.tot.female.adult.new<-local.n.tot.female.adult.new[names(local.n.tot.female.adult.new)%in%local.names]

local.new<-cbind(local.n.tot.male.young.new, local.n.tot.female.young.new, local.n.tot.male.adult.new, local.n.tot.female.adult.new)

local.prev.new <- round(atopy.new/local.new*100, digits = 3) #지역별_소아/성인_남자/여자_유병률
local.prev.new<-cbind(local.n.L20.new, local.prev.new)
colnames(local.prev.new) = c("총환자수", "남자","여자", "남자","여자")
rownames(local.prev.new)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
htmlTable(local.prev.new, rowlabel='지역',n.cgroup=c(1,2,2), cgroup = c('','소아','성인'),caption="지역/연령/성별별 유병률(%)" )


#age group 0~19세:1년단위 / 20~89세:10년단위
age.group.seq<-c(0:19, seq(from=20,to=90,by=10))
for(i in 1:(length(age.group.seq)-1)){
  Data.20.L20.new[Data.20.L20.new$PAT_AGE>=age.group.seq[i]&Data.20.L20.new$PAT_AGE<age.group.seq[i+1],"age_group"]<-age.group.seq[i]
}
head(Data.20.L20.new)


#연령/성별/지역별 심결요양급여비용총액 비교
Age.cost.total.new<-round(tapply(Data.20.L20.new$RVD_RPE_TAMT,Data.20.L20.new$age_group,mean),digits=0)
Sex.cost.total.new<-round(tapply(Data.20.L20.new$RVD_RPE_TAMT,Data.20.L20.new$SEX_TP_CD,mean),digits=0)
local.cost.total.new<-round(tapply(Data.20.L20.new$RVD_RPE_TAMT,substring(Data.20.L20.new$YNO,1,2),mean),digits=0)
local.cost.total.new<-local.cost.total.new[names(local.cost.total.new)%in%local.names]
names(local.cost.total.new)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

#연령/성별/지역별 심결보험자부담금 비교
Age.cost.insurance.new<-round(tapply(Data.20.L20.new$RVD_INSUP_BRAMT,Data.20.L20.new$age_group,mean),digits=0)
Sex.cost.insurance.new<-round(tapply(Data.20.L20.new$RVD_INSUP_BRAMT,Data.20.L20.new$SEX_TP_CD,mean),digits=0)
local.cost.insurance.new<-round(tapply(Data.20.L20.new$RVD_INSUP_BRAMT,substring(Data.20.L20.new$YNO,1,2),mean),digits=0)
local.cost.insurance.new<-local.cost.insurance.new[names(local.cost.insurance.new)%in%local.names]
names(local.cost.insurance.new)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

#연령/성별/지역별 심결본인부담금 비교
Age.cost.self.new<-round(tapply(Data.20.L20.new$RVD_SLF_BRAMT,Data.20.L20.new$age_group,mean),digits=0)
Sex.cost.self.new<-round(tapply(Data.20.L20.new$RVD_SLF_BRAMT,Data.20.L20.new$SEX_TP_CD,mean),digits=0)
local.cost.self.new<-round(tapply(Data.20.L20.new$RVD_SLF_BRAMT,substring(Data.20.L20.new$YNO,1,2),mean),digits=0)
local.cost.self.new<-local.cost.self.new[names(local.cost.self.new)%in%local.names]
names(local.cost.self.new)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")

Age.cost.new<-cbind(Age.cost.total.new,Age.cost.insurance.new,Age.cost.self.new)
colnames(Age.cost.new) = c("심결요양급여비용총액", "심결보험자부담금","심결본인부담금")
rownames(Age.cost.new) = c("0-1세","1-2세","2-3세","3-4세","4-5세","5-6세","6-7세","7-8세","8-9세","9-10세",
                       "10-11세","11-12세","12-13세","13-14세","14-15세","15-16세","16-17세","17-18세","18-19세","19-20세",
                       "20-29세","30-39세","40-49세","50-59세","60-69세","70-79세","80-89세")
htmlTable(Age.cost.new, rowlabel='연령대' ,caption = "나이별 진료비 특성(원/1인)")

Sex.cost.new<-cbind(Sex.cost.total.new,Sex.cost.insurance.new,Sex.cost.self.new)
colnames(Sex.cost.new) = c("심결요양급여비용총액", "심결보험자부담금","심결본인부담금")
rownames(Sex.cost.new) = c("남자", "여자")
htmlTable(Sex.cost.new, rowlabel='성별' ,caption = "성별별 진료비 특성(원/1인)")

local.cost.new<-cbind(local.cost.total.new,local.cost.insurance.new,local.cost.self.new)
colnames(local.cost.new) = c("심결요양급여비용총액", "심결보험자부담금","심결본인부담금")
rownames(local.cost.new)<-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
htmlTable(local.cost.new, rowlabel='지역' ,caption = "지역별 진료비 특성(원/1인)")


#방문 건수
Y_CD<-c(10,20,28,29,30,40,50,60,71,72,73,74,75,81,92,93)
#요양기관번호 3-4번째 자리: 요양기관종별
#10:전문요양기관/종합병원
#20:병원
#28:요양기관
#29:정신요양기관
#30:의원
#40:치과병원
#50:치과의원
#60:조산원
#71:보건소 
#72:보건지소 
#73:보건진료소 
#74:모자보건센타 
#75:보건의료원 
#81:약국
#92:한방병원
#93:한방의원



#연령별 방문 건수
visit.age.new<-rep(NA,length(Y_CD))
for(i in age.group.seq){
  Temp.data <- Data.20.L20.new[Data.20.L20.new$age_group==i, 9:10]
  visit.age.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.age.temp<-visit.age.temp[names(visit.age.temp)%in%Y_CD]
  visit.age.new <- rbind(visit.age.new, visit.age.temp)
}
visit.age.new<-visit.age.new[-1,]
colnames(visit.age.new)<-Y_CD.names.abb
rownames(visit.age.new) <- c("0-1세","1-2세","2-3세","3-4세","4-5세","5-6세","6-7세","7-8세","8-9세","9-10세",
                         "10-11세","11-12세","12-13세","13-14세","14-15세","15-16세","16-17세","17-18세","18-19세","19-20세",
                         "20-29세","30-39세","40-49세","50-59세","60-69세","70-79세","80-89세")
htmlTable(visit.age.new, rowlabel='연령대' ,caption = "연령별 요양기관별 방문 건수(회)")

#성별별 방문 건수
visit.sex.new<-NA
for(i in 1:2){
  Temp.data <- Data.20.L20.new[Data.20.L20.new$SEX_TP_CD==i, 9:10]
  visit.sex.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.sex.temp<-visit.sex.temp[names(visit.sex.temp)%in%Y_CD]
  visit.sex.new <- rbind(visit.sex.new, visit.sex.temp)
}
visit.sex.new<-visit.sex.new[-1,]
colnames(visit.sex.new)<-Y_CD.names.abb
rownames(visit.sex.new)<-c("남자","여자")
htmlTable(visit.sex.new, rowlabel='성별' ,caption = "성별별 요양기관별 방문 건수(회)")

#연도별 방문 건수
visit.year.new<-matrix(as.integer(0),5,16)
for(i in 2011:2015){
  Temp.data <- Data.20.L20.new[substring(Data.20.L20.new$RECU_FR_DD,1,4)==i, 9:10]
  visit.year.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.year.temp<-visit.year.temp[names(visit.year.temp)%in%Y_CD]
  if(length(visit.year.temp)!=0)visit.year.new[i-2010,] = visit.year.temp
}
colnames(visit.year.new)<-Y_CD.names.abb
rownames(visit.year.new) = c("2011년","2012년","2013년","2014년","2015년")
htmlTable(visit.year.new, rowlabel='연도' ,caption = "연도별 요양기관별 방문 건수(회)")

#지역별 방문 건수
visit.local.new<-NA
visit.local.new<-data.frame(names=Y_CD, visit.local.new)

for(i in local.names){
  Temp.data <- Data.20.L20.new[substring(Data.20.L20.new$YNO,1,2)==i, 9:10]
  visit.local.temp <-tapply(Temp.data$NO,substring(Temp.data$YNO,3,4),uniq.length)
  visit.local.temp<-visit.local.temp[names(visit.local.temp)%in%Y_CD]
  visit.local.temp<-data.frame(names=names(visit.local.temp), visit.local.temp)
  visit.local.new<- merge(visit.local.new, visit.local.temp, by.x="names", by.y="names", all.x=TRUE)
}
visit.local.new[is.na(visit.local.new)] <- 0
visit.local.new<-t(visit.local.new)

visit.local.new<-visit.local.new[-1:-2,]
colnames(visit.local.new)<-Y_CD.names.abb
rownames(visit.local.new) <-c("서울", "부산", "인천", "대구", "광주", "대전", "울산", "경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
htmlTable(visit.local.new, rowlabel='지역' ,caption = "지역별 요양기관별 방문 건수(회)")
