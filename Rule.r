Delphi=function(input.data=dataset,char.lst=char.lst,quan.lst=quan.lst)
{
  
  all.set=SplitData(input.data,char.lst,quan.lst)
  char.data=all.set$c1
  quan.data=all.set$c2
  score_char.data=CharRule(char.data)
  score_quan.data=NumRule(quan.data)
  
  tmp.score=(apply(score_char.data[,2:length(score_char.data)],1,sum)+apply(score_quan.data[,2:length(score_quan.data)],1,sum))
  input.data$TOTOAL_SCOR=tmp.score
  output.data<-merge(score_char.data,score_quan.data,by="AGENT_ID",sort=FALSE)
  output.data$TOTAL_SCOR=tmp.score
  return(output.data)
}

SplitData<-function(input.data,char.lst,quan.lst){
  ID<-names(input.data)[1]
  feature.all<-names(input.data)[2:(length(input.data)-1)]
  C1<-c(ID,char.lst)
  C2<-c(ID,quan.lst)
  char.data<-input.data[,C1]
  quan.data<-input.data[,C2]
  return(list(c1=char.data,c2=quan.data))
}

CharRule<-function(char.data){
  score_char.data<-char.data
  for(i in 1:nrow(char.data)){
  #SALE_TYPE：机票代理商类型:0,1,2,3--> 0,1,2,3
    if(!is.na(as.numeric(char.data$SALE_TYPE[i]))){
      score_char.data$SALE_TYPE[i]<-(char.data$SALE_TYPE[i])+1
    }else{
      score_char.data$SALE_TYPE[i]<-0
    }
  #ADDR:0,1 --> 2,1
    if(!is.na(char.data$ADDR[i]))
    {
      score_char.data$ADDR[i]<-ifelse(char.data$ADDR[i]>0,1,2)
    }else{
      score_char.data$ADDR[i]<-0
    }
  #DEGREE:0,1,2-->1,2,3
    if(!is.na(char.data$DEGREE[i])){
      score_char.data$DEGREE[i]<-as.numeric(char.data$DEGREE[i])+1
    }else{
      score_char.data$DEGREE[i]<-0
    }
  #MAR_STA:1,2-->1,2
    if(!is.na(char.data$MAR_STA[i])){
      score_char.data$MAR_STA[i]<-as.numeric(char.data$MAR_STA[i])
    }else{
      score_char.data$MAR_STA[i]<-0
    }
  #COOP_SINC:0,1,2-> 0,1,2
    if(!is.na(char.data$COOP_SINC[i])){
      score_char.data$COOP_SINC[i]<-as.numeric(char.data$COOP_SINC[i])
    }else{
      score_char.data$COOP_SINC[i]<-0
    }
  #SERV_CENTER:0,1,2 -> 0,1,2
    if(!is.na(char.data$SERV_CENTER[i])){
      score_char.data$SERV_CENTER[i]<-as.numeric(char.data$SERV_CENTER[i])
    }else{
      score_char.data$SERV_CENTER[i]<-0
    }
  #PLAT:0,1,2-> 0,1,2
    if(!is.na(char.data$PLAT[i])){
      score_char.data$PLAT[i]<-as.numeric(char.data$PLAT[i])
    }else{
      score_char.data$PLAT[i]<-0
    }
  #OTH_BUSI:0,1->0,2
    if(!is.na(char.data$OTH_BUSI[i])){
      score_char.data$OTH<-as.numeric(ifelse(char.data$OTH_BUSI[i],2,0))
    }else{
      score_char.data$OTH_BUSI[i]<-0
    }
  #POLICY:0,1->0,2
    if(!is.na(char.data$POLICY[i])){
      score_char.data$POLICY<-as.numeric(ifelse(char.data$POLICY[i],2,0))
    }else{
      score_char.data$POLICY[i]<-0
    }
  #ENT_COURT:0,1->0,-4 ******
    if(!is.na(char.data$ENT_COURT[i])){
      score_char.data$ENT_COURT<-as.numeric(ifelse(char.data$ENT_COURT[i],-4,0))
    }else{
      score_char.data$ENT_COURT[i]<-0
    }
  #ADMIN_COURT:0,1->0,-2
    if(!is.na(char.data$ADMIN_COURT[i])){
      score_char.data$ADMIN_COURT<-as.numeric(ifelse(char.data$ADMIN_COURT[i],-2,0))
    }else{
      score_char.data$ADMIN_COURT[i]<-0
    }
  #LPER_COURT:0,1->0,-4
    if(!is.na(char.data$LPER_COURT[i])){
      score_char.data$LPER_COURT<-as.numeric(ifelse(char.data$LPER_COURT[i],-4,0))
    }else{
      score_char.data$LPER_COURT[i]<-0
    }
  }
  return(score_char.data)
}

NumRule<-function(quan.data){
  score_quan.data=quan.data
  for(j in 1:nrow(quan.data)){
    #1."REG_ASSETS" 10w,10-50w,50w-100w,100w->1,2,3,4
     if(!is.na(quan.data$REG_ASSETS[j])){
        if(quan.data$REG_ASSETS[j] < 10){
          score_quan.data$REG_ASSETS[j]<- 1 #
        }else if(quan.data$REG_ASSETS[j] >= 10 & quan.data$REG_ASSETS[j] < 50){
          score_quan.data$REG_ASSETS[j]<- 2 #
        }else if(quan.data$REG_ASSETS[j] >= 50 & quan.data$REG_ASSETS[j] < 100){
          score_quan.data$REG_ASSETS[j]<- 3 #
        }else if(quan.data$REG_ASSETS[j] >= 100){
          score_quan.data$REG_ASSETS[j]<- 4 #
        }
     }else{
       score_quan.data$REG_ASSETS[j]<-0
     }
    #2."FIXED_YEAR":<1,1-3,3-6,6->1,2,3,4
    if(!is.na(quan.data$FIXED_YEAR[j])){
      if(quan.data$FIXED_YEAR[j] < 1){
        score_quan.data$FIXED_YEAR[j]<- 1 #
      }else if(quan.data$FIXED_YEAR[j] >= 1 & quan.data$FIXED_YEAR[j] < 3){
        score_quan.data$FIXED_YEAR[j]<- 2 #
      }else if(quan.data$FIXED_YEAR[j] >= 3 & quan.data$FIXED_YEAR[j] < 6){
        score_quan.data$FIXED_YEAR[j]<- 3 #
      }else if (quan.data$FIXED_YEAR[j] >= 6){
        score_quan.data$FIXED_YEAR[j]<- 4 #
      }    
    }else{
      score_quan.data$FIXED_YEAR[j]<-0
    }
    #3."EMPS":15,15up->1,2
    if(!is.na(quan.data$EMPS[j])){
      if(quan.data$EMPS[j]<=15){
        score_quan.data$EMPS[j]<-1 #
      }else if(quan.data$EMPS[j]>15){
        score_quan.data$EMPS[j]<-2 #
      }
    }else{
      score_quan.data$EMPS[j]<-0
    }    
    #4."BUS_AREA":in10,10-50,50+->1,2,3
    if(!is.na(quan.data$BUS_AREA[j])){
      if(quan.data$BUS_AREA[j]<= 10){
        score_quan.data$BUS_AREA[j]<-1 #
      }else if(quan.data$BUS_AREA[j]>10 & quan.data$BUS_AREA[j]<=50){
        score_quan.data$BUS_AREA[j]<-2 #
      }else if(quan.data$BUS_AREA[j]>50){
        score_quan.data$BUS_AREA[j]<-3 #
      }
    }else{
      score_quan.data$BUS_AREA[j]<-0
    }    
    #5."BUS_STAB":in1,1-3,3out->1,2,3
    if(!is.na(quan.data$BUS_STAB[j])){
      if(quan.data$BUS_STAB[j]<= 1){
        score_quan.data$BUS_STAB[j]<-1 #
      }else if(quan.data$BUS_STAB[j]>1 & quan.data$BUS_STAB[j]<=3){
        score_quan.data$BUS_STAB[j]<-2 #
      }else if(quan.data$BUS_STAB[j]>3){
        score_quan.data$BUS_STAB[j]<-3 #
      }     
    }else{
      score_quan.data$BUS_STAB[j]<-0
    }    
    #6."AGE":18-23/60+,24-34,34-60 --> 1,2,3
    if(!is.na(quan.data$AGE[j])){
      if((quan.data$AGE[j]>=18 & quan.data$AGE[j]<=23)){
        score_quan.data$AGE[j]<-1 #
      }else if(quan.data$AGE[j]>24 & quan.data$AGE[j]<=34){
        score_quan.data$AGE[j]<-2 #
      }else if(quan.data$AGE[j]>=34 & quan.data$AGE[j]<=60){
        score_quan.data$AGE[j]<- 3 #
      }else if(quan.data$AGE[j]> 60){
        score_quan.data$AGE[j]<- 1 #
      }
    }else{
      score_quan.data$AGE[j]<-0
    }    
    #7."BUS_TRA_INDUSTRY":1,1-3,3-5,5+ --> 1,2,3,4
    if(!is.na(quan.data$BUS_TRA_INDUSTRY[j])){
      if(quan.data$BUS_TRA_INDUSTRY[j]<=1){
        score_quan.data$BUS_TRA_INDUSTRY[j]<-1
      }else if(quan.data$BUS_TRA_INDUSTRY[j] >1 & quan.data$BUS_TRA_INDUSTRY[j] <=3){
        score_quan.data$BUS_TRA_INDUSTRY[j]<-2
      }else if(quan.data$BUS_TRA_INDUSTRY[j] >3 & quan.data$BUS_TRA_INDUSTRY[j] <=5){
        score_quan.data$BUS_TRA_INDUSTRY[j]<-3
      }else{
        score_quan.data$BUS_TRA_INDUSTRY[j]<-4
      }
    }else{
      score_quan.data$BUS_TRA_INDUSTRY[j]<-0
    }   
    #8."LOCAL_TIME":2,2-10,10+ -> 1,2,3
    if(!is.na(quan.data$LOCAL_TIME[j])){
      if(quan.data$LOCAL_TIME[j] < 2){
        score_quan.data$LOCAL_TIME[j]<- 1 #
      }else if(quan.data$LOCAL_TIME[j] >2 & quan.data$LOCAL_TIME[j]<=10){
        score_quan.data$LOCAL_TIME[j]<- 2 #
      }else if(quan.data$LOCAL_TIME[j]>10){
        score_quan.data$LOCAL_TIME[j]<- 3 #
      }
    }else{
      score_quan.data$LOCAL_TIME[j]<-0
    }    
    #9."HOUSE_EREA":cent,60,60-120,120+ --> 0,1,2,3
    if(!is.na(quan.data$HOUSE_EREA[j])){
      if(quan.data$HOUSE_EREA[j] <= 60){
        score_quan.data$HOUSE_EREA[j]<- 1 #
      }else if(quan.data$HOUSE_EREA[j] >60 & quan.data$HOUSE_EREA[j]<=120){
        score_quan.data$HOUSE_EREA[j]<- 2 #
      }else if(quan.data$HOUSE_EREA[j] > 120){
        score_quan.data$HOUSE_EREA[j]<- 3 #
      }
    }else{
      score_quan.data$HOUSE_EREA[j]<-0
    }    
    #10."OWN_HOUSE_VALUE":30，30-60，60-100,100+ --> 1,2,3,4
    if(!is.na(quan.data$OWN_HOUSE_VALUE[j])){
      if(quan.data$OWN_HOUSE_VALUE[j] <= 30){
        score_quan.data$OWN_HOUSE_VALUE[j]<- 1 #
      }else if(quan.data$OWN_HOUSE_VALUE[j] >60 & quan.data$OWN_HOUSE_VALUE[j]<=100){
        score_quan.data$OWN_HOUSE_VALUE[j]<- 2 #
      }else if(quan.data$OWN_HOUSE_VALUE[j] >100 & quan.data$OWN_HOUSE_VALUE[j]<=120){
        score_quan.data$OWN_HOUSE_VALUE[j]<- 3 #
      }else if(quan.data$OWN_HOUSE_VALUE[j]>120){
        score_quan.data$OWN_HOUSE_VALUE[j]<- 4 #
      }    
    }else{
    score_quan.data$OWN_HOUSE_VALUE[j]<-0
    }    
    #11."OWN_CARS":??   --> 0,1,2,3
    if(!is.na(quan.data$OWN_CARS[j])){
      if(quan.data$OWN_CARS[j] <= 1){
        score_quan.data$OWN_CARS[j]<- 1 #
      }else if(quan.data$OWN_CARS[j] >1 & quan.data$OWN_CARS[j]<= 2){
        score_quan.data$OWN_CARS[j]<- 2 #
      }else if(quan.data$OWN_CARS[j] > 3){
        score_quan.data$OWN_CARS[j]<- 3 #
      }
    }else{
      score_quan.data$OWN_CARS[j]<- 0
    }    
    #12."SIX_M_VOTES" :100-,100-400,400-1000,100-2000,2000+ --> 1,2,3,4,5
    if(!is.na(quan.data$SIX_M_VOTES[j])){
      if(quan.data$SIX_M_VOTES[j]>0 & quan.data$SIX_M_VOTES[j]<=100){
        score_quan.data$SIX_M_VOTES[j]<- 1 #
      }else if(quan.data$SIX_M_VOTES[j]>100 & quan.data$SIX_M_VOTES[j]<=400){
        score_quan.data$SIX_M_VOTES[j]<- 2 #
      }else if(quan.data$SIX_M_VOTES[j]>400 & quan.data$SIX_M_VOTES[j]<=1000){
        score_quan.data$SIX_M_VOTES[j]<- 3 #
      }else if(quan.data$SIX_M_VOTES[j]>1000 & quan.data$SIX_M_VOTES[j]<=2000){
        score_quan.data$SIX_M_VOTES[j]<- 4 #
      }else{
        score_quan.data$SIX_M_VOTES[j]<- 5 #
      }
    }else{
      score_quan.data$SIX_M_VOTES[j]<- 0
    }    
    #13."SIX_M_VOTES_RATE":0-0.05,0.05-0.2,0.2+ --> 1,2,3
    if(!is.na(quan.data$SIX_M_VOTES_RATE[j])){
      if(quan.data$SIX_M_VOTES_RATE[j]>0 & quan.data$SIX_M_VOTES_RATE[j]<=0.05){
        score_quan.data$SIX_M_VOTES_RATE[j]<- 1 #
      }else if(quan.data$SIX_M_VOTES_RATE[j]>0.05 & quan.data$SIX_M_VOTES_RATE[j]<=0.2){
        score_quan.data$SIX_M_VOTES_RATE[j]<- 2 #
      }else{
        score_quan.data$SIX_M_VOTES_RATE[j]<- 3 #
      }
    }else{
      score_quan.data$SIX_M_VOTES_RATE[j]<- 0
    }   
    #14. "SIX_M_INVALID_RATE":0-0.1,0.1-0.2,0.2-1 --> 2,1,0
    if(!is.na(quan.data$SIX_M_INVALID_RATE[j])){
      if(quan.data$SIX_M_INVALID_RATE[j]>0 & quan.data$SIX_M_INVALID_RATE[j]<=0.1){
          score_quan.data$SIX_M_INVALID_RATE[j]<- 2 #
      }else if(quan.data$SIX_M_INVALID_RATE[j]>0.1 & quan.data$SIX_M_INVALID_RATE[j] <=0.2){
          score_quan.data$SIX_M_INVALID_RATE[j]<- 1 #
      }else if(quan.data$SIX_M_INVALID_RATE[j]>0.2 & quan.data$SIX_M_INVALID_RATE[j]<=1){
          score_quan.data$SIX_M_INVALID_RATE[j]<- 0 #
      }
    }else{
      score_quan.data$SIX_M_INVALID_RATE[j]<-0
    }   
    #15."NODE_NUM" :0-1,1-5,5-10,10+ --> 0,1,2,3
    if(!is.na(quan.data$NODE_NUM[j])){
      if(quan.data$NODE_NUM[j]>0 & quan.data$NODE_NUM[j]<=1){
          score_quan.data$NODE_NUM[j]<- 0 #
      }else if(quan.data$NODE_NUM[j]>1 & quan.data$NODE_NUM[j]<=5){
          score_quan.data$NODE_NUM[j]<- 1 #
      }else if(quan.data$NODE_NUM[j]>5 & quan.data$NODE_NUM[j]<=10){
          score_quan.data$NODE_NUM[j]<- 2 #
      }else if(quan.data$NODE_NUM[j]>10){
          score_quan.data$NODE_NUM[j]<- 3 #
      }
    }else{
      score_quan.data$NODE_NUM[j]<-0
    }    
    #16."AVE_AMT_ATK_LSMON":0-20,20-50,50-100,100-300,300+ --> 1,2,3,4,5
    if(!is.na(quan.data$AVE_AMT_ATK_LSMON[j])){
      if(quan.data$AVE_AMT_ATK_LSMON[j]>0 & quan.data$AVE_AMT_ATK_LSMON[j]<=20){
        score_quan.data$AVE_AMT_ATK_LSMON[j]<- 1 #
      }else if(quan.data$AVE_AMT_ATK_LSMON[j]>20 & quan.data$AVE_AMT_ATK_LSMON[j]<=50){
        score_quan.data$AVE_AMT_ATK_LSMON[j]<- 2 #
      }else if(quan.data$AVE_AMT_ATK_LSMON[j]>50 & quan.data$AVE_AMT_ATK_LSMON[j]<=100){
        score_quan.data$AVE_AMT_ATK_LSMON[j]<- 3 #
      }else if(quan.data$AVE_AMT_ATK_LSMON[j]>100 & quan.data$AVE_AMT_ATK_LSMON[j]<=300){
        score_quan.data$AVE_AMT_ATK_LSMON[j]<- 4 #
      }else if(quan.data$AVE_AMT_ATK_LSMON[j]>300){
        score_quan.data$AVE_AMT_ATK_LSMON[j]<- 5 #
      }
    }else{
      score_quan.data$AVE_AMT_ATK_LSMON[j]<- 0
    }    
    #17. "AVE_AMT_NATK_LSMON" :0-20,20-50,50-100,100-200,200+ --> 1,2,3,4,5
    if(!is.na(quan.data$AVE_AMT_NATK_LSMON[j])){
      if(quan.data$AVE_AMT_NATK_LSMON[j]>0 & quan.data$AVE_AMT_NATK_LSMON[j]<=20){
          score_quan.data$AVE_AMT_NATK_LSMON[j]<- 1
      }else if(quan.data$AVE_AMT_NATK_LSMON[j]>20 & quan.data$AVE_AMT_NATK_LSMON[j]<=50){
          score_quan.data$AVE_AMT_NATK_LSMON[j]<- 2
      }else if(quan.data$AVE_AMT_NATK_LSMON[j]>50 & quan.data$AVE_AMT_NATK_LSMON[j]<= 100){
          score_quan.data$AVE_AMT_NATK_LSMON[j]<- 3
      }else if(quan.data$AVE_AMT_NATK_LSMON[j]>100 & quan.data$AVE_AMT_NATK_LSMON[j]<=200){
          score_quan.data$AVE_AMT_NATK_LSMON[j]<- 4
      }else if(quan.data$AVE_AMT_NATK_LSMON[j]>200){
          score_quan.data$AVE_AMT_NATK_LSMON[j]<- 5
      }
    }else{
      score_quan.data$AVE_AMT_NATK_LSMON[j]<- 0
    }   
    #18."ATK_SALES_INC_RATIO_LSMON":0-0.05,0.05-0.1,0.1-0.2,0.2+ --> 1,2,3,4
    if(!is.na(quan.data$ATK_SALES_INC_RATIO_LSMON[j])){
      if(quan.data$ATK_SALES_INC_RATIO_LSMON[j]>0 & quan.data$ATK_SALES_INC_RATIO_LSMON[j]<=0.05){
        score_quan.data$ATK_SALES_INC_RATIO_LSMON[j]<- 1
      }else if(quan.data$ATK_SALES_INC_RATIO_LSMON[j]>0.05 & quan.data$ATK_SALES_INC_RATIO_LSMON[j]<=0.1){
        score_quan.data$ATK_SALES_INC_RATIO_LSMON[j]<- 2
      }else if(quan.data$ATK_SALES_INC_RATIO_LSMON[j]>0.1 & quan.data$ATK_SALES_INC_RATIO_LSMON[j]<=0.2){
        score_quan.data$ATK_SALES_INC_RATIO_LSMON[j]<- 3
      }else if(quan.data$ATK_SALES_INC_RATIO_LSMON[j]>0.2){
        score_quan.data$ATK_SALES_INC_RATIO_LSMON[j]<- 4
      }else if(quan.data$ATK_SALES_INC_RATIO_LSMON[j]<0){
        score_quan.data$ATK_SALES_INC_RATIO_LSMON[j]<- 0
      }
    }else{
      score_quan.data$ATK_SALES_INC_RATIO_LSMON[j]<-0
    }   
    #19."GROSS_PROFIT_OF_LTYEAR":0-100,100-800,800-1500,1500-5000,5000+ --> 0,1,2,3,4
    if(!is.na(quan.data$GROSS_PROFIT_OF_LTYEAR[j])){
      if(quan.data$GROSS_PROFIT_OF_LTYEAR[j]>0 & quan.data$GROSS_PROFIT_OF_LTYEAR[j]<=100){
        score_quan.data$GROSS_PROFIT_OF_LTYEAR[j]<- 0
      }else if(quan.data$GROSS_PROFIT_OF_LTYEAR[j]>100 & quan.data$GROSS_PROFIT_OF_LTYEAR[j]<=800){
        score_quan.data$GROSS_PROFIT_OF_LTYEAR[j]<- 1
      }else if(quan.data$GROSS_PROFIT_OF_LTYEAR[j]>800 & quan.data$GROSS_PROFIT_OF_LTYEAR[j]<=1500){
        score_quan.data$GROSS_PROFIT_OF_LTYEAR[j]<- 2
      }else if(quan.data$GROSS_PROFIT_OF_LTYEAR[j]>1500 & quan.data$GROSS_PROFIT_OF_LTYEAR[j]<=5000){
        score_quan.data$GROSS_PROFIT_OF_LTYEAR[j]<- 3
      }else if(quan.data$GROSS_PROFIT_OF_LTYEAR[j]>5000){
        score_quan.data$GROSS_PROFIT_OF_LTYEAR[j]<- 4
      }
    }else{
      score_quan.data$GROSS_PROFIT_OF_LTYEAR[j]<-0
    }   
    #20."AVE_ASS_LIB_RATIO_LSMON":0-0.6,0.6-1,1+ --> 2,1,0
    if(!is.na(quan.data$AVE_ASS_LIB_RATIO_LSMON[j])){
      if(quan.data$AVE_ASS_LIB_RATIO_LSMON[j]>0 & quan.data$AVE_ASS_LIB_RATIO_LSMON[j]<= 0.6){
        score_quan.data$AVE_ASS_LIB_RATIO_LSMON[j]<- 2
      }else if(quan.data$AVE_ASS_LIB_RATIO_LSMON[j]>0.6 & quan.data$AVE_ASS_LIB_RATIO_LSMON[j]<=1){
        score_quan.data$AVE_ASS_LIB_RATIO_LSMON[j]<- 1
      }else if(quan.data$AVE_ASS_LIB_RATIO_LSMON[j]>1){
        score_quan.data$AVE_ASS_LIB_RATIO_LSMON[j]<- 0
      }
    }else{
      score_quan.data$AVE_ASS_LIB_RATIO_LSMON[j]<- 0
    }    
    #21."AVE_LIQ_RATIO_LSMON" :0-1,1-2,2+ --> 0,1,2
    if(!is.na(quan.data$AVE_LIQ_RATIO_LSMON[j])){
      if(quan.data$AVE_LIQ_RATIO_LSMON[j]>0 & quan.data$AVE_LIQ_RATIO_LSMON[j]<= 1){
        score_quan.data$AVE_LIQ_RATIO_LSMON[j]<- 0
      }else if(quan.data$AVE_LIQ_RATIO_LSMON[j]>1 & quan.data$AVE_LIQ_RATIO_LSMON[j]<=2){
        score_quan.data$AVE_LIQ_RATIO_LSMON[j]<- 1
      }else if(quan.data$AVE_LIQ_RATIO_LSMON[j]>2){
        score_quan.data$AVE_LIQ_RATIO_LSMON[j]<- 2
      }
    }else{
      score_quan.data$AVE_LIQ_RATIO_LSMON[j]<- 0
    }     
    #22."ASS_INCREASE_RATE":0-0.05,0.05-0.2,0.2+ -> 0,1,2
    if(!is.na(quan.data$ASS_INCREASE_RATE[j])){
      if(quan.data$ASS_INCREASE_RATE[j]>0 & quan.data$ASS_INCREASE_RATE[j]<=0.05){
        score_quan.data$ASS_INCREASE_RATE[j]<- 0
      }else if(quan.data$ASS_INCREASE_RATE[j]>0.05 & quan.data$ASS_INCREASE_RATE[j]<=0.2){
        score_quan.data$ASS_INCREASE_RATE[j]<- 1
      }else if(quan.data$ASS_INCREASE_RATE[j]>0.2 & quan.data$ASS_INCREASE_RATE[j]<=1){
        score_quan.data$ASS_INCREASE_RATE[j]<- 2
      }
    }else{
      score_quan.data$ASS_INCREASE_RATE[j]<- 0
    }   
    #23."REC_BAL":0-10,10-50,50+ --> 2,1,0
    if(!is.na(quan.data$REC_BAL[j])){
      if(quan.data$REC_BAL[j]>0 & quan.data$REC_BAL[j]<=10){
        score_quan.data$REC_BAL[j]<- 2
      }else if(quan.data$REC_BAL[j]>10 & quan.data$REC_BAL[j]<=50){
        score_quan.data$REC_BAL[j]<- 1
      }else if(quan.data$REC_BAL[j]){
        score_quan.data$REC_BAL[j]<- 0
      }
    }else{
      score_quan.data$REC_BAL[j]<- 0
    }   
    #24."LPER_CRE_REC":1-30,30-60,60-90,90+ --> -1,-2,-3,-4
    if(!is.na(quan.data$LPER_CRE_REC[j])){
      if(quan.data$LPER_CRE_REC[j]>1 & quan.data$LPER_CRE_REC[j]<=30){
        score_quan.data$LPER_CRE_REC[j]<- -1 
      }else if(quan.data$LPER_CRE_REC[j]>30 & quan.data$LPER_CRE_REC[j]<=60){
        score_quan.data$LPER_CRE_REC[j]<- -2
      }else if(quan.data$LPER_CRE_REC[j]>60 & quan.data$LPER_CRE_REC[j]<=90){
        score_quan.data$LPER_CRE_REC[j]<- -3
      }else if(quan.data$LPER_CRE_REC[j]>90){
        score_quan.data$LPER_CRE_REC[j]<- -4
      }
    }else{
      score_quan.data$LPER_CRE_REC[j]<-0
    }   
    #25."LPER_CRECARD_REC":1-30,30-60,60-90,90+ --> -1,-2,-3,-4
    if(!is.na(quan.data$LPER_CRECARD_REC[j])){
      if(quan.data$LPER_CRECARD_REC[j]>1 & quan.data$LPER_CRECARD_REC[j]<=30){
        score_quan.data$LPER_CRECARD_REC[j]<- -1 
      }else if(quan.data$LPER_CRECARD_REC[j]>30 & quan.data$LPER_CRECARD_REC[j]<=60){
        score_quan.data$LPER_CRECARD_REC[j]<- -2
      }else if(quan.data$LPER_CRECARD_REC[j]>60 & quan.data$LPER_CRECARD_REC[j]<=90){
        score_quan.data$LPER_CRECARD_REC[j]<- -3
      }else if(quan.data$LPER_CRECARD_REC[j]>90){
        score_quan.data$LPER_CRECARD_REC[j]<- -4
      }     
    }else{
      score_quan.data$LPER_CRECARD_REC[j]<-0
    }   
    #26."ENT_CRE_REC":1-30,30-60,60-90,90-180,180+ --> -2,-3,-4,-5,-100
    if(!is.na(quan.data$ENT_CRE_REC[j])){
      if(quan.data$ENT_CRE_REC[j]>1 & quan.data$ENT_CRE_REC[j]<=30){
        score_quan.data$ENT_CRE_REC[j]<- -2 
      }else if(quan.data$ENT_CRE_REC[j]>30 & quan.data$ENT_CRE_REC[j]<=60){
        score_quan.data$ENT_CRE_REC[j]<- -3
      }else if(quan.data$ENT_CRE_REC[j]>60 & quan.data$ENT_CRE_REC[j]<=90){
        score_quan.data$ENT_CRE_REC[j]<- -4
      }else if(quan.data$ENT_CRE_REC[j]>90 &  quan.data$ENT_CRE_REC[j]<=180){
        score_quan.data$ENT_CRE_REC[j]<- -5
      }else if(quan.data$ENT_CRE_REC[j]>180){
        score_quan.data$ENT_CRE_REC[j]<- -100
      }
    }else{
      score_quan.data$ENT_CRE_REC[j]<-0
    }   
    #27."TRADE_OUT_OF_LINE_REC":1,2,3,4,5+ -->-1,-2,-3,-4,-5
    if(!is.na(quan.data$TRADE_OUT_OF_LINE_REC[j])){
      if(quan.data$TRADE_OUT_OF_LINE_REC[j]==1){
        score_quan.data$TRADE_OUT_OF_LINE_REC[j]<- -1
      }else if(quan.data$TRADE_OUT_OF_LINE_REC[j]==2){
        score_quan.data$TRADE_OUT_OF_LINE_REC[j]<- -2
      }else if(quan.data$TRADE_OUT_OF_LINE_REC[j]==3){
        score_quan.data$TRADE_OUT_OF_LINE_REC[j]<- -3
      }else if(quan.data$TRADE_OUT_OF_LINE_REC[j]==4){
        score_quan.data$TRADE_OUT_OF_LINE_REC[j]<- -4
      }else if(quan.data$TRADE_OUT_OF_LINE_REC[j]>=5){
        score_quan.data$TRADE_OUT_OF_LINE_REC[j]<- -5
      }
    }else{
      
    }  
    #28."RYX_BUS_OVERDUE_REC":1-5,5-20,20+ --> -1,-2,-3
    if(!is.na(quan.data$RYX_BUS_OVERDUE_REC[j])){
      if(quan.data$RYX_BUS_OVERDUE_REC[j]>1 & quan.data$RYX_BUS_OVERDUE_REC[j]<=5){
        score_quan.data$RYX_BUS_OVERDUE_REC[j]<- -1
      }else if(quan.data$RYX_BUS_OVERDUE_REC[j]>5 & quan.data$RYX_BUS_OVERDUE_REC[j]<=20){
        score_quan.data$RYX_BUS_OVERDUE_REC[j]<- -2
      }else if(quan.data$RYX_BUS_OVERDUE_REC[j]>20){
        score_quan.data$RYX_BUS_OVERDUE_REC[j]<- -3
      }
    }else{
      score_quan.data$RYX_BUS_OVERDUE_REC[j]<-0
    }   
    #29."ARI_COV":0-5,5-10,10+ --> 0,1,2
    if(!is.na(quan.data$ARI_COV[j])){
      if(quan.data$ARI_COV[j]>0 & quan.data$ARI_COV[j]<=5){
        score_quan.data$ARI_COV[j]<- 0
      }else if(quan.data$ARI_COV[j]>5 & quan.data$ARI_COV[j]<=10){
        score_quan.data$ARI_COV[j]<- 2
      }else if(quan.data$ARI_COV[j]>10){
        score_quan.data$ARI_COV[j]<- 2
      }
    }else{
      score_quan.data$ARI_COV[j]<- 0 
    }    
    #30. "SIX_CON_ORDER_RATE":0-0.3,0.3+ --> 1,0
    if(!is.na(quan.data$SIX_CON_ORDER_RATE[j])){
      score_quan.data$SIX_CON_ORDER_RATE[j]<-ifelse(quan.data$SIX_CON_ORDER_RATE[j]>=0.3,0,1)
    }else{
      score_quan.data$SIX_CON_ORDER_RATE[j]<- 0
    }   
    #31. "SIX_GAP_RATE":0-0.2,0.2+ --> 1,0
    if(!is.na(quan.data$SIX_GAP_RATE[j])){
      score_quan.data$SIX_GAP_RATE[j]<-ifelse(quan.data$SIX_GAP_RATE[j]>=0.2,0,1)
    }else{
      score_quan.data$SIX_GAP_RATE[j]<-0
    }   
    #32."SIX_CHID_RATE" :0-0.2,0.2+ --> 1,0
    if(!is.na(quan.data$SIX_CHID_RATE[j])){
      score_quan.data$SIX_CHID_RATE[j]<-ifelse(quan.data$SIX_CHID_RATE[j]>=0.2,0,1)
    }else{
      score_quan.data$SIX_CHID_RATE[j]<-0
    }   
  }
  return(score_quan.data)
}
  