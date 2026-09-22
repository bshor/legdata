process_votes <- function (revision=2024, fn="health", method="search") {
  
  # revision = 2023; fn="health"
  
  tic("Process votes")
  
  # load leg info
  
  fi(str_c("../Votesmart/Objects/",revision,"/legz and opinion merged ",revision,".Rdata"))
  load(str_c("../Votesmart/Objects/",revision,"/legz and opinion merged ",revision,".Rdata")) # 229039
  nrow(legz.m)
  
  bill.search <- read_feather(str_c("Objects/",method," results bills ",fn,".fthr"))
  rc.search <- read_feather(str_c("Objects/",method," results rollcalls ",fn,".fthr"))
  vote.search <- read_feather(str_c("Objects/",method," results votes slim ",fn,".fthr"))
  
  # merge
  # 11s R37L
  
  tic("merge data")
  leg.m.sub <- legz.m %>%
    filter(year>2007 & !is.na(people.id) & people.id%in%vote.search$people.id) %>%
    select(st,year,party,stdist,geography,k.id,people.id,vs.id,e.id,u.id,matchname,fullname,mrp_estimate,pres,score.pres.pred,score.pres.resid,pred.np) %>%
    mutate(year=as.integer(year))
  nrow(leg.m.sub) # 104758 110230
  # get rid of duplicate opinion entries by keeping only the first <- fix this in merge opinion
  leg.m.sub = distinct(leg.m.sub,people.id,party,year,st,.keep_all = T)
  nrow(leg.m.sub) # 104557 110064
  
  vote.m<-rc.search %>%
    left_join(vote.search %>% select(-floor),by="rc.id") %>%
    left_join(bill.search %>% select(bill_id,title,status,description,subject,session,
                                     n.hearings,n.votes,n.events,bill.passed,roll.count,
                                     rc.count,hadavote,contains("sponsor"),polar,contains("median"),legparty,gparty)) %>%
    left_join(leg.m.sub %>% select(-st), by=c("year","people.id")) %>%
    filter(year>2007)
  
  # lobstr::obj_size(vote.m) # 4.9Gb 7.2GB (search) 6.2GB (topic)
  
  with(vote.m,cor.test(v,pred.np)) # -.33
  with(vote.m,cor.test(vote,pred.np)) # .09
  
  with(vote.m,cor.test(pred.np,mrp_estimate)) # 0.72
  with(vote.m,cor.test(pred.np,score.pres.pred)) # 0.96
  
  print(str_c("Unique bills: ",length(unique(vote.m$bill_id)))) # climate 2325 health 71612, topic: health 62399 
  print(str_c("Unique legislators: ",length(unique(vote.m$k.id)))) # health 16860
  print(str_c("Unique roll calls: ",length(unique(vote.m$rc.id)))) # search: health 220615. topic: health 186630
  length(unique(vote.m$rc.id[vote.m$floor==1])) #103856 103633 116429
  
  print("Missing k.id")
  tabyl(is.na(vote.m$k.id)) # 0.008 0.007 0.006 0.004
  print("Missing pred.np")
  tabyl(is.na(vote.m$pred.np)) # 0.008 0.006 0.005 0.004
  tabyl(is.na(vote.m$score.pres.pred)) # 0.07 0.06
  
  nrow(vote.m) # health 3.6m 4.7m 5.5m 8.6m 11.3m, topic health 9.7m, climate 351k 
  
  toc()
  
  # generate summary tables
  
  # all.votes <- vote.m %>%
  #   filter(party %in%c("D","R")) %>%
  #   mutate(year=as.integer(year),
  #          #liberalvote=as.integer(liberalvote),
  #          #chamb=str_to_lower(str_sub(chamber,1,1)),
  #          source="Search Data")
  
  tic("Saving")
  write_feather(vote.m, str_c("Objects/",method," ",fn," votes processed.fthr"),compression="zstd", compression_level = 5)
  toc()
  
  print(fi(str_c("Objects/",method," ",fn," votes processed.fthr")))
  
  toc()
}


process_data <- function (fn="health", ncsl=F, revision = 2024, method="search") {
  
  library(tidyr)
  library(janitor)
  library(dplyr)
  library(stringr)
  library(fst)
  library(fs)
  library(arrow)
  library(tictoc)
  #library(cspp)
  library(haven)
  
  # fn="climate"; method = "topic"
  # fn="health"; method="topic"; revision = 2023
  
  # load search results
  
  #system.time(load("Objects/search results health.Rdata"))
  
  tic.clearlog()
  
  #1.5s R37L
  tic("Process search data")
  
  print("File dates of the search results.")
  fi(c(str_c(str_c("Objects/",method," results bills ",fn,".fthr")),
       str_c(str_c("Objects/",method," results rollcalls ",fn,".fthr")),
       str_c(str_c("Objects/",method," results votes slim ",fn,".fthr"))))
  
  bill.search <- read_feather(str_c("Objects/",method," results bills ",fn,".fthr"))
  rc.search <- read_feather(str_c("Objects/",method," results rollcalls ",fn,".fthr"))
  
  
  nrow(bill.search) # 9707, topic health 191k
  nrow(rc.search) # 11164, topic health 186k
  tabyl(rc.search$floor) # 50%, topic health 116k
  
  
  fi(str_c("../Votesmart/Objects/",revision,"/state year aggregates plus.Rdata"))
  load(str_c("../Votesmart/Objects/",revision,"/state year aggregates plus.Rdata"))
  
  #load(str_c("../Votesmart/Objects/2023/legz and opinion merged 2023.Rdata"))
  
  # merge it all
  
  bill.m<-bill.search %>%
    filter(year>2008) %>%
    #select(-title, -description, -contains("url"),-status_date) %>%
    select(-status_date) %>%
    #left_join(styr.long,by=c("st","year","chamb")) %>%
    #left_join(topleaders %>% select(st,year,chamb,leader.score,leader.diff,leader.abs) ,by=c("st","year","chamb")) %>%
    #left_join(legprof,by=c("st","year")) %>%
    mutate(sponsor.maj.dist=sponsor.median-maj.median,
           sponsor.abs.dist=abs(sponsor.maj.dist),
           #sponsor.leader.dist=sponsor.median-leader.score,
           sponsor.chamb.dist=sponsor.median-chamb.median) %>%
    #n.chamb = n.chamber/100) %>%
    tibble
  
  tabyl(bill.m$bowen1.t)
  tabyl(bill.m$bowen2.t)
  
  print("Size of bills")
  print(gdata::humanReadable(lobstr::obj_size(bill.m))) # 13MiB
  
  
  rc.m<-rc.search %>%
    left_join(bill.m %>% select(-title, -description, -contains("url"))) %>%
    filter(year>2008) %>%
    tibble
  
  print("Size of rcs")
  print(gdata::humanReadable(lobstr::obj_size(rc.m))) # 9.2MiB
  
  
  ####
  write_feather(bill.m, str_c("Objects/",method," ",fn," bills processed.fthr"),compression="zstd", compression_level = 15)
  write_feather(rc.m,str_c("Objects/",method," ",fn," rcs processed.fthr"),compression="zstd", compression_level = 15)
  
  fi(c(str_c("Objects/",method," ",fn," bills processed.fthr"),
       str_c("Objects/",method," ",fn," rcs processed.fthr")))
  
  toc(log=T)
}

gen_summaries <- function (fn="health", method="search", ncsl=F, rmd = F) {
  
  library(tidyr)
  library(janitor)
  library(dplyr)
  library(stringr)
  library(fst)
  library(fs)
  library(arrow)
  library(tictoc)
  
  
  #glimpse(bill.m)
  # rmd = F
  # fn="health"
  
  print("I'm in Legiscan!")
  
  if (rmd) { dirpath = "../"} else { dirpath = ""}
  
  if (method %in% c("search","topic")) {
    if (method == "search") {
      bill.m <- read_feather(str_c(dirpath,"Objects/search ",fn," bills processed.fthr"))
      rc.m <- read_feather(str_c(dirpath,"Objects/search ",fn," rcs processed.fthr"))
    }
    
    if (method == "topic") {
      bill.m <- read_feather(str_c(dirpath,"Objects/topics ",fn," bills processed.fthr"))
      rc.m <- read_feather(str_c(dirpath,"Objects/topics ",fn," rcs processed.fthr"))
    }
    
    search.summary <- list()
    
    search.summary[["all"]] <- rc.m %>%
      summarize(Subset="All",
                #Legislators=length(unique(k.id)),
                #Bills=length(unique(bill_id)),
                Bills=nrow(bill.m),
                'Cmte Roll Calls'=length(unique(rc.id[floor==0])),
                'Floor Roll Calls'=length(unique(rc.id[floor==1])),
                'Cmte Votes'=sum(yea[floor==0],na.rm=T)+sum(nay[floor==0],na.rm=T),
                'Floor Votes'=sum(yea[floor==1],na.rm=T)+sum(nay[floor==1],na.rm=T))
    
    search.summary[["non-unam"]] <- rc.m %>%
      filter(unanimous==FALSE) %>%
      summarize(Subset="Non-unanimous",
                #Legislators=length(unique(k.id)),
                Bills=length(unique(bill_id)),
                'Cmte Roll Calls'=length(unique(rc.id[floor==0])),
                'Floor Roll Calls'=length(unique(rc.id[floor==1])),
                'Cmte Votes'=sum(yea[floor==0],na.rm=T)+sum(nay[floor==0],na.rm=T),
                'Floor Votes'=sum(yea[floor==1],na.rm=T)+sum(nay[floor==1],na.rm=T))
    
    search.summary[["passed"]] <- rc.m %>%
      filter(bill.passed==TRUE) %>%
      summarize(Subset="Passed",
                #Legislators=length(unique(k.id)),
                Bills=sum(bill.m$bill.passed),
                'Cmte Roll Calls'=length(unique(rc.id[floor==0])),
                'Floor Roll Calls'=length(unique(rc.id[floor==1])),
                'Cmte Votes'=sum(yea[floor==0],na.rm=T)+sum(nay[floor==0],na.rm=T),
                'Floor Votes'=sum(yea[floor==1],na.rm=T)+sum(nay[floor==1],na.rm=T))
    
    search.summary[["failed"]] <- rc.m %>%
      filter(bill.passed==FALSE) %>%
      summarize(Subset="Failed",
                #Legislators=length(unique(k.id)),
                Bills=sum(!bill.m$bill.passed),
                'Cmte Roll Calls'=length(unique(rc.id[floor==0])),
                'Floor Roll Calls'=length(unique(rc.id[floor==1])),
                'Cmte Votes'=sum(yea[floor==0],na.rm=T)+sum(nay[floor==0],na.rm=T),
                'Floor Votes'=sum(yea[floor==1],na.rm=T)+sum(nay[floor==1],na.rm=T))
    
    
    search.summary.df <- search.summary %>% bind_rows()  # %>% pivot_longer(cols=everything())
    search.summary.df
    
    if (method == "search") {
      print("Saving search")
      save(search.summary.df,file=str_c("Objects/Summaries/", fn, " search bills summary.Rdata"))
    } else {
      print("Saving topic")
      save(search.summary.df,file=str_c("Objects/Summaries/", fn, " topic bills summary.Rdata"))
    }
    
    
  }
  
  
  
  
  
  #toc(log=T)
  
  ##
  # NCSL
  
  # 1.2s R37L
  
  if (ncsl) {
    tic("Processing NCSL data")
    
    fi(str_c("Objects/lookup results bills ",fn,".fst"))
    fi(str_c("Objects/lookup results rollcalls ",fn,".fst"))
    #fi("Objects/lookup results votes climate.fst")
    
    bill.search <- read.fst(str_c("Objects/lookup results bills ",fn,".fst"))
    #vote.search <- read.fst(str_c("Objects/lookup results votes ",fn,".fst"))
    rc.search <- read.fst(str_c("Objects/lookup results rollcalls ",fn,".fst"))
    #toc(log=T)
    
    rc.ncsl<-rc.search %>%
      left_join(bill.search %>% select(bill_id,title,status,description,subject,session,
                                       n.hearings,n.votes,n.events,bill.passed,roll.count,
                                       rc.count,hadavote,contains("sponsor"),polar,contains("median"),legparty,gparty)) %>%
      filter(year>2007) %>%
      #filter(!is.na(majparty)) %>%
      #filter(!is.na(sponsor.median)) %>%
      #filter(gparty!="X") %>%
      tibble
    
    # generate summary table
    ncsl.summary <- list()
    
    ncsl.summary[["all"]] <- rc.ncsl %>%
      summarize(Subset="All",
                #Legislators=length(unique(k.id)),
                Bills=length(unique(bill_id)),
                'Cmte Roll Calls'=length(unique(rc.id[floor==0])),
                'Floor Roll Calls'=length(unique(rc.id[floor==1])),
                'Cmte Votes'=sum(yea[floor==0],na.rm=T)+sum(nay[floor==0],na.rm=T),
                'Floor Votes'=sum(yea[floor==1],na.rm=T)+sum(nay[floor==1],na.rm=T))
    
    ncsl.summary[["sub"]] <- rc.ncsl %>%
      filter(unanimous==FALSE) %>%
      summarize(Subset="Non-unanimous",
                #Legislators=length(unique(k.id)),
                Bills=length(unique(bill_id)),
                'Cmte Roll Calls'=length(unique(rc.id[floor==0])),
                'Floor Roll Calls'=length(unique(rc.id[floor==1])),
                'Cmte Votes'=sum(yea[floor==0],na.rm=T)+sum(nay[floor==0],na.rm=T),
                'Floor Votes'=sum(yea[floor==1],na.rm=T)+sum(nay[floor==1],na.rm=T))
    
    ncsl.summary.df <- ncsl.summary %>% bind_rows()  # %>% pivot_longer(cols=everything())
    ncsl.summary.df
    
    save(ncsl.summary.df,file=str_c("Objects/Summaries/ncsl ", fn, " bills summary.Rdata"))
    
    toc(log=T)
    
  } 
  
}


descriptives_year_summary <- function (fn="climate", method = "search", revision=2024) {
  library(janitor)
  library(ggplot2)
  
  #fn="health"
  
  print("I'm in Legiscan!")
  
  year.summary = styear.summary = styr.sub = list()

  st.codes<-haven::read_dta(file="../Data/States/Constant/state data.dta")
  
  #for (method in c("search","topic")) {
    
    fi(str_c("Objects/",method," ",fn," bills processed.fthr"))
    bill.m <- read_feather(str_c("Objects/",method," ",fn," bills processed.fthr"))
    
    # year summary
    
    year.summary[[method]] <- bill.m %>%
      filter(year>2007) %>%
      group_by(year) %>%
      summarise(bill_passed=mean(bill.passed,na.rm=T),
                bill_passed_n=n()*bill_passed,
                sponsor.median=median(sponsor.median[bill.passed],na.rm=T),
                liberal_passed=mean(bill.passed & sponsor.t==-1,na.rm=T)/mean(bill.passed),
                moderate_passed=mean(bill.passed & sponsor.t==0,na.rm=T)/mean(bill.passed),
                conservative_passed=mean(bill.passed & sponsor.t==1,na.rm=T)/mean(bill.passed),
                liberal_advantage=liberal_passed/conservative_passed,
                bill_failed=mean(!bill.passed,na.rm=T),
                bill_failed_n=n()*bill_failed,
                count=n())  %>%
      print(n=2)
    
    
    tabyl(year.summary[[method]]$liberal_advantage>1,show_na=F) # 86% of years have policy moving leftward
    summary(year.summary[[method]]$liberal_advantage) # 34% advantage in liberal bills
    
    summary(year.summary[[method]]$bill_passed_n[year.summary[[method]]$year>2009 & year.summary[[method]]$year<2024]) # 3271
    summary(year.summary[[method]]$count) # 13316
    
    # state-year summary
    
    styear.summary[[method]] <- bill.m %>%
      filter(!year%in%c(2008,2009)) %>%
      filter(bill.passed) %>%
      group_by(st,year) %>%
      summarise(sponsor.median=median(sponsor.median,na.rm=T),
                liberal_passed=mean(sponsor.t==-1,na.rm=T)/n(),
                moderate_passed=mean(sponsor.t==0,na.rm=T)/n(),
                conservative_passed=mean(sponsor.t==1,na.rm=T)/n(),
                liberal_advantage=liberal_passed/conservative_passed) %>%
      left_join(st.codes %>% select(st,region),by="st") %>%
      mutate(sponsor.avg=round(zoo::rollapply(data=sponsor.median,width=3,FUN=mean,partial=T,na.rm=T),1)) %>%
      print(n=2)
    
    end.yr = ifelse(revision %% 2 == 0,revision-1,revision)
    styr.sub[[method]] <- styear.summary[[method]] %>% 
      filter(year %in% seq(2011,end.yr,by=2)) %>% 
      filter(!st%in%c("KY")) %>%
      select(st,region,year,sponsor.avg) %>%
      mutate(year=as.character(year)) %>%
      print(n=2)

  #}  
  
  if (!dir.exists("Objects/Summaries")) { dir.create("Objects/Summaries") }
  filen=str_c("Objects/Summaries/",fn," ",method," state and year summaries.Rdata")
  print(filen)
  save(year.summary,styear.summary,styr.sub,file=str_c("Objects/Summaries/",fn," ",method," state and year summaries.Rdata"))
  
}

descriptives.st.yr.outcomes <- function (region.num, method = "topic") {
  
  if (!requireNamespace("ggslopegraph", quietly = TRUE)) {
    stop(
      "descriptives.st.yr.outcomes() requires the ggslopegraph package. ",
      "Install it with pak::pak(\"bshor/ggslopegraph\").",
      call. = FALSE
    )
  }
  
  styr.region <- styr.sub[[method]] %>% filter(region==region.num)
  ggslopegraph::ggslopegraph(styr.region, Times=year, Measurement=sponsor.avg, Grouping=st, Title = NULL, SubTitle= NULL, Caption=NULL) +  
    theme_bw() + labs(x=NULL, y=NULL) + theme(legend.position = "none") + ylim(-1.9,1.9)
  
}


descriptives.sponsors.party.all <- function (fn="health") {
  sponsor.summary <- bill.m %>% 
    filter(!is.na(legparty) & !is.na(sponsor.party)) %>%
    mutate(legparty=recode(legparty,"D"="D Majority","R"="R Majority","S"="Split Majority")) %>%
    group_by(legparty,sponsor.party) %>% 
    summarise(count = n()) %>%
    mutate(freq=prop.table(count)) 
  
  cols3 = c("grey50","blue4","red4")
  
  ggplot(sponsor.summary,aes(x=sponsor.party, y=freq, fill=sponsor.party)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    geom_text(aes(label=scales::percent(freq,accuracy = 1), y = freq + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Party of Bill Sponsors",y=str_c("% Sponsored of Introduced ",str_to_title(fn)," Bills")) +
    scale_y_continuous(labels = scales::label_percent(), limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.sponsors.party.passed <- function () {
  sponsor.summary <- bill.m %>% 
    filter(!is.na(legparty) & !is.na(sponsor.party)) %>%
    filter(bill.passed) %>%
    mutate(legparty=recode(legparty,"D"="D Majority","R"="R Majority","S"="Split Majority")) %>%
    group_by(legparty,sponsor.party) %>% 
    summarise(count = n()) %>%
    mutate(freq=prop.table(count)) 
  
  cols3 = c("grey50","blue4","red4")
  
  ggplot(sponsor.summary,aes(x=sponsor.party, y=freq, fill=sponsor.party)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    #geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    geom_text(aes(label=scales::percent(freq,accuracy = 1), y = freq + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Party of Bill Sponsors",y="% Sponsored of Passed Health Bills") +
    scale_y_continuous(labels = scales::label_percent(), limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.sponsors.ideology.all <- function () {
  
  sponsor.summary <- bill.m %>% 
    filter(legparty%in%c("D","R","S") & !is.na(sponsor.t)) %>%
    mutate(legparty=recode(legparty,"D"="D Majority","R"="R Majority","S"="Split Majority"),
           sponsor.t=recode(sponsor.t,'1'="Right",
                            '0'="Moderate",
                            '-1'="Left")) %>%
    group_by(legparty,sponsor.t) %>% 
    summarise(count = n()) %>%
    mutate(freq=prop.table(count))
  
  cols3 = c("blue4","grey50","red4")
  
  ggplot(sponsor.summary,aes(x=sponsor.t, y=freq, fill=sponsor.t)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    geom_text(aes(label=scales::percent(freq,accuracy = 1), y = freq + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Ideology Tercile of Bill Sponsors",y="% Sponsored of Health Bills") +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.sponsors.ideology.passed <- function () {
  
  sponsor.summary <- bill.m %>% 
    filter(legparty%in%c("D","R","S") & !is.na(sponsor.t)) %>%
    filter(bill.passed) %>%
    mutate(legparty=recode(legparty,"D"="D Majority","R"="R Majority","S"="Split Majority"),
           sponsor.t=recode(sponsor.t,'1'="Right",
                            '0'="Moderate",
                            '-1'="Left")) %>%
    group_by(legparty,sponsor.t) %>% 
    summarise(count = n()) %>%
    mutate(freq=prop.table(count))
  
  cols3 = c("blue4","grey50","red4")
  
  ggplot(sponsor.summary,aes(x=sponsor.t, y=freq, fill=sponsor.t)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    geom_text(aes(label=scales::percent(freq,accuracy = 1), y = freq + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Ideology Tercile of Bill Sponsors",y="% of Passed Health Bills") +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.sponsors.party.hitrate <- function () {
  
  sponsor.summary <- bill.m %>% 
    filter(!is.na(legparty) & !is.na(sponsor.party)) %>%
    mutate(legparty=recode(legparty,"D"="D Majority","R"="R Majority","S"="Split Majority")) %>%
    group_by(legparty,sponsor.party) %>% 
    summarise(count = n(),passed=sum(bill.passed)) %>%
    mutate(hit=passed/count)
  
  sponsor.summary
  
  cols3 = c("grey50","blue4","red4")
  
  ggplot(sponsor.summary,aes(x=sponsor.party, y=hit, fill=sponsor.party)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    geom_text(aes(label=scales::percent(hit,accuracy = 1), y = hit + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Party of Bill Sponsors",y="% Passed of Health Bills Introduced") +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.sponsors.ideology.hitrate <- function () {
  
  sponsor.summary <- bill.m %>% 
    filter(legparty%in%c("D","R","S") & !is.na(sponsor.t)) %>%
    mutate(legparty=recode(legparty,"D"="D Majority","R"="R Majority","S"="Split Majority"),
           sponsor.t=recode(sponsor.t,'1'="Right",
                            '0'="Moderate",
                            '-1'="Left")) %>%
    group_by(legparty,sponsor.t) %>% 
    summarise(count = n(),passed=sum(bill.passed)) %>%
    mutate(hit=passed/count)
  
  sponsor.summary
  
  cols3 = c("blue4","grey50","red4")
  
  ggplot(sponsor.summary,aes(x=sponsor.t, y=hit, fill=sponsor.t)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    geom_text(aes(label=scales::percent(hit,accuracy = 1), y = hit + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Ideology Tercile of Bill Sponsors",y="% Passed of Health Bills Introduced") +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.ideology.trend <- function () {
  
  library(lubridate)
  
  sponsor.summary <- bill.m %>%
    #mutate(year=as.Date(as.character(year), format="%Y")) %>%
    mutate(year=ymd(str_c(year,"-01-01"))) %>%
    filter(bill.passed) %>%
    group_by(year) %>%
    summarize(
      sponsor = mean(sponsor.median,na.rm=T),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.long <- sponsor.summary %>%
    gather(key = "value", value = "measure", sponsor) %>%
    mutate(value=recode(value,"sponsor"="Sponsor Ideology"))
  
  sponsor.mean <- sponsor.long %>% summarise(mean=mean(measure))
  
  sponsor.range <- c(-max(abs(pretty(sponsor.summary$sponsor))),max(abs(pretty(sponsor.summary$sponsor))))
  
  ggplot(data=sponsor.long, aes(x=year,y=measure)) +
    ylim(sponsor.range) +
    scale_x_date(date_breaks = "2 year",date_labels="%Y",limits = as.Date(c("2011-01-01","2023-01-01"))) +
    geom_line(linewidth=1.25) + geom_smooth(method="lm",formula = y ~ x,linetype="dotted", se=F) +
    geom_hline(yintercept = 0, linetype="dashed", color="grey50") +
    labs(x="",y="Average Sponsor Ideology") +  
    theme_bw() + theme(legend.position = "none", legend.title = element_blank()) #+ facet_wrap(~majparty)
  
}

descriptives.legparty.ideology.trend <- function () {
  # trend in sponsor ideology for passed bills
  
  sponsor.summary <- bill.m %>%
    #mutate(year=as.Date(as.character(year), format="%Y")) %>%
    mutate(year=ymd(str_c(year,"-01-01"))) %>%
    filter(bill.passed) %>%
    group_by(legparty,year) %>%
    summarize(
      sponsor = mean(sponsor.median,na.rm=T),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.long <- sponsor.summary %>%
    gather(key = "value", value = "measure", sponsor) %>%
    mutate(value=recode(value,"sponsor"="Sponsor Ideology"))
  
  sponsor.mean <- sponsor.long %>% group_by(legparty) %>% summarise(mean=mean(measure))
  
  sponsor.range <- c(-max(abs(pretty(sponsor.summary$sponsor))),max(abs(pretty(sponsor.summary$sponsor))))
  
  
  #https://stackoverflow.com/questions/26195231/ggplot2-manually-specifying-colour-with-geom-line
  
  cols3 = c("blue4","red4","grey25")
  
  ggplot(data=sponsor.long, aes(x=year,y=measure,color=legparty,group=legparty)) +
    ylim(sponsor.range) +
    scale_x_date(date_breaks = "2 year",date_labels="%Y",limits = as.Date(c("2011-01-01","2023-01-01"))) +
    scale_color_manual(values = cols3) +
    geom_line(linewidth=1.25) + geom_smooth(method="lm",formula = y ~ x,linetype="dotted", se=F) +
    geom_hline(yintercept = 0, linetype="dashed", color="grey50") +
    labs(x="",y="Average Sponsor Ideology") +  
    theme_bw() + theme(legend.position = "none", legend.title = element_blank()) #+ facet_wrap(~majparty)
  
  #ggs("sponsor_ideology_majparty_bill_passed",plottype="Trends",subfolder=T, width=8,height=5)
  
}

descriptives.st.outcomes <- function() {
  library(usmap)
  st.summary <- bill.m %>%
    #filter(!year%in%c(2009,2021)) %>%
    group_by(st) %>%
    summarise(bill_passed=mean(bill.passed,na.rm=T),
              bill_passed_n=n()*bill_passed,
              sponsor.median=median(sponsor.median[bill.passed],na.rm=T),
              liberal_passed=mean(bill.passed & sponsor.t==-1,na.rm=T)/mean(bill.passed,na.rm=T),
              moderate_passed=mean(bill.passed & sponsor.t==0,na.rm=T)/mean(bill.passed,na.rm=T),
              conservative_passed=mean(bill.passed & sponsor.t==1,na.rm=T)/mean(bill.passed,na.rm=T),
              liberal_advantage=liberal_passed/conservative_passed,
              liberal_binary=liberal_passed>conservative_passed,
              bill_failed=mean(!bill.passed,na.rm=T),
              bill_failed_n=n()*bill_failed,
              count=n()) %>%
    mutate(fips=fips(st),
           liberal_advantage=ifelse(liberal_advantage==Inf,10,liberal_advantage))
  
}


descriptives.maps <- function (map.type="sponsor.median") {
  
  library(usmap)
  library(scales)
  
  # do state summaries
  
  st.summary <- bill.m %>%
    #filter(!year%in%c(2009,2021)) %>%
    group_by(st) %>%
    summarise(bill_passed=mean(bill.passed,na.rm=T),
              bill_passed_n=n()*bill_passed,
              sponsor.median=median(sponsor.median[bill.passed]),
              liberal_passed=mean(bill.passed & sponsor.t==-1,na.rm=T)/mean(bill.passed,na.rm=T),
              moderate_passed=mean(bill.passed & sponsor.t==0,na.rm=T)/mean(bill.passed,na.rm=T),
              conservative_passed=mean(bill.passed & sponsor.t==1,na.rm=T)/mean(bill.passed,na.rm=T),
              liberal_advantage=liberal_passed/conservative_passed,
              liberal_binary=liberal_passed>conservative_passed,
              bill_failed=mean(!bill.passed,na.rm=T),
              bill_failed_n=n()*bill_failed,
              count=n()) %>%
    mutate(fips=fips(st),
           liberal_advantage=ifelse(liberal_advantage==Inf,10,liberal_advantage))
  
  #tabyl(st.summary$liberal_advantage>1,show_na=F) # 59% of states have policy moving leftward
  
  if (map.type=="sponsor.median") {
    plot_usmap("states", data = st.summary, values="sponsor.median") +
      scale_fill_gradient2(low = muted("blue"), high = muted("red"), guide = "none")
  }
  
  if(map.type=="liberal_passed") {
    plot_usmap("states", data = st.summary, values="liberal_passed") +
      scale_fill_gradient2(low = muted("white"), high = muted("blue"), guide = "none")
  }
  
  if(map.type=="conservative_passed") {
    plot_usmap("states", data = st.summary, values="conservative_passed") +
      scale_fill_gradient2(low = muted("white"), high = muted("red"), guide = "none")
  }
  
  if(map.type=="moderate_passed") {
    plot_usmap("states", data = st.summary, values="moderate_passed") +
      scale_fill_gradient2(low = muted("white"), high = muted("beige"), guide = "none")
  }
  
}

descriptives.maps.median <- function (st.summary) {
  
  plot_usmap("states", data = st.summary, values="sponsor.median") +
    scale_fill_gradient2(low = muted("blue"), high = muted("red"), guide = "none")
  
}

descriptives.maps.liberal <- function (st.summary) {
  
  plot_usmap("states", data = st.summary, values="liberal_passed") +
    scale_fill_gradient2(low = muted("white"), high = muted("blue"), guide = "none")
  
}

descriptives.maps.conservative <- function (st.summary) {
  
  plot_usmap("states", data = st.summary, values="conservative_passed") +
    scale_fill_gradient2(low = muted("white"), high = muted("red"), guide = "none")
  
}

descriptives.maps.moderate <- function (st.summary) {
  
  plot_usmap("states", data = st.summary, values="moderate_passed") +
    scale_fill_gradient2(low = muted("white"), high = muted("beige"), guide = "none")
  
}


descriptives.bill.outcomes <- function() {
  # includes failed, currently unfinished
  
  sponsor.summary <- bill.m %>%
    filter(!is.na(legparty)) %>%
    group_by(legparty) %>%
    summarize(
      pass = mean(bill.passed),
      failed = mean(!bill.passed),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.summary
  
  # failed vs not
  
  sponsor.long <- sponsor.summary %>%
    gather(key = "value", value = "measure", pass, failed) %>%
    mutate(
      legparty=recode(legparty,`D`="D Majority",`R`="R Majority",`S`="Split Majority"),
      value=factor(value),
      value=recode(value,"failed"="Failed","pass"="Passed"))
  
  cols2 = c("grey25","grey75")
  ggplot(sponsor.long, aes(x=value, y=measure, fill=value)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols2) +
    geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    geom_text(aes(label=scales::percent(measure,1), y = measure + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Bill Outcome",y="Proportion Bill Outcome") +
    #scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
  
}

descriptives.bill.ideology.outcomes <- function() {
  
  # includes failed, currently unfinished
  
  sponsor.summary <- bill.m %>%
    filter(!is.na(legparty) & !is.na(sponsor.t)) %>%
    group_by(legparty) %>%
    summarize(
      pass = mean(bill.passed),
      failed = mean(!bill.passed),
      liberalwin = mean(sponsor.t==-1 & bill.passed,na.rm=T),
      moderwin = mean(sponsor.t==0 & bill.passed,na.rm=T),
      conservwin = mean(sponsor.t==1 & bill.passed,na.rm=T),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.summary
  
  # failed vs not
  
  sponsor.long <- sponsor.summary %>%
    select(-pass,-count,-freq) %>%
    pivot_longer(c(failed,contains("win"))) %>%
    mutate(
      legparty=recode(legparty,`D`="D Majority",`R`="R Majority",`S`="Split Majority"),
      name=factor(name),
      name=dplyr::recode(name,"failed"="Failed","liberalwin"="Liberal Bill Passed", "moderwin"="Moderate Bill Passed", "conservwin"="Conservative Bill Passed"))
  
  sponsor.long
  
  cols4 = c("red4","grey50","blue4","purple4")
  ggplot(sponsor.long, aes(x=name, y=value, fill=legparty)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols4) +
    geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    geom_text(aes(label=scales::percent(value,1), y = value + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Bill Outcome",y="Proportion Bill Outcome") +
    #scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}


descriptives.bill.ideology.passed <- function () {
  sponsor.summary <- bill.m %>%
    filter(!is.na(legparty)) %>%
    filter(bill.passed) %>%
    group_by(legparty) %>%
    summarize(
      liberalwin = mean(sponsor.t==-1,na.rm=T),
      moderwin = mean(sponsor.t==0,na.rm=T),
      conservwin = mean(sponsor.t==1,na.rm=T),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.long <- sponsor.summary %>%
    gather(key = "value", value = "measure", liberalwin,conservwin,moderwin) %>%
    mutate(
      legparty=recode(legparty,`D`="D Majority",`R`="R Majority",`S`="Split"),
      value=factor(value),
      value=recode(value,"liberalwin"="Liberal Bill Wins","moderwin"="Moderate Bill Wins","conservwin"="Conservative Bill Wins"))
  
  
  cols3 = c("red4","blue4","purple4")
  ggplot(sponsor.long, aes(x=value, y=measure, fill=value)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3) +
    geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    geom_text(aes(label=scales::percent(measure,1), y = measure + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Bill Outcome",y="Proportion Bill Outcome") +
    scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw() + facet_wrap(~legparty)
  
}

descriptives.bill.outcomes.trends <- function () {
  # last modified 10/16/23
  # bill outcome trends
  
  sponsor.summary <- bill.m %>%
    filter(bill.passed) %>%
    filter(year>2010) %>%
    mutate(year=ymd(str_c(year,"-01-01"))) %>%
    group_by(year) %>%
    summarize(
      liberalwin = mean(sponsor.t==-1,na.rm=T),
      moderwin = mean(sponsor.t==0,na.rm=T),
      conservwin = mean(sponsor.t==1,na.rm=T),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.long <- sponsor.summary %>%
    gather(key = "value", value = "measure", liberalwin,conservwin,moderwin) %>%
    mutate(value=recode(value,"conservwin"="Conservative Bill Wins","liberalwin"="Liberal Bill Wins","moderwin"="Moderate Bill Wins"))
  
  #cols4 = c("red4","grey50","blue4","purple4")
  cols3 = c("red4","blue4","grey50")
  ggplot(data=sponsor.long, aes(x=year,y=measure,color=value,group=value)) +
    #ylim(0,1) +
    scale_x_date(date_breaks = "2 year",date_labels="%Y",limits = as.Date(c("2011-01-01","2023-01-01"))) +
    scale_y_continuous(labels = scales::label_percent(), limits=c(0,1)) +
    geom_line(size=1.25) + 
    scale_color_manual(values=cols3) + 
    geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    labs(x=NULL,y="Bill Outcomes as Proportions of Passed Bills") +  
    theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())
  
}

descriptives.bill.outcomes.majparty.trends <- function () {
  # bill outcome trends
  
  sponsor.summary <- bill.m %>%
    filter(bill.passed) %>%
    filter(year>2009) %>%
    group_by(legparty,year) %>%
    summarize(
      # pass = mean(bill.passed),
      # failed = mean(!bill.passed),
      # liberalwin = mean(sponsor.t==-1 & bill.passed,na.rm=T),
      # moderwin = mean(sponsor.t==0 & bill.passed,na.rm=T),
      # conservwin = mean(sponsor.t==1 & bill.passed,na.rm=T),
      liberalwin = mean(sponsor.t==-1,na.rm=T),
      moderwin = mean(sponsor.t==0,na.rm=T),
      conservwin = mean(sponsor.t==1,na.rm=T),
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.long <- sponsor.summary %>%
    # gather(key = "value", value = "measure", liberalwin,conservwin,moderwin,failed) %>%
    # mutate(value=recode(value,"failed"="Failed","conservwin"="Conservative Bill Wins","liberalwin"="Liberal Bill Wins","moderwin"="Moderate Bill Wins"))
    gather(key = "value", value = "measure", liberalwin,conservwin,moderwin) %>%
    mutate(value=recode(value,"conservwin"="Conservative Bill Wins","liberalwin"="Liberal Bill Wins","moderwin"="Moderate Bill Wins"))
  
  #cols4 = c("red4","grey50","blue4","purple4")
  cols3 = c("red4","blue4","grey50")
  ggplot(data=sponsor.long, aes(x=year,y=measure,color=value,group=value)) +
    #ylim(0,1) +
    geom_line(size=1.25) + 
    scale_color_manual(values=cols3) + 
    geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    labs(x=NULL,y="Bill Outcomes as Proportions of Bill Introductions") +  
    theme_bw() + theme(legend.position = "bottom", legend.title = element_blank()) +
    facet_wrap(~legparty)
  
}


descriptives <- function () {
  
  descriptives.bill.outcomes()
  
  descriptives.sponsors.party.all()
  descriptives.sponsors.party.passed()
  descriptives.sponsors.party.hitrate()
  
  descriptives.sponsors.ideology.all()
  descriptives.sponsors.ideology.passed()
  descriptives.sponsors.ideology.hitrate()
  
  descriptives.sponsors.ideology.trend()
  
  
  
  
  
  
}

stcodes <- function() {
  library(haven)
  
  st.codes<-read_dta(file="../Data/States/Constant/state data.dta")
  
  state.region[20]="Northeast" # put Maryland into the Northeast
  state.region[8]="Northeast" # put Delaware into the Northeast
  
  states.st = state.abb
  states.sta = sort(states.st)
  
  states = c(1:50)
  
  g = list()
  
  g[[1]] = which(state.region=="Northeast") # Northeast
  g[[2]] = which(state.region=="South") # South
  g[[3]] = which(state.region=="North Central") # Midwest
  g[[4]] = which(state.region=="West") # WEst
  
}

descriptives.legcontrol <- function (revision = 2024, path="../") {
  
  library(forcats)
  library(ggplot2)
  
  # revision = 2023; path = ""
  load(file=(str_c(path,"../Votesmart/Objects/",revision,"/state year aggregates.Rdata")))
  
  styr.sum <- styr %>%
    filter(year>2008) %>%
    rename("legparty"="legecont") %>%
    group_by(legparty) %>%
    summarize(
      count = n(), 
      freq= n()/nrow(.)
    ) %>%
    mutate(legparty=as.factor(legparty)) %>%
    mutate(legparty=fct_relevel(legparty,"D","S","R")) %>%
    mutate(legparty=recode(legparty,!!!leg.recode)) 
  
  ggplot(styr.sum, aes(x=legparty, y=freq, fill=legparty)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3bgr) +
    #geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    geom_text(aes(label=scales::percent(freq,1), y = freq + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Legislative Control",y="Proportion") +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw()
  
}

descriptives.bill.ideology.sum.passed <- function () {
  
  sponsor.summary <- bill.m %>%
    filter(!is.na(sponsor.t)) %>%
    filter(bill.passed) %>%
    mutate(sponsor.t=recode(sponsor.t,`-1`="Left",`0`="Moderate",`1`="Right")) %>%
    group_by(sponsor.t) %>%
    summarize(
      count = n(), 
      freq= n()/nrow(.)
    )
  
  sponsor.long <- sponsor.summary %>%
    gather(key = "value", value = "measure", sponsor.t) %>%
    mutate(
      value=factor(value))
  
  
  #cols3 = c("red4","blue4","gray")
  ggplot(sponsor.long, aes(x=measure, y=freq, fill=measure)) + geom_col(show.legend = FALSE) + 
    scale_fill_manual(values = cols3bgr) +
    #geom_hline(yintercept = 0.5, linetype="dashed", color="grey50") +
    geom_text(aes(label=scales::percent(freq,1), y = freq + .01), position=position_dodge(0.9), vjust=0) +
    labs(x="Bill Outcome",y="Proportion Bill Outcome") +
    #scale_x_discrete(guide = guide_axis(angle = 45)) +
    scale_y_continuous(labels = scales::label_percent(),  limits=c(0,1)) +
    theme_bw()
  
}

google_output <- function (bill.m, g.sheet) {
  
  # g.sheet = "13g9D9Xp8Eq4CmN5YSTTF4-RqoxtA423A9faXUzvtLk4"

  library(googlesheets4)
  
  bill.m %>%
    #filter(type=="B") %>%
    filter(bill.passed) %>%
    select(st, year, bill_number, bill_id, type, title, description, sponsor.median, sponsor.maj.dist, sponsor.chamb.dist, roll.count, bill.passed, url_legiscan) %>%
    arrange(year,st) %>%
    sheet_write(g.sheet, sheet="Bills Passed")

  bill.m %>%
    #filter(type=="B") %>%
    filter(!bill.passed) %>%
    select(st, year, bill_number, bill_id, type, title, description, sponsor.median, sponsor.maj.dist, sponsor.chamb.dist, roll.count, bill.passed, url_legiscan) %>%
    arrange(year,st) %>%
    sheet_write(g.sheet, sheet="Bills Failed")
  
}
