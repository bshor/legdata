# set of model functions for 2011- data

.model_core <- c("dplyr", "stringr", "tictoc")

bill_models <- function(fn="health") {
  .legdata_attach(c(.model_core, "arrow", "lme4"))
  
  # fn = "climate"
  
  fi(str_c("Objects/search ",fn," bills processed.fthr"))
  fi(str_c("Objects/search ",fn," rcs processed.fthr"))
  
  bill.m <- read_feather(str_c("Objects/search ",fn," bills processed.fthr"))
  #rc.m <- read_fst("Objects/search health rcs processed.fst")
  rc.m <- read_feather(str_c("Objects/search ",fn," rcs processed.fthr"))
  
  fits.all = list()
  fits.all[["Democratic Majority"]] <- lmer(bill.passed ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median + polar + sponsor.count + sponsor.range + gparty, data = subset(bill.m,legparty=="D" & !is.na(sponsor.q)))
  fits.all[["Republican Majority"]] <- lmer(bill.passed ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median +  polar + sponsor.count + sponsor.range + gparty, data = subset(bill.m,legparty=="R" & !is.na(sponsor.q)))
  fits.all[["Split Majority"]] <- lmer(bill.passed ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median + polar + sponsor.count + sponsor.range + gparty, data = subset(bill.m,legparty=="S" & !is.na(sponsor.q)))
  
  # modelsummary(fits.all,fmt=3,statistic ="p.value",stars=T)
  # "legparty.median"="Majority Party Conservatism",
  
  # modelsummary(fits.all,fmt=3,statistic ="p.value",stars=T,
  #              gof_omit = "REML|ICC",
  #              coef_map = c("maj.median"="Majority Party Conservatism",
  #                           "sponsor.median"="Sponsor Conservatism", 
  #                           "sponsor.partyD"="Democratic Only Sponsors",
  #                           "sponsor.partyR"="Republican Only Sponsors",
  #                           "sponsor.count"="Sponsor Count","sponsor.range"="Sponsor Ideological Range",
  #                           "polar"= "Polarization",
  #                           "gpartyR"="Republican Governor"),
  #              title = "Bill Passage Models (All)")
  
  
  fits.vote = list()
  fits.vote[["Democratic Majority"]] <- lmer(hadavote ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median + polar + sponsor.count + sponsor.range + gparty, data = subset(bill.m,legparty=="D" & !is.na(sponsor.q)))
  fits.vote[["Republican Majority"]] <- lmer(hadavote ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median +  polar + sponsor.count + sponsor.range + gparty, data = subset(bill.m,legparty=="R" & !is.na(sponsor.q)))
  fits.vote[["Split Majority"]] <- lmer(hadavote ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median + polar + sponsor.count + sponsor.range + gparty, data = subset(bill.m,legparty=="S" & !is.na(sponsor.q)))


  # modelsummary(fits.vote,fmt=3,statistic ="p.value",stars=T)
    
  fits = list()
  fits[["Democratic Majority"]] <- lmer(bill.passed ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median + polar + sponsor.count + sponsor.range + majparty.roll + partyvote + unanimous + gparty, data = subset(bill.m,legparty=="D" & !is.na(sponsor.q)))
  fits[["Republican Majority"]] <- lmer(bill.passed ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median +  polar + sponsor.count + sponsor.range + majparty.roll + partyvote + unanimous + gparty, data = subset(bill.m,legparty=="R" & !is.na(sponsor.q)))
  fits[["Split Majority"]] <- lmer(bill.passed ~ (1 | st) + (1 | year) + maj.median + sponsor.party + sponsor.median + polar + sponsor.count + sponsor.range + majparty.roll + partyvote + unanimous + gparty, data = subset(bill.m,legparty=="S" & !is.na(sponsor.q)))
  
  # "legparty.median"="Majority Party Conservatism",
  
  # modelsummary(fits,fmt=3,statistic ="p.value",stars=T,
  #              gof_omit = "REML|ICC",
  #              coef_map = c("maj.median"="Majority Party Conservatism",
  #                           "sponsor.median"="Sponsor Conservatism", 
  #                           "sponsor.partyD"="Democratic Only Sponsors",
  #                           "sponsor.partyR"="Republican Only Sponsors",
  #                           "polar"= "Polarization","sponsor.count"="Sponsor Count","sponsor.range"="Sponsor Ideological Range",
  #                                 "majparty.roll"="1+ Majority Rolls","partyvote"="1+ Party Unity Votes","unanimous"="1+ Unanimous RCs",
  #                           "gparty"="Republican Governor"),
  #              title = "Bill Passage Models (Floor Vote)")

  save(fits.all, fits, file = "Objects/Models/bill models.Rdata", compress = "xz", compression_level = 3)

  fi("Objects/Models/bill models.Rdata")
  
}

billmodel_output <- function (fit, title) {
  .legdata_attach("modelsummary")
  modelsummary(fit,fmt=3,stars=T,
               #estimate = "{estimate}{stars}",
               statistic=NULL,
               gof_omit = "REML|ICC|AIC|BIC|RMSE|R2 Cond",
               coef_map = c("maj.median"="Majority Party Conservatism",
                            "sponsor.median"="Sponsor Conservatism", 
                            "sponsor.partyD"="Democratic Only Sponsors",
                            "sponsor.partyR"="Republican Only Sponsors",
                            "polar"= "Polarization","sponsor.count"="Sponsor Count","sponsor.range"="Sponsor Ideological Range",
                            "majparty.roll"="1+ Majority Rolls","partyvote"="1+ Party Unity Votes","unanimous"="1+ Unanimous RCs",
                            "gparty"="Republican Governor"),
               title = title)
  
}

billmodel_predict <- function (fit, party,
                               predictor="sponsor.median", 
                               xaxis = "Sponsor Ideology",
                               xrange=c(-1.5,1.5),
                               plottype = "xcontinuous") {
  
  .legdata_attach(c("ggplot2", "sjPlot", "stringr"))
  
  # fit = fits.all
  #party="Democratic"; predictor="sponsor.median"; xaxis = "Sponsor Ideology"; xrange=c(-1.5,1.5)
  

  if(plottype=="xcontinuous") {
    plot_model(fit[[str_c(party," Majority")]],type="eff",term=c(str_c(predictor)),pred.type="re",title=str_c(party," majority"),axis.title = c(xaxis, "Probability Bill Passes"),colors="bw")+
      theme_bw()+theme(legend.position="none")+geom_hline(yintercept=0.5, linetype="dashed",color="grey")+
      coord_cartesian(xlim=xrange,ylim=c(0,1))
    
  } else if (plottype == "xnominal") {
    plot_model(fit[[str_c(party," Majority")]],type="eff",term=c(str_c(predictor, " [all]")),pred.type="re",title=str_c(party," majority"),axis.title = c(xaxis, "Probability Bill Passes"),colors="bw")+
      theme_bw()+theme(legend.position="none")+geom_hline(yintercept=0.5, linetype="dashed",color="grey")+
      coord_cartesian(ylim=c(0,1))
    
  } else if (plottype == "both") {
    plot_model(fit[[str_c(party," Majority")]],type="eff",ci.lvl = NA,
               term=c("sponsor.median","sponsor.party"),
               pred.type="re",title=str_c(party," majority"),
               axis.title = c(xaxis, "Probability Bill Passes"),colors=c("green4","blue4","red4"))+
      theme_bw()+geom_hline(yintercept=0.5, linetype="dashed")+
      coord_cartesian(xlim=xrange,ylim=c(0,1))
    
  }
  
  
}

votemodel_predict <- function () {
    .legdata_attach(c("cowplot", "ggplot2", "sjPlot", "stringr", "tictoc"))

    # 33s R37
    tic("Load model fits") 
    load("Objects/Models/search lpm minimal fits.Rdata")
    toc()
  
    names(fit.all) = c("Democratic","Bipartisan","Republican")
    
    tic("Generate predictive plots")
    p<-list()
    for (i in names(fit.all)) {
      p[[i]]<-plot_model(fit.all[[i]],type="eff",ci.lvl = NA,
                    term=c("pred.np","republican"),
                    pred.type="re",title=str_c(i, " Sponsored Bills"),
                    axis.title = c("Legislator Conservatism", "Probability Vote Yea"),colors=c("blue4","red4"))+
        theme_bw()+theme(legend.position="none")+geom_hline(yintercept=0.5, linetype="dashed")+
        coord_cartesian(xlim=c(-1.5,1.5),ylim=c(0,1))
    }
    toc()
    
    tic("Save plot")
    save(p, file="Plots/Models/vote model marginal effect.Rdata" , compress="xz", compression_level = 1)
    toc()
    
    tic("Save sjplot")
    save_plot(filename="Plots/Models/vote model marginal effect d.svg",fig=p[["Democratic"]])
    save_plot(filename="Plots/Models/vote model marginal effect b.svg",fig=p[["Bipartisan"]])
    save_plot(filename="Plots/Models/vote model marginal effect r.svg",fig=p[["Republican"]])
}


model_ncsl <- function (estimator = "lpm", minimal = T, topics.model = F, save.model = T, para="FORK") {
  .legdata_attach(c(.model_core, "cowplot", "doParallel", "fs", "fst",
                    "ggplot2", "lmerTest", "modelsummary", "sjPlot",
                    "stargazer", "tibble"))
  
  tic.clearlog()  
  tic("Model NCSL")
  # topics.model = F; estimator = "lpm"; save.model = T; para = "FORK"; minimal = T
  
  tic("Loading health policy data")
  file_info("Objects/ncsl health policy bills 2022.Rdata") %>% select(size,modification_time)
  load(file="Objects/ncsl health policy bills 2022.Rdata") # generated by health functions.R
  
  file_info("Objects/lookup results votes health.fst") %>% select(size,modification_time)
  vote.m <- read_fst ("Objects/lookup results votes health.fst")
  #load("Objects/lookup results health.Rdata") # generated by legiscan lookup by rc in legiscan functions.R
  toc(log=T)
  
  model.data=vote.m # used to be ncsl.data
  nrow(model.data)
  
  cat(str_c("Cluster type is ",para,"\n"))
  
  model.formula.all = "vote~republican + mrp_estimate + pred.np + (1+republican+pred.np+mrp_estimate|rc.id)+(1|st)"
  
  if (topics.model) { 
    topic.top = str_replace_all(topic.top,"\\/|\\(|\\)",".")
    topic.top = str_replace_all(topic.top,"\\&","and")
    
    tt = topic.top[-which(topic.top%in%c("NA","Other",""))]
    models = tt
    model.formula.topic = rep(model.formula.all,length(tt)); names(model.formula.topic) = tt
    filen.topic = "topic"
    model.formula = model.formula.topic
  } else {
    if (!minimal) {
      models = c("d","r","party.only","ideology.only","opinion.only","all")
      model.formula = c(d="vote~mrp_estimate + pred.np + (1+pred.np+mrp_estimate|rc.id)+(1|st)",
                        r="vote~mrp_estimate + pred.np + (1+pred.np+mrp_estimate|rc.id)+(1|st)",
                        party.only="vote~republican + (1+republican|rc.id)+(1|st)",
                        ideology.only="vote~pred.np + (1+pred.np|rc.id)+(1|st)",
                        opinion.only="vote~mrp_estimate + (1+mrp_estimate|rc.id)+(1|st)",
                        all=model.formula.all)
    } else {
      models = c("d","r")
      model.formula = c(d="vote~mrp_estimate + pred.np + (1+pred.np+mrp_estimate|rc.id)+(1|st)",
                        r="vote~mrp_estimate + pred.np + (1+pred.np+mrp_estimate|rc.id)+(1|st)")
    }
    filen.topic = "" 
  }
  
  print(models)
  
  
  #n.threads = min(length(models),28)
  #n.threads=8
  
  n.threads=min(length(models),30)
  
  print(str_c("Thread count: ",n.threads))
  
  # time.start = Sys.time()
  # print(str_c("Estimator: ",estimator))
  # print(str_c("Time started: ",as.character(Sys.time())))
  
  
  tic("Estimation")
  
  cl <- makeCluster(n.threads,type=para,outfile="Output/mlm ncsl fits.txt")
  registerDoParallel(cl)
  
  fit.ncsl<-foreach (m=models,.packages=c('lme4'),.export=c('model.data','model.formula','estimator','topics.model'),.verbose = F) %dopar% {
    # if (m=="d") { m.data = subset(model.data,party=="D")}
    # if (m=="r") { m.data = subset(model.data,party=="R")}
    # 
    if (m %in% c("d","r")) { 
      m.data = subset(model.data,party==toupper(m)) 
    } else { 
      if (topics.model) { 
        m.data = subset(model.data,get(m)==TRUE)
        m.data$rc.id = as.factor(m.data$rc.id)
        print(dim(m.data))
      } else {
        m.data = model.data     
      }
    }
    
    m.data = subset(m.data,party%in%c("D","R"))
    m.data$republican = ifelse(m.data$party=="R",1,0)
    
    if (estimator == "logit") {
      fit.ncsl <- glmer(formula = as.formula(model.formula[m]), family=binomial(link="logit"),data=m.data,control=glmerControl(optimizer = "nloptwrap"),nAGQ=0)
    } else {
      fit.ncsl <- lmer(formula = as.formula(model.formula[m]), data=m.data,control=lmerControl(optimizer = "nloptwrap"))
    }
    
  }
  
  stopCluster(cl)
  # print(str_c("Time finished: ",Sys.time()))
  # time.process = round(lubridate::as.duration(Sys.time()-time.start))
  # print(str_c("Time elapsed: ",time.process)) # 67s
  names(fit.ncsl)=models
  
  toc(log=T)
  
  # gen ncsl fits
  if (minimal) {
    if (topics.model) { 
      filen = str_c("Objects/ncsl topics ",estimator," minimal fits.Rdata") 
    } else {
      filen = str_c("Objects/ncsl ",estimator," minimal fits.Rdata") 
    }
  } else {
    if (topics.model) { 
      filen = str_c("Objects/ncsl topics ",estimator," fits.Rdata") 
    } else {
      filen = str_c("Objects/ncsl ",estimator," fits.Rdata") 
    }
  }
  
  if (save.model) {
    tic("Saving fits")
    save(fit.ncsl,file=filen,compress = "xz",compression_level = 1)
    names(fit.ncsl) = c("Democrat","Republican")
    
    #https://stackoverflow.com/questions/1581232/add-commas-into-number-for-output
    rows <- tribble(~term, ~Democrat,  ~Republican, 
                    'Observations', scales::comma(nrow(fit.ncsl[["Democrat"]]@frame)), 
                    scales::comma(nrow(fit.ncsl[["Republican"]]@frame)))
    
    attr(rows, 'position') <- c(7,0)
    
    fit.ncsl.summary <- modelsummary::modelsummary(fit.ncsl,add_rows = rows,output = "modelsummary_list")
    summary.filen <- filen %>% str_replace("fits","summary") %>% str_replace("Objects","Tables")
    save(fit.ncsl.summary,rows,file=summary.filen,compress = "xz",compression_level = 1)
    toc(log = T)
    
  }
  
  
  # system.time(load(filen)) # 17.7s
  
  fit.stats.ncsl = sapply(fit.ncsl,function(x){fit.stats(x)})
  fsa.df = fit.stats.ncsl %>% t() %>% as.data.frame()
  rownames(fsa.df) = models
  
  if (estimator == "lpm") {
    # All - parties - LPM - latex
    
    # stargazer(fit.ncsl[["d"]],fit.ncsl[["r"]],type="text",
    #           covariate.labels = c("District Conservatism", "Legislator Conservatism"),
    #           column.labels = c("Democrats","Republicans"),
    #           title = "Party-Level Health Reform Models - NCSL (LPM)",
    #           dep.var.labels.include=F,
    #           dep.var.caption="",
    #           digits =2,
    #           add.lines = list(c("PCP", unlist(fsa.df[c("d","r"),"pcp"])),c("PRE",unlist(fsa.df[c("d","r"),"pre"])),c("R2 GLMM (C)",unlist(fsa.df[c("d","r"),"r2c"])))
    # )
    # 
    # stargazer(fit.ncsl[["d"]],fit.ncsl[["r"]],type="latex",
    #           covariate.labels = c("District Conservatism", "Legislator Conservatism"),
    #           column.labels = c("Democrats","Republicans"),
    #           out="Tables/party_health_models_lpm_ncsl.tex",
    #           title = "Party-Level Health Reform Models - NCSL (LPM)",
    #           label = "health_models_party_lpm_ncsl", 
    #           dep.var.labels.include=F,
    #           dep.var.caption="",
    #           digits =2,
    #           add.lines = list(c("PCP", unlist(fsa.df[c("d","r"),"pcp"])),c("PRE",unlist(fsa.df[c("d","r"),"pre"])),c("R2 GLMM (C)",unlist(fsa.df[c("d","r"),"r2c"])))
    
    stargazer(fit.ncsl[["d"]],fit.ncsl[["r"]],type="text",
              covariate.labels = c("District Conservatism", "Legislator Conservatism"),
              column.labels = c("Democrats","Republicans"),
              out="Tables/party_health_models_lpm_ncsl.tex",
              title = "Party-Level Health Reform Models - NCSL (LPM)",
              label = "health_models_party_lpm_ncsl",
              dep.var.labels.include=F,
              dep.var.caption="",
              digits =2,
              add.lines = list(c("PCP", unlist(fsa.df[c("d","r"),"pcp"])),c("PRE",unlist(fsa.df[c("d","r"),"pre"])),c("R2 GLMM (C)",unlist(fsa.df[c("d","r"),"r2c"])))
              
    )
  } else {
    # stargazer(fit.ncsl[["d"]],fit.ncsl[["r"]],type="text",
    #           covariate.labels = c("District Conservatism", "Legislator Conservatism"),
    #           column.labels = c("Democrats","Republicans"),
    #           title = "Party-Level Health Reform Models - NCSL (Logit)",
    #           dep.var.labels.include=F,
    #           dep.var.caption="",
    #           digits =2,
    #           add.lines = list(c("PCP", unlist(fsa.df[c("d","r"),"pcp"])),c("PRE",unlist(fsa.df[c("d","r"),"pre"])))
    # )
    
    stargazer(fit.ncsl[["d"]],fit.ncsl[["r"]],type="text",
              covariate.labels = c("District Conservatism", "Legislator Conservatism"),
              column.labels = c("Democrats","Republicans"),
              out="Tables/party_health_models_logit_ncsl.tex",
              title = "Party-Level Health Reform Models - NCSL (Logit)",
              label = "health_models_party_logit_ncsl",
              dep.var.labels.include=F,
              dep.var.caption="",
              digits =2,
              add.lines = list(c("PCP", unlist(fsa.df[c("d","r"),"pcp"])),c("PRE",unlist(fsa.df[c("d","r"),"pre"])))
    )
    
  }
  
  qu(model.data$pred.np[model.data$party=="D"],c(0.025,0.975)) # -2.04, .04
  qu(model.data$pred.np[model.data$party=="R"],c(0.025,0.975)) # 0, 1.75
  
  m.data = subset(model.data,party=="D")  
  d.full<-plot_model(fit.ncsl[["d"]],type="eff",term=c("pred.np [all]"),pred.type="re",title="Democrats",axis.title = c("Legislator Ideology","Probably Vote Yes"),colors="bw")+
    theme_bw()+theme(legend.position="none")+geom_hline(yintercept=0, linetype="dashed",color="grey")+
    coord_cartesian(xlim=c(-2,0.05),ylim=c(0,1))
  d.full
  
  m.data = subset(model.data,party=="R")  
  r.full<-plot_model(fit.ncsl[["r"]],type="eff",term=c("pred.np [all]"),pred.type="re",title="Republicans",axis.title = c("Legislator Ideology","Probably Vote Yes"),colors="bw")+
    theme_bw()+theme(legend.position="none")+geom_hline(yintercept=0, linetype="dashed",color="grey")+
    coord_cartesian(xlim=c(0,1.75),ylim=c(0,1))
  r.full
  
  p<-plot_grid(list(d.full,r.full))
  
  if(estimator=="lpm") {
    
  } else {
    ggsave("Plots/PNG/Models/pred_ideology_party_logit.png",plot=p,width=11,height=14)
    ggsave("Plots/PDF/Models/pred_ideology_party_logit.pdf",plot=p,width=11,height=14)
    
  }
  
  # all pool lpm
  
  if (estimator == "lpm") {
    # stargazer(fit.ncsl[["party.only"]],fit.ncsl[["ideology.only"]],fit.ncsl[["opinion.only"]],fit.ncsl[["all"]],type="text",
    #           covariate.labels = c("Republican (R=1)","Legislator Conservatism", "District Conservatism"),
    #           column.labels = c("Party Only","Ideology Only","Opinion Only","Combined"),
    #           dep.var.labels.include=F,
    #           dep.var.caption="",
    #           digits =2,
    #           add.lines = list(c("PCP",unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pcp"])),
    #                            c("PRE",unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pre"])),
    #                            c("R2 GLMM (C)",unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"r2c"])))
    # )
    
    
    stargazer(fit.ncsl[["party.only"]],fit.ncsl[["ideology.only"]],fit.ncsl[["opinion.only"]],fit.ncsl[["all"]],type="text",
              out="Tables/pooled_reform_models_lpm_ncsl.tex",
              title = "Health Reform Models - NCSL (LPM)",
              label = "pooled_health_models_lpm_search",
              covariate.labels = c("Republican (R=1)","Legislator Conservatism", "District Conservatism"),
              column.labels = c("Party Only","Ideology Only","Opinion Only","Combined"),
              dep.var.labels.include=F,
              dep.var.caption="",
              digits = 2,
              add.lines = list(c("PCP",unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pcp"])),
                               c("PRE",unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pre"])),
                               c("R2 GLMM (C)",unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"r2c"])))
    )  
    
  } else {
    # stargazer(fit.ncsl[["party.only"]],fit.ncsl[["ideology.only"]],fit.ncsl[["opinion.only"]],fit.ncsl[["all"]],type="text",
    #           covariate.labels = c("Republican (R=1)","Legislator Conservatism", "District Conservatism"),
    #           column.labels = c("Party Only","Ideology Only","Opinion Only","Combined"),
    #           dep.var.labels.include=F,
    #           dep.var.caption="",
    #           digits =2,
    #           add.lines = list(c("PCP", unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pcp"])),
    #                            c("PRE", unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pre"])))
    # )
    
    
    stargazer(fit.ncsl[["party.only"]],fit.ncsl[["ideology.only"]],fit.ncsl[["opinion.only"]],fit.ncsl[["all"]],type="text",
              out="Tables/pooled_reform_models_logit_ncsl.tex",
              title = "Health Reform Models - NCSL (Logit)",
              label = "pooled_health_models_logit_ncsl",
              covariate.labels = c("Republican (R=1)","Legislator Conservatism", "District Conservatism"),
              column.labels = c("Party Only","Ideology Only","Opinion Only","Combined"),
              dep.var.labels.include=F,
              dep.var.caption="",
              digits = 2,
              add.lines = list(c("PCP", unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pcp"])),
                               c("PRE", unlist(fsa.df[c("party.only","ideology.only","opinion.only","all"),"pre"])))
    )
    
  }
  
  
  
}

model_intros <- function (revision = 2021) {
  .legdata_attach(c(.model_core, "fst", "stargazer", "tibble"))
  #revision = 2021
  load(str_c("../Votesmart/Objects/",revision,"/state year aggregates.Rdata"))
  
  load("../Leaders/Objects/leaderz 1989-2021.Rdata")
  
  topleaders<-read_fst("../Votesmart/Leaders/Objects/topleaders.fst") %>%
    mutate(chamb=tolower(str_sub(chamber,1,1))) %>%
    rename(leader.diff=diff,leader.abs=abs,leader.score=pred.np) %>%
    filter(leadertype=="chamber") %>%
    select(-chamber)
  
  rc.m <- read_fst("Objects/search results rollcalls health.fst")
  bill.m <- read_fst(str_c("Objects/search results bills health.fst")) %>%
    left_join(styr.long %>% select(-contains(".sd"),-chamb.median,-dem.median,-rep.median,-gparty,-legparty,-majparty,-polar),by=c("st","year","chamb"))
    left_join(topleaders %>% select(st,year,chamb,leader.score,leader.diff,leader.abs) ,by=c("st","year","chamb")) %>%
    left_join(legprof,by=c("st","year")) %>%
    #mutate(health=ifelse(bill_id%in%bills.health$bill_id,1,0)) %>%
    select(-description,-contains("url"),-filename) %>%
    filter(!is.na(legparty)) %>% # &  !is.na(sponsor.party) & !is.na(sponsor.q)
    #filter(!st%in%c("ID")) %>%
    mutate(sponsor.maj.dist=sponsor.median-maj.median,
           sponsor.abs.dist=abs(sponsor.maj.dist),
           sponsor.leader.dist=sponsor.median-leader.score,
           n.chamb = n.chamber/100) %>%
    ungroup() %>%
    tibble()
  
  
  fit.intro <- list()
  
  # fit.intro[["d"]] <- lm(hadavote ~ )
  
  fit.test.r <- lm(vote~abs.mrp.dist + dist + I(dist^2) + sponsor.rep.pct, data = subset(model.data,party=="R"))
  fit.test.d <- lm(vote~abs.mrp.dist + dist + I(dist^2) + sponsor.rep.pct, data = subset(model.data,party=="D"))
  stargazer(fit.test.d,fit.test.r,type="text")
  
}

model_all <- function (estimator = "lpm", minimal = T, drop.unan = T, sequential = T, opinion.measure = "score.pres.pred", ideology.measure = "pred.np", save.model = T, para="FORK", revision=2023) {
  .legdata_attach(c(.model_core, "arrow", "doParallel", "fs", "fst",
                    "janitor", "lme4", "lmerTest", "scales"))
  #estimator = "lpm"; drop.unan=T; para="FORK"; revision = 2023; sequential = T; minimal = T; 
  #ideology.measure = "pred.np"; opinion.measure = "score.pres.pred"
  #ideology.measure = "abs.dist"
  #estimator = "logit"; drop.unan=T
  
  tic.clearlog()
  tic("Model all")
  
  tic("Load and merge search vote data")
  load(file=(str_c("../Votesmart/Objects/",revision,"/state year aggregates.Rdata")))
  styr.long$chamber=str_sub(styr.long$chamber,1,1)
  load(str_c("../Votesmart/Objects/",revision,"/legz and opinion merged ",revision,".Rdata")) # 187059 188281 196397 222231
  print(nrow(legz.m))
  
  legz.m.sub <- legz.m %>%
    filter(!is.na(people.id)) %>%
    #filter(party%in%c("D","R")) %>%
    distinct(people.id,party,st,.keep_all = T) 
  
  length(unique(legz.m.sub$people.id)) # 16.9k -> 18.5k
  
  legz.m.sub[1,]
  
  # last updated 2022-04-06
  # file_info("Objects/search results rollcalls health.fst") %>% select(size,modification_time)
  # file_info("Objects/search results bills health.fst") %>% select(size,modification_time)
  # rc.m <- read_fst("Objects/search results rollcalls health.fst")
  # bill.m <- read_fst(str_c("Objects/search results bills health.fst"))

  # fn = "climate"
  
  fi(str_c("Objects/search ",fn," bills processed.fthr"))
  fi(str_c("Objects/search ",fn," rcs processed.fthr"))
  
  bill.m <- read_feather(str_c("Objects/search ",fn," bills processed.fthr"))
  #rc.m <- read_fst("Objects/search health rcs processed.fst")
  rc.m <- read_feather(str_c("Objects/search ",fn," rcs processed.fthr"))
  vote.m <- read_feather(str_c("Objects/search results votes slim ",fn,".fthr"))
  
  # last updated 2022-04-06
  # file_info("Objects/search results votes slim health.fst") %>% select(size,modification_time)
  
  model.data <- vote.m %>%
    filter(floor) %>%
    filter(!is.na(vote)) %>%
    left_join(rc.m %>% select(bill_id,rc.id,unanimous,year),by=c("rc.id")) %>%
    filter(unanimous == ifelse(drop.unan,0,1)) %>%
    left_join(bill.m%>%select(bill_id,sponsor.median,sponsor.party,sponsor.rep.pct,sponsor.mrp),by="bill_id") %>%
    select(-vote.code,-v,-floor) %>%
    filter(year>2010) %>%
    left_join(legz.m.sub %>% select(people.id,st,party,pred.np,mrp_estimate,score.pres.pred),by=c("people.id")) %>%
    filter(party%in%c("D","R")) %>%
    mutate(dist = pred.np-sponsor.median,
           abs.dist = abs(dist),
           abs.mrp.dist = abs(sponsor.mrp-mrp_estimate),
           republican=ifelse(party=="R",1,0)) # %>%
  # slice_sample(n=100000)
  toc(log = T)
  format(lobstr::obj_size(model.data))
  
  # health 3.45m->3.78m
  # climate 266k
  
  nrow(model.data)
  
  print("Observations matched to ideology")
  print(tabyl(!is.na(model.data$pred.np))) # 98.7 -> 99.7 -> 100
  #print("Observations with mrp opinion data")
  tabyl(!is.na(model.data$mrp_estimate)) # 77.2 -> 77.8 -> 78.2
  #print("Observations with pres opinion data")
  tabyl(!is.na(model.data$score.pres.pred)) # 87.7 -> 88.3
  
  
  with(model.data,cor.test(pred.np,mrp_estimate)) # 0.70
  with(model.data,cor.test(pred.np,score.pres.pred)) # 0.96

  #tic("Prep models")
  
  #model.data=slice_sample(model.data,n=100000)
  
  print("Number of observations is...")
  print(scales::comma(nrow(model.data), accuracy = 1)) # 1.57m 1.85m 1.93m 1.98m 2.01m 3.6m 3.78m
  
  # model.data$republican = ifelse(model.data$party=="R",1,0)
  # model.data$republican[model.data$party=="X"]=NA
  #model.d
  
  cat(str_c("Cluster type is ",para,"\n"))
  
  time.start = Sys.time()
  print(str_c("Time started: ",as.character(Sys.time())))
  
  # opinion.measure = "score.pres.pred"
  # opinion.measure = "mrp_estimate"
  
  model.formula.all = str_c("vote~republican + ",opinion.measure," + dist + (1+republican+dist+",opinion.measure,"|rc.id)+(1|st)")
  if (estimator == "lpm") {
    if (!minimal) {
      models = c("d","r","party.only","ideology.only","opinion.only","all",c(2009:2022))
      model.formula.year = rep(model.formula.all,12); names(model.formula.year) = c(2009:2022)
      model.formula = c(d=str_c("vote~",opinion.measure," + ",ideology.measure," + (1+",ideology.measure,"+",opinion.measure,"|rc.id)+(1|st)"),
                        r=str_c("vote~",opinion.measure," + ",ideology.measure," + (1+",ideology.measure,"+",opinion.measure,"|rc.id)+(1|st)"),
                        party.only="vote~republican + (1+republican|rc.id)+(1|st)",
                        ideology.only="vote~",ideology.measure," + ,(1+",ideology.measure,"|rc.id)+(1|st)",
                        opinion.only=str_c("vote~",opinion.measure," + (1+",opinion.measure,"|rc.id)+(1|st)"),
                        all=model.formula.all,model.formula.year)
    } else {
      # models = c("d","r")
      # model.formula = c(d=str_c("vote~",opinion.measure," + pred.np + (1+pred.np+",opinion.measure,"|rc.id)+(1|st)"),
      #                   r=str_c("vote~",opinion.measure," + pred.np + (1+pred.np+",opinion.measure,"|rc.id)+(1|st)"))
      
      models = c("d","b","r")
      model.formula = c(d=str_c("vote~",opinion.measure," + ", ideology.measure," + (1+",ideology.measure,"+",opinion.measure,"|rc.id)+(1|st)"),
                        b=str_c("vote~",opinion.measure," + ", ideology.measure," + (1+",ideology.measure,"+",opinion.measure,"|rc.id)+(1|st)"),
                        r=str_c("vote~",opinion.measure," + ", ideology.measure," + (1+",ideology.measure,"+",opinion.measure,"|rc.id)+(1|st)"))
      
    }
    
  } else {
    models = c("d","r","party.only","ideology.only","opinion.only","all")
    model.formula = c(d=str_c("vote~",opinion.measure," + pred.np + (1+pred.np+",opinion.measure,"|rc.id)+(1|st)"),
                      r=str_c("vote~",opinion.measure," + pred.np + (1+pred.np+",opinion.measure,"|rc.id)+(1|st)"),
                      party.only="vote~republican + (1+republican|rc.id)+(1|st)",
                      ideology.only="vote~pred.np + (1+pred.np|rc.id)+(1|st)",
                      opinion.only=str_c("vote~",opinion.measure," + (1+",opinion.measure,"|rc.id)+(1|st)"),
                      all=model.formula.all)
    
  }
  
  print("Models are:")
  print(model.formula)
  
  fit.test <- fit.test2 <- list()
  
  fit.test[["d"]] <- lm(vote~pred.np + republican + score.pres.pred, data = subset(model.data,sponsor.party=="D"))
  fit.test[["b"]] <- lm(vote~pred.np + republican + score.pres.pred, data = subset(model.data,sponsor.party=="B"))
  fit.test[["r"]] <- lm(vote~pred.np + republican + score.pres.pred, data = subset(model.data,sponsor.party=="R"))
  
  with(model.data %>% filter(sponsor.party=="D"), cor.test(dist,vote) )
  
  fit.test2[["d"]] <- lm(vote~abs.dist + republican + score.pres.pred, data = subset(model.data,sponsor.party=="D"))
  fit.test2[["b"]] <- lm(vote~abs.dist + republican + score.pres.pred, data = subset(model.data,sponsor.party=="B"))
  fit.test2[["r"]] <- lm(vote~abs.dist + republican + score.pres.pred, data = subset(model.data,sponsor.party=="R"))
  
  model.data %>% filter(party=="R") %>% reframe(q=quantile(dist,na.rm=T,p=c(.005,.995)))
  model.data %>% filter(party=="D") %>% reframe(q=quantile(dist,na.rm=T,p=c(.005,.995)))
  
  #plot_model(fit.test.r,type="pred",terms = c("dist [-3.75,.9]"))
  #plot_model(fit.test.d,type="pred",terms = c("dist [-1.3,3]"))
  
  if (sequential) {
    tic("Sequential models")  
    #n.threads=min(length(models),8)
    #print(str_c("Thread count:",n.threads))
    #cl <- makeCluster(n.threads,type=para)
    registerDoSEQ()
    #cl <- makeCluster(n.threads,type=para)#,outfile="Output/mlm all fits.txt")
    #registerDoParallel(cl)
    
    # 262s minimal
    fit.all<-foreach (m=models,.packages=c('lme4'),.export=c('model.data','model.formula','estimator'),.verbose = F) %do% {
      #print(paste0(m," started at ",Sys.time()))
      
      m.data <- model.data
      if (m=="d") { m.data = subset(model.data,sponsor.party=="D")}
      if (m=="b") { m.data = subset(model.data,sponsor.party=="B")}
      if (m=="r") { m.data = subset(model.data,sponsor.party=="R")}
      if (nchar(m)==4) {m.data = subset(model.data,year==as.integer(m))  }
      if (estimator == "logit") {
        fit <- glmer(formula = as.formula(model.formula[m]), family=binomial(link="logit"),data=m.data,control=glmerControl(optimizer = "nloptwrap",  calc.derivs = FALSE),nAGQ=0)
      } else {
        #fit <- lmer(formula = as.formula(model.formula.test), data=model.data,control=lmerControl(optimizer = "nloptwrap"))
        fit <- lmer(formula = as.formula(model.formula[m]), data=m.data,control=lmerControl(optimizer = "nloptwrap",  calc.derivs = FALSE))
      }
      #print(paste0(m," finished at ",Sys.time()))
    }  
    #stopCluster(cl)
    # print(str_c("Time finished: ",Sys.time()))
    # time.process = round(lubridate::as.duration(Sys.time()-time.start))
    # print(str_c("Time elapsed: ",time.process)) # 12m 13.7m
    
    toc(log=T)
    
  } else {
    tic("Parallel models")  
    n.threads=min(length(models),8)
    print(str_c("Thread count:",n.threads))
    cl <- makeCluster(n.threads,type=para)#,outfile="Output/mlm all fits.txt")
    registerDoParallel(cl)
    
    # 262s minimal
    fit.all<-foreach (m=models,.packages=c('lme4'),.export=c('model.data','model.formula','estimator'),.verbose = F) %dopar% {
      #print(paste0(m," started at ",Sys.time()))
      
      m.data <- model.data
      if (m=="d") { m.data = subset(model.data,sponsor.party=="D")}
      if (m=="b") { m.data = subset(model.data,sponsor.party=="B")}
      if (m=="r") { m.data = subset(model.data,sponsor.party=="R")}
      if (nchar(m)==4) {m.data = subset(model.data,year==as.integer(m))  }
      if (estimator == "logit") {
        fit <- glmer(formula = as.formula(model.formula[m]), family=binomial(link="logit"),data=m.data,control=glmerControl(optimizer = "nloptwrap",  calc.derivs = FALSE),nAGQ=0)
      } else {
        #fit <- lmer(formula = as.formula(model.formula.test), data=model.data,control=lmerControl(optimizer = "nloptwrap"))
        fit <- lmer(formula = as.formula(model.formula[m]), data=m.data,control=lmerControl(optimizer = "nloptwrap",  calc.derivs = FALSE))
      }
      #print(paste0(m," finished at ",Sys.time()))
    }  
    stopCluster(cl)
    # print(str_c("Time finished: ",Sys.time()))
    # time.process = round(lubridate::as.duration(Sys.time()-time.start))
    # print(str_c("Time elapsed: ",time.process)) # 12m 13.7m
    toc(log=T)

  }
  
  names(fit.all)=models
  
  # gen fits
  if (minimal) {
    filen = str_c("Objects/Models/search ",estimator," ",ideology.measure," minimal fits.Rdata")
    #models = c("d","r")
  } else {
    filen = str_c("Objects/Models/search ",estimator," fits.Rdata")
  }
  
  
  if(save.model){

    tic("Saving")
    save(fit.all,file=filen,compress = "xz",compression_level = 1) # 431s 151s 66s
    toc(log=T)
    
    fi(filen)
    
  }
  
  toc(log=T)
}

display_model <- function (estimator = "lpm", minimal = T, drop.unan = T, sequential = T, opinion.measure = "score.pres.pred", ideology.measure = "pred.np", save.model = T, para="FORK", revision=2023, rmd=T) {
  .legdata_attach(c("gt", "modelsummary", "scales", "stringr", "tibble"))
  
  
  #rmd = F; estimator = "lpm"; drop.unan=T; para="FORK"; revision = 2023; sequential = T; minimal = T; ideology.measure = "pred.np"
  #ideology.measure = "abs.dist"; opinion.measure = "score.pres.pred"
  #estimator = "logit"; drop.unan=T
  
  if (rmd) { dirpath = "../"} else { dirpath = ""}
    
  # load model fits
  if (minimal) {
    filen = str_c(dirpath,"Objects/Models/search ",estimator," ",ideology.measure," minimal fits.Rdata")
  } else {
    filen = str_c("Objects/Models/search ",estimator," fits.Rdata")
  }
  
  #fi(filen)
  load(filen)

  
  if(minimal) { names(fit.all) = c("Democratic","Bipartisan","Republican") }
  #https://stackoverflow.com/questions/1581232/add-commas-into-number-for-output
  rows <- tribble(~term, ~Democratic, ~Bipartisan,  ~Republican, 
                  'Observations', 
                  scales::comma(nrow(fit.all[["Democratic"]]@frame)),
                  scales::comma(nrow(fit.all[["Bipartisan"]]@frame)),
                  scales::comma(nrow(fit.all[["Republican"]]@frame)))
  
  attr(rows, 'position') <- c(9,0)
  
  # modelsummary(fit.all.summary,fmt=3,statistic ="p.value",stars=T,
  #              gof_omit = "REML|ICC",
  #              coef_map = c("(Intercept)"="Intercept","mrp_estimate"="District Conservatism","pred.np"="Legislator Conservatism"),
  #              add_rows=rows,
  #              caption="Search Vote Models")
  
  
  f1 <- function(x) format(round(x, 3), big.mark=",")
  f2 <- function(x) format(round(x, 0), big.mark=",")
  
  gm <- list(
    list("raw" = "RMSE", "clean" = "RMSE", "fmt" = f1),
    list("raw" = "nobs", "clean" = "N", "fmt" = f2))
  
  #modelsummary::modelsummary(fit.all,gof_map=gm,coef_omit = c("SD|Intercept|Cor"),output="markdown")
  
  #tic("Generating model summary") # 60s
  # if (!sequential) {
  #   fit.all.summary <- modelsummary::modelsummary(fit.all,add_rows = rows,gof_map=c("rmse"),mc.cores=n.threads,output = "modelsummary_list")
  # } else {
  #   modelsummary(fit.all,gof_map=gm, output="markdown")
    
    tab <- modelsummary(fit.all,
                        #metrics="RMSE",
                        output = "gt",
                        fmt=3,
                        stars= c('*' = .05,
                                 '**' = .01,
                                 '***' = .001),
                        note = "Standard errors displayed in parens. *p<0.05; **p<0.01; ***p<0.001",
                        statistic = "({std.error})",
                        estimate  = "{estimate}{stars}",
                        gof_omit = "BIC|AIC|SD|ICC|REML",
                        gof_map=gm,
                        coef_map = c("score.pres.pred"="District Presidential Vote",
                                     "pred.np"="Legislator Conservatism",
                                     "abs.dist"="Legislator-Bill Ideological Distance"),
                        coef_omit = "SD|Cor")
    tab
  
  #}
  #toc()
  
  # tic("Generating model summary") # 120s
  # fit.all.summary <- modelsummary::modelsummary(fit.all,add_rows = rows,gof_map=c("rmse"),output = "modelsummary_list")
  # toc()
  
  #summary.filen <- filen %>% str_replace("fits","summary") %>% str_replace("Objects","Tables")
  #save(fit.all.summary,rows,file=summary.filen,compress = "xz",compression_level = 1)
  
  #fi(summary.filen)
  
}
