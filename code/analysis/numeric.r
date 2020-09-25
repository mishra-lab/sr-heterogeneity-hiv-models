do.counts = function(X){
  # general characteristics
  save.count(X$stage==1,'all')
  save.count(X$det.or.stoc=='det', 'det')
  save.count(X$comp.or.ind=='comp','comp')
  # geo.scale
  for (scale in M$geo$all){
    save.count(X$geo.scale == scale, namefun('geo',scale))
  }
  for (scale in c('city','sub.national','national')){
    save.count(X$geo.scale %in% scale, namefun('geo','any',scale))
  }
  # countries
  co.data = load.co.data()
  mph = double(0)
  for (co in get.cols.co(X)){
    name  = sub('co\\.','',co)
    plhiv = co.data[name,'plhiv']/1e6
    mph.co = sum(X[[co]])/plhiv
    mph = c(mph,ifelse(is.na(mph.co),0,mph.co))
    save.count(X[[co]], namefun(name))
    save.tex(round(mph.co,d=2), namefun('mph',name))
  }
  save.tex(median(mph), namefun('mph','med'))
  # risk groups
  save.count(X$RG.def.sex, 'RG.sex')
  save.count(X$RG.def.np,  'RG.np')
  save.count(X$RG.def.pt,  'RG.pt')
  save.count(X$RG.turnover,'RG.turnover')
  save.count(X$RG.turn.bal,'RG.turn.bal')
  save.count(X$age.n > 1,  'RG.age')
  save.count(X$risk,       'RG.risk')
  # key pops
  for (kp in M$kp$main){
    save.count(X[[namefun('RG.KP',kp,'semi')]], namefun('KP',kp,'semi'))
    save.count(X[[namefun('RG.KP',kp)]], namefun('KP',kp))
  }
  save.count(0,'KP.agyw')
  # partnership types
  for (pt in M$pt$main){
    name.semi = namefun('PT',pt,'semi')
    name      = namefun('PT',pt)
    save.count(X[[name.semi]], name.semi)
    save.count(X[[name]],      name)
  }
  # interventions
  Xi = X[X$RQi.Any.Any,]
  for (int in M$int$main){
    save.count(Xi[[namefun('RQi',int,'Any')]], namefun('RQi',int))
    save.count(Xi[[namefun('RQi',int,'All')]], namefun('RQi',int,'all'))
    save.count(Xi[[namefun('RQi',int,'Pri')]], namefun('RQi',int,'pri'))
  }
  save.count(Xi$RQi.ARTCD4.Any | Xi$RQi.ARTUTT.Any, namefun('RQi','ART.any'))
  save.count(Xi$RQi.ARTCD4.Pri | Xi$RQi.ARTUTT.Pri, namefun('RQi','ART.pri'))
  save.count(Xi$RQi.ARTCD4.Any & Xi$RQi.ARTUTT.Any, namefun('RQi','ART.both'))
  # historical
  for (int in M$int$BHi){
    save.count(X[[namefun('BHi',int)]], namefun('BHi',int))
  }
  # ART < 2011, 2015
  for (year in c(2011,2015)){
    Xy = X[X$pub.year<year,]
    save.count(Xy$RQi.ARTCD4.All, namefun('RQi',year,'ARTCD4','all'))
    save.count(Xy$RQi.ARTUTT.All, namefun('RQi',year,'ARTUTT','all'))
    save.count(Xy$RQi.ARTCD4.Pri, namefun('RQi',year,'ARTCD4','pri'))
    save.count(Xy$RQi.ARTUTT.Pri, namefun('RQi',year,'ARTUTT','pri'))
  }
  # int combos
  combos = combo.fun(Xi)
  for (name in names(combos)){
    save.tex(combos[[name]],namefun('n.combo',name))
  }
  # misc
  save.count(X$hiv.acute,'hiv.acute')
  save.count(X$age.risk, 'age.risk')
  save.count(X$age.n > 1, 'age.any')
  save.count(X$age.mix=='prop', 'age.mix.prop')
  save.count(X$age.mix=='asso', 'age.mix.asso')
  save.count(X$age.mix=='offd', 'age.mix.offd')
}

combo.fun = function(X){
  Xc = X[X$RQix.combo,]
  combos = list()
  for (i in 1:nrow(Xc)){
    combo = ''
    for (int in M$int$Any){
      name = namefun('RQi',int,'Any')
      if (name %in% names(Xc) & Xc[i,name]){
        combo = paste(combo,int,sep='.')
      }
    }
    combo = sub('^\\.','',combo)
    if (combo %in% names(combos)){
      combos[[combo]] = 1 + combos[[combo]]
    } else {
      combos[[combo]] = 1
    }
  }
  return(combos)
}

do.distrs = function(X){
  distr.fun(X$age.n, 'age.n')
  distr.fun(X$RG.n,  'RG.n')
  distr.fun(X$RG.z.n,'RGz.n')
  distr.fun(X$hiv.n, 'hiv.n')
}

distr.fun = function(x,name){
  x = x[!is.na(x)]
  save.tex(round(mean(x),d=2), namefun(name,'mu'))
  save.tex(round(sd(x),  d=2), namefun(name,'sd'))
  save.tex(quantile(x,.25),    namefun(name,'q1'))
  save.tex(quantile(x,.50),    namefun(name,'q2'))
  save.tex(quantile(x,.75),    namefun(name,'q3'))
}

do.tests = function(X){
  # beh vs year
  S = summary(glm((RQi.Partners.Any|RQi.Condoms.Any)~pub.year,data=X))$coefficients
  save.tex(round(S['pub.year','Estimate'],3),'behi-vs-pub.year-beta')
  save.tex(round(S['pub.year','Pr(>|t|)'],3),'behi-vs-pub.year-p')
  # BHi vs year
  S = summary(glm(BHix.combo~pub.year,data=X))$coefficients
  save.tex(round(S['pub.year','Estimate'],3),'BHi-vs-pub.year-beta')
  save.tex(round(S['pub.year','Pr(>|t|)'],3),'BHi-vs-pub.year-p')
  # RG.n vs year
  S = summary(glm(RG.n~pub.year,data=X))$coefficients
  save.tex(round(S['pub.year','Estimate'],3),'RG.n-vs-pub.year-beta')
  save.tex(round(S['pub.year','Pr(>|t|)'],3),'RG.n-vs-pub.year-p')
  # RG.n vs int
  for (int in M$int$main){
    RQi = namefun('RQi',int,'Any')
    S = summary(glm(paste0('RG.n~',RQi),data=X))$coefficients
    save.tex(round(S[paste0(RQi,TRUE),'Estimate'],3),paste0('RG.n-vs-',int,'-beta'))
    save.tex(round(S[paste0(RQi,TRUE),'Pr(>|t|)'],3),paste0('RG.n-vs-',int,'-p'))
  }
}