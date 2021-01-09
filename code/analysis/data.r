load.main.data = function(...){
  X = readxl::read_excel(file.path(Sys.getenv('ROOT'),'data','main.xlsx'))
  # filter by X[name] %in% value for each (...)
  args = list(...)
  for (name in names(args)){
    X = X[boolfun(X[[name]],args[[name]],na=FALSE),]
  }
  # geographic Scale
  X$geo.scale = factor(X$geo.scale,levels=M$geo$all)
  # numeric
  numeric.cols = c('age.n','hiv.n','act.n','api.t0',
                   'act.HRW.p','act.HRM.p','act.HRW.cr')
  X$hiv.n.cts = boolfun(X$hiv.n,true='cts')
  X$age.n.cts = boolfun(X$age.n,true='cts')
  X$act.n.cts = boolfun(X$act.n,true='cts')
  for (col in numeric.cols){
    X[[col]] = clean.numeric(X[[col]],cts=NaN)
  }
  X$act.HR.pr = X$act.HRW.p / X$act.HRM.p
  X = clean.api.common(X)
  # boolean
  bool.cols = list(
    na = c(
      C$act.def, C$pt.feat, C$pt.types, 'pt.def.act', 'age.risk', 'mc',
      C$kp, C$fsw.crit, 'kp.FSW.named', 'kp.Cli.named',
      C$hiv.def, C$hiv.x, 'hiv.mort', C$hiv.morb,
      C$dx.bc, C$art.x, C$art.r, 'art.tdr'
    ),
    false = c(
      get.cols.co(X),
      C$diff # TEMP hopefully
    )
  )
  for (pt in C$pt.types){
    X[[namefun(pt,'semi')]] = boolfun(X[[pt]],true='x')
    X[[namefun(pt,'any')]]  = boolfun(X[[pt]],true=c('Y','x'))
  }
  for (diff in C$diff){
    X[[namefun(diff,'H')]]   = boolfun(X[[diff]],true='H',       na=FALSE)
    X[[namefun(diff,'any')]] = boolfun(X[[diff]],true=c('Y','H'),na=FALSE)
    X[[namefun(diff,'j')]]   = boolfun(X[[diff]],true='j',       na=FALSE)
  }
  X$art.bc.cond = boolfun(X$dx.bc.cond,true='A')
  X$bc.cond.any = boolfun(X$dx.bc.cond,true=c('Y','A'))
  X$kp.Cli.p = boolfun(X$kp.Cli,true='p')
  X$art.r.frop = boolfun(X$art.r.fail,true='x') & boolfun(X$art.r.drop,true='x')
  for (na.case in names(bool.cols)){
    for (col in bool.cols[[na.case]]){
      X[[col]] = boolfun(X[[col]],na=res.map[[na.case]])
    }
  }
  # factors
  factor.cols = c('act.mix','age.mix','pt.def.act.drive','art.rbeta.x')
  for (col in factor.cols){
    X[[col]] = factor(X[[col]])
  }
  # collect some stuff
  X$art.r.fail   = X$art.r.fail & !X$art.x.fail
  X$art.r.drop   = X$art.r.drop & !X$art.x.drop
  X$art.fail.any = anyfun(X[,c('art.x.fail','art.r.fail','art.r.frop')])
  X$art.drop.any = anyfun(X[,c('art.x.drop','art.r.drop','art.r.frop')])
  X$kp.any       = anyfun(X[,C$kp])
  X$dx.bc.any    = anyfun(X[,C$dx.bc])
  X$bc.any       = anyfun(X[,c('art.bc.cond',C$dx.bc)])
  X$hiv.morb.any = anyfun(X[,C$hiv.morb])
  X = make.diff.any(X)
  X = make.act.kp(X)
  X = make.risk(X)
  X = make.act.n.cat(X)
  X = make.act.n.sex.max(X)
  X = make.turnover(X)
  X = make.fsw.crit(X)
  X = make.cli.crit(X)
  X = make.pt.def(X)
  return(X)
}

boolfun = function(x,true='Y',na=NA){
  b = logical(length(x))
  x.na = is.na(x)
  b[!x.na] = x[!x.na] %in% true
  b[ x.na] = x[ x.na] = na
  return(b)
}

anyfun = function(X){
  return(apply(X,1,any))
}

clean.numeric = function(x,...){
  map = list('*'=NaN, '?'=NA, ...)
  for (value in names(map)){
    if (value == 'na') {
      x[is.na(x)] = map[[value]]
    } else {
      x[x==value] = map[[value]]
    }
  }
  return(as.numeric(x))
}

get.cols.co = function(X){
  co.names = sapply(rownames(load.co.data()),function(co){ paste0('co.',co) })
  return(co.names[co.names %in% colnames(X)])
}

make.act.n.cat = function(X){
  X$act.n.cat = factor(mapply(function(n,sex,kp){
      if (is.na(n) | is.na(sex)) { return(NA) }
      if (n==1)            { return(1) }
      if (n==2 & sex)      { return(2) } else
      if (n >= 2 & n <= 4) { return(3) }
      if (n >= 5 & n <= 8) { return(4) }
      if (n >= 9)          { return(5) }
    }, X$act.n, X$act.def.sex),
    levels = c(1,2,3,4,5),
    labels = c('None','Sex Only','2-4','5-8','9+')
  )
  return(X)
}

make.act.kp = function(X){
  X$act.kp = factor(1 + X$act.def.np + X$kp.any,
    levels = c(1,2,3),
    labels = c('none','some.no.kp','any.kp')
  )
  return(X)
}

make.risk = function(X){
  X$Risk = factor(mapply(function(n, sex, act, kp, kp.diff){
      if (!sex & !act) { return(1) }
      if (sex & !act)  { return(2) }
      if (act & !kp)   { return(3) }
      if (kp & kp.diff == 'priority') { return(4) }
      if (kp & kp.diff == 'same')     { return(5) }
      if (kp & kp.diff == 'gaps')     { return(6) }
    }, X$act.n, X$act.def.sex, X$act.def.np, X$kp.any, X$diff.any.kp.cat),
    levels = c(1,2,3,4,5,6),
    labels = c('None','Sex only','Activity (no KP)','KP (priority)','KP (same)','KP (gaps)')
  )
  return(X)
}
  
make.fsw.crit = function(X){
  X$kp.FSW.crit = factor(rowSums(X[,C$fsw.crit]))
  return(X)
}

make.cli.crit = function(X){
  X$kp.Cli.crit = factor(clean.numeric(X$kp.FSW.crit.pr))
  return(X)
}

make.act.n.sex.max = function(X){
  X$act.n.z = pmax(X$act.n.hw,X$act.n.hm,X$act.n.msm,na.rm=TRUE)
  return(X)
}

make.turnover = function(X){
  X$act.turnover[X$act.turnover=='D'] = 'Y'
  X$act.turn.any = X$act.turnover %in% c('H','Y')
  X$act.turnover = factor(X$act.turnover,
    levels = c('N','H','Y','R'),
    labels = c('none','high','multi','repl')
  )
  return(X)
}

make.pt.def = function(X){
  X$pt.def = factor(mapply(function(gen,act){
      if (gen){ return(1) } else if (act){ return(2) } else { return(3) }
    },X$pt.Generic,X$pt.def.act),
    levels = c(1,2,3),
    labels = c('gen','grp','phen')
  )
  return(X)
}

make.diff.any = function(X){
  app = function(s,cols){ sapply(cols,function(col){ namefun(col,s) }) }
  X$diff.any.any   = anyfun(X[app('any',C$diff)])
  X$diff.any.any.j = anyfun(X[app('j',  C$diff)])
  for (grp in M$diff$grp){
    X[[namefun('diff','any',grp)]] = anyfun(X[app('any',C[[namefun('diff',grp)]])])
    X[[namefun('diff','j',grp)]]   = anyfun(X[app('j',  C[[namefun('diff',grp)]])])
  }
  for (step in M$diff$step){
    X[[namefun('diff',step,'any')]] = anyfun(X[app('any',C[[namefun('diff',step)]])])
    X[[namefun('diff',step,'j')]]   = anyfun(X[app('j',  C[[namefun('diff',step)]])])
  }
  X$diff.any.kp.H = anyfun(X[app('H',C$diff.kp)])
  X$diff.any.kp.cat = factor(mapply(function(lo,hi){
      if (hi) { return(1) } else
      if (lo) { return(3) } else
      { return(2) }
    },X$diff.any.kp, X$diff.any.kp.H),
    levels = c(1,2,3),
    labels = c('priority','same','gaps')
  )
  return(X)
}

make.cut = function(x,cuts){
  # i.nan = is.nan(x)
  # x.cut = cut(x,c(cuts,1e6),labels=as.character(cuts),right=FALSE)
  # x.cut = factor(x.cut,levels=c(levels(x.cut),'Varies'))
  # x.cut[i.nan] = 'Varies'
  return(cut(x,c(cuts,1e6),labels=as.character(cuts),right=FALSE))
}

make.api.prev.cat = function(X){
  X$api.prev.cat = cut(X$api.prev,c(0,1,10,100),labels=c('Low','Mid','High'))
  return(X)
}

load.co.polygons = function(region='ssa'){
  Xc = load.co.data(region=region)
  X = ne_countries(
    country=Xc$admin,
    scale='medium',
    returnclass='sf')
  X = X[match(Xc$admin,X$admin),]
  Ci = suppressWarnings({ st_coordinates(st_centroid(X)) })
  rownames(Ci) = X$name
  C0 = Ci;
  colnames(Ci) = c('X0','Y0')
  scooch = list(
    Lesotho   = list(x=+5,y=-5),
    Swaziland = list(x=+5,y=-2),
    Rwanda    = list(x=-2,y=+2)
  )
  for (co in names(scooch)){
    C0[co,'X'] = C0[co,'X'] + scooch[[co]]$x
    C0[co,'Y'] = C0[co,'Y'] + scooch[[co]]$y
  }
  X = cbind(X,Ci,C0,
    Incid = Xc$inc,
    Prev  = Xc$prev,
    PLHIV = Xc$plhiv/1e6
  )
  return(X)
}

load.co.data = function(region='ssa'){
  fname = paste0(region,'.csv')
  X = read.csv(file.path(Sys.getenv('ROOT'),'data','countries',fname))
  X$name  = as.character(X$name)
  X$admin = as.character(X$admin)
  rownames(X) = X$name
  return(X)
}

load.api.data = function(X){
  A = readxl::read_excel(file.path(Sys.getenv('ROOT'),'data','api.xlsx'))
  A = A[A$art.pop=='All',]
  A = A[A$hiv.pop=='All',]
  A$api.dt       = clean.numeric(A$api.dt,na=0)
  A$api.t0       = clean.numeric(A$api.t0)
  A$art.cov      = clean.numeric(A$art.cov)
  A$art.cov.cat  = make.cut(A$art.cov,c(0,.6,.85))
  A$art.init     = clean.numeric(A$art.init)
  A$art.init.cat = make.cut(A$art.init,c(0,.2,.5,1))
  A = make.api.any(A)
  A = clean.api.common(A)
  cn = colnames(A)
  X[cn[cn!='bib']] = NULL
  XA = merge(A,X,by='bib')
  return(XA)
}

clean.api.common = function(X){
  X$art.rbeta = clean.numeric(X$art.rbeta)
  X$art.rbeta.cat = make.cut(X$art.rbeta,c(.00,.04,.10))
  for (col in C$api.hiv){
    X[[col]] = clean.numeric(X[[col]])
  }
  X$api.prev = X$api.prev * 100
  X$api.inc  = X$api.inc * 1000
  X$api.prev.cat = cut(X$api.prev,c( 0, 1,10,100),labels=c('Low','Mid','High'))
  X$api.inc.cat  = cut(X$api.inc, c(00,10,20,100),labels=c('Low','Mid','High'))
  X = make.api.phase(X)
  return(X)
}

make.api.any = function(X){
  X$api.inc.any = anyfun(!is.na(X[C$api.inc]))
  X$api.chi.any = anyfun(!is.na(X[C$api.chi]))
  X$api.both.any = (X$api.inc.any & X$api.chi.any)
  return(X)
}

make.api.phase = function(X){
  X$api.phase = factor(mapply(function(d1,d2){
      if (is.nan(d1) | is.nan(d2)) { return(NaN) }
      if (is.na (d1) | is.na (d2)) { return(NA) }
      if (d1 <  0 & d2 <= 0){ return(-2) }
      if (d1 <  0 & d2 >  0){ return(-1) }
      if (d1 == 0 & d2 == 0){ return( 0) }
      if (d1 >  0 & d2 <  0){ return(+1) }
      if (d1 >  0 & d2 >= 0){ return(+2) }
    },X$api.inc.d1,X$api.inc.d2),
    levels = c(-2,-1, 0,+1,+2),
    labels = c('decr','dts','stab','its','incr')
  )
  return(X)
}

make.api.data = function(XA,which='chi'){
  tcut = c(0,10,20,30)
  name.pat = namefun('api',which,'red','(\\d*)$')
  XA = XA %>% pivot_longer(
      matches(name.pat),
      names_pattern=name.pat,
      values_drop_na=TRUE) %>%
    mutate(t=as.numeric(name)+api.dt) %>%
    mutate(t.cat=cut(t,c(tcut,100),as.character(tcut),right=FALSE))
  return(XA)
}

agg.api.data = function(XA,fun=median,...){
  if (is.null(fun)){
    return(XA)
  } else {
    f = paste('value ~ bib + t',...,sep=' + ')
    return(aggregate(formula(f), XA, fun))
  }
}

make.bib.wt = function(XA){
  key = apply(XA[,c('bib','t')],1,paste,collapse=':')
  n.key = table(key)
  XA$wt = sapply(key, function(key){ 1/sqrt(n.key[[key]]) })
  return(XA)
}