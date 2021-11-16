numeric.main = function(X){
  # basic counts
  dir = tex.dir('n')
  save.count(X$api.include %in% c('Y','x'),'A',dir)
  save.count(X$api.include=='Y','B',dir)
  numeric.geo(X)
  numeric.co(X)
  numeric.t0(X)
  numeric.kp(X)
  numeric.act(X)
  numeric.age(X)
  numeric.pt(X)
  numeric.hiv(X)
  numeric.art(X)
  numeric.cov(X)
}
numeric.geo = function(X){
  dir = tex.dir('geo')
  for (scale in M$geo$all){
    save.count(X$geo.scale == scale, namefun(scale), dir)
  }
  for (scale in c('sub.ssa','nat','sub.nat','city')){
    save.count(X$geo.scale %in% M$geo[[scale]], namefun('any',scale), dir)
  }
}
numeric.co = function(X){
  dir = tex.dir('co')
  co.data = load.co.data()
  mph = double(0)
  n.region = list(north=1,east=2,west=3,south=4,central=5)
  n.co.other = 0
  for (co in get.cols.co(X)){
    name = sub('co\\.','',co)
    plhiv = co.data[name,'plhiv']/1e6
    mph.co = sum(X[[co]])/plhiv
    mph = c(mph,ifelse(is.na(mph.co),0,mph.co))
    re = as.character(co.data[name,'region'])
    n.region[[re]] = n.region[[re]] + sum(X[[co]])
    save.count(X[[co]], namefun(name), dir)
    save.tex(round(mph.co,d=2), namefun('mph',name), dir)
  }
  print(n.region)
  for (re in names(n.region)){
    save.tex(n.region[[re]], namefun('n','re',re), dir)
  }
  save.tex(round(median(mph),d=2), namefun('mph','med'), dir)
  co.popular = c('co.South Africa','co.Kenya','co.Zambia')
  co.other = setdiff(get.cols.co(X),co.popular)
  save.count(anyfun(X[,co.other]),namefun('other'), dir)
}
numeric.t0 = function(X){
  dir = tex.dir('t0')
  save.distr(X$api.t0,   't0', dir, d=0)
  save.distr(X$api.prev, 'prev', dir, d=0)
  save.distr(X$api.inc,  'inc',  dir, d=0)
  save.count.fct(X$api.prev.cat, 'prev',  dir)
  save.count.fct(X$api.phase,    'phase', dir)
}
numeric.kp = function(X){
  dir = tex.dir('kp')
  for (kp in C$kp){
    save.count(X[[kp]], tailfun(kp), dir)
  }
  # FSW
  save.count(X$kp.FSW.named, 'FSW.named', dir)
  Xn = X[ X$kp.FSW.named,]
  Xu = X[!X$kp.FSW.named,]
  for (crit in M$kp$fsw.crit){
    save.count(Xn[[namefun('kp.FSW.crit',crit)]], namefun('FSW.n.crit',crit), dir)
    save.count(!is.na(Xn[[namefun('kp.FSW.crit',crit)]]), namefun('FSW.n.crit',crit,'v'), dir)
    save.count(is.na(Xn[[namefun('kp.FSW.crit',crit)]]), namefun('FSW.n.crit',crit,'na'), dir)
  }
  save.count.fct(X$kp.FSW.crit,  'FSW.crit',   dir)
  save.count.fct(Xn$kp.FSW.crit, 'FSW.n.crit', dir)
  # save.count.fct(Xu$kp.FSW.crit, 'FSW.u.crit', dir)
  save.count(as.numeric(Xn$kp.FSW.crit) < 3+1, 'FSW.n.crit.fail', dir)
  # clients
  save.count(X$kp.Cli.named, 'Cli.named', dir)
  save.count(X$kp.Cli.named.p, 'Cli.named.p', dir)
  Xn = X[X$kp.Cli.named,]
  Xp = X[X$kp.Cli.named.p,]
  save.count(Xn$kp.FSW.crit.pr, namefun('Cli.n.crit'), dir)
  save.count(Xp$kp.FSW.crit.pr, namefun('Cli.n.p.crit'), dir)
  save.count(!is.na(Xn$kp.FSW.crit.pr), namefun('Cli.n.crit.v'), dir)
  save.count(!is.na(Xp$kp.FSW.crit.pr), namefun('Cli.n.p.crit.v'), dir)
}
numeric.act = function(X){
  dir = tex.dir('act')
  save.distr(X$act.n,   'act.n',   dir, d=0)
  save.distr(X$act.n.z, 'act.n.z', dir, d=0)
  for (col in C$act.def){
    save.count(X[[col]], col,  dir)
  }
  save.distr(100 * X$act.HRW.p, 'hrw.p', dir, d=0, lt=1)
  save.distr(100 * X$act.HRM.p, 'hrm.p', dir, d=0, lt=1)
  save.count(X$act.turn.any, 'turnover.any', dir)
  save.count.fct(X$act.turnover, 'turnover', dir)
  save.count(X$mc, 'mc', dir)
}
numeric.pt = function(X){
  dir = tex.dir('pt')
  save.count.fct(X$act.mix, 'mix', dir)
  for (pt in M$pt$types){
    save.count(X[[namefun('pt',pt)]],        namefun(pt),        dir)
    save.count(X[[namefun('pt',pt,'semi')]], namefun(pt,'semi'), dir)
    save.count(X[[namefun('pt',pt,'any')]],  namefun(pt,'any'),  dir)
  }
  for (def in M$pt$def){
    Xd = X[X$pt.def == def,]
    save.count(Xd$pt.feat.vol,    namefun(def,'vol'),    dir)
    save.count(Xd$pt.feat.condom, namefun(def,'condom'), dir)
    for (pt in M$pt$types){
      save.count(Xd[[namefun('pt',pt)]],        namefun(def,pt),        dir)
      save.count(Xd[[namefun('pt',pt,'semi')]], namefun(def,pt,'semi'), dir)
      save.count(Xd[[namefun('pt',pt,'any')]],  namefun(def,pt,'any'),  dir)
    }
  }
  save.count.fct(X$pt.def, 'pt', dir)
  save.count.fct(X$pt.def.act.drive, 'act.drive', dir)
}
numeric.age = function(X){
  dir = tex.dir('age')
  save.count(X$age.n>1 | X$age.n.cts, 'age.any', dir)
  save.distr(X[X$age.n>1,'age.n'],    'age.n',   dir, d=0)
  save.count(X$age.n>1,     'age.cat', dir)
  save.count(X$age.n.cts,   'age.cts', dir)
  save.count(X$age.risk,    'risk',    dir)
  save.count.fct(X$age.mix, 'mix',     dir)
}
numeric.hiv = function(X){
  dir = tex.dir('hiv')
  save.distr(X$hiv.n, 'hiv.n', dir, d=0)
  save.count(X$hiv.n.cts,   'hiv.cts',   dir)
  save.count(X$hiv.x.acute, 'hiv.acute', dir)
  save.count(X$hiv.x.late,  'hiv.late',  dir)
  save.count(X$hiv.mort,    'hiv.mort',  dir)
  for (col in C$hiv.def){
    save.count(X[[col]], col, dir)
  }
  save.count(X$hiv.morb.any,'hiv.morb.any',dir)
  for (col in C$hiv.morb){
    save.count(X[[col]], col, dir)
  }
}
numeric.art = function(X){
  dir = tex.dir('art')
  save.distr(X$art.rbeta, 'rbeta', dir, d=2)
  save.count.fct(X$art.rbeta.x, 'rbeta.x', dir)
  for (col in c(C$art.x,C$art.r)){
    save.count(X[[col]], col, dir)
  }
  save.count(X$art.r.frop, 'art.r.frop', dir)
  save.count(X$art.drop.any, 'art.drop.any', dir)
  save.count(X$art.fail.any, 'art.fail.any', dir)
  save.count(X$art.tdr,      'tdr',    dir)
  for (col in C$dx.bc){
    save.count(X[[col]], col, dir)
  }
  save.count(X$bc.any, 'bc.any', dir)
  save.count(X$dx.bc.any, 'dx.bc.any', dir)
  save.count(X$art.bc.cond, 'art.bc.cond', dir)
  save.count(X$bc.cond.any, 'bc.cond.any', dir)
}
numeric.cov = function(X){
  dir = tex.dir('cov')
  save.count(X$diff.any.any,  'diff.any.any',  dir)
  save.count(X$diff.any.any.j,'diff.any.any.j',dir)
  for (grp in M$diff$grp){
    col = namefun('diff','any',grp)
    save.count(X[[col]], col, dir)
  }
  for (col in C$diff){
    save.count(X[[col]], col, dir)
    save.count(X[[namefun(col,'H')]],namefun(col,'H'),dir)
  }
}
save.api.count = function(bib,x,name,dir='n'){
  save.count(x,namefun('s',name),dir)
  save.count(length(unique(bib[x])),namefun('a',name),dir)
}
fit.model = function(XA.,vars){
  for (var in vars){ XA.[[var]] = factor(XA.[[var]]) } # HACK: avoid empty levels
  f = formula(paste('value ~',paste(vars,collapse='+')))
  m = geeglm(f,'gaussian',XA.,id=factor(XA.$bib),corstr='i')
  return(m)
}
debug.model = function(model,which,XA.){
  # g = ggpairs(XA.,columns=vars.all,lower=list(discrete='count'),upper=list(discrete='count'))
  # ggsave(g,file=paste0('pairs-',which,'.pdf'),w=16,h=16)
  # print(summary(model[[which]]))
  # print(XA.$bib[abs(dffits(lm(model[[which]])))>2.5])
  print(plot(lm(model[[which]])))
  # print(QIC(model[[which]]))
}
numeric.api = function(XA){
  # counts
  save.api.count(XA$bib,!logical(nrow(XA)),'api')
  save.api.count(XA$bib,XA$api.inc.any,'api.inc')
  save.api.count(XA$bib,XA$api.chi.any,'api.chi')
  save.api.count(XA$bib,XA$api.both.any,'api.both')
  # distributions & stats models
  rm.vars = c('api.phase','art.cov.cat','art.init.cat','art.fail.any','art.drop.any')
  vars.all = setdiff(M$api$table,rm.vars)
  ci.fun = function(coef,var,level,name){
    varlevel = paste0(var,level)
    m.ci = 100 * coef[varlevel,1] + c(0,qnorm(.025),qnorm(.975)) * coef[varlevel,2]
    save.tex(round(m.ci[1],d=0),namefun(var,level,name,'beta'),   dir=dir,na='')
    save.tex(round(m.ci[2],d=0),namefun(var,level,name,'beta.lo'),dir=dir,na='')
    save.tex(round(m.ci[3],d=0),namefun(var,level,name,'beta.hi'),dir=dir,na='')
  }
  distr.funs = c('q2','q1','q3')
  dir = tex.dir('api')
  model = list()
  for (which in c('inc','chi')){
    dir = tex.dir(file.path('api',which))
    XA. = make.api.data(XA,which=which)[c('bib','value',M$api$table)]
    model[[which]] = fit.model(XA.,vars.all)
    # debug.model(model,which,XA.); next
    coef.all = summary(model[[which]])$coef
    for (var in M$api$table){
      x = XA.[[var]]
      for (level in levels(factor(x))){
        varlevel = paste0(var,level)
        coef.vl = coef.all[varlevel,]
        nums = c(sum(x==level,na.rm=TRUE),
          100 * quantile(XA.[['value']][x==level],c(.5,.25,.75),na.rm=TRUE),
          100 * (coef.vl[1,1] + c(0,qnorm(.025),+qnorm(.975)) * coef.vl[1,2]))
        pstr = print.p(coef.vl[1,4]) # NOTE: removed
        strfun = function(fmt,...){ do.call(sprintf,c(list(fmt),...)) }
        if (!is.na(nums[5]))  {
          tabstr = strfun('%.0f & %.0f & ( %.0f , %.0f ) & %.0f & ( %.0f , %.0f ) ',nums)
        } else if (nums[1]==0 | !var %in% vars.all){
          tabstr = strfun('%.0f & %.0f & ( %.0f , %.0f ) & & ',nums[1:4])
        } else {
          tabstr = strfun('%.0f & %.0f & ( %.0f , %.0f ) & \\textsc{ref} & ',nums[1:4])
        }
        save.tex(tabstr,namefun(var,level,'xtab'),dir=dir)
      }
    }
  }
  names(model) = c('IR','CIA')
  g = plot.effects(model,group='Outcome')
  save.plot(g,'effects',dir='api',w=6,h=10)
}
save.distr = function(x,name,dir='',d=2,funs=NULL,lt=NULL){
  fun.list = list(
    mu = function(x){ mean(x) },
    sd = function(x){ sd(x) },
    q0 = function(x){ quantile(x,  0) },
    q1 = function(x){ quantile(x,.25) },
    q2 = function(x){ quantile(x,.50) },
    q3 = function(x){ quantile(x,.75) },
    q4 = function(x){ quantile(x,  1) }
  )
  if (is.null(funs)) { funs = names(fun.list) }
  ltfun = ifelse(is.null(lt),identity,function(x){
    return( ifelse(x < lt, paste('$<$',lt), x) )
  })
  x = x[!is.na(x)]
  for (f in funs){
    fun = fun.list[[f]]
    save.tex(ltfun(round(fun(x),d=d)),namefun(name,f), dir)
  }
}
tex.dir = function(dir){
  dir.create(path.tex(dir), showWarnings=FALSE)
  return(dir)
}