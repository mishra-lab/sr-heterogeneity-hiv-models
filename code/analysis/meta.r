namefun = function(...){
  return(gsub('/','-',gsub(' ','-',paste(...,sep='.'))))
}
tailfun = function(name){
  return(gsub('(.*\\.)*','',name))
}
name.iter = function(iter,...){
  return(sapply(iter,function(i){ namefun(...,i) }))
}
res.map = list(na=NA,false=FALSE,true=TRUE)
M = list(# groups of misc
  pop = list(
    All  = c('All'),
    pretty = list(
      High.Risk = 'HR',
      Low.Risk  = 'LR',
      Women     = 'Wom',
      Men       = 'Men',
      Yng.All   = 'YA',
      Yng.Wom   = 'YW',
      Yng.Men   = 'YM',
      FSW       = 'FSW',
      Clients   = 'Cli',
      MSM       = 'MSM',
      PWID      = 'PWID',
      All       = 'All'
    )
  ),
  kp = list(
    main = c('FSW','Cli','MSM','PWID'), # AGYW, MP
    fsw.crit = c('p','pr','cr')
  ),
  pt = list(
    def   = c('gen','grp','phen'),
    feat  = c('vol','condom'),
    types = c('Generic','Main','Casual','SW','Transact')
  ),
  geo = list(
    all = c(
      'ssa', 'sub.ssa', 'sub.ssa.multi',
      'nat.multi', 'nat', 'sub.nat.multi', 'sub.nat',
      'city.multi', 'city', 'sub.city', 'sub.city.multi'
    ),
    sub.ssa = c('sub.ssa.multi', 'sub.ssa'),
    nat     = c('nat.multi', 'nat'),
    sub.nat = c('sub.nat.multi','sub.nat'),
    city    = c('city.multi','city', 'sub.city','sub.city.multi')
  ),
  bc = list(
    dx  = c('ss','vol','np','cond','gen')
  ),
  hiv = list(
    def  = c('cd4','vl','t'),
    morb = c('inact','np','vol','beta'),
    x    = c('acute','late')
  ),
  act = list(
    def = c('sex','np','pt')
  ),
  art = list(
    x = c('dx','vlus','drop','fail'),
    r = c('drop','fail')
  ),
  diff = list(
    grp = c('sex','act','kp','age'),
    step = c('dx','art.i','art.o')
  ),
  api = list(
    t     = c(5,10,15,20,30,40),
    hiv   = c('prev','prev.d1','prev.d2','inc','inc.d1','inc.d2'),
    table = c(
      't.cat','api.prev.cat',
      'art.rbeta.cat','art.cd4','art.cov.cat','art.init.cat',
      'hiv.x.acute','hiv.x.late','hiv.morb.any',
      'art.tdr','art.fail.any','art.drop.any','bc.any',
      'act.def.sex','act.kp','Risk','act.turn.any','pt.def',
      'diff.any.kp.cat'
    )
  )
)
C = list(# groups of columns
  act.def  = name.iter(M$act$def,    'act','def'),
  pt.def   = name.iter(M$pt$def,     'pt','def'),
  pt.feat  = name.iter(M$pt$feat,    'pt','feat'),
  pt.types = name.iter(M$pt$types,   'pt'),
  kp       = name.iter(M$kp$main,    'kp'),
  kp.named = name.iter(M$kp$named,   'kp'),
  fsw.crit = name.iter(M$kp$fsw.crit,'kp','FSW','crit'),
  dx.bc    = name.iter(M$bc$dx,      'dx','bc'),
  hiv.def  = name.iter(M$hiv$def,    'hiv','def'),
  hiv.morb = name.iter(M$hiv$morb,   'hiv','morb'),
  hiv.x    = name.iter(M$hiv$x,      'hiv','x'),
  art.x    = name.iter(M$art$x,      'art','x'),
  art.r    = name.iter(M$art$r,      'art','r'),
  api.hiv  = name.iter(M$api$hiv,    'api'),
  api.inc  = name.iter(M$api$t,      'api','inc','red'),
  api.chi  = name.iter(M$api$t,      'api','chi','red'),
  diff.dx    = name.iter(M$diff$grp, 'diff','dx'),
  diff.art.i = name.iter(M$diff$grp, 'diff','art.i'),
  diff.art.o = name.iter(M$diff$grp, 'diff','art.o'),
  diff.sex   = c('diff.dx.sex','diff.art.i.sex','diff.art.o.sex'),
  diff.act   = c('diff.dx.act','diff.art.i.act','diff.art.o.act'),
  diff.kp    = c('diff.dx.kp', 'diff.art.i.kp', 'diff.art.o.kp'),
  diff.age   = c('diff.dx.age','diff.art.i.age','diff.art.o.age')
) # HACK
C$diff = c(C$diff.dx,C$diff.art.i,C$diff.art.o)
P = list(# stuff to plot
  api = c(
    'api.prev','api.inc','api.phase',
    'art.rbeta','art.cd4','art.cov','art.init',
    'hiv.x.acute','hiv.x.late','hiv.morb.any','art.tdr','art.fail.any','art.drop.any','bc.any',
    'act.def.sex','act.kp','Risk','act.n','act.mix','act.turn.any','pt.def','age.n','age.mix',
    'act.HRW.p','act.HRM.p','act.HRW.cr','act.HR.pr'
  ),
  dist = c('api.prev','api.inc','art.rbeta','act.n','age.n',
           'act.HRW.p','act.HRM.p','act.HRW.cr','act.HR.pr',
           'api.prev.cat','api.phase','Risk','pt.def')
  # TODO: api.inc, api.chi from XA
)
D = list(# definitions
  api.prev     = 'HIV prevalence at $t_0$ (\\%)',
  api.inc      = 'HIV incidence at $t_0$ (per 1000 PY)',
  api.phase    = 'HIV epidemic phase',
  
  art.rbeta    = 'relative infectiousness on ART',
  art.cd4      = 'CD4 initiation criteria (less than shown count)',
  art.cov      = 'ART coverage target',
  art.init     = 'ART initiation rate (per PY)',
  
  hiv.x.acute  = 'increased infectiousness during acute infection',
  hiv.x.late   = 'increased infectiousness during late-stage infection',
  hiv.morb.any = 'decreased sexual activity during late-stage infection',
  art.tdr      = 'any transmitted drug resistance',
  art.fail.any = 'any rate or state of ART failure',
  art.drop.any = 'any rate or state of ART dropout',
  bc.any       = 'any behaviour change associated with diagnosis or ART',
  
  act.def.sex  = 'stratified by sex',
  act.n        = 'number of activity groups',
  act.mix      = 'type of activity mixing',
  act.turn.any = 'any activity group turnover',
  pt.def       = 'type of partnership definition',
  age.n        = 'number of age groups',
  age.mix      = 'type of age mixing',
  
  act.kp       = 'activity groups \\& key populations',
  Risk         = 'summary of risk heterogeneity',
  act.HRW.p    = 'proportion of women in the highest female activity group',
  act.HRM.p    = 'proportion of men in the client or highest activity male group',
  act.HRW.cr   = 'ratio of partners per year in the highest vs lowest female activity groups',
  act.HR.pr    = 'ratio of highest female to highest male activity group sizes'
)