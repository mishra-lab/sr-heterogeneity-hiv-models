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
      Transgen. = 'TG',
      PWID      = 'PWID',
      Prisoners = 'pris',
      All       = 'All'
    )
  ),
  kp = list(
    main = c('FSW','Cli','MSM','TG','PWID','pris'), # AGYW, MP
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
      't.cat','api.prev.cat','api.phase',
      'art.rbeta.cat','art.cd4','art.cov.cat','art.init.cat',
      'hiv.x.acute','hiv.x.late','hiv.morb.any',
      'art.tdr','art.fail.any','art.drop.any','bc.any',
      'Sex','Risk','act.turn.any','pt.def'
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
    'api.prev.cat','api.phase',
    'art.rbeta.cat','art.cd4','art.cov.cat','art.init.cat',
    'hiv.x.acute','hiv.x.late','hiv.morb.any','art.tdr','art.fail.any','art.drop.any','bc.any',
    'act.def.sex','Sex','Risk','act.n.cat','act.mix','act.turn.any','pt.def','age.n.cat','age.mix'
    # 'act.HRW.p','act.HRM.p','act.HRW.cr','act.HR.pr'
  ),
  dist = c('api.prev','api.inc','art.rbeta','act.n','age.n','hiv.n',
           'act.HRW.p','act.HRM.p','act.HRW.cr','act.HR.pr',
           'api.prev.cat','api.phase','Risk','pt.def'),
  # TODO: api.inc, api.chi from XA
  eff.sub = c('Sex','Risk','act.turn.any','pt.def')
)
D = list(# definitions
  api.prev     = 'HIV prevalence at $t_0$ (\\%)',
  api.inc      = 'HIV incidence at $t_0$ (per 1000 PY)',
  api.phase    = 'HIV epidemic phase',

  art.rbeta    = 'Relative infectiousness on ART',
  art.cd4      = 'CD4 initiation criteria',
  art.cov      = 'ART coverage target',
  art.init     = 'ART initiation rate (per PY)',

  hiv.n        = 'Number of HIV states',
  hiv.x.acute  = 'Increased infectiousness during acute infection',
  hiv.x.late   = 'Increased infectiousness during late-stage infection',
  hiv.morb.any = 'Decreased sexual activity during late-stage infection',
  art.tdr      = 'Any transmitted drug resistance',
  art.fail.any = 'Any rate or state of ART failure',
  art.drop.any = 'Any rate or state of ART dropout',
  bc.any       = 'Any behaviour change associated with diagnosis or ART',

  act.def.sex  = 'Stratified by sex',
  act.n        = 'Number of activity groups',
  act.mix      = 'Type of activity mixing',
  act.turn.any = 'Any activity group turnover',
  pt.def       = 'Type of partnership definition',
  age.n        = 'Number of age groups',
  age.mix      = 'Type of age mixing',

  act.kp       = 'Activity groups \\& key populations',
  Risk         = 'Risk Stratification \\& ART cascade differences',
  Sex          = 'Sex stratification \\& any ART cascade differences',
  act.HRW.p    = 'Proportion of women in the highest female activity group',
  act.HRM.p    = 'Proportion of men in the client or highest activity male group',
  act.HRW.cr   = 'Ratio of partners per year in the highest vs lowest female activity groups',
  act.HR.pr    = 'Ratio of highest female to highest male activity group sizes'
)
R = list(# Rename stuff
  vars = list(
    '(Intercept)'='(Intercept)',
    'Time Horizon (years)' = 't.cat',
    'HIV Prevalence' = 'api.prev.cat',
    'HIV Incidence Trend' = 'api.phase',
    'Partnership Types' = 'pt.def',
    'CD4 Threshold for ART' = 'art.cd4',
    'RR Trans. on ART' = 'art.rbeta.cat',
    'Activity Mixing' = 'act.mix',
    'Age Mixing' = 'age.mix',
    'Acivity & KP' = 'act.kp',
    'Acute HIV' = 'hiv.x.acute',
    'Late Stage HIV' = 'hiv.x.late',
    'HIV Morbidity' = 'hiv.morb.any',
    'Trans. Drug Resist.' = 'art.tdr',
    'Any ART Failure' = 'art.fail.any',
    'Any ART Dropout' = 'art.drop.any',
    'HTC Behav. Change' = 'bc.any',
    'Sex Stratification' = 'act.def.sex',
    'Any ART Diff. by Sex' = 'diff.any.sex',
    'Sex Stratif. & Cascade Diff.' = 'Sex',
    'Activity Turnover' = 'act.turn.any',
    'Risk Stratif. & Cascade Diff.' = 'Risk'),
  t.cat = list(
    '0-10'='0',
    '11-20'='10',
    '21-30'='20',
    '31+'='30'),
  api.prev.cat = list(
    'Low (<1%)'='Low',
    'Middle (1-10%)'='Mid',
    'High (>10%)'='High'),
  api.phase = list(
    'Decr'='decr',
    'Decr to Stable'='dts',
    'Stable'='stab',
    'Incr to Stable'='its',
    'Incr'='incr'),
  pt.def = list(
    'Generic'='gen',
    'By Group'='grp',
    'Overlapping'='phen'),
  art.cd4 = list(
    'Varies'='*',
    '200'='200',
    '350'='350',
    '500'='500',
    'Any'='All',
    'Symptomatic'='symp'),
  art.cov.cat = list(
    '0.0-0.59'='0',
    '0.60-0.84'='0.6',
    '0.85+'='0.85'),
  art.rbeta.cat = list(
    '0.0-0.039'='0',
    '0.04-0.099'='0.04',
    '0.1+'='0.1'),
  act.mix = list(
    'Proportionate'='prop',
    'Assortative'='asso'),
  age.mix = list(
    'Proportionate'='prop',
    'Sex Differences'='offd',
    'Assortative'='asso'),
  act.kp = list(
    'None'='none',
    'Acivity (No KP)'='some.no.kp',
    'Any KP'='any.kp'),
  hiv.x.acute  = list('No'=FALSE,'Yes'=TRUE),
  hiv.x.late   = list('No'=FALSE,'Yes'=TRUE),
  hiv.morb.any = list('No'=FALSE,'Yes'=TRUE),
  art.tdr      = list('No'=FALSE,'Yes'=TRUE),
  art.fail.any = list('No'=FALSE,'Yes'=TRUE),
  art.drop.any = list('No'=FALSE,'Yes'=TRUE),
  bc.any       = list('No'=FALSE,'Yes'=TRUE),
  act.def.sex  = list('No'=FALSE,'Yes'=TRUE),
  act.turn.any = list('No'=FALSE,'Yes'=TRUE)
)