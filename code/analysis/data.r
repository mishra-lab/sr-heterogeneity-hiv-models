
M = list(
  pop = list(
    main = c('All',
             'Women','Men',
             'FSW','Clients','MSM',#'PWID',
             'Low.Risk','High.Risk',
             'Yng.All','Yng.Wom','Yng.Men'),
    Any  = c('All',
             'Wom','Men','YA','YW','YM',
             'LR','LRW','LRM','HR','HRW','HRM',
             'FSW','Cli','MSM','PWID'),
    Pri  = c('Wom','Men','YA','YW','YM',
             'LR','LRW','LRM','HR','HRW','HRM',
             'FSW','Cli','MSM','PWID'),
    All  = c('All'),
    High.Risk = c('HR','HRW','HRM'),
    Low.Risk  = c('LR','LRW','LRM'),
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
  int = list(
    main = c('ARTCD4','ARTUTT','VMMC','PrEP','Condoms','Partners','STI'),
    Any  = c('ARTCD4','ARTUTT','VMMC','PrEP','Condoms','Partners','STI',
             'Generic','Vaccine','Cure','Cash','Mix','Other'),
    All  = c('All'),
    BHi  = c('Generic','ART','VMMC','Condoms','Partners','STI'),
    ART  = c('ARTCD4','ARTUTT')
  ),
  kp = list(
    main = c('FSW','Cli','MSM','PWID') # AGYW
  ),
  pt = list(
    main = c('Generic','Main','Casual','SW','Transact')
  ),
  geo = list(
    all = c(
      'ssa',
      'national.multi',
      'national',
      'sub.national.multi',
      'sub.national',
      'city.multi',
      'city'
    ),
    national     = c('national.multi','national'),
    sub.national = c('sub.national.multi','sub.national'),
    city         = c('city.multi','city')
  )
)

load.sr.data = function(){
  X = readxl::read_excel(file.path(Sys.getenv('ROOT'),'data','main.xlsx'))
  X = X[boolfun(X$stage,1),]
  # Geographic Scale
  X$geo.scale = factor(X$geo.scale,levels=M$geo$all)
  # as.numeric
  numeric.cols = c('age.n','hiv.n','RG.n')
  for (col in numeric.cols){
    X[[col]] = as.numeric(X[[col]])
  }
  # booleanize everything!
  bool.cols = c(
    get.cols.co(X),
    get.cols.BHi(X,all=TRUE),
    get.RQi.cols(X),
    'RG.def.sex','RG.def.np','RG.def.pt',
    'BHix.combo','RQix.combo','BHix.gaps','RQix.gaps',
    'age.risk',
    'hiv.acute',
    'mc'
  )
  for (col in bool.cols){
    X[[col]] = boolfun(X[[col]])
  }
  X$RG.turn.bal = boolfun(X$RG.turnover,true='B')
  X$RG.turnover = boolfun(X$RG.turnover,true='Y')
  for (kp in M$kp$main){
    X = make.semi(X,namefun('RG','KP',kp))
  }
  for (pt in M$pt$main){
    X = make.semi(X,namefun('PT',pt))
  }
  # collect some stuff
  X = make.RQi.int.map(X,pop='Pri')
  X = make.RQi.int.map(X,pop='Any')
  X = make.RQi.int.map(X,pop='All')
  X = make.RQi.pop.map(X,int='Any')
  X = make.RQi.pop.map(X,int='All')
  X = make.RG.no.sex(X)
  X = make.risk(X)
  X = make.RQi.int.risk(X)
  X$RG.KP.Any = anyfun(X[,get.cols.kp(X)])
  return(X)
}

namefun = function(...){
  return(gsub(' ','-',paste(...,sep='.')))
}

boolfun = function(X,true='Y'){
  return(!is.na(X) & (X %in% true))
}

anyfun = function(X){
  return(apply(X,1,any))
}

get.cols.co = function(X){
  return(names(X)[grepl('^co\\..+',names(X))])
}

get.cols.kp = function(X){
  return(names(X)[grepl('^RG\\.KP\\..+',names(X))])
}

get.cols.BHi = function(X,all=FALSE){
  if (all){
    return(names(X)[grepl('^BHi\\..+',names(X))])
  } else {
    return(sapply(M$int$BHi,function(int){ namefun('BHi',int) }))
  }
}

make.semi = function(X,col){
  X[[namefun(col,'semi')]] = boolfun(X[[col]],true='x')
  X[[col]]                 = boolfun(X[[col]],true='Y')
  return(X)
}

make.risk = function(X){
  X$risk = (X$RG.n > 2) | (X$RG.n > 1 & !X$RG.def.sex) | (X$age.risk)
  return(X)
}

make.RG.no.sex = function(X){
  X$RG.z.n = mapply(function(n,sex){ifelse(sex,n/2,n)}, as.numeric(X$RG.n), X$RG.def.sex)
  return(X)
}

make.RQi.int.risk = function(X){
  # WARNING: this overwrites RQi.***.HR/LR cols
  for (xr in c('HR','LR')){
    for (int in M$int$main){
      cols = sapply(c('','W','M'),function(i){ namefun('RQi',int,paste0(xr,i)) } )
      cols = cols[cols %in% names(X)]
      if (length(cols)){
        X[[namefun('RQi',int,xr)]] = apply(X[cols],1,any)
      }
    }
  }
  return(X)
}

get.RQi.cols = function(X,ints=NULL,pops=NULL){
  if (is.null(ints)){ ints = M$int$Any }
  if (is.null(pops)){ pops = M$pop$Any }
  cols = sapply(ints,function(int){
    sapply(pops,function(pop){
      namefun('RQi',int,pop)
    })
  })
  return(cols[cols %in% names(X)])
}

make.RQi.int.map = function(X,pop='Any'){
  pops = M$pop[[pop]]
  for (int in M$int$Any){
    name = namefun('RQi',int,pop)
    X[[name]] = anyfun(X[,get.RQi.cols(X,int,pops)])
  }
  return(X)
}

make.RQi.pop.map = function(X,int='Any'){
  ints = M$int[[int]]
  for (pop in names(M$pop)){
    name = namefun('RQi',int,pop)
    X[[name]] = anyfun(X[,get.RQi.cols(X,ints,pop)])
  }
  return(X)
}

get.BHi.ints = function(X){
  Xi = X[,get.cols.BHi(X)]
  Xi = rename_all(Xi,function(i){
    str_replace(i,'BHi.','')
  })
  return(Xi)
}

get.RQi.ints = function(X){
  Xi = X[,sapply(M$int$main,function(int){namefun('RQi',int,'Any')})]
  Xi = rename_all(Xi,function(i){
    str_replace(str_replace(i,'RQi.',''),'.Any','')})
  return(Xi)
}

load.co.polygons = function(region='ssa'){
  C = load.co.data(region=region)
  X = ne_countries(
    country=C$admin,
    scale='medium',
    returnclass='sf')
  X = X[match(C$admin,X$admin),]
  Ci = suppressWarnings({ st_coordinates(st_centroid(X)) })
  rownames(Ci) = X$name
  C0 = Ci;
  colnames(Ci) = c('X0','Y0')
  scooch = list(
    Lesotho   = list(x=+5,y=-5),
    Swaziland = list(x=+5,y=-2)
  )
  for (co in names(scooch)){
    C0[co,'X'] = C0[co,'X'] + scooch[[co]]$x
    C0[co,'Y'] = C0[co,'Y'] + scooch[[co]]$y
  }
  X = cbind(X,Ci,C0,
    Incid = C$inc,
    Prev  = C$prev,
    PLHIV = C$plhiv/1e6
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