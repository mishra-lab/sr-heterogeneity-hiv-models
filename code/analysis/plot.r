plot.api.list = function(XA,drop=FALSE,...){
  args = c(...)
  if (length(args)){ plot.vars = args } else { plot.vars = P$api }
  for (which in c('chi','inc')){
    XA. = make.api.data(XA,which=which)
    for (var in plot.vars){
      XA. %>% make.bib.wt %>%
        plot.api(which=which,drop=drop,color=var,size='wt') %>%
        save.plot(namefun(which,'s',var),dir='api',width=5,height=4)
      XA. %>% agg.api.data(fun=median,var) %>%
        plot.api(which=which,drop=drop,color=var) %>%
        save.plot(namefun(which,'a',var),dir='api',width=5,height=4)
    }
  }
}

plot.api = function(XAi,which='chi',drop=FALSE,...){
  ylabs = list(
    'chi' = 'Cumulative HIV Infections Averted',
    'inc' = 'Reduction in HIV Incidence'
  )
  args = list(...)
  clr = XAi[[ifelse(is.null(args$color),NA,args$color)]]
  s   = XAi[[ifelse(is.null(args$size), NA,args$size )]]
  size.lims = 4*c(ifelse(length(s),min(s),1),ifelse(length(s),max(s),1))
  clr.args = list(option='inferno',begin=.1,end=.75)
  if (!is.numeric(clr)){ clr.args = c(clr.args,list(discrete=TRUE,drop=drop)) }
  g = XAi %>% rename.lvls(args) %>%
      ggplot(aes_string(x='t',y='value',...,size=3)) +
      geom_hline(yintercept=0,color='gray') +
      geom_point(alpha=.6,position='jitter') +
      ylim(-.15,  1) + ylab(ylabs[[which]]) +
      xlim(  -1, 41) + xlab('Time since Roll-Out (years)') +
      scale_size(range=size.lims) +
      guides(size=FALSE) +
      do.call(scale_color_viridis,clr.args) +
      theme_light() +
      theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0))
  return(g)
}

plot.map.co = function(X,fill='PLHIV',size='Studies'){
  colnames(X) <- gsub('co.','',colnames(X))
  Xc = load.co.polygons()
  Xc[[size]] = sapply(Xc$name,
    function(co){ ifelse(co %in% colnames(X), sum(X[[co]],na.rm=TRUE), NA) })
  g = ggplot(Xc) +
      geom_sf(aes_string(fill=fill),
        alpha=0.8,color='gray',size=.3) +
      geom_segment(aes(x=X,y=Y,xend=X0,yend=Y0),alpha=.5,size=.2) +
      geom_point(aes_string(x='X',y='Y',size=size),
        alpha=0.6,color='turquoise',stroke=0) +
      scale_fill_viridis(option='inferno',begin=.1,end=.9) +
      scale_size(range=c(-.5,15),breaks=c(1,2,5,10,20,50)) +
      xlab(NULL) + ylab(NULL) +
      theme_light()
  return(g)
}

plot.distr.list = function(X){
  for (var in P$dist){
    X %>% plot.distr(var) %>%
      save.plot(namefun('d',var),dir='dist',width=5,height=4)
  }
}

plot.distr = function(X,var){
  if (is.numeric(X[[var]])){
    geom = geom_histogram
    args = list(bins=16,alpha=.4,color='red',fill='red')
  } else {
    geom = geom_bar
    args = list(alpha=.4)
  }
  clr.args = list(option='inferno',discrete=TRUE,begin=.1,end=.85,na.value='gray')
  g = X %>% rename.lvls(var) %>%
    ggplot(aes_string(x=var,color=var,fill=var)) +
    do.call(geom,args) +
    do.call(scale_color_viridis,clr.args) +
    do.call(scale_fill_viridis,clr.args) +
    ylab('Studies') + xlab(detex(D[[decat(var)]])) +
    guides(color=FALSE,fill=FALSE) +
    theme_light()
  return(g)
}

gen.effects = function(model,name=NULL,group=''){
  coef = summary(model)$coef
  coef$low  = coef$Estimate - 1.96 * coef$Std.err
  coef$high = coef$Estimate + 1.96 * coef$Std.err
  dc = dummy.coef(model)
  E = do.call(rbind,lapply(names(dc),function(var){
    lvls = names(dc[[var]])
    varlvls = paste0(var,lvls)
    if (var=='(Intercept)'){ varlvls = var; i = TRUE; } else { i = FALSE; }
    E.var = coef[varlvls,c('Estimate','low','high')]
    lvl.ref = paste0('\n    REF = ',rename.lvl(lvls[is.na(E.var$Estimate)],var))
    E.var$var = paste0(rename.lvl(var,'vars'),ifelse(i,'',lvl.ref))
    E.var$lvl = rename.lvl(lvls,var)
    return(E.var)
  }))
  E[[group]] = name
  E = E[!is.na(E$Estimate),]
  return(E)
}

plot.effects = function(model,group='.'){
  if ('coefficients' %in% names(model)){
    E = gen.effects(model,name='',group=group)
  } else {
    E = do.call(rbind,lapply(names(model),function(name){
      gen.effects(model[[name]],name=name,group=group) }))
  }
  pos = position_dodge(width=.6)
  g = ggplot(E,aes_string(x='Estimate',y='lvl',xmin='low',xmax='high',color=group)) +
    geom_point(position=pos) +
    geom_linerange(position=pos) +
    scale_color_viridis(option='inferno',discrete=TRUE,begin=.3,end=.7) +
    facet_grid(rows=vars(E$var),scales='free_y',space='free_y',switch='y') +
    labs(x='Effect',y='Factor') +
    theme_light() +
    theme(legend.position='top',
      panel.spacing = unit(.4,'lines'),
      strip.placement='outside',
      strip.background=element_blank(),
      strip.text.y.left=element_text(color='grey30',angle=0,vjust=1,hjust=0))
  return(g)
}

detex = function(name){
  subs = list('~'=' ','\\$'='','\\\\'='','\\_'='')
  for (s in names(subs)){
    name = gsub(s,subs[[s]],name)
  }
  return(name)
}

decat = function(name){
  return(gsub('\\.cat','',name))
}

rename.lvl = function(x,name){
  rmap = R[[name]]
  if (!is.null(rmap)){
    return(sapply(x,function(xi){ names(rmap)[xi==rmap] }))
  } else {
    return(x)
  }
}

rename.lvls = function(X,...){
  names = c(...)
  for (name in names){
    if (!is.null(R[[name]])){
      X[[name]] = as.factor(X[[name]])
      levels(X[[name]]) <- R[[name]]
    }
  }
  return(X)
}

save.plot.lists = function(){
  dir = tex.dir('config')
  for (name in names(P)){
    plot.list.tex = paste(P[[name]],D[ decat(P[[name]]) ],sep='/',collapse=',')
    save.tex(plot.list.tex,namefun('plot','list',name),dir=dir)
  }
}