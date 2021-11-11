plot.api.list = function(XA,...){
  args = c(...)
  if (length(args)){ plot.vars = args } else { plot.vars = P$api }
  for (which in c('chi','inc')){
    XA. = make.api.data(XA,which=which)
    for (var in plot.vars){
      plot.api(XA.,which=which,geom='box',color=var,fill=var) %>%
        save.plot(namefun(which,var),dir='api',width=5,height=4)
      plot.api(XA.,which=which,geom='point',color=var) %>%
        save.plot(namefun(which,'s',var),dir='api',width=5,height=4)
    }
  }
}

plot.api = function(XAi,which='chi',geom='box',...){
  ylabs = list(
    'chi' = 'Cumulative HIV Infections Averted',
    'inc' = 'Reduction in HIV Incidence'
  )
  args = list('t.cat',...)
  clr.args = list(option='inferno',begin=.1,end=.75,discrete=TRUE,drop=TRUE)
  g = XAi %>% rename.lvls(args) %>%
      ggplot(aes_string(x=ifelse(geom=='box','t.cat','t'),y='value',...)) +
      geom_hline(yintercept=0,color='gray') +
      do.call(scale_color_viridis,clr.args) +
      do.call(scale_fill_viridis,clr.args) +
      labs(y=ylabs[[which]],x='Time since Roll-Out (years)') +
      ylim(-.15, 1.05) +
      theme_light() +
      theme(legend.title=element_blank(),legend.margin=margin(0,0,0,0))
  if (geom=='box'){
    pos = position_dodge2(width=.8,preserve='single')
    g = g + geom_boxplot(alpha=.4,position=pos) +
      stat_summary(geom='text',show.legend=FALSE,size=2.5,position=pos,
        fun.data=function(y){ data.frame(y=1.03,label=length(y)) })
  }
  if (geom=='point'){
    g = g + geom_point(alpha=.6,position='jitter',stroke=0,size=2) + xlim(-1,41)
  }
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
  clr.args = list(option='inferno',discrete=TRUE,begin=.1,end=.85,na.value='gray')
  g = X %>% rename.lvls(var) %>%
    ggplot(aes_string(x=var,color=var,fill=var)) +
    do.call(scale_color_viridis,clr.args) +
    do.call(scale_fill_viridis,clr.args) +
    ylab('Studies') + xlab(detex(D[[decat(var)]])) +
    guides(color=FALSE,fill=FALSE) +
    theme_light()
  if (is.numeric(X[[var]])){
    g = g + geom_histogram(bins=16,alpha=.4,color='#BB3754',fill='#BB3754')
  } else {
    g = g + geom_bar(alpha=.4)
  }
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