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
  clr.args = list(option='inferno',end=.85)
  if (!is.numeric(clr)){ clr.args = c(clr.args,list(discrete=TRUE,drop=drop)) }
  g = ggplot(XAi,aes_string(x='t',y='value',...,size=3)) +
      geom_hline(yintercept=0,color='gray') +
      geom_point(alpha=.6,position='jitter') +
      ylim(-.15,  1) + ylab(ylabs[[which]]) +
      xlim(   0, 40) + xlab('Time since Roll-Out (years)') +
      scale_size(range=size.lims) +
      guides(size=FALSE) +
      do.call(scale_color_viridis,clr.args) +
      theme_light()
  return(g)
}

plot.map.co = function(X,fill='PLHIV',size='Count'){
  colnames(X) <- gsub('co.','',colnames(X))
  Xc = load.co.polygons()
  Xc[[size]] = sapply(Xc$name,
    function(co){ ifelse(co %in% colnames(X), sum(X[[co]],na.rm=TRUE), NA) })
  g = ggplot(Xc) +
      geom_sf(aes_string(fill=fill),
        alpha = 0.8, color = 'gray') +
      geom_segment(aes(x=X,y=Y,xend=X0,yend=Y0),alpha=.5,size=.2) +
      geom_point(aes_string(x='X',y='Y',size=size),
        alpha = 0.6, color = 'red') +
      scale_fill_viridis(option='inferno',end=.85) +
      scale_size(range=c(-.5,15)) +
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
  x = X[[var]]
  if (is.numeric(x)){
    geom = geom_histogram
    args = list(bins=16,alpha=.4,color='red',fill='red')
  } else {
    geom = geom_bar
    args = list(alpha=.4)
  }
  clr.args = list(option='inferno',discrete=TRUE,end=.85,na.value='gray')
  g = ggplot(X,aes_string(x=var,color=var,fill=var)) +
    do.call(geom,args) +
    do.call(scale_color_viridis,clr.args) +
    do.call(scale_fill_viridis,clr.args) +
    ylab('Count') + xlab(detex(D[[decat(var)]])) +
    guides(color=FALSE,fill=FALSE) +
    theme_light()
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

save.plot.lists = function(){
  dir = tex.dir('config')
  for (name in names(P)){
    plot.list.tex = paste(P[[name]],D[ decat(P[[name]]) ],sep='/',collapse=',')
    save.tex(plot.list.tex,namefun('plot','list',name),dir=dir)
  }
}