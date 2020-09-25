plot.map.co = function(X,fill='PLHIV',size='Count'){
  colnames(X) <- gsub('co.','',colnames(X))
  Xc = load.co.polygons()
  Xc[[size]] = sapply(Xc$name,
    function(co){ ifelse(co %in% colnames(X), sum(X[[co]],na.rm=TRUE), 0) })
  g = ggplot(Xc) +
      geom_sf(aes_string(fill=fill),
        alpha = 0.8, color = 'gray') +
      geom_segment(aes(x=X,y=Y,xend=X0,yend=Y0),alpha=.5,size=.2) +
      geom_point(aes_string(x='X',y='Y',size=size),
        alpha = 0.5) +
      viridis::scale_fill_viridis(option='viridis') +
      scale_radius(range=c(-.5,15)) +
      xlab(NULL) + ylab(NULL) +
      theme_light()
  
  return(g)
}

plot.risk.cascade = function(X){
  comp  = X$comp.or.ind == 'comp'
  aall  = comp & (X$RQi.Any.All & (X$RQi.ARTCD4.Any | X$RQi.ARTUTT.Any))
  risk  = aall & (X$risk)
  kpop  = risk & (X$RG.KP.Any)
  cgaps = risk & (X$RQix.gaps)
  ART.by = factor(X$RQi.ARTCD4.Any + 2*X$RQi.ARTUTT.Any,
    levels = c(0,1,3,2),
    labels = c('None','CD4','Both','UTT')
  )
  g = data.frame(aall,risk,kpop,cgaps,ART.by) %>%
    pivot_longer(-c('ART.by'),names_to='char',values_to='.') %>%
    filter(as.logical(.)) %>%
    mutate(char=fct_relevel(char,'aall','risk','kpop','cgaps')) %>%
    mutate(char=fct_recode(char,
      'ART for All Risk' = 'aall',
      'Any Risk Strata'  = 'risk',
      'Any Key Pops'     = 'kpop',
      'Coverage Gaps'    = 'cgaps',
    )) %>%
    ggplot(aes(x=char,fill=ART.by)) + geom_bar(alpha=.8) +
    ylab('Count') + xlab('Model Characteristic') +
    scale_y_continuous(sec.axis = sec_axis(~100*./sum(aall),name='%')) +
    viridis::scale_fill_viridis(option='viridis',discrete=TRUE) +
    theme_light() +
    theme(legend.position=c(.99,.99),legend.justification=c(1,1))
  return(g)
}

plot.ints.cooc.hh = function(X){
  plot.ints.cooc(
    get.BHi.ints(X),
    get.BHi.ints(X)
  ) + xlab('Historical') + ylab('Historical')
}

plot.ints.cooc.hn = function(X){
  plot.ints.cooc(
    get.BHi.ints(X[X$BHix.combo,]),
    get.RQi.ints(X[X$BHix.combo,])
  ) + xlab('Historical') + ylab('Counterfactual')
}

plot.ints.cooc.nn = function(X){
  plot.ints.cooc(
    get.RQi.ints(X[X$RQix.combo,]),
    get.RQi.ints(X[X$RQix.combo,])
  ) + xlab('Counterfactual') + ylab('Counterfactual')
}

plot.ints.cooc = function(X,Y){
  XY = t(as.matrix(sapply(X,as.numeric))) %*% as.matrix(sapply(Y,as.numeric))
  XY[XY==0] = NA
  g = plot.tile(XY)
  return(g)
}

plot.tile = function(XY){
  g = XY %>%
    as.data.frame() %>%
    cbind(nx=row.names(XY), .) %>%
    pivot_longer(-c('nx'),names_to='ny',values_to='Count') %>%
    mutate(nx=factor(nx,levels=row.names(XY))) %>%
    mutate(ny=factor(ny,levels=colnames(XY))) %>%
    ggplot(aes_string(x='nx',y='ny',fill='Count')) +
    geom_tile() +
    viridis::scale_fill_viridis(option='viridis') +
    xlab(NULL) + ylab(NULL) +
    theme_light() +
    theme(legend.position='top')
  return(g)
}

plot.ints.vs.pops = function(X){
  pops = M$pop$main
  ints = M$int$main
  N = matrix(ncol=length(pops),nrow=length(ints))
  colnames(N) = pops
  rownames(N) = ints
  g = ggplot()
  for (p in 1:length(pops)){
    for (i in 1:length(ints)){
      col = namefun('RQi',ints[i],M$pop$pretty[[pops[p]]])
      rows = X[[col]]
      if (sum(rows)){
        x = X[rows,]
        x = x[order(-x$RG.n),]
        x = cbind(x, hex.coord(p,i,1:sum(rows),s=.15))
        x$Risk.Groups = cut(x$RG.n,
          breaks=c(0,1,2,8,12,99),
          labels=c('1','2','3-8','9-12','13+')
        )
        g = g + geom_circle(data=x,aes(x0=x0,y0=y0,r=r,fill=Risk.Groups))
      }
    }
  }
  scale.fun = function(name,labels){
    return(list(name,
      breaks=1:length(labels),
      labels=gsub('\\.',' ',labels),
      minor=FALSE
    ))
  }
  g = g +
    do.call(scale_x_continuous,scale.fun('Population',  pops)) +
    do.call(scale_y_continuous,scale.fun('Intervention',ints)) +
    viridis::scale_fill_viridis(option='viridis',discrete=TRUE) +
    theme_light()
  return(g)
}

hex.coord = function(x0,y0,n,r=.5,s=1,...){
  yv = sqrt(3)/2; yh = 0; xv = 0.5; xh = 1; n;
  dxl = c(-xv,-xh,-xv,+xv,+xh,+xv)
  dyl = c(+yv, yh,-yv,-yv, yh,+yv)
  repr = function(di,k,end){
    di = rep(di,each=k)
    di[6*k] = di[6*k] + end
    return(di)
  }
  dx = c(0.0,+1.0,repr(dxl,1,+1.0),repr(dxl,2,+1.0),repr(dxl,3,+1.0))
  dy = c(0.0, 0.0,repr(dyl,1, 0.0),repr(dyl,2, 0.0),repr(dyl,3, 0.0))
  return(data.frame(
    x0 = x0 + s*sapply(n, function(ni){ sum(dx[1:ni]) }),
    y0 = y0 + s*sapply(n, function(ni){ sum(dy[1:ni]) }),
    r  = s*r
  ))
}