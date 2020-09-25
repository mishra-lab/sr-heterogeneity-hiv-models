# VERB = TRUE
VERB = FALSE

path.tex = function(name){
  return(file.path(Sys.getenv('ROOT'),'out','tex',name))
}

path.fig = function(name){
  return(file.path(Sys.getenv('ROOT'),'out','fig',name) %>% paste0('.pdf'))
}

# FFS R you GD unicorn
regex = function(re,strings){
  matches = regmatches(strings,regexec(re,strings,perl=TRUE))
  return(sapply(matches,function(match){
    ifelse(length(match),match[[2]],'')
  }))
}

save.tex = function(string,fname){
  if (VERB){ print(paste('tex:',fname)) }
  cat(string,file=path.tex(fname))
}

save.plot = function(g,fname,width=6,height=3.5){
  if (VERB){ print(paste('fig:',fname)); print(g) }
  ggsave(path.fig(fname),plot=g,width=width,heigh=height)
}

print.count = function(x,pct=FALSE){
  return(paste0(
    sprintf('%d',sum(x,na.rm=TRUE)),
    ifelse(pct,sprintf('~(%.0f)',100*mean(x,na.rm=TRUE)),'')
  ))
}

save.count = function(x,name){
  save.tex(print.count(x),namefun('n',name))
  save.tex(print.count(x,pct=TRUE),namefun('n',name,'pct'))
}
