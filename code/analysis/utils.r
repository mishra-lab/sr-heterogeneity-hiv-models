VERB = TRUE
# VERB = FALSE
# SAVE = TRUE # TODO: implement this
set.seed(5040)
options(width=200,scipen=100)

path.tex = function(...){
  return(file.path(Sys.getenv('ROOT'),'out','tex',...))
}

path.fig = function(...){
  return(file.path(Sys.getenv('ROOT'),'out','fig',...) %>% paste0('.pdf'))
}

# FFS R you GD unicorn
regex = function(re,strings){
  matches = regmatches(strings,regexec(re,strings,perl=TRUE))
  return(sapply(matches,function(match){
    ifelse(length(match),match[[2]],'')
  }))
}

save.tex = function(string,fname,dir='',na='---'){
  string = gsub('NA',na,paste(string))
  if (VERB){ print(paste('tex:',fname)) }
  cat(string,file=path.tex(dir,fname))
}

save.plot = function(g,fname,dir='',width=6,height=3.5){
  if (VERB){ print(paste('fig:',fname)); }
  ggsave(path.fig(dir,fname),plot=g,width=width,heigh=height)
  return(NULL)
}

do.count = function(x){
  return(sum(x,na.rm=TRUE))
}

print.p = function(p,d=3){
  pr = round(p,d=d)
  return(ifelse(pr,paste(pr),paste('$<$',10^-d)))
}

print.count = function(x,pct=FALSE){
  return(paste0(
    sprintf('%d',do.count(x)),
    ifelse(pct,sprintf('~(%.0f)',100*mean(x,na.rm=TRUE)),'')
  ))
}

save.count = function(x,name,dir='',pct=FALSE){
  save.tex(print.count(x),namefun('n',name),dir)
  if (pct){
    save.tex(print.count(x,pct=TRUE),namefun('n',name,'pct'),dir)
  }
}

save.count.fct = function(x,name,dir='',pct=FALSE){
  for (level in levels(x)){
    save.count(x==level,namefun(name,level),dir,pct)
  }
  save.count(is.na(x),namefun(name,'NA'),dir,pct)
}
