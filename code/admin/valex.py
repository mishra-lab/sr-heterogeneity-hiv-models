import os
import pandas as pd
from collections import OrderedDict as odict

def check(*oks):
  return '['+''.join('.' if ok else '#' for ok in oks)+']'

def rootpath(*args):
  return os.path.join(os.environ['ROOT'],*args)

def findfile(base,exts):
  return any(os.path.exists(base+'.'+ext) for ext in exts)

def valfun(ref,X):
  val = odict(
    uid  = sum(ref['id']  == X['id'])  == 1,
    ubib = sum(ref['bib'] == X['bib']) == 1,
    pdf  = findfile(rootpath('refs','pdf',ref['bib']),['pdf']),
    app  = findfile(rootpath('refs','pdf',ref['bib'])+'x',['pdf','zip','doc','docx']),
  )
  val['OK'] = val['uid'] and val['ubib'] and val['pdf']
  return(val)

def printval(ref,val):
  print(
    ref['bib'].rjust(20)+
    ' OK: '+check(val['OK'])+
    ' UID: '+check(val['uid'],val['ubib'])+
    ' PDF: '+check(val['pdf'])+
    ' APP: '+check(val['app'])+
  '')

X = pd.read_excel(rootpath('data','main.xlsx'))
fail = []
for ref in X.to_dict(orient='rows'):
  val = valfun(ref,X)
  printval(ref,val)
  if not val['OK']: fail.append(ref['bib'])

print('FAIL: {}'.format(','.join(fail)) if fail else
      'OK [{}]'.format(len(X)))
