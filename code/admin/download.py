import os,sys,re
import pandas as pd

def redirect(filename):
  regex = '\<A\ HREF\=\"(.*?)\"\>here\<\/A\>'
  with open(filename,'r') as f:
    try:
      return re.findall(regex,f.read())[0]
    except:
      return None

def download(filename,url):
  os.system('curl -s > {} {}'.format(filename, url))
  url = redirect(filename)
  if url:
    download(filename,url)

temp = 'tmp.xlsx'
cols = os.path.join(os.environ['ROOT'],'data','{}.cols')
xlsx = os.path.join(os.environ['ROOT'],'data','{}.xlsx')
gurl = 'https://docs.google.com/spreadsheets/d/{}/export'
gids = {
  # 'v0':   '1DVnosgUx5jc-Pev73Nouh_FfbyldRsKUdc1pcknYUmw',
  'main': '1GI2g1Kg7xOFRrEpXLwJUHySkcgLrdBMt-56qwLG3ylA',
  'api':  '1WwS_j7yv7W9RJ0fDUrKeejs5v6NT9Rqdk0vr5tfTMEs',
}
# for name,gid in gids.items():
for name in sys.argv[1:]:
  gid = gids[name]
  print('downloading '+name+' ...',flush=True)
  download(temp,gurl.format(gid))

  print('cleaning '+name+' ...',flush=True)
  X = pd.read_excel(temp)
  X[0:0].to_csv(cols.format(name),index=False,sep='\n')
  X.to_excel(xlsx.format(name),index=False)
  os.system('rm {}'.format(temp))

print('done')
