import os
import re
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
main = os.path.join(os.environ['ROOT'],'data','main.xlsx')
# gid = '1DVnosgUx5jc-Pev73Nouh_FfbyldRsKUdc1pcknYUmw' # v1
gid = '1GI2g1Kg7xOFRrEpXLwJUHySkcgLrdBMt-56qwLG3ylA' # reboot

print('downloading...')
download(temp,'https://docs.google.com/spreadsheets/d/'+gid+'/export')

print('cleaning...')
X = pd.read_excel(temp)
# X = X[X['stage'] == 1]
X.to_excel(main,index=False)
os.system('rm {}'.format(temp))

print('done')
