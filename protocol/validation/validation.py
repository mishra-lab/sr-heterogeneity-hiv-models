
import sys
import bibtexparser as btp
from collections import OrderedDict as odict
bib_main   = '../refs.bib'    # source bib
bib_val    = 'validation.bib' # output bib with only validation
val_bibid  = '*.bibid'  # input bibid file
val_search = '*.search' # output search file
wrap = '({}).m_titl'         # template search string
groups = ['ssa','extra']     # groups (+full)
dbo = btp.bibdatabase.BibDatabase()
search = []
# read main bib file
with open(bib_main,'r') as f:
  ddbi = btp.load(f).entries_dict
clean = lambda title: '"{}"'.format(title.strip('{}?'))
for group in groups:
  # read validation bibids
  with open(val_bibid.replace('*',group),'r') as f:
    ids = f.read().split(',\n')
  # create the search string (titles)
  search += [' OR\n '.join([clean(ddbi[id]['title']) for id in ids ])]
  # write the output
  with open(val_search.replace('*',group),'w') as f:
    f.write(wrap.format(' '+search[-1]))
  # update the validation database
  dbo.entries += [ddbi[id] for id in ids]
with open(bib_val,'w') as f:
  btp.dump(dbo,f)
with open(val_search.replace('*','full'),'w') as f:
  f.write(wrap.format(' '+' OR\n '.join(search)))
