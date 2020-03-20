# Validation References

Here we define some references that we expect to be in the search results.
We specify them as .bib IDs in .bibid files, referring to ../refs.bib
Then, we run validation.py which does the following:

1. for each .bibid file, create a .search file to identify the refs - currently uses ("title A" OR "title B" OR ...).m_titl
2. creates a similar master .search file using all refs
3. copies the complete .bib info into validation.bib

Note: .bibid and .search are not real file extensions. I use them for convenience. Not sure if Windows will get confused.
