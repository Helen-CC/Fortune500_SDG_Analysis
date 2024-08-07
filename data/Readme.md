# In the `data` folder, we only store the following binary/text files:

1. The keyword sets: These are not mutated often
2. The company-rank-year mapping: This is the original unique identifiers we have,
before adding gvkey-year pairs
3. Compustat gvkey-year and firm characteristics: This is going to change if we add more data. But for now, we want git to manage the gvkey-year mappings

4. Results including graphs and tables


# What we don't include in the `data` folder
- RDS binaries: These are intermediate files and they take a lot of disk space. We move them to Dropbox, and import them when needed
- Raw company filings: These are large pdf and txt files. We put them in Dropbox.

