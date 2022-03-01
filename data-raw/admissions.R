# Data references here: https://stats.idre.ucla.edu/r/dae/logit-regression/
# Downloaded from here: https://stats.idre.ucla.edu/stat/data/binary.csv
if(!file.exists('data-raw/admissions.csv')) {
	download.file('https://stats.idre.ucla.edu/stat/data/binary.csv',
				  destfile = 'data-raw/admissions.csv')
}
admissions <- read.csv('data-raw/admissions.csv')
save(admissions, file = 'data/admissions.Rda')
