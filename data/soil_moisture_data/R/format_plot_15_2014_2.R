##############################################################################
# 
# DATA for plot 15 required special formatting.  Run this script once to 
# to generate the output file and then rename the original input file to 
# special formatting script for plot 15 

if ( file.exists('data/raw_soil_data/2014_2/15_15Sep14-1802.txt') ) { 
  
df <- read.table(file = 'data/raw_soil_data/2014_2/15_15Sep14-1802.txt', sep = '\t', header = F)

cn <- df[ 1, ]

df <- df [ -1 , ]

df$date <- strptime( df$V1, '%m/%d/%y %I:%M %p', tz = 'MST' ) 

df <- df[ order(df$date), ]

df <- df [ , -7 ]

df <- rbind( cn, df )

write.table( df, 'data/raw_soil_data/2014_2/15_reorderd.txt', sep = '\t', row.names = F, col.names = F, quote = F)

file.rename('data/raw_soil_data/2014_2/15_15Sep14-1802.txt', 'data/raw_soil_data/2014_2/original_15_15Sep14-1802.txt')
  
}else if(file.exists('data/raw_soil_data/2014_2/15_reordered.txt')) { stop('File already exists, no formatting needed' ) 
}else { stop('Original file missing!')
}


