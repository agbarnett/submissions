# prepare_smallsets.R
# using smallsets to describe the data management
# see https://github.com/lydialucchesi/smallsets
# August 2022
library(smallsets)

#
snaps <- prepare_smallset(
  data = jpc,
  code = "1_prepare_data.R",
  rowCount = 10,
   #auto = 1
  rowNums = 1:10 # obtained with initial run of auto = 2 (optimisation model)
)

# then add captions

# 
Timeline <- create_timeline(
  snapshotList = snaps,
  highlightNA = TRUE,
  rotateHeader = TRUE,
  sizing = list(columns = 1, tiles = 0.3, captions = 3, data = 2.5, legendText = 7,
                legendIcons = 1, title = 10, subtitle = 8, footnote = 7, resume = 0.25),
  headerSpace = c(6, 2), # increase header above tables and to right to fit in 45 degree text
  captionTemplateName = "captionTemplate"
)

jpeg('figures/smallsets.jpg', width=5, height=4, units='in', res=600, quality=100)
print(Timeline)
dev.off()

