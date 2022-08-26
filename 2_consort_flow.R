# 2_consort_flow.R
# CONSORT flow diagram of numbers of submissions and reviews
# called from within 2_analysis.Rmd
# August 2022
library(diagram)

## function to run same plot for submission and reviewer numbers
# had to do separately because of statistical reviews
flow.plot.submission = function(indata, actual.data){

# labels
l1 = paste(
  'Submissions (n=',
  format(
    with(indata, n.jpc),
    big.mark = ','
  ),
  ')',
  sep = ''
)
l2 = paste(
  'Excluded (n=',
  format(
    with(indata, n.jpc) - nrow(actual.data),
    big.mark = ','
  ),
  ')\n',
  '- No address given (n=',
  format(
    with(
      indata,
      n.jpc - n.missing.address
    ),
    big.mark = ','
  ),
  ')\n',
  '- No geocoded address (n=',
                                            format(
                                            with(
                                            indata,
                                            n.missing.address  - n.after.geomatch
                                            ),
                                            big.mark = ','
                                            ),
                                            ')\n',
  '- Different country in geocoded\naddress (n=',
  format(
    with(
      indata,
      n.after.geomatch  - n.post.exclude.differing.country
    ),
    big.mark = ','), ')\n',
  '- Duplicate manuscripts with same\ntime zone: one excluded (n=',
  format(
    with(
      indata,
      n.post.exclude.differing.country  - n.after.duplicates
    ),
    big.mark = ','), ')\n',
  '- Duplicate manuscripts with different\ntime zones: both excluded (n=',
  format(
    with(
      indata,
      n.after.duplicates  - n.after.double.timezones
    ),
    big.mark = ','), ')\n',
  '- Results at start or end from\nincomplete weeks (n=',
  format(
    with(
      indata,
      n.after.double.timezones  - n.after.windows
    ),
    big.mark = ','), ')', sep = '')
# get numbers with holidays
with.holidays = filter(actual.data, holiday=='Yes') %>%
  dplyr::select(country) %>%
  unique()
n.hols = nrow(filter(actual.data, country %in% with.holidays$country))
l3 = paste('Analysed (n=', format(nrow(actual.data), big.mark = ','), ')\n',
           '- With holiday data (n=', format(n.hols, big.mark = ','), ')' , sep =
             '')
labels = c(l1, l2, l3)
n.labels = length(labels)
### make data frame of box chars
frame = read.table(
  sep = '\t',
  stringsAsFactors = F,
  skip = 0,
  header = T,
  text = '
i	x	y	box.col	box.type	box.prop	box.size
1	0.27	0.85	white	square	0.22	0.25
2	0.7	0.48	white	square	0.93	0.30
4	0.27	0.11	white	square	0.26	0.25
'
)
#
pos = as.matrix(subset(frame, select = c(x, y)))
M = matrix(
  nrow = n.labels,
  ncol = n.labels,
  byrow = TRUE,
  data = 0
)
M[3, 1] = "' '"

to.return = list()
to.return$M = M
to.return$pos = pos
to.return$labels = labels
to.return$frame = frame
return(to.return)

} # end of function for submissions


# run function
sub = flow.plot.submission(indata = jpc.numbers, actual.data = jpc)

# function to repeat plot in Rmarkdown and tiff
plot.it = function(inlist, header, type='tiff') {
  # 
  to.plot = inlist
  if(type == 'rmarkdown'){
    to.plot$frame$box.size = to.plot$frame$box.size * 1 # adjustment for rmarkdown (trial and error)
    to.plot$frame$box.prop = to.plot$frame$box.prop * 1
  }
  par(mai = c(0, 0.04, 0.04, 0.04))
  with(to.plot, 
  plotmat(
    M,
    pos = pos,
    name = labels,
    lwd = 1,
    shadow.size = 0,
    curve = 0,
    box.lwd = 2,
    box.cex = 0.85, ## slightly smaller text in boxes
    cex.txt = 1, 
    box.size = frame$box.size,
    box.prop = frame$box.prop,
    box.col = frame$box.col,
    box.type = frame$box.type,
    arr.type = 'simple',
    txt.col = 'black'
  ))
  text(0.5, 0.98, header, font=2) # bold
  # extra arrow to excluded
  arrows(
    x0 = 0.27,
    x1 = 0.415,
    y0 = 0.49,
    length = 0.12
  )
} # end of plotit function for reviews

# export
tiff(
  'figures/consort.flow.tif',
  width = 10.5,
  height = 5.5,
  units = 'in',
  res = 300,
  compression = 'lzw'
)
plot.it(sub, header='')
dev.off()
# and jpeg
jpeg(
  'figures/consort.flow.jpg',
  width = 10.5,
  height = 5.5,
  units = 'in',
  res = 300,
  quality=100
)
plot.it(sub, header='')
dev.off()

