# 0_make_smallset.R
# not yet used
# August 2022

snaps <- prepare_smallset(
  data = mydata,
  code = "0_address_data.R",
  rowCount = 5,
  # auto = 2,
  rowNums = c(3, 32, 80, 97, 99) # obtained with initial run of auto = 2 (optimisation model)
)