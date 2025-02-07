library(blockr.core)

serve(
  new_rbind_block(),
  `2` = data.frame(a = 1),
  `1` = data.frame(a = 2)
)
