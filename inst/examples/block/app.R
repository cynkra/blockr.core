library(blockr.core)

serve(
  new_merge_block(),
  data = list(
    x = datasets::BOD,
    y = utils::head(datasets::ChickWeight, n = 10)
  )
)
