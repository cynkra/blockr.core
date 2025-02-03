# block constructor

    Code
      print(board)
    Output
      <board>
      
      Blocks[5]:
      
      <merge_block<transform_block<block>>>
      Name: "Merge block"
      Data inputs: "x" and "y"
      Initial block state:
       $ by   : chr(0)
       $ all_x: logi FALSE
       $ all_y: logi FALSE
      Constructor: blockr.core::new_merge_block()
      
      <dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      <subset_block<transform_block<block>>>
      Name: "Subset block"
      Data inputs: "data"
      Initial block state:
       $ subset: chr ""
       $ select: chr ""
      Constructor: blockr.core::new_subset_block()
      
      <subset_block<transform_block<block>>>
      Name: "Subset block"
      Data inputs: "data"
      Initial block state:
       $ subset: chr ""
       $ select: chr ""
      Constructor: blockr.core::new_subset_block()
      
      <dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      Links[4]:
      
      ad: a -> d (x)
      cd: c -> d (y)
      bc: b -> c (data)
      de: d -> e (data)

