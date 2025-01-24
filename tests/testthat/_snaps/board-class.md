# block constructor

    Code
      print(board)
    Output
      <board>
      
      Blocks[5]:
      
      <join_block<transform_block<block>>>
      Name: "Join block"
      Data inputs: "x" and "y"
      Initial block state:
       $ type: chr "left_join"
       $ by  : chr(0)
      Constructor: blockr.dplyr::new_join_block()
      
      <dataset_block<data_block<block>>>
      Name: "Dataset block"
      No data inputs
      Initial block state:
       $ dataset: chr(0)
       $ package: chr "datasets"
      Constructor: blockr.core::new_dataset_block()
      
      <select_block<transform_block<block>>>
      Name: "Select block"
      Data inputs: "data"
      Initial block state:
       $ columns: chr(0)
      Constructor: blockr.dplyr::new_select_block()
      
      <select_block<transform_block<block>>>
      Name: "Select block"
      Data inputs: "data"
      Initial block state:
       $ columns: chr(0)
      Constructor: blockr.dplyr::new_select_block()
      
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

