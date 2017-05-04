data.index <-
function (dataset, col.index, col.ref, value.ref) {
  
  refcolumn <- dataset[[col.ref]]
  
  refvaluerows <- which (dataset[[col.ref]] == value.ref)
  
  index_values <- dataset[[col.index]]/dataset[[col.index]][refvaluerows]*100
  
  return(index_values)
}
