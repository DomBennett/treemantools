setOldClass ('phylo')

setAs(from="TreeMan", to="phylo", def=function(from, to) {
  writeTree(from, file='temp.tre')
  tree <- ape::read.tree(file='temp.tre')
  file.remove('temp.tre')
  return(tree)
})
