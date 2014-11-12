

query fields warpped in combine, e.g.
        #combine(11799106.entrez_gene_id)

you cannot query for GO_xxx ids, but you can query for xxx in a GO field


Proper field length normalization, here for field "go" and query "0051471"
 #combine:w=1.0( #dirichlet:lengths=go( #lengths:go:part=lengths() #counts:@/0051471/:part=field.go() ) )


 Proper field lengths normalization in bigrams (unordered:8)
 #combine:w=1.0( #dirichlet:lengths=desc( #lengths:desc:part=lengths() #unordered:8( #extents:solute:part=field.desc() #extents:binding:part=field.desc() )) )

... same with escaping...
 #combine:w=1.0( #dirichlet:lengths=desc( #lengths:desc:part=lengths() #unordered:8( #extents:@/solute/:part=field.desc() #extents:@/binding/:part=field.desc() )) )