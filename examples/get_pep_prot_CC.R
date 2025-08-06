data(sub_R25)
se <- sub_R25[[1]]
g <- buildGraph(get_cc(se)[[1]])
display.CC.visNet(g)