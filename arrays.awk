    { x[NR] = $0 }
END { for (i = NR; i > 0; i--) print x[i] }
END { for (v in x) print x[v] }
END { if ("Sample" in x)
        print "Should work!"
        delete x["Sample"]
    }
