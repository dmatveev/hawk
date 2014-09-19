# The AWK Book, p. 50

BEGIN { FS = "\t" }
      { for (i = 1; i <= NF && $i != ""; i++)
            ;
        if (i <= NF)
            print
      }
