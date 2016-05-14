# The first program interpreted by Hawk

BEGIN { print "Hello world!"
        i = 0
      }
      { print "Line", ++i, $0
      }
END   { print "Total lines", i }
END   { for (j = 0; j < 100; j += 10)
          print "LOOP!", j
      }
