BEGIN {  # LOL I can read only comments now
         # And it is not my code, actually.

         # Initialize some variables
         a = 100
         b = 100500

         FS = "\t"
      }

      {  # This action should be executed for every input line
         # I have no print(f) now, so just alter some state
         a += $1
         b += $2
      }

$1==0 {  # When the first column is zero, execute this one too
         a /= b
      }

END   {  # Print results. It is hard, when your parser does not
         # understand it. So just make a sum.
         c = a + b
      }
