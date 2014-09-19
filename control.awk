{
   for (i = 1; i < NF; i++) {
      if ($i < 33)
         break
      print $i
   }
}
