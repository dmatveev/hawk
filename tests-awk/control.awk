{
   if ($1 > 23)
      next
   if ($1 - $2 > 10)
      exit $2
}
{
   for (i = 1; i < NF; i++) {
      if ($i < 33)
         break
      print $i
   }
}
