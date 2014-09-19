{
   for (i = 1; i < NF; i++)
      print $i
}
{
   for (i = 1; i < NF; i++) {
      print $i
      print $0, "cool!"
   }
}
