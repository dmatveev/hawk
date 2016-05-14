{
   if ($1 == 0)
      printf("Oh way\n")
   printf("I am a top-level statement! Not under any conditions.\n")
   printf("I am too.\n")
}
{
   if ($1 == "foo") {
      $1 = "bar"
      printf("Compound one!\n")
   }
}
{
   if ($1 == "foo") {
      $1 = "bar"
      printf("Compound one!\n")
   } else
      printf("Not foo\n")
   printf("Really\n")
}
{
   if ($1 != $2) {
      #do nothing
   } else {
      printf("One. ")
      printf("Two. ")
      printf("Three. ")
   }
}
