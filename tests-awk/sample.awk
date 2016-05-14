BEGIN  { FS = "\t"  # Set field separator to Tab
         printf("%10s %6s %5s   %s\n\n", "COUNTRY", "AREA", "POP", "CONTINENT")
         digits = "^[0-9]+$"
       }
       {
         printf("%10s %6d %5d   %s", $1, $2, $3, $4)
         area = area + $2
         pop = pop + $3
         $5 = $3
       }
$2 !~ /Asia/ {
         asian += 1
       }
$2 ~ /Asia/ && $3 > 100 {
         printf("Wow\n")
       }
$2 == "Asia" || $2 == "Europe" {
         printf("Another wow\n")
       }
FNR == 1, FNR == 5 {
         printf("1..5\n")
       }
/Europe/, /Africa/ {
         printf("Europe..Africa\n")
       }
END    { printf("\n%10s %6d %5d\n", "TOTAL", area, pop)  }
