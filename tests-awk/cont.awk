BEGIN { i = 0
        j = 0
        for (k = 0; k < 10; k++)
        {
           if (k < 2)
              continue;

           i = k + 1;
           j = k * 2;
           print "k:", k, ", i:", i, ", j:", j
           if (i + j > 10)
              break;
        }
        print "The final values", i, j, k, i+j

        a = 0
        b = 0
        while (a < 10) {
           a++;

           if (a < 3)
              continue;

           b += a*2;

           print "a:", a, "b:", b

           if (b > 20)
              break;
        }
        print "The final values", a, b

        x = 0
        y = 10
        do {
           x++

           if (x < 2)
              continue;

           y-=2
           print "x", x, "y", y

           if (x + y < 8) {
              print "Stop!"
              break
           }
        } while (y > 0)

        print "Time for ultrahardcore!"
        for (i = 0; i < 5; i++) {
           if (i > 2) {
              print "Skip i =", i
              continue
           }
           if (i > 2) {
              print "NOOOOOOOOOOOOOOOOOO"
           }

           for (j = 0; j < 5; j++)
              print i, ";", j
        }
}
