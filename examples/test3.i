(* oblicz potege x^n *)
read x; //wczytuje x
read n; //wczytuje n
wynik := 1;
while n > 0 do
        wynik := wynik * x;
        n := n - 1;
done
write wynik;
