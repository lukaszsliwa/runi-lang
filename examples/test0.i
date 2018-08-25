(* wypisuje dzielniki pierwsze podanej liczby w porzadku rosnacym *)
read x;
while x > 1 do
        y := 2;
        while x mod y <> 0 do 
                y := y + 1;
        done
        x := x div y;
        write y;
done
