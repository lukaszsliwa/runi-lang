(* wypisuje n liczb fibonacciego, po wczesniejszym wczytaniu *)
read n;
x := 0;
y := 1;
while n > 0 do
        t := x;
        x := x + y;
        y := t;
        n := n - 1;
        write x;
done
