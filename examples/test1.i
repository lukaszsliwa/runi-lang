(* silnia z x *)
y := 1;
read x;
while x > 1 do
        y := x * y;
        x := x - 1;
done
write y;
