(* wczytuje wartosc n, a nastepnie sumuje n-liczb *)
read n;
suma := 0;
while n > 0 do
        read a;
        suma := suma + a;
        n := n - 1;
done
write suma;