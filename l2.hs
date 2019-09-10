--zad 1
--Napisać funkcję dodającą dany element
--a)
--na początek listy
dodaj1 x xs = x:xs
--b)
--jako drugi element listy
dodaj2 z x= head(x):z:tail(x)
--c)
--na koniec listy
ost x xs= xs++[x]
--zad2
--Napisać funkcje zwracające
--a)
--drugi element listy
second x=head (tail x)
--b)
--trzeci element listy
trzeci x=head (tail(tail x))
--c)
--przedostatni element listy
przed x= second (reverse x)
--zad3
--Napisać funkcję przestawiającą elementy listy w odwrotnym porządku
odwroc []=[]
odwroc(x:xs)=odwroc xs++[x]
--zad4
--Napisać funkcję przestawiającą w liście ostatni element z pierwszym.
zam [] = []
zam [x] = [x]
zam (x:xs) = (last xs : init xs) ++ [x]
--zad5
--Napisać funkcję sprawdzającą, czy lista ma parzystą liczbę elementów
parz x=even(length x)
--zad6
--Zdefiniować funkcję podnoszącą do kwadratu wszystkie elementy danej listy liczb
całkowitych, np. sqrlist [1,2,-3] zwraca [1, 4, 9]
--a)
--z użyciem map
sqrlist1 x=map square x 
    where square y=y*y
--b)
--bez użycia map
sqrlist2 x=test square x
    where square y=y*y
          test f xs = [f x | x <- xs]
--zad7
--Zdefiniować polimorficzną funkcję obliczającą, ile razy dany obiekt występuje w danej
--liście, np. count('a', ['a', 'l', 'a']) zwraca 2. Jaki jest typ tej funkcji? 
count(_,[]) = 0
count(x,list) = sum ( map (\a -> 1) ( filter (== x) list ) )
--zad8
--Zdefiniować polimorficzną funkcję powtarzającą dany obiekt określoną liczbę razy
--i zwracającą wynik w postaci listy, np. duplicate("ppd",3) zwraca ["ppd","ppd","ppd"]. Jaki
--jest typ tej funkcji? 
duplicate(xs,n) = concat ( map (\x -> take n (repeat x)) [xs])
--zad9
--Zdefiniować polimorficzną funkcję usuwającą pierwsze wystąpienie danego elementu
w liście. 
usunf _ [] = [] 
usunf a (b:bc) | a == b    = bc 
               | otherwise = b : usunf a bc
--zad10
--Zdefiniować polimorficzną funkcję usuwającą element na n-tym miejscu w liście. 
usunN _ []     = []
usunN f (a:b)
   | f == 0    = b
   | otherwise = a : usunN (f-1) b
--zad11
--Napisać definicję dwuargumentowej funkcji określonej dla list, której wartością jest prawda,
--jeśli wszystkie elementy pierwszej listy występują na drugiej liście.
--rowne a b = sort a == sort b
        where sort [] = []
              sort(x:xs)=sort(filter(<x)xs)++
                    [x]++
                    sort(filter(>=x)xs)
--zad12
--Napisać definicję funkcji, której argumentem jest lista krotek 2-elementowych, a wartością
--lista krotek z przestawionymi elementami, np. wartością zamiana [(1,’a’),(2,’b’)]
--jest [(’a’,1),( ’b’,2)].
krotki :: [(Int, Char)]->[(Char, Int)]
krotki [] = []
krotki (x:xs) = (snd x,fst x) : (krotki xs)
--Zadania domowe
--a
--obliczającą potęgę 
potega a n=if n==0 then 1 else potegaPOM a n 1
potegaPOM a n ak=if n==0 then ak else potegaPOM a (n-1) (ak*a)
--b
--sprawdzającą, który z dwóch punktów płaszczyzny jest dalej położony od początku
--układu współrzędnych
punkty x1 y1 x2 y2 = if ((0-x1)^2+(0-y1)^2)**0.5 > ((0-x2)^2+(0-y2)^2)**0.5 then "P1 jest dalej" else if ((0-x1)^2+(0-y1)^2)**0.5 == ((0-x2)^2+(0-y2)^2)**0.5 then "rowna odleglosc" else "P2 jest dalej" 
--c
--obliczającą sumę liczb o 0 do n - definicja rekurencyjna
suma1 n = if n==0 then 0 else n+suma1(n-1)
--d
--obliczającą sumę liczb od 0 do n - definicja „akumulatorowa”
suma2 n = sumaPOM n 0 
sumaPOM n ak=if n==0 then ak else sumaPOM (n-1) (ak+n)        
--e
--obliczającą n-ty wyraz ciągu danego wzorem: a1=0, a2=1, an=2an-1-an-2 - definicja
--rekurencyjna
an1 n | n==1         = 0
      | n==2         = 1
      | n>2          = 2*an1(n-1) - an1(n-2)
--f
--obliczającą n-ty wyraz ciągu z podpunktu k) – definicja „akumulatorowa”
an2 n=if n==1 then 0 else if n==2 then 1 else anPOM n 0 1
anPOM n f1 f2=if n==1 then f1 else anPOM (n-1) (f1+f2)f2
--g
--obliczającą pierwiastki równania ax^2+bx+c=0
fkw a b c =if d ==0 then (-b/(2*a),-b/(2*a)) else if d >0 then x01 else (-0,-0)
        where x01= ((-b+(b^2-4*a*c)**0.5)/(2*a),(-b-(b^2-4*a*c)**0.5)/(2*a))
              d=b^2-4*a*c
---fkw zwraca dla d <0 => (-0,-0) 
--h
--znajdującą maksimum z trzech danych liczb
lel a b c | a>=b && a>=c   =  a
          | b>=a && b>=c   =  b
          | c>=a && c>=b   =  c
