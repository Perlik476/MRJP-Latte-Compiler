# Kompilator Latte

## Rozszerzenia

- Tablice jednowymiarowe i pętle for

- Struktury

- Obiekty

- Metody wirtualne

## Frontend

Kluczowe informacje o języku:


- Wszystkie wyrażenia złożone ze stałych, operatorów arytmetycznych i relacyjnych są ewaluowane, a ich wynik jest używany do ustalenia wartości logicznej wyrażeń warunkowych, co pozwala na ustalenie, czy funkcje zwracają wartości w każdym możliwym przypadku (z tych, które wynikają z ewaluacji wyrażeń warunkowych).

- Funkcje, zmienne i klasy są od siebie niezależne, przez co np. może istnieć funkcja o tej samej nazwie jak zmienna czy klasa. To samo dotyczy pól i metod w klasach.

- Wewnątrz klas można używać `self` do odwoływania się do atrybutów i metod klasy, choć nie jest to wymagane.

- W bezpośrednim bloku `for` nie można zadeklarować zmiennej o nazwie zmiennej, która iteruje po kolekcji, choć można zrobić to w blokach w nim zagnieżdżonych.

- Poprawne operacje na napisach: `+`, `==`, `!=`.

- Poprawne operacje na liczbach: `+`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`.

- Poprawne operacje na wartościach logicznych: `==`, `!=`, `&&`, `||`.
