Marcin Skowron, Informatyka II rok, Projekt na Paradygmaty Programowania

URUCHAMIANIE:

Aby uruchomić warcaby, należy wczytać plik Checkers.hs, a następnie uruchomić grę w jednym
z dostępnych trybów:
(w przykładowym wywołaniu wstawiam już konkretne nazwy dlatego można dosłownie użyć wywołania):

Gracz przeciwko graczowi, należy wywołać funkcję:
Definicja: playerVsPlayer board color
Przykładowe wywołanie: playerVsPlayer startingBoard White

Gracz przeciwko komputerowi:
W tym trybie podajemy dodatkowy parametr czyli głębokość MinMaxa
(może on oznaczać trudność gry przeciwko komputerowi)
Należy wywołać funkcję:
Definicja: playerVsComputer board playerColor mmDepth
Przykładowe wywołanie: playerVsComputer startingBoard White 4

Komputer przeciwko komputerowi:
W tym trybie również podajemy dodatkowy parametr czyli głębokość MinMaxa
Należy wywołać funkcję:
Definicja: computerVsComputer board color mmDepth
Przykładowe wywołanie: computerVsComputer startingBoard White 4

Dodatkowe informacje co do wywołania:
Wydaje mi się, że do wywołania 4 to optymalna wysokość MinMax, bo długość liczenia ogranicza
się przy dużej ilości ruchów (np. na początku) do około sekundy, w trakcie gry do pół sekundy,
a gdy pojawiają się królowe do około dwóch sekund. Im większa wysokość tym dłużej komputer liczy
optymalny ruch, dodatkogwo dodam, że przy testowałem grając przy wysokości 6 poniosłem porażkę, 
być może i przy niższych by się tak stało, ale jeszcze nie grałem na niższych.


CO ZAIMPLEMENTOWAŁEM (i raczej działa sprawnie):
- Dla pionków i damek: ruchy, bicia, sekwencje bić
- Teraz czytając zasady ponownie zastanawiam się czy damka powinna bić przeskakując na dowolne pole
  za pionkiem przeciwnika czy na pole bezpośrednio za nim, u mnie bije na pole bezpośrednio
  za przeciwnikiem
- Generowanie ruchów z poszczególnego pola i wszystkich możliwych ruchów
- Generowanie drzewa bić, drzewa gry
- Ocena planszy, MinMax
- Tryby gry: Player vs. Player, Player vs. Computer, Computer vs. Computer
