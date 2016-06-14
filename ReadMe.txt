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
W tym trybie również podajemy dodatkowy parametr czyli głębokość MinMaxa, a także wartośc True or False oznaczającą
czy chcemy wyświetlać mapę po ruchu czy tylko zwracać sekwencję wykonywanych przez komputer ruchów
Należy wywołać funkcję:
Definicja: computerVsComputer board color mmDepth shBoard
Przykładowe wywołanie: computerVsComputer startingBoard White 3 False

Dodatkowe informacje co do wywołania:
Dla playerVsComputer działa input taki jak w przykładzie, np. 21-17, 22x15x8
Wydaje mi się, że w moim programie do wywołania 4 (może do testów 3) to optymalna wysokość MinMax,
bo długość liczenia ogranicza się przy dużej ilości ruchów (np. na początku) do około sekundy, w trakcie gry do pół sekundy,
a gdy pojawiają się królowe do około dwóch sekund. Im większa wysokość tym dłużej komputer liczy optymalny ruch.
