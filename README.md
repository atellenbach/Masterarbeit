# Masterarbeit "Geschlechterspezifische Hausarbeitsteilung"

In diesem Repository befinden sich die öffentlichen Datensätze und die Stata Do-Files zu meiner Masterarbeit.

## Datensätze

Die öffentlichen Datensätze befinden sich im Ordner `Data`. Deren Quellen befinden sich in [Datenquellen.pdf](Datenquellen.pdf).

Für den Zugriff auf den Datensatz des ISSP ist eine Anmeldung erforderlich und er muss manuell heruntergeladen werden:

1. Öffnen Sie folgende URL und registrieren Sie sich:
   https://login.gesis.org/realms/gesis/protocol/openid-connect/auth?client_id=gesis-gws-client&redirect_uri=https%3A%2F%2Fsearch.gesis.org%2Fresearch_data%2FZA5900&state=ac578924-0a8a-4594-a7b4-955756c27967&response_mode=fragment&response_type=code&scope=openid&nonce=0328cbd7-6824-4522-83ca-9f8fc19be4cc&ui_locales=de
2. Sie sind auf der Seite
   > International Social Survey Programme: Family and Changing Gender Roles IV - ISSP 2012
3. Klicken Sie rechts auf `Datensätze` und laden Sie folgende Datei herunter:
   > ZA5900_v4-0-0.dta.zip Stata (Datensatz) 4.69 MB
5. Entpacken Sie die Datei `ZA5900_v4-0-0.dta` und speichern Sie sie unter `Data/ZA5900_v4-0-0.dta`.


## Do-files ausführen

Alle Do-files für das Zusammenfügen der Datensätze sind im Do-file [main.do](main.do) integriert.

Bearbeiten Sie erst Zeile 8 von `main.do` und geben Sie den absoluten Pfad zum Root-Order ein:
https://github.com/atellenbach/Masterarbeit/blob/0652428277c24fc15f45c31faf8877f6e4d3afe5/main.do#L21

Nun kann das ganze Do-file ausgeführt werden. Die Grafiken und Tabellen werden im Ordner `Output` gespeichert.
