# Lecture 1 - atak wykorzystujący napisy do filmów 
Paper: Hacked in Translation

## atak XSS (cross site scripting) 
w html mamy boxa który interpretuje input jako html (np żeby dodać kursywę `<i>kursywa</i>`)  
przykład: strona pobiera arguemnty z linku na który weszliśmy, atakujący podsyła link ofiarze podając jako arguemnt wartość `<script>wyślij ciasteczka do atakującego</script>` - to np klucz sesji zostanie przesłany do atakującego  


> jak się przed czymś takim bronić (jako programista)  
> łatanie problemów nie jest specjalnie efektywne (bo mogą istanieć/powstać inne)  
> lepiej skorzystać z:  
> - deserializacji  
> - sanityzacji 


> mXSS - mutation XSS, technika bazująca na tym że coś jest serializowane (tylko wtedy ona działa) 




