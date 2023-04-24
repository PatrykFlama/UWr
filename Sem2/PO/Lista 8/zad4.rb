class Jawna
    def initialize(slowo)
        @slowo = slowo
    end

    def to_s
        return @slowo
    end

    def zaszyfruj(klucz)
        slowo = ""
        @slowo.split('').each { |c|
            slowo += 
                if klucz[c] == nil
                    c
                else
                    klucz[c]
                end
        }
        return Zaszyfrowana.new(slowo);
    end  
end

class Zaszyfrowana
    def initialize(slowo)
        @slowo = slowo
    end

    def to_s
        return @slowo
    end

    def odszyfruj(klucz)
        slowo = ""
        klucz = klucz.invert()
        @slowo.split('').each { |c|
            slowo += 
                if klucz[c] == nil
                    c
                else
                    klucz[c]
                end
        }
        return Jawna.new(slowo) 
    end
end


prz_klucz = {'a' => 'b',
             'b' => 'r',
             'r' => 'y',
             'y' => 'u',
             'u' => 'a'}
przykladowe_slowo = Jawna.new("ruby");
zaszyfrowane = przykladowe_slowo.zaszyfruj(prz_klucz)

print przykladowe_slowo, " => "
print zaszyfrowane, " => "
print zaszyfrowane.odszyfruj(prz_klucz), "\n" 
