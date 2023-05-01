# Patryk Flama
# Zadanie 1
# Kompilacja: ruby zad.rb

class Function
    def initialize(block)
        @block = block
    end

    def value(x)
        @block.call(x)
    end

    def zero(a, b, e)
        zeros = []
        dx = (b - a).to_f / 100.0

        while a <= b
            v = value(a)
            if v.abs < e
                zeros.push(a)
            end
            a += dx
        end

        if zeros.length == 0
            return nil
        else
            return zeros
        end
    end

    def field(a, b, e = 1000)
        res = 0
        dx = (b - a) / e.to_f
        e.times do |i|
            res += value(a + i * dx) * dx
        end
        return res
    end

    def deriv(x, h = 0.0001)
        return (value(x + h) - value(x)) / h
    end

    # ------ plotting ------
    
end

sq = Function.new proc {|x| x*x}

puts sq.value(2)
puts sq.zero(0, 2, 0.0001)
puts sq.field(0, 2)
puts sq.deriv(2)
