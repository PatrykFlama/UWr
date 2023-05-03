# Patryk Flama
# Zadanie 1 i 3
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

    def plot(x_from, y_from, x_to, y_to, e = 0.5)
        (y_from..y_to).reverse_each do |y|
            print " "
            
            (x_from..x_to).each do |x|
            begin
                if (value(x) - y).abs < e
                    print "#"
                elsif x == 0 and y == 0
                    print "+"
                elsif x == 0
                    print "|"
                elsif y == 0
                    print "-"
                else
                    print " "
                end
            rescue Exception
                if x == 0 and y == 0
                    print "+"
                elsif x == 0
                    print "|"
                elsif y == 0
                    print "-"
                else
                    print " "
                end
                end
            end

            puts " "
        end
    end
end

sq = Function.new proc {|x| x*x}
puts sq.value(2)
print sq.zero(0, 2, 0.0001), "\n"
puts sq.field(0, 2)
puts sq.deriv(2)
print sq.plot(-4, -1, 4, 10), "\n"

sin = Function.new proc {|x| Math.sin(x)}
puts sin.value(2)
print sin.zero(0, 2, 0.0001), "\n"
puts sin.field(0, 2)
puts sin.deriv(2)
print sin.plot(-10, -2, 10, 2), "\n"