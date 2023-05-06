class Collection
    def initialize(tab)
        unless tab.kind_of?(Array)
            raise "Expected Array"
        end

        @tab = tab
        @length = tab.length
    end

    def swap(i, j)
        @tab[i], @tab[j] = @tab[j], @tab[i]
    end

    def length
        @length
    end

    def get(i)
        return @tab[i]
    end

    def to_s
        return @tab.to_s
    end
end

class Sorter 
private 
    def quicksort_aux(collection, l, r)

    end

public 
    def bubble_sort(collection)         # slow
        len = collection.length - 1
        len.times do |i|
            (len-i).times do |j|
                collection.swap(j, j+1) if collection.get(j) > collection.get(j+1)
            end
        end
    end

    def quicksort(collection, l=0, r=collection.length-1)           # quick
        return if l >= r

        pivot = collection.get(rand(l..r))
        
        _l, _r = l, r
        while _l < _r do
            _l += 1 while collection.get(_l) < pivot
            _r -= 1 while collection.get(_r) > pivot
            collection.swap(_l, _r) if _l < _r

            _r -= 1 if collection.get(_r) == pivot && collection.get(_l) == pivot
        end

        quicksort(collection, l,   _l-1)
        quicksort(collection, _l+1, r)
    end
end


sorter = Sorter.new

print "Bubble sort: \n"
A = Collection.new(Array.new(15){|x| rand(40)-20})
puts A
sorter.bubble_sort(A)
puts A

print "\nQuicksort: \n"
B = Collection.new(Array.new(15){|x| rand(40)-20})
puts B
sorter.quicksort(B)
puts B


print "\nTime comparison: \n"
require 'benchmark'
Benchmark.bm do |x|
    x.report("Bubble sort: ") { sorter.bubble_sort(Collection.new(Array.new(10000){|x| rand(5000)})) }
    x.report("Quicksort:   ") { sorter.quicksort  (Collection.new(Array.new(10000){|x| rand(5000)})) }
end
