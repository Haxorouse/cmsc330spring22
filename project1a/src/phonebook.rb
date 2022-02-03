class PhoneBook
    def initialize
        @names = Array.new(0)
        @numbers = Array.new(0)
        @listed = Array.new(0)
    end

    def add(name, number, is_listed)
        if lookup(name) || lookupByNum(number)
            return false
        end
        unless  number =~ /\d\d\d-\d\d\d-\d\d\d\d/
            return false
        end
        #conditions
        @names.push(name)
        numbers.push(number)
        listed.push(is_listed)
        return true
    end

    def lookup(name)
        i = 0
        while i < @names.length
            if @names[i] == name && @listed[i]
                return @numbers[i]
                break
            end
            i += 1
        end
        return nil
    end

    def lookupByNum(number)
        i = 0
        while i < @numbers.length
            if @numbers[i] == number && @listed[i]
                return @names[i]
                break
            end
            i += 1
        end
        return nil
    end

    def namesByAc(areacode)
        raise Exception, "Not implemented"
    end
end
