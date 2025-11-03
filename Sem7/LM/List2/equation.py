class Equation:
    def __init__(self, s):
        self.equation = self.string_to_equation(s)

    def evaluate(self):
        return self.eq_calc_result(self.equation)

    def to_string(self):
        return self.eq_to_string(self.equation)

    def string_to_equation(self, s):
        """
        Converts string to list of lists, where strings are operators and numbers are integers.
        """

        def parse(i):
            result = []
            current_number = ''
            operators = set('+-*/')

            while i < len(s):
                c = s[i]
                if c.isdigit():
                    current_number += c
                else:
                    if current_number:
                        result.append(int(current_number))
                        current_number = ''
                    if c in operators:
                        result.append(c)
                    elif c == '(':
                        sub, j = parse(i + 1)
                        result.append(sub)
                        i = j
                        continue
                    elif c == ')':
                        return result, i + 1
                i += 1

            if current_number:
                result.append(int(current_number))

            return result, i

        parsed, _ = parse(0)
        return parsed


    def eq_calc_result(self, eq):
        """
        Calculates the result of the equation represented as a list of lists.
        """

        def evaluate(expression):
            if isinstance(expression, int):
                return expression

            # First handle multiplication and division
            stack = []
            i = 0
            while i < len(expression):
                token = expression[i]
                if token == '*':
                    prev = stack.pop()
                    next_num = evaluate(expression[i + 1])
                    stack.append(prev * next_num)
                    i += 2
                elif token == '/':
                    prev = stack.pop()
                    next_num = evaluate(expression[i + 1])
                    stack.append(prev / next_num)
                    i += 2
                else:
                    if isinstance(token, list):
                        stack.append(evaluate(token))
                    else:
                        stack.append(token)
                    i += 1

            # Now handle addition and subtraction
            result = stack[0]
            i = 1
            while i < len(stack):
                operator = stack[i]
                next_num = stack[i + 1]
                if operator == '+':
                    result += next_num
                elif operator == '-':
                    result -= next_num
                i += 2

            return result

        return evaluate(eq)

    def eq_to_string(self, eq):
        """
        Converts list of lists back to string representation of the equation.
        """

        def build_string(expression):
            result = ''
            for token in expression:
                if isinstance(token, list):
                    result += '(' + build_string(token) + ')'
                else:
                    result += str(token)
            return result

        return build_string(eq)



