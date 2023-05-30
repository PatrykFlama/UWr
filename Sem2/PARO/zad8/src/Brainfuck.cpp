#include "Brainfuck.hpp"
#include "Memory.hpp"

#include <bits/stdc++.h>


std::string Brainfuck::interpret(Code const& code, Input const& input) const {
    Memory memory;
    std::string result;

    long unsigned int input_pos = 0;
    long unsigned int code_pos  = 0;
    long unsigned int global_nest_level = 0;

    while(code_pos < code.size()){

        switch (code[code_pos])
        {
        case '>':
            memory.moveRight();
            break;
        case '<':
            memory.moveLeft();
            break;
        case '+':
            memory.increment();
            break;
        case '-':
            memory.decrement();
            break;
        case '.':
            result += memory.get();
            break;
        case ',':
            //memory.set((input[input_pos++]));
            memory.set((unsigned char)(input[input_pos++]));
            //result += "---memset " + input[input_pos-1];
            break;
        case '[':
            {   
                unsigned long int local_nestness = 0;
                if(memory.get() == 0){
                    code_pos++;
                    while(not (code[code_pos] == ']' && local_nestness == 0)){
                        if(code[code_pos] == ']') local_nestness++;
                        else if(code[code_pos] == '[') local_nestness--;
                        code_pos++;
                    }
                    code_pos--;
                }
                break;
            }
        case ']':
        {
            unsigned long int local_nestness = 0;
            if(memory.get() != 0){
                code_pos--;
                while(not (code[code_pos] == '[' && local_nestness == 0)){
                    if(code[code_pos] == '[') local_nestness--;
                    else if(code[code_pos] == ']') local_nestness++;
                    code_pos--;
                }
                code_pos--;
            }
            break;
        }
        default:
            break;
        }
        code_pos++;
    }
    return result;
}
