#include <bits/stdc++.h>
using namespace std;
#define type double
#define PI 3.14159265358979323846
#define E 2.718281828459045
#define FI 1.618033988749895


class Expression {
    static unordered_map<string, type> variables;
public:
    int operation_priority = 5;

    Expression(){}

    Expression(Expression&& other) = default;
    Expression& operator=(Expression&& other) = default;
    Expression(const Expression& other) = delete;
    Expression& operator=(Expression& other) = delete;

    type evaluate(){
        return 0;
    }

    string toString(){
        return "";
    }

    static void setVariable(string name, type val){
        variables[name] = val;
    }

    static type getVariable(string name){
        auto ptr = variables.find(name);
        if(ptr == variables.end()) throw runtime_error("Variable " + name + " has no assigned value!");
        return ptr->second;
    }

    static void deleteVariable(string name){
        variables.erase(name);
    }
};

/* #region //* operands */
class Number final : Expression {
    type val;
public:
    Number(type _val = 0) : val(_val) {}

    type evaluate(){
        return val;
    }

    string toString(){
        return to_string(val);
    }
};

class Constant final : Expression {
    string val;
public:
    Constant(string _val = "pi") : val(_val) {}

    type evaluate(){
        if(val == "pi") return PI;
        if(val == "e") return E;
        if(val == "fi") return FI;
        return 0;
    }

    string toString(){
        return val;
    }
};

class Variable final : Expression{
    string name;
public:
    Variable(string _name) : name(_name) {}

    type evaluate(){
        return getVariable(name);
    }

    string toString(){
        return name;
    }
};
/* #endregion */


/* #region //* operators 1arg */
class Operator1arg : public Expression {
protected:
    static Expression *e;
public:
    Operator1arg(Expression* _e) { e = _e; }
    ~Operator1arg(){ delete e; }

    static void setVariable(string name, type val){
        e->setVariable(name, val);
    }
    static type getVariable(string name){
        return e->getVariable(name);
    }
    static void deleteVariable(string name){
        e->deleteVariable(name);
    }
};

class Sin final : Operator1arg {
    type evaluate(){ return sin(e->evaluate()); }
    string toString(){ return "sin(" + e->toString() + ")"; }
};
class Cos final : Operator1arg {
    type evaluate(){ return cos(e->evaluate()); }
    string toString(){ return "cos(" + e->toString() + ")"; }
};
class Exp final : Operator1arg {
    type evaluate(){ return exp(e->evaluate()); }
    string toString(){ return "exp(" + e->toString() + ")"; }
};
class Ln final : Operator1arg {
    type evaluate(){ return log(e->evaluate()); }
    string toString(){ return "ln(" + e->toString() + ")"; }
};
class Abs final : Operator1arg {
    type evaluate(){ return abs(e->evaluate()); }
    string toString(){ return "|" + e->toString() + "|"; }
};
class Opposite final : Operator1arg {
    type evaluate(){ return -(e->evaluate()); }
    string toString(){ return "opposite(" + e->toString() + ")"; }
};
class Inverse final : Operator1arg {
    type evaluate(){ return 1/(e->evaluate()); }
    string toString(){ return "inverse(" + e->toString() + ")"; }
};
/* #endregion */


/* #region //* operators 2arg */
class Operator2arg : public Expression{
protected:
    static Expression *L, *R;
public:
    Operator2arg(Expression* _L, Expression* _R) {
        L = _L;
        R = _R;
    }
    ~Operator2arg(){
        delete L;
        delete R;
    }

    static void setVariable(string name, type val){
        L->setVariable(name, val);
        R->setVariable(name, val);
    }
    static type getVariable(string name){
        return L->getVariable(name);
    }
    static void deleteVariable(string name){
        L->deleteVariable(name);
        R->deleteVariable(name);
    }
};

class Add final : Operator2arg {
    int operation_priority = 0;
    type evaluate(){ return L->evaluate() + R->evaluate(); }
    string toString(){ return L->toString() + " + " + R->toString(); }
};
class Sub final : Operator2arg {
    int operation_priority = 0;
    type evaluate(){ return L->evaluate() - R->evaluate(); }
    string toString(){ return L->toString() + " - " + R->toString(); }
};
class Mult final : Operator2arg {
    int operation_priority = 2;
    type evaluate(){ return L->evaluate() * R->evaluate(); }
    string toString(){
        string tempL = L->toString();
        string tempR = R->toString();

        if(L->operation_priority < operation_priority) tempL = "(" + tempL + ")";
        if(R->operation_priority < operation_priority) tempR = "(" + tempR + ")";
        
        return tempL + " * " + tempR;
    }
};
class Div final : Operator2arg {
    int operation_priority = 2;
    type evaluate(){ return L->evaluate() / R->evaluate(); }
    string toString(){
        string tempL = L->toString();
        string tempR = R->toString();

        if(L->operation_priority < operation_priority) tempL = "(" + tempL + ")";
        if(R->operation_priority < operation_priority) tempR = "(" + tempR + ")";
        
        return tempL + " / " + tempR;
    }
};
class Log final : Operator2arg {
    type evaluate(){ return log(R->evaluate()) / log(L->evaluate()); }
    string toString(){
        return "log(" + L->toString() + ", " + R->toString() + ")";
    }
};
class Mod final : Operator2arg {
    type evaluate(){ return fmod(R->evaluate(), L->evaluate()); }
    string toString(){
        string tempL = L->toString();
        string tempR = R->toString();

        if(L->operation_priority < operation_priority) tempL = "(" + tempL + ")";
        if(R->operation_priority < operation_priority) tempR = "(" + tempR + ")";
        
        return tempL + " % " + tempR;
    }
};
class Pow final : Operator2arg {
    type evaluate(){ return pow(R->evaluate(), L->evaluate()); }
    string toString(){
        return "pow(" + L->toString() + ", " + R->toString() + ")";
    }
};
/* #endregion */


int main(){
    Expression *e = new Operator2arg(new Expression(), new Expression());
}
