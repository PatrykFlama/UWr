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

    // Expression(Expression&& other) = default;
    // Expression& operator=(Expression&& other) = default;
    // Expression(const Expression& other) = delete;
    // Expression& operator=(Expression& other) = delete;

    type evaluate(){
        cout <<"EVAL\n";
        return 0;
    }

    string toString(){
        cout << "TS\n";
        return "";
    }

    void setVariable(string name, type val){
        variables[name] = val;
    }

    type getVariable(string name){
        auto ptr = variables.find(name);
        if(ptr == variables.end()) throw runtime_error("Variable " + name + " has no assigned value!");
        return ptr->second;
    }

    void deleteVariable(string name){
        variables.erase(name);
    }
};

/* #region //* operands */
class Number final : public Expression {
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

class Constant final : public Expression {
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

class Variable final : public Expression{
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
    Expression *e;
public:
    Operator1arg(Expression* _e) : e(_e) {}
    ~Operator1arg(){ delete e; }

    void setVariable(string name, type val){
        e->setVariable(name, val);
    }
    type getVariable(string name){
        return e->getVariable(name);
    }
    void deleteVariable(string name){
        e->deleteVariable(name);
    }
};

class Sin final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return sin(e->evaluate()); }
    string toString(){ return "sin(" + e->toString() + ")"; }
};
class Cos final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return cos(e->evaluate()); }
    string toString(){ return "cos(" + e->toString() + ")"; }
};
class Exp final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return exp(e->evaluate()); }
    string toString(){ return "exp(" + e->toString() + ")"; }
};
class Ln final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return log(e->evaluate()); }
    string toString(){ return "ln(" + e->toString() + ")"; }
};
class Abs final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return abs(e->evaluate()); }
    string toString(){ return "|" + e->toString() + "|"; }
};
class Opposite final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return -(e->evaluate()); }
    string toString(){ return "opposite(" + e->toString() + ")"; }
};
class Inverse final : public Operator1arg {
public:
    using Operator1arg::Operator1arg;
    type evaluate(){ return 1/(e->evaluate()); }
    string toString(){ return "inverse(" + e->toString() + ")"; }
};
/* #endregion */


/* #region //* operators 2arg */
class Operator2arg : public Expression{
protected:
    Expression *L, *R;
public:
    Operator2arg(Expression* _L, Expression* _R) : L(_L), R(_R) {}
    ~Operator2arg(){
        delete L;
        delete R;
    }

    void setVariable(string name, type val){
        L->setVariable(name, val);
        R->setVariable(name, val);
    }
    type getVariable(string name){
        return L->getVariable(name);
    }
    void deleteVariable(string name){
        L->deleteVariable(name);
        R->deleteVariable(name);
    }
};

class Add final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
    int operation_priority = 0;
    type evaluate(){ return L->evaluate() + R->evaluate(); }
    string toString(){ return L->toString() + " + " + R->toString(); }
};
class Sub final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
    int operation_priority = 0;
    type evaluate(){ return L->evaluate() - R->evaluate(); }
    string toString(){ return L->toString() + " - " + R->toString(); }
};
class Mult final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
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
class Div final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
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
class Log final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
    type evaluate(){ return log(R->evaluate()) / log(L->evaluate()); }
    string toString(){
        return "log(" + L->toString() + ", " + R->toString() + ")";
    }
};
class Mod final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
    type evaluate(){ return fmod(R->evaluate(), L->evaluate()); }
    string toString(){
        string tempL = L->toString();
        string tempR = R->toString();

        if(L->operation_priority < operation_priority) tempL = "(" + tempL + ")";
        if(R->operation_priority < operation_priority) tempR = "(" + tempR + ")";
        
        return tempL + " % " + tempR;
    }
};
class Pow final : public Operator2arg {
public:
    using Operator2arg::Operator2arg;
    type evaluate(){ return pow(R->evaluate(), L->evaluate()); }
    string toString(){
        return "pow(" + L->toString() + ", " + R->toString() + ")";
    }
};
/* #endregion */


int main(){
    Sub *e = new Sub(new Number(1), new Number(2));
    cout << e->toString() << ' ' << e->evaluate() << '\n';
}
