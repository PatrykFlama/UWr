#include <bits/stdc++.h>
#include "compare.hpp"
#include "list.hpp"
using namespace std;


int main(){
    cout << "Testing list:\n";
    calc::List<int> *list = new calc::List<int>{1, 2, 3, 4, 5};
    cout << *list << "\n";
    list->push_back(7);
    cout << *list << "\n";
    list->push_front(0);
    cout << *list << "\n";
    list->push_pos(6, 6);
    cout << *list << "\n";
    list->push_pos(5, 5);
    cout << *list << "\n";
    list->push_pos(2, 2);
    list->push_pos(2, 2);
    list->push_pos(2, 2);
    list->push_pos(2, 2);
    cout << *list << "\n";
    list->pop_back();
    cout << *list << "\n";
    list->pop_front();
    cout << *list << "\n";
    list->pop(3);
    cout << *list << "\n";
    list->pop_data(2);
    cout << *list << "\n";
    list->pop_all_data(5);
    cout << *list << "\n";
    cout << list->pos_data(2) << "\n";
    cout << list->count_data(2) << "\n";
    cout << list->get_size() << "\n";
    cout << list->empty() << "\n";

    cout << calc::is_sorted<int>(*list) << '\n';
    calc::sort<int>(*list);
    cout << *list << '\n';
    delete list;

    cout << "\nTesting stack:\n";
    calc::Stack<int> *stack = new calc::Stack<int>;
    stack->push(1);
    stack->push(2);
    stack->push(3);
    cout << stack->top() << "\n";
    stack->pop();
    cout << stack->top() << "\n";
    stack->pop();
    cout << stack->top() << "\n";
    stack->pop();
    cout << stack->get_size() << "\n";
    cout << stack->empty() << "\n";
    delete stack;

    cout << "\nTesting queue:\n";
    calc::Queue<int> *queue = new calc::Queue<int>;
    queue->push(1);
    queue->push(2);
    queue->push(3);
    cout << queue->pop() << "\n";
    cout << queue->pop() << "\n";
    cout << queue->pop() << "\n";
    cout << queue->get_size() << "\n";
    cout << queue->empty() << "\n";
    delete queue;
}
