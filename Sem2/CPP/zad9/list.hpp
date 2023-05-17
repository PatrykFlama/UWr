#pragma once
#include <bits/stdc++.h>
#include "compare.hpp"
using namespace std;


namespace calc{
    template <class T>
    class List{
        class Node{
            public:
                T data;
                Node* next;
                Node(T data){
                    this->data = data;
                    this->next = nullptr;
                }
        };

    protected:
        Node* front;
        int size;

    public:
        /* #region //* constructors */
        List(){
            this->front = nullptr;
            this->size = 0;
        }
        List(initializer_list<T> list){
            this->front = nullptr;
            this->size = 0;
            for(auto it : list){
                this->push_back(it);
            }
        }
        ~List(){
            Node* here = this->front;
            while(here != nullptr){
                Node* tmp = here;
                here = here->next;
                delete tmp;
            }
        }
        List(const List& list){
            this->front = nullptr;
            this->size = 0;
            Node* here = list.front;
            while(here != nullptr){
                this->push_back(here->data);
                here = here->next;
            }
        }
        List(List&& list){
            this->front = list.front;
            this->size = list.size;
            list.front = nullptr;
            list.size = 0;
        }
        List operator=(const List& list){
            Node* here = this->front;
            while(here != nullptr){
                Node* tmp = here;
                here = here->next;
                delete tmp;
            }
            this->front = nullptr;
            this->size = 0;
            here = list.front;
            while(here != nullptr){
                this->push_back(here->data);
                here = here->next;
            }
        }
        List operator=(List&& list){
            Node* here = this->front;
            while(here != nullptr){
                Node* tmp = here;
                here = here->next;
                delete tmp;
            }
            this->front = list.front;
            this->size = list.size;
            list.front = nullptr;
            list.size = 0;
        }
        /* #endregion */

        /* #region //* push */
        void push_pos(T data, int pos){     // pos: 0 [front] 1 [front->next] 2 ... size-1 [back] size
            Node* prev = nullptr;
            Node* here = this->front;
            while(pos > 0){
                if(here == nullptr){ throw runtime_error("push_pos: index out of range"); }
                prev = here;
                here = here->next;
                pos--;
            }
            Node* tmp = new Node(data);
            tmp->next = here;
            if(prev != nullptr) prev->next = tmp;
            else this->front = tmp;
            this->size++;
        }

        void push_back(T data){
            push_pos(data, this->size);
        }

        void push_front(T data){
            push_pos(data, 0);
        }
        /* #endregion */

        /* #region //* pop */
        void pop(int pos){
            Node* prev = nullptr;
            Node* here = this->front;
            while(pos > 0){
                if(here->next == nullptr){ throw runtime_error("pop: index out of range");}
                prev = here;
                here = here->next;
                pos--;
            }
            if(prev != nullptr) prev->next = here->next;
            else this->front = here->next;
            delete here;
            this->size--;
        }

        void pop_back(){
            pop(this->size - 1);
        }

        void pop_front(){
            pop(0);
        }

        void pop_data(T data){
            Node* prev = nullptr;
            Node* here = this->front;
            while(here != nullptr){
                if(here->data == data){
                    if(prev != nullptr) prev->next = here->next;
                    else this->front = here->next;
                    delete here;
                    this->size--;
                    return;
                }
                prev = here;
                here = here->next;
            }
            throw runtime_error("pop_data: data not found");
        }

        void pop_all_data(T data){
            Node* prev = nullptr;
            Node* here = this->front;
            while(here != nullptr){
                if(here->data == data){
                    if(prev != nullptr) prev->next = here->next;
                    else this->front = here->next;
                    delete here;
                    this->size--;
                }else{
                    prev = here;
                }
                here = here->next;
            }
        }
        /* #endregion */

        /* #region //* other */
        T get(int pos){
            Node* here = this->front;
            while(pos > 0){
                if(here->next == nullptr) throw runtime_error("get: index out of range");
                here = here->next;
                pos--;
            }
            return here->data;
        }

        int pos_data(T data){
            Node* here = this->front;
            int pos = 0;
            while(here != nullptr){
                if(here->data == data) return pos;
                here = here->next;
                pos++;
            }
            throw runtime_error("pos_data: data not found");
        }

        int count_data(T data){
            Node* here = this->front;
            int count = 0;
            while(here != nullptr){
                if(here->data == data) count++;
                here = here->next;
            }
            return count;
        }

        int get_size(){
            return this->size;
        }

        bool empty(){
            return this->size == 0;
        }

        friend ostream& operator<<(ostream& os, const List& list){
            Node* here = list.front;
            while(here != nullptr){
                os << here->data << " ";
                here = here->next;
            }
            return os;
        }
        /* #endregion */
    };

    template <class T>
    class Stack : protected List<T>{
    public:
        void push(T data){
            this->push_front(data);
        }

        void pop(){
            this->pop_front();
        }

        T top(){
            if(this->size == 0) throw runtime_error("top: stack is empty");
            return this->get(0);
        }

        int get_size(){
            return this->size;
        }

        bool empty(){
            return this->size == 0;
        }
    };

    template <class T>
    class Queue : protected List<T>{
    public:
        void push(T data){
            this->push_back(data);
        }

        T pop(){
            if(this->size == 0) throw runtime_error("front: queue is empty");
            T res = this->get(0);
            this->pop_front();
            return res;
        }

        int get_size(){
            return this->size;
        }

        bool empty(){
            return this->size == 0;
        }
    };
    
    template <typename T, class Trait = compare_types<T>>
    bool is_sorted(List<T> list){
        for(int i = 0; i < list.get_size() - 1; i++){
            if(Trait::leq(list.get(i), list.get(i + 1))) return false;
        }
        return true;
    }

    template <typename T, class Trait = compare_types<T>>
    void sort(List<T> list){
        for(int j = 0; j < list.get_size() - 1; j++){
            for(int i = j+1; i < list.get_size(); i++){
                if(Trait::leq(list.get(i), list.get(j))){
                    T tmp = list.get(i);
                    list.pop(i);
                    list.push_pos(tmp, j);
                }
            }
        }
    }
}
