#include <thread>
#include <iostream>
#include "ThreadGuard.hpp"

void bar() { throw 1; }
void foo() {}

int main()
{
  try
  {
    std::thread t(&foo);
    thread_guard guard(t);

    bar();    // rzuca wyjątek
    t.join(); // pomijamy joina
  }
  catch (...)
  {
    std::cout << "Ups! Wyjatek! Na szczescie go zlapalismy... uff!" << std::endl;
  }

  // since c++20
  // try
  // {
  //   std::jthread t(&foo);
    
  //   bar();    // rzuca wyjątek
  // }
  // catch (...)
  // {
  //   std::cout << "Ups! Wyjatek! Na szczescie go zlapalismy... uff!" << std::endl;
  // }
}
