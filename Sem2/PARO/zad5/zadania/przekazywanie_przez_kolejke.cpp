#include <thread>
#include <typeinfo>
#include <iostream>
#include "Queue.hpp"
#include "isPrime.hpp"
#include <mutex>
#include <condition_variable>
#include "SafeQueue.hpp"

constexpr auto range = 1000u*1000u;
Queue queue;
SafeQueue safequeue;
std::mutex mtx;
std::condition_variable cv;


void producer()
{
  for(unsigned i=0; i<range; ++i){
    std::lock_guard<std::mutex> lock(mtx);
    queue.push( Queue::value_type{i, isPrime(i)} );
    cv.notify_one();
  }
}

void safeproducer(){
    for(unsigned i=0; i<range; ++i){
        safequeue.push( SafeQueue::value_type{i, isPrime(i)} );
        cv.notify_one();
    }
}


void consumer()
{ 
  for(unsigned i=0; i<range; ++i)
  {
    std::unique_lock<std::mutex> lock(mtx);
    cv.wait(lock, []{ return !queue.empty(); });
    const auto e = queue.pop();
    if(e.isPrime_) std::cout << e.n_ << " is " << (e.isPrime_?"":"not ") << "a prime number" << std::endl;
  }
}

void safeconsumer(){
    for(unsigned i=0; i<range; ++i)
    {
        std::unique_lock<std::mutex> lock(mtx);
        cv.wait(lock, []{ return !safequeue.empty(); });
        const auto e = safequeue.pop();
        if(e.isPrime_) std::cout << e.n_ << " is " << (e.isPrime_?"":"not ") << "a prime number" << std::endl;
    }
}


int main()
{
  try
  {
    // std::thread prod(producer);
    // std::thread cons(consumer);
    std::thread prod(safeproducer);
    std::thread cons(safeconsumer);
    prod.join();
    cons.join();
  }
  catch(std::exception const &ex)
  {
    std::cerr << "Oops: " << ex.what() << " (" << typeid(ex).name() << ")" << std::endl;
    return 42;
  }
}
