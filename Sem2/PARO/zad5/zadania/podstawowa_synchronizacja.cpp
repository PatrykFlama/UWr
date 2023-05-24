#include <thread>
#include <vector>
#include <string>
#include <typeinfo>
#include <iostream>
#include <sstream>
#include <mutex>
#include <chrono>

std::string output;
std::mutex outputMutex;

void addLines(unsigned count)
{
  std::ostringstream ss;
  ss << std::this_thread::get_id();

  // manual lock and unlock
  // for(unsigned i=0; i<count; ++i)
  // {
  //   outputMutex.lock();
  //   output += ss.str();
  //   output += ": #";
  //   output += std::to_string(i);
  //   output += "\n";
  //   outputMutex.unlock();
  // }
  // lock guard verison
  for(unsigned i=0; i<count; ++i)
  {
    std::lock_guard<std::mutex> lock(outputMutex);
    output += ss.str();
    output += ": #";
    output += std::to_string(i);
    output += "\n";
  }
}


int main()
{
  auto start = std::chrono::high_resolution_clock::now();

  try
  {
    constexpr auto threadsCount = 3;
    constexpr auto opertaions   = 10000000;

    std::vector<std::thread> threads;
    threads.reserve(threadsCount);
    for(auto i=0; i<threadsCount; ++i)
      threads.emplace_back(addLines, opertaions/threadsCount);

    for(auto& th: threads)
      th.join();

    // std::cout << output << std::endl;
  }
  catch(std::exception const &ex)
  {
    std::cerr << "Oops: " << ex.what() << " (" << typeid(ex).name() << ")" << std::endl;
    return 42;
  }

  auto end = std::chrono::high_resolution_clock::now();
  auto time = end - start;
  std::cout << "Program took " << time/std::chrono::milliseconds(1) << " ms to run.\n";
}
