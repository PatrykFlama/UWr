#pragma once
#include <queue>
#include <mutex>

struct SafeQueue
{
public:
  struct Entry
  {
    unsigned n_;
    bool     isPrime_;
  };

private:
  using Container = std::queue<Entry>;
  mutable std::mutex m_;    //! mutable to use in const function

public:
  using size_type  = Container::size_type;
  using value_type = Container::value_type;

  void push(const value_type v)
  {
    std::lock_guard<std::mutex> lock(m_);
    q_.push(v);
  }

  value_type pop()
  {
    std::lock_guard<std::mutex> lock(m_);
    const auto tmp = q_.front();
    q_.pop();
    return tmp;
  }

  bool empty() const
  {
    std::lock_guard<std::mutex> lock(m_);
    return q_.empty();
  }

  size_type size() const
  {
    std::lock_guard<std::mutex> lock(m_);
    return q_.size();
  }

private:
  Container q_;
};
