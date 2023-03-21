#include <cstdio>
#include <memory>
#include <string>

struct Logger
{
    Logger(const char* path) : logfile(std::fopen(path, "wb"))
    {
        std::fputs("Starting logger...\n", logfile);
    }
    Logger(const Logger& other) = delete;
    Logger& operator=(const Logger& other) = delete;

    void log(std::string text) { std::fputs(text.c_str(), logfile); }

private:
    std::FILE* logfile;
};

std::shared_ptr<Logger> createLogger()
{
    return std::make_shared<Logger>("log.txt");
}

struct Service
{
    Service(std::string name, std::shared_ptr<Logger> logger)
        : text_(name + " is working\n"),
          logger_(logger)
    {}

    void doStuff() const { logger_->log(text_); }

private:
    std::string text_;
    std::shared_ptr<Logger> logger_;
};

int main()
{
    auto logger = createLogger();
    auto serviceA = Service("A", logger);
    auto serviceB = Service("B", logger);
    auto serviceC = Service("C", logger);

    for (const auto& s : {serviceA, serviceB, serviceC}) { s.doStuff(); }
}
