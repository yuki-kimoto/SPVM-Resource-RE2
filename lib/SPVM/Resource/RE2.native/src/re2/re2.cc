#include <sstream>


#ifndef UTIL_LOGGING_H_
#define UTIL_LOGGING_H_

#define LOG_FATAL LogMessageFatal(__FILE__, __LINE__)
#define LOG_DFATAL LOG_FATAL
#define LOG(severity) LOG_ ## severity.stream()

class LogMessage {
 public:
  LogMessage(const char* file, int line) {
    stream() << file << ":" << line << ": ";
  }
  void Flush() {
    std::string s = str_.str();
  }
  std::ostream& stream() { return str_; }

 private:
  std::ostringstream str_;
};

class LogMessageFatal : public LogMessage {
 public:
  LogMessageFatal(const char* file, int line)
      : LogMessage(file, line) {}
    ~LogMessageFatal() {
    Flush();
    abort();
  }
 private:
  LogMessageFatal(const LogMessageFatal&) = delete;
  LogMessageFatal& operator=(const LogMessageFatal&) = delete;
};

#endif  // UTIL_LOGGING_H_

namespace re2 {

// Deletes this object; ref count has count reached 0.
void Destroy() {
  LOG(DFATAL) << "Bad reference count ";
}

}  // namespace re2
