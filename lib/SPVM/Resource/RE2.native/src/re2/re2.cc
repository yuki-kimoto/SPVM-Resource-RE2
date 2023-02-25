#include <sstream>


#ifndef UTIL_LOGGING_H_
#define UTIL_LOGGING_H_

#define LOG_FATAL LogMessageFatal(__FILE__, __LINE__)
#define LOG_DFATAL LOG_FATAL
#define LOG(severity) LOG_ ## severity.stream()

class LogMessage {
 public:
  LogMessage(const char* file, int line)
      : flushed_(false) {
    stream() << file << ":" << line << ": ";
  }
  void Flush() {
    stream() << "\n";
    std::string s = str_.str();
    size_t n = s.size();
    if (fwrite(s.data(), 1, n, stderr) < n) {}  // shut up gcc
    flushed_ = true;
  }
  ~LogMessage() {
    if (!flushed_) {
      Flush();
    }
  }
  std::ostream& stream() { return str_; }

 private:
  bool flushed_;
  std::ostringstream str_;
};

// Silence "destructor never returns" warning for ~LogMessageFatal().
// Since this is a header file, push and then pop to limit the scope.
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4722)
#endif

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

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#endif  // UTIL_LOGGING_H_

namespace re2 {

// Deletes this object; ref count has count reached 0.
void Destroy() {
  LOG(DFATAL) << "Bad reference count ";
}

}  // namespace re2
