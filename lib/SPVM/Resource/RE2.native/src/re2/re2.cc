#include <sstream>

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

namespace re2 {

// Deletes this object; ref count has count reached 0.
void Destroy() {
  LogMessage(__FILE__, __LINE__).stream();
}

}  // namespace re2
