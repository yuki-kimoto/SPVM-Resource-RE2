#include <sstream>

class LogMessage {
 public:
  LogMessage() {
  
  }
  std::ostream& stream() { return str_; }

 private:
  std::ostringstream str_;
};

namespace re2 {

// Deletes this object; ref count has count reached 0.
void Destroy() {
  LogMessage().stream();
}

}  // namespace re2
