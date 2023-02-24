#include "spvm_native.h"

#include "re2/re2.h"

extern "C" {

int32_t SPVM__TestCase__Resource__RE2__test(SPVM_ENV* env, SPVM_VALUE* stack) {
  (void)env;
  (void)stack;
  
  stack[0].ival = 1;
  
  return 0;
}

}
