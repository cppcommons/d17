#include <nan.h>

namespace demo {
  NAN_METHOD(Method) {
    info.GetReturnValue().Set(Nan::New("world").ToLocalChecked());
  }

  NAN_MODULE_INIT(init) {
    Nan::SetMethod(target, "hello", Method);
  }

  NODE_MODULE(addon, init)
}