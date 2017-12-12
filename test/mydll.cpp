extern "C" {
  typedef void(*NodeCallback)(int);

  _declspec(dllexport) int twice(int a) {
    return a * 2;
  }

  _declspec(dllexport) int doSomething(NodeCallback callback, int x) {
    // do something
    x *= 2;

    callback(x);

    return 0;
  }
}