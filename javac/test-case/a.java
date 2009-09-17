public class program {

  int x, y;

  int fib(int x) {
    if ( x == 0 || x == 1)
      return 0;
    else
      return fib(x - 1) + fib(x - 2);
  };

  int sum(int x) {
    int i, sum;

    for(i = x; i >= 0; i = i + 1)
      sum = sum + i;

    return sum;
  };

  int sqrt(double x, double error) {
    double approx, slope;

    approx = x / 2;

    while ((approx * approx) > (x - error) ||
	   (approx * approx) < (x + error)) {
      slope = -(2 * approx);
      approx = approx + approx * slope;
    };

    return approx;
  };

}

