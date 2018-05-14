interface Counter {
  public int invoke();
}

class Counter1 implements Counter {
  // Flesh out this skeleton                                                                                    
}

class Counter2 implements Counter {
  // Flesh out this skeleton                                                                                    
}

class Counter3 implements Counter {
  // Flesh out this skeleton                                                                                    
}

public class Counters {

  public static void testCounters(Counter a, Counter b) {
    System.out.println(a.invoke());
    System.out.println(b.invoke());
    System.out.println(a.invoke());
  }

  public static void main (String [] args) {
    System.out.println("testCounters(new Counter1(), new Counter1()):");
    testCounters(new Counter1(), new Counter1());
    System.out.println("testCounters(new Counter2(), new Counter2()):");
    testCounters(new Counter2(), new Counter2());
    System.out.println("testCounters(new Counter3(), new Counter3()):");
    testCounters(new Counter3(), new Counter3());
  }

}
