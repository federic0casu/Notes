  Java: Threads and Synchronizers  
===================================

## Java Threads

Before we can start addressing Java Threads, we should state what is a thread: a **thread** is a sequence of instructions that can be executed independently from other sequences of instructions. The execution of a thread can potentially occur in parallel with the main flow of execution. 

From a programmer's point of view, a sequence of instructions can be seen as a **task**. So, we can think about threads as entities of a program that can execute tasks task independently from each other, possibly in a parallel way.

### How are threads organized in Java? (Memory)
The Java memory is composed of 3 different areas:

  1. **Method Area**. Within the method area, when the program is running, all the classes are stored. To be precise, the method area stores the code's method of each class defined within the program scope, as well as the `Running Constant Pool` (a sort of symbol table) and all the classes' attributes. The method area is shared among all the threads of a program.
  2. **Heap**. The heap represents the dynamic memory of a program. Similar to the method area, the heap is shared among all the threads. At runtime, the program uses the heap to store all the newly created objects. The JVM performs garbage collection on it.
  3. **Stack Area**. The JVM is a stack machine: each method call will be carried out creating a frame on which the caller places the parameters. As well as other programming languages, each thread has its own stack area.

### How are threads created and started? (`Thread` and `Runnable`)
Basically, there are two ways of creating and running a thread:

  1. `Thread` class. By extending the `Thread` class, another class inherits all the methods useful to manage a thread. In more detail:
    a. `run()`: by overriding `run()`, the extended class defines a task to be carried out during the thread's execution.
    b. `start()`: a method inherited from the `Thread` class used to schedule the thread's execution. The JVM scheduler is in charge of managing thread scheduling and thread execution.
  2. `Runnable` interface. Such way is a far from flexible mechanism: it allows the programmer to implements such interface, define an anonymous class or via 0-parameter lambda.

Example 1: extending `Thread`
```java
public class MyThread extends Thread {
  @Override
  public void run() {
    System.out.println("Hello, I'm " + Thread.currentThread().getId());
  }
}

public class Main {
  public static void main(String[] args) {
    MyThread myThread = new MyThread();
    myThread.start();
  }
}
```

Example 2: `Runnable` is very flexible...
```java
public class MyThread implements Runnable {
  public void run() {
    System.out.println("Hello, I'm " + Thread.currentThread().getId());
  }
}

public class Main {
  public static void main(String[] args) {
    MyThread myThread = new MyThread();
    myThread.start();

    Runnable task = () -> {
      System.out.println("Hello, I'm " + Thread.currentThread().getId());
    }

    task.run()

    Thread thread = new Thread(task);
    thread.start()
  }
}
```
In the example, we can see how `task` is a `Runnable` object instantiated using a lambda with no parameters and the specified body. If we invoke the `run()` method on `task`, this is not a spawning action: it is executed by the `Thread` that instantiated the `task` object. If we want to spawn a different `Thread`, we have to use the `Thread` constructors that takes as input a Runnable object and than use the `start()` function on the new object.

### How to stop a thread
Back in the days, there were two mechanism to stop a thread:

  1. `stop()`: The stopped thread releases all its locks and then stops. Such a mechanism may lead to state inconsistencies.
  2. `suspend()`: The thread is temporarily stopped. A thread can be "resumed" by invoking `resume()` on it. Such mechanism is deadlock-prone: if a thread stops without releasing its locks, another running thread could not acquire the locks and is force to wait.

Java developers implements a thread interruption policy based on thread inter-collaboration: a thread can be interrupted by another thread by setting its `interrupted` flag: it’s up to the interrupted thread whether to stop or not, without being obliged to "suicide". A thread can invoke the `interrupt()` method to interrupt a thread; then, the interrupted thread, can check its status by accessing its own `interrupt` flag with the help of `Thread.interrupt()` static method. The latter will clear and read the flag. Also, a thread can check the status of another thread using `isInterrupted()` method: the latter will not modify the flag.



## Thread Pooling and the Executor Framework
It is often the case that our application needs to execute a bunch of tasks. We already saw that, thanks to the `Runnable` interface, the developer has a mechanism to define tasks. The major concern of a developer facing tasks is the following: discerning what has to be done from how has to be done. 

Java offers a very powerful mechanism: `Executor`s. The `Executor` interface offers an API that can be used to manage task and its execution. Thanks to the `Executor` interface, the developer can manage more easily (and better) the task's definition and the task execution.

Let's make an example: consider a server which expose a HTTP-based service. Each incoming HTTP request should be handled as fast as possible. One possible solution could be to spawn a new thread for each new incoming request. The thread's task is very simple: to handle the HTTP request.

```java
@WebServlet("/api/service" )
public class Server extends HttpServlet {
  private static final Executor executor = new CustomExecutor(); 
  @Override 
  protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
    Task task = new Task(response);
    
    executor.execute(task);
  }
  private class Task implements Runnable {
    private final HttpServletResponse response;
    public Task(HttpServletResponse response) {
      this.response = response;
    } 
    @Override
    public void run() {
      response.setContentType("text/html");

      try (PrintWriter out = response.getWriter()) {
        out.println("<html><body>");
        out.println("<h2>Hello from WebServlet!</h2>");
        out.println("</body></html>");
      } catch (IOException e) {
        e.printStackTrace();
      }
    }
  }
  private static class CustomExecutor implements Executor {
    public void execute(Runnable command) {
      new Thread(command).start();
    }
  }
}
```

The Java Server is very simple: for each new incoming request, the server will instantiate a new task (`Task task = new Task(response);`) and then execute it by means of the `executor`. The `executor` implementation will spawn a new thread for each task.

Even though, as it stands, developing a multithreaded application would be easy, the mechanism is not very efficient. There are at least three drawbacks that we are not taking into account:

  1. **Context switches**: Not only do they take time, but they also cause cache flushes that slow down subsequent execution.
  2. **Lock contention**: This determines task serialization (lack of parallelism), hampering scalability by increasing thread waiting time.
  3. **Memory synchronization**: Many threads may need to see the latest updates.

A better solution would be to fix the maximum number of working threads using a technique called thread pooling. The idea is simple:

> Keep some threads ready to execute upcoming tasks and, upon completion of each execution, make the thread available again for another task.

`java.util.concurrent.Executors` has factory methods to create instances of executors with pooled threads, e.g.:

  1. `newFixedThreadPool(int n)` creates a pool of `n` threads.
  2. `newCachedThreadPool()` provides a thread pool that creates new threads as needed, but will reuse previously constructed threads when they are available.
  3. `newWorkStealingPool(int parallelism)` creates a pool with dynamically managed threads, able to handle the specified level of parallelism; the task execution order does not necessarily reflect the order of submission.

For now, we have presented a mechanism to define and submitting to execution a task. How can we manage a result of a task execution? Java `Future`: the latter is an interface that made possible task's results handling. A `Future` can be considered as a *proxy* for a result that has been computed asynchronously with respect to the thread that submitted the task. 

The problem here is that we delegate the execution of a task to someone else and this task will provide at the end a result, but we don’t know when this result will be available to us. One possibility is to stop while waiting for it, but it wastes time: the solution is in the way the task is submitted, because when we submit the task for execution we can ask to get back an object whose type is `Future<V>`: an object which will include the result as soon as it will be available. In this way, we can avoid to block and waste time: we delegate `Future<V>` to check when the result is available.

The result produced by the thread can be later retrieved by the `Futures method `get()`. Also, the thread status can be checked by means of the `Future` object that will return. The method which is in charge of managing the task result can check the executor thread's status calling the method `isDone()` on the `Future` object. Also, we can stop the task execution by means of `cancel()`'s `Future` method.

```java
public interface Future<V> {
  boolean isDone();
  V get(); //also a timed version exists
  boolean cancel(boolean mayInterruptIfRunning);
  boolean isCancelled();
}
```

If we want to shut down an executor, we need to take care: the relative threads might possibly be still running at that time. 
Distinct operations, as methods of ExecutorService:

  1.`shutdown()`starts the shutdown procedure; previously submitted tasks are completed, and no new task will be accepted.
  2.`shutdownNow()`: only currently executing tasks are completed. Check state by using `isShutdown()`.
  3. `isTerminated()` checks if all tasks have been terminated.

Methods above are not-blocking: to wait for completion of all tasks, call `waitTermination()`.

## Java Synchronizers

When we have threads that running at the same time, often they require to cooperate for one reason or another, requiring *synchronization*. A construct designed to address a specific synchronization pattern is called `Synchronizer` and it  coordinates the control flow of the threads based on its internal state. Java provides several classes for common special-purpose synchronization.