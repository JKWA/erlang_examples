## Erlang's Concurrency Primitives:

### 1. **Processes**

In Erlang, processes are foundational units of concurrency, more lightweight than typical OS-level threads. Processes have isolated memory.

- **Example:**
  ```erlang
  spawn(fun() -> add(RandomList) end)
  ```
  The `spawn/1` function initiates a new process executing the given function.

### 2. **Message Passing**

Communication between processes is achieved using message passing, eliminating the need for locks.

- **Example:**
  ```erlang
  Caller ! {result, Ref, Result}
  ```
  Here, `!` denotes the message send operation, dispatching the tuple `{result, Ref, Result}` to the `Caller` process.

### 3. **Message Receiving**

Every Erlang process possesses its own message queue. The `receive` construct enables them to handle incoming messages.

- **Example:**
  ```erlang
  receive
      {result, Ref, Result} -> ...
  end
  ```
  This structure awaits a matching message `{result, Ref, Result}` and then processes the corresponding code.

### 4. **Process Monitoring and Links**

Processes can be linked or monitored in Erlang. If one process in a linked pair fails, its counterpart will be terminated as well. Monitoring allows a process to be notified of another's state changes.

- **Example (Monitoring):**
  ```erlang
  MonitorRef = monitor(process, Pid)
  ```
  This command starts monitoring the process identified by `Pid`.

### 5. **Recursion as Iteration**

Being a functional language, Erlang employs recursive functions rather than loops.

- **Example:**
  ```erlang
  loop() ->
      receive ... end,
      loop().
  ```
  The function processes an incoming message and then recursively invokes itself, creating an ongoing message handler.

## Erlang Demo: Asynchronous Summation with Failures

This section illustrates the application of Erlang's concurrency primitives.

### Installing and Running Erlang

#### On MacOS:

Using [Homebrew](https://brew.sh/):

```bash
brew install erlang
```

#### On Windows:

1. Download the installer from [Erlang Solutions website](https://www.erlang-solutions.com/resources/download.html).

2. Follow the on-screen instructions.

---

## Compiling and Utilizing Modules

To execute an Erlang applciation, the `.erl` files need to be compiled into `.beam` bytecode. Start the Erlang shell with `erl`, and use the `c/1` function to compile each module. Next, the application is run using the `start/0` function in the calculator module.

compile erl files

```erlang
c(calculator).
c(sum_list).
```

start calculator

```erlang
calculator:start().
```

Call the add function in the calculator module.

```erlang
calculator:add([1,2,3]).
```

Use the calculator `add/1` function to create unknown crashes in the sum_list process. Note that the process crashes, but the application contiues to work.

```erlang
calculator:add(1).
calculator:add(["ABC"]).
```

Use the `generate_adds/1` to create multiple `add/1` processes.

```erlang
calculator:generate_adds(1000).
```

## `sum_list` Module

**Overview:** The `sum_list` module asynchronously calculates the sum of a number list. It incorporates random crashes to illustrate Erlang's fault tolerance.

### Key Functions:

#### - `sum/3`

Initiates the summation of a number list.

#### - `compute_sum/2`

A helper recursive function computing the sum of a list of numbers.

### Usage:

```erlang
sum_list:sum([1, 2, 3, 4, 5], CallerPid, Ref).
```

## `calculator` Module

**Overview:** The `calculator` module handles asynchronous summation using `sum_list`, demonstrating parallel computation, failure management, and inter-process communication in Erlang.

### Key Functions:

#### - `start/0`

Initializes the `calculator` process.

#### - `add/1`

Begins the summation of a specified number list.

#### - `loop/0`

Waits for and processes messages.

#### - `generate_adds/1`

Generates a number of random lists for summation.

### Usage:

Initiate with `calculator:start()`. For specific summations, use `calculator:add([1, 2, 3, 4, 5])`. To generate `N` random lists for summation, invoke `calculator:generate_adds(N)`.

## "Let It Crash"

In Erlang, the "let it crash" mantra encourages building resilient systems that can quickly recover from failures, rather than trying to prevent every possible error.

### **Intentional Vulnerabilities in `sum_list`**:

1. **Lack of Guards**: No checks validate input types or constraints, trusting the caller to provide correct input.
2. **Random Failures**: Simulated errors might trigger a `known_crash`, showing the unpredictable nature of real-world systems.
3. **Pure Computation Logic**: By keeping error-handling separate, `compute_sum` remains clean, focusing solely on its core functionality.

### **Resilience in `calculator`**:

1. **Process Lifecycle**: If a previously registered `calc` process exists, it's terminated, and a new one spawns, ensuring fresh starts.
2. **Monitoring**: After spawning a `sum_list` process, it's immediately monitored. If it terminates unexpectedly, a `DOWN` message notifies the `calculator`.
3. **Error Handling**:
   - **Known Crashes**: Handled by retrying the summing process up to a defined limit (`?MAX_RETRIES`).
   - **Unknown Crashes**: Acknowledged, logged, and the associated data entry is cleared without retries.
4. **Timeout Management**: No response within 2 seconds prompts a retry, ensuring the system remains responsive.
5. **State Management with ETS**: Temporary storage retains the numbers being summed, facilitating retries even after crashes.
6. **Graceful Termination**: Existing processes are shut down gracefully, ensuring no stale processes or data.
7. **Transparency**: Through logging, the `calculator` provides a clear feedback loop on retries, timeouts, and crashes.

## Supervision between `calculator` and `sum_list`

In Erlang, resilience is implemented with supervision. In this example, the `calculator` acts as the supervisor for `sum_list`.

### **Role of `calculator` as a Supervisor**:

1. **Active Monitoring**: Once the `calculator` spawns a `sum_list` process, it sets up active monitoring. This allows the `calculator` to immediately be alerted if the `sum_list` process terminates, whether normally or due to a crash.

2. **Decision Making on Failures**:

   - **Known Crashes**: If `sum_list` exits with a `known_crash`, the `calculator` perceives this as an expected error. Instead of surrendering, it retries the computation, up to a predefined number of times (`?MAX_RETRIES`).
   - **Unknown Crashes**: If `sum_list` crashes for any other reasons, the `calculator` logs the unexpected termination, cleans up the associated data, and opts not to retry.

3. **Timeout Handling**: The `calculator` doesn't wait indefinitely. If no response from `sum_list` within 2 seconds, it assumes a failure and retries the process.

4. **State Preservation with ETS**: The `calculator` ensures that even in case of failures, the original data (numbers being summed) is preserved. It uses an ETS table for this temporary storage. This data can be leveraged for retries, and once the operation is concluded or all retries are exhausted, the data is cleaned up (cleared).

5. **Feedback Mechanism**: Users are kept in the loop. The `calculator` logs crucial events like retrials, timeouts, or crashes, providing a transparent insight into the supervision process.

### **Potential Vulnerability**:

While the `calculator` supervises `sum_list`, the `calculator` itself is unsupervised. Errors in the `calculator` module result in a crash that requires manually restarting with `calculator:start()`. A typical Erlang application has a hierarchy of supervisors, with the top-level performing a system restart.
