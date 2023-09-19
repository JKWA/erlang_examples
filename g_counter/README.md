# G-Counter in Erlang

The `g_counter` module provides an implementation of the G-Counter (Grow-Only Counter) CRDT (Conflict-Free Replicated Data Type). Each node keeps its local count and nodes achieve a consistent global count by mergeing states.

## CRDTs

**Conflict-Free Replicated Data Type** enable replicas of data on different nodes to be updated independently and merged without conflicts.

### Core Properties

1. **Local Commutativity**: The order in which operations occur does not affect the final state of the CRDT.

2. **Deterministic Merge**: The result will be the same regardless of the order in which states are merged or the history of operations.

3. **Monotonicity**: Once an update is observed, replicas will not roll back to a previous state.

4. **Idempotence**: In distributed systems, messages can be duplicated, lost, or delayed.

5. **Associativity**: Merging A with B and then merging with C is the same as merging A with the result of merging B and C.

6. **Concurrency**: Multiple replicas can be updated simultaneously without the need for locks or immediate synchronization.

Over time, as every replica is merged with others, all replicas become consistent, achieving eventual consistency.

### G-Counter Specifics

Nodes in the G-Counter maintain their individual counts. To aggregate these counts for a global perspective, nodes merge their states. For example, with nodes having counts [2, 0, 3], merging would result in [2, 0, 3]. If one node updates its count to 1 and a subsequent merge occurs, the new state becomes [2, 1, 3].

## Overview

### Project Structure

- `g_counter.erl`: Contains the core logic for the G-Counter, including operations like increment and state merges.
- `g_counter_mgr.erl`: Manager for G-Counters. It coordinates merges, monitors instances for terminations, and handles process restarts.

## Reliability Analysis

- **Downtime**: Each `g_counter` process is expected to experience a downtime of approximately 5 milliseconds every 45 seconds due to its randomized terminations.
- **System Resilience**: The manager (`g_counter_mgr.erl`) is designed to monitor and restart any terminated `g_counter` processes. This ensures a swift recovery mechanism, minimizing data loss.
- **Overall Reliability**: With the CRDT architecture and recovery mechanisms in place, the probability of data loss at any point during a year is virtually zero. While occasionally you will see a process fall behind, the next merge will bring it back into alignment.

## Getting Started

### Prerequisites

Before you start, ensure you have `rebar3` installed. If you don't have it, you can get it from the [official rebar3 website](https://www.rebar3.org/).

### Setup and Running the Application

1. **Compile the Application**:  
   Using rebar3, compile the project.

   ```
   rebar3 compile
   ```

   This step will compile all the Erlang files and fetch dependencies, ensuring everything is ready for execution.

2. **Start the Application Shell**:  
   With the project compiled, you can start the Erlang shell for the application.
   ```
   rebar3 shell
   ```
