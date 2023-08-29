# **Incident Management**

This Erlang application simulates a process for managing incidents with actors.

## **Overview**

When a dispatch operator reports an incident, the information is immediately persisted to disk, and a message is sent to the incident supervisor instructing it to spawn a new incident actor. This actor retrieves the incident data from storage and interacts with the dispatch manager to secure a suitable unit for response. If the incident remains unresolved within a predefined time window (currently 40 seconds), the actor proactively reaches out to the reporting dispatch operator to determine the appropriate next steps. Once an incident is successfully closed, its assigned unit is released, and the corresponding actor is terminated.

In the event that an incident actor unexpectedly crashes, the supervisor will automatically respawn a new actor. This new actor will read the persisted state from storage, check if a unit PID (process identifier) is already associated with the incident, and, if so, skip the unit assignment step and continue working towards resolving the incident.

## **Prerequisites**

### **1. Installing Erlang/OTP**:

- **For MacOS** (using Homebrew):

  ```bash
  brew install erlang
  ```

- **For Windows**: Download the installer from the [official Erlang website](https://www.erlang.org/downloads) and follow the installation instructions.

### **Setup and Initialization**

1. **Start Erlang Shell**:

   ```bash
   erl
   ```

   And here is how you quite Erlang

   ```erlang
   q().
   ```

2. **Compile the processes**:

   If the beam files are not compiled, then compile from within the Erlang shell:

   ```erlang
    c(unit_manager).
    c(dispatch_operator).
    c(incident_supervisor).
    c(incident).
    c(unit).
   ```

   Erlang uses periods like other languages semicolons.

3. **Initialization**:

   Spawn the `unit_manager`, `dispatch_operator` and `incident_supervisor` from within the Erlang shell:

   ```erlang
   unit_manager:start_link().
   dispatch_operator:start_link().
   incident_supervisor:start_link().
   ```

   The `unit` and `incident` actors are spawened by the `unit_manager` and `incident_supervisor`

   Erlang uses the colon like others use periods.

   This is read as, "from unit_manager, call the start_link function".

## **Usage**

### **Report an Incident**

Utilize the `dispatch_operator` to report an incident:

```erlang
dispatch_operator:report_incident(Type, Description, Severity).
```

**Parameters**:

- `Type`: String - Incident type (e.g., `"ems"`).
- `Description`: String - Concise incident description.
- `Severity`: Integer (1-5) - 1 is the least severe, 5 the most.

**Examples**:

```erlang
  dispatch_operator:report_incident("fire", "Fire in the park", 3).
  dispatch_operator:report_incident("police", "Alarm at hardware store", 2).
  dispatch_operator:report_incident("ems", "Accident with injury", 1).
```

**Force an incident to crash**:

You can watch the system reacting to a crash with the dispactch_operator

```erlang
  dispatch_operator:crash_last().
```

### **Incident Alerts**

Should an incident remain unresolved past 40 seconds, the operator receives a prompt:

```
Alert: Incident 'description' has not been resolved yet!
Do you want to close this incident? (y/n):
```

Respond with:

- `y`: The incident is closed and removed from the `dispatch_operator`.
- `n`: The incident remains under monitoring and will prompt again.

## Cleanup Mechanism

When an incident concludes:

- The incident monitoring timer is deactivated.
- The designated unit is apprised of the incident's closure and returned to the available pool.
- The incident actor is terminated.

## System Actors & Their Responsibilities

1. **incident_supervisor**: Monitors the incident actors, and is responsible for respawning a new actor in case of unexpected crashes to ensure the incident progresses towards resolution.
2. **unit_manager**: Responsible for managing the available units and selecting the most suitable one for responding to incidents.
3. **Unit**: Keeps the `unit_manager` informed of its current status and location. Currently, it does not have any other specific objectives.
4. **incident**: Represents a reported incident and is responsible for assigning a response unit and facilitating a swift resolution.
5. **dispatch_operator**: Represents the dispatch operator, whose main responsibilities include reporting incidents and monitoring their progress until resolution.

## Other Functions

### Retrieve assigned incidents:

Review the `dispatch_operator`'s ongoing incidents:

```erlang
dispatch_operator:get_active_incidents().
```

### Retrieve All Units:

```erlang
unit_manager:get_units().
```

### Identify the Preferred Unit:

```erlang
unit_manager:get_preferred_unit().
```
