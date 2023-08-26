# **Incident Management**

This Erlang application simulates a process for managing incidents with actors.

## **Overview**

Operators can use this system to manage reported incidents. Once an incident is reported, a dedicated incident actor is spawned. This actor interacts with the dispatch manager to secure a suitable unit for the incident. If the incident remains unresolved within a pre-defined time window (40 seconds), the actor reaches out to the operator to determine further actions. An incident's closure releases its assigned unit, and the actor is terminated.

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
    c(incident).
    c(unit).
   ```

   Erlang uses periods like other languages semicolons.

3. **Initialization**:

   Spawn the `unit_manager` and `dispatch_operator` from within the Erlang shell:

   ```erlang
   unit_manager:start_link().
   dispatch_operator:start_link().
   ```

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

1. **unit_manager**: Manages units and seeks the best fit for incidents.
2. **Unit**: Notifies the `unit_manager` of its status and position. As of now, doesn't pursue any specific goals.
3. **incident**: Embodies a reported incident, strives to assign a unit and resolve swiftly.
4. **dispatch_operator**: Represents the operator, aspires to report and oversee incidents.

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

### Modify Availability for a Specific Unit:

```erlang
Pid = <The_PID_of_the_Unit>.
Availability = true | false.
unit_manager:unit_availability_changed(Pid, Availability).
```
