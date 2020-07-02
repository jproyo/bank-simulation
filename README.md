# Bank Simulation Challenge
This project are divided into 2 parts:

- `src`: Folder containing a Generic Simulation Library based on an Event Scheduling simulation model.
- `app`: It is the instantiation of that Library for the particular problem of Bank simulation system.

# Assumptions
As in any other software or solution there are a couple of assumptions to be made in order to explain some decisions taken during the design and development process.

1. **Simulation Model**: The simulation library design here is a representation of an [Event Scheduling Simulation Model](https://en.wikipedia.org/wiki/Discrete-event_simulation)
2. **Unit time**: This has not been parameterized and all the units time are in terms of `Integer` types in which each Integer Unit represents a second virtual clock, starting in 1. This is similar for example to [GPSS](https://en.wikipedia.org/wiki/GPSS) in which arrivals, servers, queues, seized and other resources of the system are design in terms of discrete `Integer` counters.
3. **Extensibility**: The model is extensible on **Type Classes** instances in which we can provide different distributions for generating the Events Series and the Service times. Also we can generate deterministically the event series for a testing purpose.
4. **Modularity**: The solution is divided in two parts, one with the `simulation` library that is used by the `bank-simulation` application. In this sense the solution can simulate any problem, **not only** the **Bank - Teller - Queue** problem.
5. **Servers**: Servers represents the **resources** in the system that has the capability to provide some service to the **Entities**. For a matter of time I have only implemented a Simulation System with 1 SERVER but it can be easily be extended to multiple.

# Configuration

There are 2 kind of configuration in this project:

- **Bank Simulation Generic Configuration**: This configuration represents the different type of customers with the Beta Distribution parameters, the maximum amount of time units this simulation is going to run, and the ![formula](https://render.githubusercontent.com/render/math?math=\alpha) parameter value. You can see an example in [`app/config.yaml`](./app/config.yaml)
- **Bank Simulation Running Instance Configuration**: This configuration is provided only by command line arguments to the program and it is mainly for selecting the type of Customer `Yellow | Red | Blue` and the results that you want to show, for example the **average waiting time per customer**.

## Example Yaml Config file

```yaml
max_running_time_in_sec: 28800

yellow:
  alpha: 2
  beta: 5

blue:
  alpha: 5
  beta: 1

red:
  alpha: 2
  beta: 2

exp_distribution:
  alpha: 100
```

## Command line Arguments

If you run the executable without any arguments you are going to be prompted with the help:

```shell
> stack exec bank-simulation

----- BANK SIMULATION PROGRAM ------

Missing: --fileConfig STRING --custType CUSTOMER

Usage: bank-simulation --fileConfig STRING --custType CUSTOMER [--showAvgWait]
                       [--showAvgQueue] [--showMaxWait] [--showMaxQueue]
                       [--showDiff] [--showAll]
```

All parameters that are between *square brackets* are optional.

- `--fileConfig`: It is the location in your file system of the Configuration Yaml file describe in [section](#command-line-arguments)
- `--custType`: Customer Type for this simulation instance. Could be `Yellow | Red | Blue`
- `--showAvgWait`: Show in the Result the Average Waiting Time in the system
- `--showAvgQueue`: Show in the Result the Average Queue Length in the System. In this case the Average Queue in front of the teller.
- `--showMaxWait`: Show the Maximum Waiting time in the System
- `--showMaxQueue`: Show the Maximum Queue Length in the System.
- `--showDiff`: Show the difference between the maximum waiting time and the average.
- `--showAll`: Enable all the previous results.


Example:

```shell
> stack exec bank-simulation -- --fileConfig app/config.yaml --custType Blue --showAvgWait

----- BANK SIMULATION PROGRAM ------


------- BANK CONFIG SELECTION ------
Customer Type: Blue
Show Average Wait Time: True
Show Average Queue Length: False
Show Maximum Wait Time: False
Show Maximum Queue Length: False
Show Diff between Average and Maximum: False
------------------------------------

 ----- SIMULATION CONFIG ------
Max Running Time in Sec: 28800
Beta Distribution: \alpha[5] - \beta[1]
Exponential Distribution: \alpha[100]
 ------------------------------

--------- BANK SIMULATION RESULT ----------

Average Waiting Time: 74

-------------------------------------------
```

# Running Simulation

## Prerequisites

In order to run this solution you are going to need the following distributions installed.

- Stack 2.1.3+ | GHC 8.6.3

## Configuration File

There is an already configuration file provided in `app/config.yaml` which contains the general configuration of the problem

## Bank Simulation

For Getting the help of the system

```shell
> stack build
> stack exec bank-simulation -- -help
```

