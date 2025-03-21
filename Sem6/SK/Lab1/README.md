[(back)](../)

# Traceroute
## How to run
First execute `make` to compile the program `main.cpp` to `traceroute`. Then run the program with `sudo ./traceroute <ip addr>`. The program will print the IP addresses of the routers between your machine and the destination.

## Default behaviour
The _traceroute_ behaviour can be adjusted by changing valoues of the following variables in the code:
- `MAX_TTL` (default **30**) - maximum number of hops to reach the destination
- `WAIT_TIME_MS` (default 1000) - time to wait for a response packet
- `PACKETS_PER_TTL` (default **3**) - number of packets to send for each TTL distance

## Additional tools
### `run_par_diff.bash`
Executed with `./run_par_diff.bash <program> <arg1> <arg2> ... <argN>`. It runs the program **N** times (in parallel) for each of the given arguments, results are saved in `results_run_parallel` directory.  
Additionally, alongside program, `traceroute` and `ping` is run for each argument.

### `run_par_same.bash`
Executed with `./run_par_same.bash <N> <program> [<args>...]`. It runs the program **N** times (in parallel) with the same arguments, results are saved in `results_run_parallel` directory.
Additionally, alongside program, `traceroute` and `ping` is run for each argument.
