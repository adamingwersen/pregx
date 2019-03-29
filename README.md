# PREGX

This implementation uses [stack](https://docs.haskellstack.org/en/stable/README/), but relies on very few external libraries.

Build the main file by calling:
```.sh
stack build
``` 

Use the string generator: 
```.sh
stack ghci src/GenerateString.hs
*GenerateString> "A0.3B0.2CDE" ?# 1000
...
```
Although no interface is provided for StateMachine, it can be inspected:
```.sh
stack ghci
> :l StateMachine
*StateMachine> toNFA "A0.3B0.2CDE"
...
```
