# Label : 
for GOTO statements : 
```
(LOOP)
...
@LOOP 
0 ; jmp 
```

# Variable Symbol : 
```
@i 
@sum 
```
# Registers : 

## D : 
the reg for calcs 
 
## A : 
represents line in the RAM 

## M : 
The reg for the value of LINE A in the RAM 

# Commands : 

# Type A : 
enters value to reg A 
|command | description | 
|------------|------------|
|```@<const>```|enters const value to reg A |
|||

```
Examples : 
E1 : 
@17 //A=17
D=A //D=17 
E2 : 
@17 
D=M//D=RAM[17]
E3 : 
@17 
JMP // jump to ROM[17]
E4 : 
@20 
M=0 // RAM[20]=0
E5 : 
@25 
D = M //D=Ram[25]=3 
M = D+1 // Ram[25]=3+1 = 4


```

# Type C : 
jump and compare 
|command | description | 
|------------|------------|
|dest=comp;jump|enters const value to reg A |
|||
```

```
# Stack : 
@SP to get the head of the stack to reg A 
in the ROM it will be @0 
so M will accsses the stack 
