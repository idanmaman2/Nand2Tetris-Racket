# Commands : 
## Stack 
* push 
* pop 
## Arthmetic 
* add : x+y 
* sub : x-y
* neg : -y 
* eq : x=y 
* gt : x>y
* lt : x<y 
* and : x && y 
* or : x || y 
* not : !y 
  
  Notes: 
    * true : 0xFFFF 
    * false : 0x000
## Symbols 
* push static 0 - data segment in x86
* push argument 1  - on stack in x86
* push local 2  - on stack in x86 
### all memory segments : 
* local
* argument
* static
* constant
* this
* that
* pointer
* temp
  
accsses : ```push/pop <segment> <value> ```



# Virtual regs : 
* RAM[0] - @SP 
* RAM[1] - local vars 
* RAM[2] - params 
* RAM[3] - the object 
* RAM[4] - dynamic object 
* RAM[5-12] - avbinable regs 
* RAM[13-15] - generec use avbinable regs 

# translate : 

## ADD : 
```
// vm command: add
@SP		// A = 0
A=M-1		//A = RAM[A]-1 = RAM[0]-1 = 258-1 = 257 => A=257
D=M		//D = RAM[A] = RAM[257] = 5
		//D saves the second item in the stack
A=A-1		//A = 257-1 = 256
M=D+M	//RAM[A] = D+RAM[A] => RAM[256] = 8+RAM[256] = 5+4 = 9  
		//save the add result in the place of the first item on the stack
		//this is equal to:  pop second item, pop first item, 		//push the result of their addition to the stack.
@SP		//after pushing the result to the stack,
		// we want to decrement the stack pointer.
		//current command is: A=0
M=M-1		//RAM[A] = RAM[A]-1
		// => RAM[0] = RAM[0] - 1 
		// => RAM[0] = 258-1 = 257.
		//so now the stack pointer, saved in RAM[0], points to RAM[257]


```

## EQ 
```
// vm command: eq 
@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE0
D;JEQ
D=0
@SP
A=M-1
A=A-1
M=D
@IF_FALSE0
0;JMP
(IF_TRUE0) //label 
D=-1
@SP
A=M-1
A=A-1
M=D
(IF_FALSE0)
@SP
M=M-1


```
## Push constant : 
```
// vm command: push constant 7
@7		//register A = 7
D=A		//save the value of A in register D, D = 7
@SP		//SP is Stack Pointer: points to the top of the stack 
		//  (next free location of the stack).
		//SP = 0 and points to RAM[0]. 
		//suppose the stack top points to RAM[256] so SP = RAM[0]=256.
		//current command is: A=0
A=M		//M is shortcut name to RAM[A]. 
		//since A=0 from the previous command 
		//then current command is: A = RAM[A] = RAM[0] = 256. => A=256
M=D		//RAM[A]= D. RAM[256]=7. 
		//meaning: push 7 to the top of the stack, which is now RAM[256].
 
@SP		//after pushing 7 to the stack, we want to increment the stack pointer.
		//current command is: A=0
M=M+1		//RAM[A]=RAM[A]+1 => RAM[0] = RAM[0] + 1 => RAM[0] = 256+1 = 257.
		//so now the stack pointer, saved in RAM[0], points to RAM[257]

```