@10
D=A
@SP
A=M
M=D
@SP
M=M+1
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE1
D;JGT
D=0
@IF_FALSE1
0;JMP
(IF_TRUE1) 
D=-1
(IF_FALSE1)
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
@10
D=A
@SP
A=M
M=D
@SP
M=M+1
@SP
A=M-1
D=M
A=A-1
D=D-M
@IF_TRUE2
D;JLT
D=0
@IF_FALSE2
0;JMP
(IF_TRUE2) 
D=-1
(IF_FALSE2)
@SP
A=M-1
A=A-1
M=D
@SP
M=M-1
