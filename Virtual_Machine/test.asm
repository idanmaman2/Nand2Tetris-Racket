@LCL
D=M
//ok 


@5
A=D-A
D=M
@13
M=D
//ok

@SP 
A=M-1 
D=M   
@ARG   
A=M    
M=D
@SP
M=M-1
//ok 



@ARG
D=M
@SP
M=D+1
//ok

@LCL
M=M-1
A=M
D=M
@THAT
M=D
//ok

@LCL
M=M-1
A=M
D=M
@THIS
M=D
//ok

@LCL
M=M-1
A=M
D=M
@ARG
M=D
//ok

@LCL
M=M-1
A=M
D=M
@LCL
M=D
//ok

@13
A=M
0;JMP
//ok