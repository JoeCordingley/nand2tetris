
  //D=D+M
  //D=D+A


  @2 //output
  M=0
(LOOP)
  @0
  D=M
  @END
  D;JEQ
  @1
  D=M
  @2
  M=D+M
  @0
  M=M-1
  @LOOP
  0;JMP
(END)
