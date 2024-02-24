// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen
// by writing 'black' in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen by writing
// 'white' in every pixel;
// the screen should remain fully clear as long as no key is pressed.

(RESET)
  @SCREEN //screenstart
  D=A //D=screenstart
  @writelocation
  M=D //writelocation=screenstart
(LOOP)
  @KBD //A=keyboard
  D=M //D=M[keyboard]
  @SETONE //A=SETONE
  D;JNE //jump to SETONE if M[keyboard] != 0
  @pixelcolour //A=pixelcolour
  M=0 //M[pixelcolour] = 0
  @CHANGESCREEN //A=CHANGESCREEN
  0;JMP //jump to CHANGESCREEN
(SETONE)
  @pixelcolour //A=pixelcolour
  M=-1 //M[pixelcolour] = 1
(CHANGESCREEN)
  @pixelcolour //A=pixelcolour
  D=M //D=M[pixelcolour]
  @writelocation //A=writelocation
  A=M
  M=D //M[writeLocaltion] = M[pixelcolour]
  @writelocation
  D=M+1 //D=writeLocation +1
  @KBD //A=keyboard
  D=D-A //D=writeLocation + 1 - keyboard
  @RESET //A=reset
  D;JEQ //jump if writelocation + 1 == keyboard
  @writelocation //A=writelocation
  M=M+1
  @LOOP
  0;JMP
