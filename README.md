# VazoVM
A small Virtual machine and assembler
Here is a small example I made for learning exercise documentation will follow soon as well as other updates.

## Using the assembler
Compile vasm.lpi in Lazarus or free pascal open a command prompt and type **vasm table.txt table.vzo**
the vzo file is the one that works with the VM

## Using the VM
Drop the vzo file made above onto the **Vazo.exe table.vzo** file this will execute the file.
If your unsure I left two batch files in each of the folders with an example.
You can also find many more examples in the Examples folder.

## Simple while loop example

```
PUSH 10
STORE A

Loop:
 LOAD A
 PUSH 0
 IfIcmplt end
 LOAD A
 Dec A
 Syscall 2
 goto loop

end:
HLT
```
