unit Instructions;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TInstructions = (OP_ERR = 0, HLT, PUSH, POP,
    ICONST_0, ICONST_1, ICONST_2, ICONST_3, ICONST_4, ICONST_5, ILOAD_0, ILOAD_1,
    ILOAD_2, ILOAD_3, ILOAD_4, ILOAD_5, ISTORE_0, ISTORE_1,
    ISTORE_2, ISTORE_3, ISTORE_4, ISTORE_5, IADD, ISUB, IMUL,
    IDIV, IREM, IDUP, STORE, LOAD, IGOTO, IFICMPEQ, IFICMPNE,
    IFICMPLT, IFICMPLE, IFICMPGT, IFICMPGE, IFINZ, IINC, IDEC, IAND, IOR, IXOR,
    INOT, ISHL, ISHR, RETURN, INEG, SYSCALL, ISWAP, CALL);

function InstToOP(S: string): TInstructions;

implementation

function InstToOp(S: string): TInstructions;
var
  op: string;
begin
  op := UpperCase(S);

  if op = 'HLT' then
  begin
    Result := TInstructions.HLT;
  end
  else if op = 'PUSH' then
  begin
    Result := TInstructions.PUSH;
  end
  else if op = 'POP' then
  begin
    Result := POP;
  end
  else if op = 'ADD' then
  begin
    Result := TInstructions.IADD;
  end
  else if op = 'SUB' then
  begin
    Result := TInstructions.ISUB;
  end
  else if op = 'MUL' then
  begin
    Result := TInstructions.IMUL;
  end
  else if op = 'DIV' then
  begin
    Result := TInstructions.IDIV;
  end
  else if op = 'DUP' then
  begin
    Result := TInstructions.IDUP;
  end
  else if op = 'GOTO' then
  begin
    Result := TInstructions.IGOTO;
  end
  else if op = 'IFICMPEQ' then
  begin
    Result := TInstructions.IFICMPEQ;
  end
  else if op = 'IFICMPNE' then
  begin
    Result := TInstructions.IFICMPNE;
  end
  else if op = 'IFICMPLT' then
  begin
    Result := TInstructions.IFICMPLT;
  end
  else if op = 'IFICMPLE' then
  begin
    Result := TInstructions.IFICMPLE;
  end
  else if op = 'IFICMPGT' then
  begin
    Result := TInstructions.IFICMPGT;
  end
  else if op = 'IFICMPGE' then
  begin
    Result := TInstructions.IFICMPGE;
  end
  else if op = 'IFINZ' then
  begin
    Result := TInstructions.IFINZ;
  end
  else if op = 'INC' then
  begin
    Result := TInstructions.IINC;
  end
  else if op = 'DEC' then
  begin
    Result := TInstructions.IDEC;
  end
  else if op = 'AND' then
  begin
    Result := TInstructions.IAND;
  end
  else if op = 'OR' then
  begin
    Result := TInstructions.IOR;
  end
  else if op = 'XOR' then
  begin
    Result := TInstructions.IXOR;
  end
  else if op = 'NOT' then
  begin
    Result := TInstructions.INOT;
  end
  else if op = 'REM' then
  begin
    Result := TInstructions.IREM;
  end
  else if op = 'SHL' then
  begin
    Result := TInstructions.ISHL;
  end
  else if op = 'SHR' then
  begin
    Result := TInstructions.ISHR;
  end
  else if op = 'SWAP' then
  begin
    Result := TInstructions.ISWAP;
  end
  else if op = 'CALL' then
  begin
    Result := TInstructions.CALL;
  end
  else if op = 'RETURN' then
  begin
    Result := TInstructions.RETURN;
  end
  else if op = 'SYSCALL' then
  begin
    Result := TInstructions.SYSCALL;
  end
  else if op = 'STORE' then
  begin
    Result := TInstructions.STORE;
  end
  else if op = 'LOAD' then
  begin
    Result := TInstructions.LOAD;
  end
  else if op = 'NEG' then
  begin
    Result := TInstructions.INEG;
  end
  else if op = 'ICONST_0' then
  begin
    Result := TInstructions.ICONST_0;
  end
  else if op = 'ICONST_1' then
  begin
    Result := TInstructions.ICONST_1;
  end
  else if op = 'ICONST_2' then
  begin
    Result := TInstructions.ICONST_2;
  end
  else if op = 'ICONST_3' then
  begin
    Result := TInstructions.ICONST_3;
  end
  else if op = 'ICONST_4' then
  begin
    Result := TInstructions.ICONST_4;
  end
  else if op = 'ICONST_5' then
  begin
    Result := TInstructions.ICONST_5;
  end
  else if op = 'ISTORE_0' then
  begin
    Result := TInstructions.ISTORE_0;
  end
  else if op = 'ISTORE_1' then
  begin
    Result := TInstructions.ISTORE_1;
  end
  else if op = 'ISTORE_2' then
  begin
    Result := TInstructions.ISTORE_2;
  end
  else if op = 'ISTORE_3' then
  begin
    Result := TInstructions.ISTORE_3;
  end
  else if op = 'ISTORE_4' then
  begin
    Result := TInstructions.ISTORE_4;
  end
  else if op = 'ISTORE_5' then
  begin
    Result := TInstructions.ISTORE_5;
  end
  else if op = 'ILOAD_0' then
  begin
    Result := TInstructions.ILOAD_0;
  end
  else if op = 'ILOAD_1' then
  begin
    Result := TInstructions.ILOAD_1;
  end
  else if op = 'ILOAD_2' then
  begin
    Result := TInstructions.ILOAD_2;
  end
  else if op = 'ILOAD_3' then
  begin
    Result := TInstructions.ILOAD_3;
  end
  else if op = 'ILOAD_4' then
  begin
    Result := TInstructions.ILOAD_4;
  end
  else if op = 'ILOAD_5' then
  begin
    Result := TInstructions.ILOAD_5;
  end
  else
  begin
    Result := TInstructions.OP_ERR;
  end;
end;

end.
