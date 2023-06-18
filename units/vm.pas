unit vm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Instructions, cStack;

procedure Run;

type
  TStack = specialize MyStack<integer>;

type
  TRetStack = specialize MyStack<integer>;

type
  TStackItem = class(TObject)
    Value: integer;
  end;

const
  MAX_STACK = 256;

const
  MAX_RETURN_STACK = 100;

const
  MAX_VARS = 26;

var
  stk: TStack;
  RS: TRetStack;
  PC: integer;
  ByteCode: array of integer;
  Vars: array[0..MAX_VARS] of integer;
  bCodeLen: integer;

procedure Abort1(id: integer; Msg: string = '');
procedure HaltVM;
procedure GC;

implementation

procedure InitVars;
var
  X: integer;
begin
  for X := 0 to MAX_VARS do
  begin
    Vars[X] := 0;
  end;
end;

procedure Abort1(id: integer; Msg: string = '');
begin
  WriteLn('Vazo VM v1.1');
  WriteLn('Error Code: ' + IntToStr(id));
  WriteLn('Runtime Error: ' + Msg);
end;

procedure HaltVM;
begin
  Abort1(2, 'Stack Underflow.');
  PC := bCodeLen;
end;

procedure GC;
begin
  bCodeLen := 0;
  stk.FreeAll;
  rs.FreeAll;
  SetLength(ByteCode, bCodeLen);
  PC := 0;
  InitVars;
end;

procedure DoCmpOps(op: TInstructions);
var
  A, B: TStackItem;
begin

  A := TStackItem.Create;
  B := TStackItem.Create;

  B.Value := stk.Pop();
  A.Value := stk.Pop();

  Inc(PC);

  case op of
    TInstructions.IFICMPEQ:
    begin
      if A.Value = B.Value then PC := ByteCode[PC];
    end;
    TInstructions.IFICMPNE:
    begin
      if A.Value <> B.Value then PC := ByteCode[PC];
    end;
    TInstructions.IFICMPLT:
    begin
      if A.Value < B.Value then PC := ByteCode[PC];
    end;
    TInstructions.IFICMPLE:
    begin
      if A.Value <= B.Value then PC := ByteCode[PC];
    end;
    TInstructions.IFICMPGT:
    begin
      if A.Value > B.Value then PC := ByteCode[PC];
    end;
    TInstructions.IFICMPGE:
    begin
      if A.Value >= B.Value then PC := ByteCode[PC];
    end
    else
    begin
      PC := bCodeLen;
    end;
  end;
end;

procedure DoBinOps(op: TInstructions);
var
  A, B: TStackItem;
begin

  A := TStackItem.Create;
  B := TStackItem.Create;

  B.Value := stk.Pop();
  A.Value := stk.Pop();

  case op of
    TInstructions.IADD:
    begin
      stk.Push(A.Value + B.Value);
    end;
    TInstructions.ISUB:
    begin
      stk.Push(A.Value - B.Value);
    end;
    TInstructions.IMUL:
    begin
      stk.Push(A.Value * B.Value);
    end;
    TInstructions.IDIV:
    begin
      stk.Push(A.Value div B.Value);
    end;
    TInstructions.IREM:
    begin
      stk.Push(A.Value mod B.Value);
    end;
    TInstructions.ISWAP:
    begin
      stk.Push(B.Value);
      stk.Push(A.Value);
    end
    else
    begin
      PC := bCodeLen;
    end;
  end;
end;

procedure DoBitwiseOps(op: TInstructions);
var
  A, B: TStackItem;
begin

  A := TStackItem.Create;
  B := TStackItem.Create;

  B.Value := stk.Pop();
  A.Value := stk.Pop();

  case op of
    TInstructions.IAND:
    begin
      stk.Push(A.Value and B.Value);
    end;
    TInstructions.IOR:
    begin
      stk.Push(A.Value or B.Value);
    end;
    TInstructions.IXOR:
    begin
      stk.Push(A.Value xor B.Value);
    end;
    TInstructions.ISHL:
    begin
      stk.Push(A.Value shl B.Value);
    end;
    TInstructions.ISHR:
    begin
      stk.Push(A.Value shr B.Value);
    end;
    else
    begin
      PC := bCodeLen;
    end;
  end;
end;

procedure DoConsts(op: TInstructions);
var
  A: TStackItem;
begin
  A := TStackItem.Create;

  case op of
    TInstructions.ICONST_0:
    begin
      A.Value := 0;
      stk.Push(A.Value);
    end;
    TInstructions.ICONST_1:
    begin
      A.Value := 1;
      stk.Push(A.Value);
    end;
    TInstructions.ICONST_2:
    begin
      A.Value := 2;
      stk.Push(A.Value);
    end;
    TInstructions.ICONST_3:
    begin
      A.Value := 3;
      stk.Push(A.Value);
    end;
    TInstructions.ICONST_4:
    begin
      A.Value := 4;
      stk.Push(A.Value);
    end;
    TInstructions.ICONST_5:
    begin
      A.Value := 5;
      stk.Push(A.Value);
    end;
    else
    begin
      PC := bCodeLen;
    end;
  end;
end;

procedure DoConstsLoad(op: TInstructions);
var
  A: TStackItem;
begin

  A := TStackItem.Create;

  case op of
    TInstructions.ILOAD_0:
    begin
      A.Value := Vars[0];
    end;
    TInstructions.ILOAD_1:
    begin
      A.Value := Vars[1];
    end;
    TInstructions.ILOAD_2:
    begin
      A.Value := Vars[2];
    end;
    TInstructions.ILOAD_3:
    begin
      A.Value := Vars[3];
    end;
    TInstructions.ILOAD_4:
    begin
      A.Value := Vars[4];
    end;
    TInstructions.ILOAD_5:
    begin
      A.Value := Vars[5];
    end;
    else
    begin
      PC := bCodeLen;
    end;
  end;

  stk.Push(A.Value);
end;

procedure DoConstsStore(op: TInstructions);
var
  A: TStackItem;
begin
  A := TStackItem.Create;
  A.Value := stk.Pop();

  case op of
    TInstructions.ISTORE_0:
    begin
      Vars[0] := A.Value;
    end;
    TInstructions.ISTORE_1:
    begin
      Vars[1] := A.Value;
    end;
    TInstructions.ISTORE_2:
    begin
      Vars[2] := A.Value;
    end;
    TInstructions.ISTORE_3:
    begin
      Vars[3] := A.Value;
    end;
    TInstructions.ISTORE_4:
    begin
      Vars[4] := A.Value;
    end;
    TInstructions.ISTORE_5:
    begin
      Vars[5] := A.Value;
    end;
    else
    begin
      PC := bCodeLen;
    end;
  end;
end;

procedure Run;
var
  A: TStackItem;
  inst: TInstructions;
begin
  A := TStackItem.Create;
  InitVars;

  stk := TStack.Create(MAX_STACK);
  RS := TRetStack.Create(MAX_RETURN_STACK);
  PC := 0;

  while PC < bCodeLen do
  begin
    inst := TInstructions(ByteCode[PC]);

    case inst of

      TInstructions.HLT:
      begin
        PC := bCodeLen;
      end;

      TInstructions.PUSH:
      begin
        Inc(PC);
        A.Value := ByteCode[PC];
        stk.Push(A.Value);
      end;

      TInstructions.POP:
      begin
        if stk.Count < 1 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          stk.Pop();
        end;
      end;

      TInstructions.ICONST_0,
      TInstructions.ICONST_1,
      TInstructions.ICONST_2,
      TInstructions.ICONST_3,
      TInstructions.ICONST_4,
      TInstructions.ICONST_5:
      begin
        DoConsts(inst);
      end;

      TInstructions.ISTORE_0,
      TInstructions.ISTORE_1,
      TInstructions.ISTORE_2,
      TInstructions.ISTORE_3,
      TInstructions.ISTORE_4,
      TInstructions.ISTORE_5:
      begin
        if stk.Count < 1 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          DoConstsStore(inst);
        end;
      end;

      TInstructions.ILOAD_0,
      TInstructions.ILOAD_1,
      TInstructions.ILOAD_2,
      TInstructions.ILOAD_3,
      TInstructions.ILOAD_4,
      TInstructions.ILOAD_5:
      begin
        DoConstsLoad(inst);
      end;

      TInstructions.STORE:
      begin
        if stk.Count < 1 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          Inc(PC);
          A.Value := stk.Pop();
          Vars[ByteCode[PC]] := A.Value;
        end;
      end;

      TInstructions.LOAD:
      begin
        Inc(PC);
        A.Value := Vars[ByteCode[PC]];
        stk.Push(A.Value);
      end;

      TInstructions.IGOTO:
      begin
        Inc(PC);
        PC := ByteCode[PC];
      end;

      TInstructions.IADD,
      TInstructions.ISUB,
      TInstructions.IMUL,
      TInstructions.IDIV,
      TInstructions.IREM,
      TInstructions.ISWAP:
      begin
        if stk.Count <> 2 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          DoBinOps(inst);
        end;
      end;

      TInstructions.IAND,
      TInstructions.IOR,
      TInstructions.IXOR,
      TInstructions.ISHL,
      TInstructions.ISHR:
      begin
        if stk.Count <> 2 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          DoBitwiseOps(inst);
        end;
      end;

      TInstructions.INOT:
      begin
        A.Value := stk.Pop();
        stk.Push(not A.Value);
      end;

      TInstructions.INEG:
      begin
        A.Value := stk.Pop();
        stk.Push(-A.Value);
      end;

      TInstructions.IFICMPEQ,
      TInstructions.IFICMPNE,
      TInstructions.IFICMPLT,
      TInstructions.IFICMPLE,
      TInstructions.IFICMPGT,
      TInstructions.IFICMPGE:
      begin
        if stk.Count <> 2 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          DoCmpOps(inst);
        end;
      end;

      TInstructions.IFINZ:
      begin
        Inc(PC);
        A.Value := stk.Peek();
        if a.Value <> 0 then
        begin
          PC := ByteCode[PC];
        end;
      end;

      TInstructions.IINC:
      begin
        Inc(PC);
        A.Value := Vars[ByteCode[PC]];
        Vars[ByteCode[PC]] := A.Value + 1;
      end;

      TInstructions.IDEC:
      begin
        Inc(PC);
        A.Value := Vars[ByteCode[PC]];
        Vars[ByteCode[PC]] := A.Value - 1;
      end;

      TInstructions.IDUP:
      begin
        if stk.Count < 1 then
        begin
          HaltVM;
          Break;
        end
        else
        begin
          A.Value := stk.Peek();
          stk.Push(A.Value);
        end;
      end;

      TInstructions.CALL:
      begin
        Inc(PC);
        A.Value := PC;
        RS.Push(A.Value);
        PC := ByteCode[PC];
      end;

      TInstructions.RETURN:
      begin
        A.Value := RS.Pop();
        PC := A.Value;
      end;

      TInstructions.SYSCALL:
      begin
        Inc(PC);
        A.Value := ByteCode[PC];

        if (A.Value = 1) or (A.Value = 2) or (A.Value = 3) then
        begin
          if stk.Count < 1 then
          begin
            HaltVM;
            Break;
          end
          else
          begin
            if A.Value = 1 then Write(stk.Pop());
            if A.Value = 2 then WriteLn(stk.Pop());
            if A.Value = 3 then Write(chr(stk.Pop()));
          end;
        end
        else
        begin
          if A.Value = 4 then
          begin
            ReadLn(A.Value);
            stk.Push(A.Value);
          end;
        end;
      end
      else
      begin
        PC := bCodeLen;
      end;
    end;
    Inc(PC);
  end;
end;

end.
