unit UAsm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Instructions, Tools, cLables;

const
  MAX_LABLES = 512;

var
  ByteCode: array of integer;
  bCodeLen: integer;

procedure Abort1(id: integer; Msg: string = '');
procedure AssembleSource(Filename, OutFilename: string);

implementation

procedure Abort1(id: integer; Msg: string = '');
begin
  WriteLn('Vazo Assembler v1.1');
  WriteLn('Error Code: ' + IntToStr(id));
  WriteLn('Compile Error: ' + Msg);
end;

procedure AssembleSource(Filename, OutFilename: string);
var
  X: integer;
  TempLines: TStringList;
  Parts, Tokens: TStringList;
  sLine: string;
  sPos: integer;
  bCode: TInstructions;
  ch: char;
  chAsc: integer;
  sVarIndex: integer;
  Lables: TLables;
  bFlag: boolean;
  fp: file of integer;
begin

  TempLines := TStringList.Create;
  Parts := TStringList.Create;
  Tokens := TStringList.Create;
  Lables := TLables.Create(MAX_LABLES);
  bFlag := True;

  if not FileExists(Filename) then
  begin
    Abort1(1, 'Source File Not Found: ' + Filename);
  end
  else
  begin
    TempLines.LoadFromFile(Filename);

    //Loop tho the lines and remove comments and blank lines
    for X := 0 to TempLines.Count - 1 do
    begin
      sLine := Trim(TempLines[X]);

      if (Length(sLine) > 1) and (sLine[1] <> '#') then
      begin
        sPos := GetWSPos(sLine);
        if sPos <> 0 then
        begin
          //Check for token and parm
          Parts.Add(trim(leftstr(sline, sPos)));
          Parts.Add(trim(Copy(sLine, sPos)));
        end
        else
        begin
          Parts.Add(sLine);
        end;
      end;
    end;

    //GC
    FreeAndNil(TempLines);

    for X := 0 to Parts.Count - 1 do
    begin
      sLine := Parts[X];
      //All methods and lables must end with :
      if RightStr(sLine, 1) = ':' then
      begin
        //Delete : from end of string
        Delete(sLine, Length(sLine), 1);
        //Check if lable is already in the collection
        if Lables.LabelIndex(sLine) <> -1 then
        begin
          Abort1(2, 'Label or Method Is AlLready Assigned ''' + sLine + '''');
          bFlag := False;
          Break;
        end
        else
        begin
          //Add label index
          Lables.NewLabel(sLine, Tokens.Count - 1);
        end;
      end
      else
      begin
        //Keep adding tokens
        Tokens.Add(sLine);
      end;
    end;

    //Clear up this we finished with it
    FreeAndNil(Parts);

    if bFlag then
    begin
      //Bytecode length
      bCodeLen := Tokens.Count;
      //Set the bytecode array length
      SetLength(ByteCode, bCodeLen + 1);

      //Here we assemble the byte code
      for X := 0 to Tokens.Count - 1 do
      begin
        sLine := Tokens[X];
        bCode := InstToOP(sLine);

        if bCode <> Instructions.TInstructions.OP_ERR then
        begin
          ByteCode[X] := Ord(TInstructions(bCode));
        end
        else if IsStrNum(sLine) then
        begin
          ByteCode[X] := StrToInt(sLine);
        end
        else if IsVar(sLine) then
        begin
          ch := UpCase(sLine[1]);
          sVarIndex := Ord(ch) - 65;
          ByteCode[X] := sVarIndex;
        end
        else if Lables.LabelIndex(sLine) <> -1 then
        begin
          ByteCode[X] := Lables.LabelIndex(sLine);
        end
        else if IsCharConst(sLine) then
        begin
          //Delete starting and ending lines
          Delete(sLine, 1, 1);
          Delete(sLine, Length(sLine), 1);
          sLine := sLine;
          ch := sLine[1];
          chAsc := Ord(ch);
          ByteCode[X] := chAsc;
        end
        else
        begin
          writeln('Unkoen Token Found ' + sLine);
        end;
      end;
    end;
  end;

  //GC
  FreeAndNil(Tokens);
  Lables.FreeAll;

  if bCodeLen > 0 then
  begin
    //Write VM file.
    Assign(fp, OutFilename);
    Rewrite(fp);
    //Write byte code to file.
    for X := 0 to bCodeLen do
    begin
      BlockWrite(fp, ByteCode[X], 1);
    end;
    //Close file
    CloseFile(fp);
  end;

  //Clear up
  SetLength(ByteCode, 0);
end;

end.
