unit Tools;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function BoolToInt(Value: boolean): integer;
function IsVar(S: string): boolean;
function GetWSPos(S: string): integer;
function IsStrNum(S: string): boolean;
function IsCharConst(S: string): boolean;

implementation

function BoolToInt(Value: boolean): integer;
begin
  if Value then
  begin
    Result := 1;
  end
  else
  begin
    Result := 0;
  end;
end;

function IsCharConst(S: string): boolean;
var
  S0: string;
  flag: boolean;
begin
  flag := True;
  S0 := trim(S);

  if Length(s0) <> 3 then
  begin
    flag := False;
  end
  else
  begin
    if not (leftstr(s0, 1) = '''') and not (rightstr(s0, 1) = '''') then
    begin
      flag := False;
    end;
  end;

  Result := flag;
end;

function IsVar(S: string): boolean;
var
  C: string;
  ch: char;
begin
  C := UpperCase(S);

  if (Length(C) = 1) then
  begin
    ch := C[1];
    if ch in ['A'..'Z'] then
    begin
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end
  else
  begin
    Result := False;
  end;
end;

function GetWSPos(S: string): integer;
var
  i, idx: integer;
begin
  idx := 0;

  for i := 1 to length(s) do
  begin
    if s[i] in [#32, #9] then
    begin
      idx := i;
      Break;
    end;
  end;

  Result := idx;
end;

function IsStrNum(S: string): boolean;
var
  I: integer;
  flag: boolean;
begin

  flag := True;
  for I := 1 to Length(S) do
  begin
    if not (S[I] in ['0'..'9', '-']) then
    begin
      flag := False;
      Break;
    end;
  end;
  Result := flag;
end;

end.
