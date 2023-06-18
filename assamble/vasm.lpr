program vasm;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
    {$ENDIF}
  Classes,
  UAsm;

begin
  if ParamCount < 2 then
  begin
    WriteLn('Incorrect Number Of Arguments');
    WriteLn('Use vasm Input.txt output.vzo');
  end
  else
  begin
    AssembleSource(ParamStr(1), ParamStr(2));
  end;
end.
