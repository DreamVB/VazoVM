program Vazo;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}
  cthreads,
     {$ENDIF}
  Classes,
  vm,
  SysUtils { you can add units after this };

var
  fp: file of integer;
  X: integer;

begin

  if ParamCount = 0 then
  begin
    writeln('Vazo VM Ver 1.1');
    writeln('Input Filename Found');
    writeln('Use: Vazo Filename.vzo');
  end
  else
  begin
    if FileExists(ParamStr(1)) then
    begin
      //Load binary vm file
      AssignFile(fp, ParamStr(1));
      Reset(fp);
      //Get cde length
      bCodeLen := FileSize(fp) - 1;
      //Set byte array to hold pcode
      SetLength(ByteCode, bCodeLen);
      //Load the pcde into the array
      for X := 0 to bCodeLen do
      begin
        BlockRead(fp, ByteCode[X], 1);
      end;
      //Check cde length not zero
      if bCodeLen <> 0 then
      begin
        //Begin VM
        vm.Run;
        //GC
        vm.GC;
      end;
    end;
  end;
end.
