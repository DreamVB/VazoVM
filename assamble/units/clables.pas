unit cLables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  Item = packed record
    lName: string;
    lIdx: integer;
  end;

type
  TLables = class
  private
    m_Lables: array of Item;
    m_LablePtr: integer;
  public
    constructor Create(Size: integer);
    procedure NewLabel(lName: string; Index: integer);
    function LabelIndex(lName: string): integer;
    procedure FreeAll;
  end;


implementation

constructor TLables.Create(Size: integer);
begin
  m_LablePtr := 0;
  SetLength(m_Lables, Size + 1);
end;

procedure TLables.NewLabel(lName: string; Index: integer);
begin
  m_Lables[m_LablePtr].lName := lName;
  m_Lables[m_LablePtr].lIdx := Index;
  Inc(m_LablePtr);
end;

function TLables.LabelIndex(lName: string): integer;
var
  idx: integer;
  X: integer;
begin
  idx := -1;

  for X := 0 to m_LablePtr - 1 do
  begin
    if Uppercase(m_Lables[X].lName) = UpperCase(lName) then
    begin
      idx := X;
      Break;
    end;
  end;

  if idx <> -1 then
  begin
    Result := m_Lables[idx].lIdx;
  end
  else
  begin
    Result := -1;
  end;
end;

procedure TLables.FreeAll;
begin
  m_LablePtr := 0;
  SetLength(m_Lables, 0);
end;

end.
