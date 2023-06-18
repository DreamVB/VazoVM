unit cStack;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  generic MyStack<T> = class

  private
    t_items: array of T;
    t_ItemPtr: integer;
    t_items_size: integer;
  public
    constructor Create(Size: integer);
    procedure Push(Value: T);
    function Pop(): T;
    function Peek(): T;
    function IsFull(): boolean;
    function IsEmpty(): boolean;
    function Count: integer;
    procedure FreeAll;
  end;

implementation

constructor MyStack.Create(Size: integer);
begin
  t_items_size := Size;
  t_ItemPtr := 0;
  SetLength(t_items, Size);
end;

procedure MyStack.Push(Value: T);
begin
  t_items[t_ItemPtr + 1] := Value;
  Inc(t_ItemPtr);
end;

function MyStack.Pop(): T;
begin
  Result := t_items[t_ItemPtr];
  Dec(t_ItemPtr);
end;

function MyStack.Peek(): T;
begin
  Result := t_items[t_ItemPtr];
end;

function MyStack.IsFull(): boolean;
begin
  Result := (t_ItemPtr >= t_items_size);
end;

function MyStack.IsEmpty(): boolean;
begin
  Result := (t_ItemPtr = 0);
end;

function MyStack.Count: integer;
begin
  Result := t_ItemPtr;
end;

procedure MyStack.FreeAll;
begin
  t_items_size := 0;
  t_ItemPtr := 0;
  SetLength(t_items, 0);
end;

end.
