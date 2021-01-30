unit GTA.WaterPuzzle;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.Utils;

type
  TWaterPuzzle = class(TObject)
  private
    _pre: TArr_int;
    _end: integer;

  public
    constructor Create;
    destructor Destroy; override;

    function Return: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
begin
  with TWaterPuzzle.Create do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ TWaterPuzzle }

constructor TWaterPuzzle.Create;
var
  queue: IQueue_int;
  visited: TArr_bool;
  cur, a, b, x, y, Next: integer;
  nextS: IList_int;
begin
  queue := TQueue_int.Create;
  SetLength(visited, 100);
  SetLength(_pre, 100);
  _end := -1;

  queue.EnQueue(0);
  visited[0] := true;

  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;
    a := cur div 10;
    b := cur mod 10;
    // max a = 5, max b = 3

    ////////////////////////////////
    nextS := TArrayList_int.Create();
    nextS.AddLast(5 * 10 + b);
    nextS.AddLast(a * 10 + 3);
    nextS.AddLast(a * 10 + 0);
    nextS.AddLast(0 * 10 + b);

    x := Min(a, 3 - b);
    nextS.AddLast((a - x) * 10 + (b + x));

    y := Min(5 - a, b);
    nextS.AddLast((a + y) * 10 + (b - y));
    ////////////////////////////////

    for Next in nextS.ToArray do
    begin
      if (not visited[Next]) then
      begin
        queue.EnQueue(Next);
        visited[Next] := true;
        _pre[Next] := cur;

        if (Next div 10 = 4) or (Next mod 10 = 4) then
        begin
          _end := Next;
          Exit;
        end;
      end;
    end;
  end;
end;

destructor TWaterPuzzle.Destroy;
begin
  inherited Destroy;
end;

function TWaterPuzzle.Return: TArr_int;
var
  res: TArr_int;
  list: IList_int;
  cur: integer;
begin
  if _end = -1 then Exit(nil);

  list := TArrayList_int.Create;
  cur := _end;
  while cur <> 0 do
  begin
    list.AddLast(cur);
    cur := _pre[cur];
  end;
  list.AddLast(0);

  res := list.ToArray;
  TArrayUtils_int.Reverse(res);
  Result := res;
end;

end.
