unit GTA.EulerLoop_Hierholzer_Algorithm;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils,
  GTA.CC;

type
  TEulerLoop = class(TObject)
  private
    _Graph: IGraph;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function HasEulerLoop: boolean;
    function Return: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
begin
  with TEulerLoop.Create(TGraph.Create
      (FileName('Chapter10-Euler-Loop-and-Euler-Path', 'g.txt'))) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;

  with TEulerLoop.Create(TGraph.Create
      (FileName('Chapter10-Euler-Loop-and-Euler-Path', 'g2.txt'))) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ TEulerLoop }

constructor TEulerLoop.Create(g: IGraph);
begin
  _Graph := (g as TGraph).Clone;
end;

destructor TEulerLoop.Destroy;
begin
  inherited Destroy;
end;

function TEulerLoop.HasEulerLoop: boolean;
var
  cc: TCC;
  v: integer;
begin
  cc := TCC.Create(_Graph);
  try
    if cc.Count > 1 then
      Exit(false);
  finally
    cc.Free;
  end;

  for v := 0 to _Graph.Vertex - 1 do
  begin
    if Odd(_Graph.Degree(v)) then
      Exit(false);
  end;

  Result := true;
end;

function TEulerLoop.Return: TArr_int;
var
  list: IList_int;
  stack: IStack_int;
  cur, w: integer;
begin
  Result := [];
  if not HasEulerLoop then Exit;

  list := TArrayList_int.Create;
  stack := TStack_int.Create;
  stack.Push(-1);

  cur := 0;
  while not stack.IsEmpty do
  begin
    if _Graph.Degree(cur) <> 0 then
    begin
      stack.Push(cur);
      w := _Graph.Adj(cur)[0];
      _Graph.RemoveEdge(cur, w);
      cur := w;
    end
    else
    begin
      list.AddLast(cur);
      cur := stack.Pop;
    end;
  end;

  Result := list.ToArray;
end;

end.
