unit GTA.DirectedEulerLoop;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TEulerLoop = class(TObject)
  private
    _Graph: TGraph;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function HasEulerLoop: boolean;
    function Return: TArr_int;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
begin
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true);
  with TEulerLoop.Create(g) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;

  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'DirectedEulerLoop.txt'), true);
  with TEulerLoop.Create(g) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ TEulerLoop }

constructor TEulerLoop.Create(g: IGraph);
begin
  if not g.IsDirected then
    raise Exception.Create('EulerLoop only works in undirected graph');

  _Graph := g as TGraph;
end;

destructor TEulerLoop.Destroy;
begin
  inherited Destroy;
end;

function TEulerLoop.HasEulerLoop: boolean;
var
  //cc: TCC;
  v: integer;
begin
  //cc := TCC.Create(_Graph);
  //try
  //  if cc.Count > 1 then
  //    Exit(false);
  //finally
  //  cc.Free;
  //end;

  for v := 0 to _Graph.Vertex - 1 do
  begin
    if _Graph.InDegree(v) <> _Graph.OutDegree(v) then
      Exit(false);
  end;

  Result := true;
end;

function TEulerLoop.Return: TArr_int;
var
  g: IGraph;
  list: IList_int;
  stack: IStack_int;
  cur, w: integer;
begin
  Result := [];
  if not HasEulerLoop then Exit;

  g := _Graph.Clone;
  list := TLinkedList_int.Create;
  stack := TStack_int.Create;
  stack.Push(-1);

  cur := 0;
  while not stack.IsEmpty do
  begin
    if (g as TGraph).OutDegree(cur) <> 0 then
    begin
      stack.Push(cur);
      w := g.Adj(cur)[0];
      g.RemoveEdge(cur, w);
      cur := w;
    end
    else
    begin
      list.AddFirst(cur);
      cur := stack.Pop;
    end;
  end;

  Result := list.ToArray;
end;

end.
