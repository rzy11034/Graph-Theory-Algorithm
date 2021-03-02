unit GTA.TopoSort;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TTopoSort = class(TObject)
  private
    _Graph: TGraph;
    _HasCycle: boolean;
    _list: IList_int;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;
    function Return: TArr_int;

    property HasCycle: boolean read _HasCycle;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IGraph;
begin
  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug.txt'), true);
  with TTopoSort.Create(g) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;

  g := TGraph.Create(FileName('Chapter13-Directed-Graph', 'ug2.txt'), true);
  with TTopoSort.Create(g) do
  begin
    TArrayUtils_int.Print(Return);
    Free;
  end;
end;

{ TTopoSort }

constructor TTopoSort.Create(g: IGraph);
var
  queue: IQueue_int;
  inDegrees: TArr_int;
  cur, Next, v: integer;
begin
  if not g.IsDirected then
    raise Exception.Create('TopoSort only works in directed graph.');

  _Graph := g as TGraph;
  _list := TArrayList_int.Create;
  queue := TQueue_int.Create;
  SetLength(inDegrees, _Graph.Vertex);

  for v := 0 to _Graph.Vertex - 1 do
  begin
    inDegrees[v] := _Graph.InDegree(v);

    if inDegrees[v] = 0 then
      queue.EnQueue(v);
  end;

  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;
    _list.AddLast(cur);

    for Next in _Graph.Adj(cur) do
    begin
      inDegrees[Next] -= 1;

      if inDegrees[Next] = 0 then
        queue.EnQueue(Next);
    end;
  end;

  if _list.Count <> _Graph.Vertex then
  begin
    _HasCycle := true;
    _list.Clear;
  end;
end;

destructor TTopoSort.Destroy;
begin
  inherited Destroy;
end;

function TTopoSort.Return: TArr_int;
begin
  Result := _list.ToArray;
end;

end.
