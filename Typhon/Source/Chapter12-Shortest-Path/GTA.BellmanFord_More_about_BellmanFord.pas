unit GTA.BellmanFord_More_about_BellmanFord;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TBellmanFord = class(TObject)
  private
    _HasNegativeCycle: boolean;
    _Dis: TArr_int;
    _Graph: TWeightedGraph;
    _Pre: TArr_int;

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;
    function DistTo(v: integer): integer;
    function IsConnectedTo(v: integer): boolean;
    function PathTo(v: integer): TArr_int;

    property HasNegativeCycle: boolean read _HasNegativeCycle;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IWeightedGraph;
  v: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter12-Shortest-Path', 'g.txt'));
  with TBellmanFord.Create(g) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
        Write(DistTo(v), ' ');
      WriteLn;

      TArrayUtils_int.Print(PathTo(4));
    end
    else
      WriteLn('exist negative cycle.');

    Free;
  end;
  DrawLineBlockEnd;

  //======================================

  g := TWeightedGraph.Create(FileName('Chapter12-Shortest-Path', 'g2.txt'));
  with TBellmanFord.Create(g) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
        Write(DistTo(v), ' ');
      WriteLn;
    end
    else
      WriteLn('Exist negative cycle.');

    Free;
  end;
end;

{ TBellmanFord }

constructor TBellmanFord.Create(g: IWeightedGraph);
var
  pass, v, w, tempDis: integer;
begin
  _Graph := g as TWeightedGraph;
  _HasNegativeCycle := false;
  TArrayUtils_int.SetLengthAndFill(_Dis, _Graph.Vertex, integer.MaxValue);
  TArrayUtils_int.SetLengthAndFill(_Pre, _Graph.Vertex, -1);
  _Dis[0] := 0;

  for pass := 1 to _Graph.Vertex - 1 do
  begin
    for v := 0 to _Graph.Vertex - 1 do
    begin
      for w in _Graph.Adj(v) do
      begin
        if (_Dis[v] <> integer.MaxValue) then
        begin
          tempDis := _Dis[v] + _Graph.GetWeight(v, w);
          if tempDis < _Dis[w] then
          begin
            _Dis[w] := tempDis;
            _Pre[w] := v;
          end;
        end;
      end;
    end;
  end;

  for v := 0 to _Graph.Vertex - 1 do
  begin
    for w in _Graph.Adj(v) do
    begin
      if (_Dis[v] <> integer.MaxValue) then
      begin
        tempDis := _Dis[v] + _Graph.GetWeight(v, w);
        if tempDis < _Dis[w] then
        begin
          _HasNegativeCycle := true;
          Break;
        end;
      end;
    end;

    if _HasNegativeCycle then
      Break;
  end;
end;

destructor TBellmanFord.Destroy;
begin
  inherited Destroy;
end;

function TBellmanFord.DistTo(v: integer): integer;
begin
  _Graph.ValidateVertex(v);
  if HasNegativeCycle then
    raise Exception.Create('Exist negative cycle.');

  Result := _Dis[v];
end;

function TBellmanFord.IsConnectedTo(v: integer): boolean;
begin
  _Graph.ValidateVertex(v);
  Result := _Dis[v] <> integer.MaxValue;
end;

function TBellmanFord.PathTo(v: integer): TArr_int;
var
  list: IList_int;
  cur: integer;
begin
  list := TLinkedList_int.Create;
  cur := v;

  while cur <> 0 do
  begin
    list.AddFirst(cur);
    cur := _Pre[cur];
  end;
  list.AddFirst(0);

  Result := list.ToArray;
end;

end.
