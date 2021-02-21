unit GTA.BellmanFord;

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

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;
    function DistTo(v: integer): integer;
    function IsConnectedTo(v: integer): boolean;

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
            _Dis[w] := tempDis;
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

end.
