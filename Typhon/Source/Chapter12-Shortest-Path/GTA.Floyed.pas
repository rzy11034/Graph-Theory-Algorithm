unit GTA.Floyed;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TFloyed = class(TObject)
  private
    _Graph: TWeightedGraph;
    _HasNegativeCycle: boolean;
    _Dis: TArr2D_int;

    function __Floyed: boolean;

  public
    constructor Create(g: IWeightedGraph);
    destructor Destroy; override;

    function IsConnectedTo(v, w: integer): boolean;
    function DistTo(v, w: integer): integer;

    property HasNegativeCycle: boolean read _HasNegativeCycle;
  end;

procedure Main;

implementation

procedure Main;
var
  g: IWeightedGraph;
  v, w: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter12-Shortest-Path', 'g.txt'));
  with TFloyed.Create(g) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
      begin
        for w := 0 to g.Vertex - 1 do
          Write(DistTo(v, w), ' ');
        WriteLn;
      end;
      WriteLn;
    end
    else
      WriteLn('exist negative cycle.');

    Free;
  end;
  DrawLineBlockEnd;

  //======================================

  g := TWeightedGraph.Create(FileName('Chapter12-Shortest-Path', 'g2.txt'));
  with TFloyed.Create(g) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
        for w := 0 to g.Vertex - 1 do
          WriteLn(format(' v:%d --> w:%d = %d', [v, w, DistTo(v, w)]));
      WriteLn;
    end
    else
      WriteLn('Exist negative cycle.');

    Free;
  end;
end;

{ TFloyed }

constructor TFloyed.Create(g: IWeightedGraph);
begin
  _Graph := g as TWeightedGraph;
  TArrayUtils_int.SetLengthAndFill(_Dis, g.Vertex, g.Vertex, integer.MaxValue);

  _HasNegativeCycle := __Floyed;
end;

destructor TFloyed.Destroy;
begin
  inherited Destroy;
end;

function TFloyed.DistTo(v, w: integer): integer;
begin
  _Graph.ValidateVertex(v);
  _Graph.ValidateVertex(w);
  Result := _Dis[v, w];
end;

function TFloyed.IsConnectedTo(v, w: integer): boolean;
begin
  _Graph.ValidateVertex(v);
  _Graph.ValidateVertex(w);
  Result := _Dis[v, w] <> integer.MaxValue;
end;

function TFloyed.__Floyed: boolean;
var
  v, w, t, tempDis: integer;
begin
  for v := 0 to _Graph.Vertex - 1 do
    _Dis[v, v] := 0;

  for v := 0 to _Graph.Vertex - 1 do
    for w in _Graph.Adj(v) do
      _Dis[v, w] := _Graph.GetWeight(v, w);

  for t := 0 to _Graph.Vertex - 1 do
    for v := 0 to _Graph.Vertex - 1 do
      for w := 0 to _Graph.Vertex - 1 do
        if (_Dis[v, t] <> integer.MaxValue) and (_Dis[t, w] <> integer.MaxValue) then
        begin
          tempDis := _Dis[v, t] + _Dis[t, w];

          if tempDis < _Dis[v, w] then
            _Dis[v, w] := tempDis;
        end;

  Result := false;

  for v := 0 to _Graph.Vertex - 1 do
    if _Dis[v, v] < 0 then
      Exit(true);
end;

end.
