unit GTA.MaxFlow;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Math,
  GTA.Utils,
  DeepStar.Utils;

type
  TMaxFlow = class(TObject)
  private
    _WGraph: IWeightedGraph;
    _RGraph: TWeightedGraph;
    _Source: integer;
    _Target: integer;
    _MaxFlow: integer;

    function _GetAugmentingPath: TArrayList_int;

  public
    constructor Create(network: IWeightedGraph; s, t: integer);
    destructor Destroy; override;

    function Return: integer;
    function Flow(v, w: integer): integer;
  end;

procedure Main;

implementation

procedure Main;
var
  network: TWeightedGraph;
  v, w: integer;
begin
  network := TWeightedGraph.Create(FileName('Chapter14-Network-Flows', 'network.txt'), true);
  with TMaxFlow.Create(network, 0, 3) do
  begin
    WriteLn(Return.ToString);
    for v := 0 to network.Vertex - 1 do
    begin
      for w in network.Adj(v) do
      begin
        WriteLn(Format('%d-%d: %d / %d', [v, w, Flow(v, w), network.GetWeight(v, w)]));
      end;
    end;
    Free;
  end;

  DrawLineBlockEnd;
  ////////////////////////////////

  network := TWeightedGraph.Create(FileName('Chapter14-Network-Flows', 'network2.txt'), true);
  with TMaxFlow.Create(network, 0, 5) do
  begin
    WriteLn(Return.ToString);
    for v := 0 to network.Vertex - 1 do
    begin
      for w in network.Adj(v) do
      begin
        WriteLn(Format('%d-%d: %d / %d', [v, w, Flow(v, w), network.GetWeight(v, w)]));
      end;
    end;
    Free;
  end;

  DrawLineBlockEnd;
  ////////////////////////////////

  network := TWeightedGraph.Create(FileName('Chapter14-Network-Flows', 'baseball.txt'), true);
  with TMaxFlow.Create(network, 0, 10) do
  begin
    WriteLn(Return.ToString);
    for v := 0 to network.Vertex - 1 do
    begin
      for w in network.Adj(v) do
      begin
        WriteLn(Format('%d-%d: %d / %d', [v, w, Flow(v, w), network.GetWeight(v, w)]));
      end;
    end;
    Free;
  end;
end;

{ TMaxFlow }

constructor TMaxFlow.Create(network: IWeightedGraph; s, t: integer);
var
  g: TWeightedGraph;
  v, w, f, i: integer;
  augPath: TArrayList_int;
begin
  g := network as TWeightedGraph;

  if not g.IsDirected then
    raise Exception.Create('MaxFlow only works in directed graph.');

  if g.Vertex < 2 then
    raise Exception.Create('The network should has at least 2 vertices.');

  if s = t then
    raise Exception.Create('s and t should be different.');

  g.ValidateVertex(s);
  g.ValidateVertex(t);

  _Source := s;
  _Target := t;
  _WGraph := network;

  _RGraph := TWeightedGraph.Create(g.Vertex, true);
  for v := 0 to g.Vertex - 1 do
    for w in g.Adj(v) do
    begin
      _RGraph.AddEdge(v, w, g.GetWeight(v, w));
      _RGraph.AddEdge(w, v, 0);
    end;

  while true do
  begin
    augPath := _GetAugmentingPath;
    try
      if augPath.IsEmpty then Break;

      // 计算增广路径上的最小值
      f := integer.MaxValue;
      for i := 1 to augPath.Count - 1 do
      begin
        v := augPath[i - 1];
        w := augPath[i];
        f := Math.Min(f, _RGraph.GetWeight(v, w));
      end;

      _MaxFlow += f;

      // 根据增广路径更新 rG
      for i := 1 to augPath.Count - 1 do
      begin
        v := augPath[i - 1];
        w := augPath[i];

        _RGraph.SetWeight(v, w, _RGraph.GetWeight(v, w) - f);
        _RGraph.SetWeight(w, v, _RGraph.GetWeight(w, v) + f);
      end;
    finally
      FreeAndNil(augPath);
    end;
  end;
end;

destructor TMaxFlow.Destroy;
begin
  _RGraph.Free;
  inherited Destroy;
end;

function TMaxFlow.Flow(v, w: integer): integer;
var
  g: TWeightedGraph;
begin
  g := _WGraph as TWeightedGraph;
  if not g.HasEdge(v, w) then
    raise Exception.Create(Format('No edge %d-%d', [v, w]));

  Result := _RGraph.GetWeight(w, v);
end;

function TMaxFlow.Return: integer;
begin
  Result := _MaxFlow;
end;

function TMaxFlow._GetAugmentingPath: TArrayList_int;
var
  queue: IQueue_int;
  pre: TArr_int;
  cur, Next: integer;
begin
  queue := TQueue_int.Create;
  TArrayUtils_int.SetLengthAndFill(pre, _RGraph.Vertex, -1);

  queue.EnQueue(_Source);
  pre[_Source] := _Source;

  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;
    for Next in _RGraph.Adj(cur) do
    begin
      if (pre[Next] = -1) and (_RGraph.GetWeight(cur, Next) > 0) then
      begin
        queue.EnQueue(Next);
        pre[Next] := cur;
      end;
    end;
  end;

  Result := TArrayList_int.Create;
  if pre[_Target] = -1 then Exit;

  cur := _Target;
  while cur <> _Source do
  begin
    Result.AddLast(cur);
    cur := pre[cur];
  end;
  Result.AddLast(_Source);

  Result.Reverse;
end;

end.
