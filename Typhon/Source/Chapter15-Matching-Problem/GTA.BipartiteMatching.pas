unit GTA.BipartiteMatching;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.BipartitionDetection,
  GTA.MaxFlow;

type
  TBipartiteMatching = class(TObject)
  public type
    TColor = TBipartitionDetection.TColor;

  private
    _Graph: IGraph;
    _MaxMatching: integer;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function IsPerfectMatching: boolean;

    property MaxMatching: integer read _MaxMatching;
  end;

procedure Main;

implementation

procedure Main;
var
  g: TGraph;
begin
  g := TGraph.Create(FileName('Chapter15-Matching-Problem', 'g.txt'));
  with TBipartiteMatching.Create(g) do
  begin
    WriteLn(MaxMatching);
    WriteLn(IsPerfectMatching);
    Free;
  end;
end;

{ TBipartiteMatching }

constructor TBipartiteMatching.Create(g: IGraph);
var
  bd: TBipartitionDetection;
  network: TWeightedGraph;
  colors: TBipartitionDetection.TArr_Color;
  v, w: integer;
  maxFlow: TMaxFlow;
begin
  bd := TBipartitionDetection.Create(g);
  try
    if not bd.IsBipartite then
      raise Exception.Create('BipartiteMatching only works for bipartite graph.');

    _Graph := g;
    colors := bd.Colors;

    //源点为 _Graph.Vertex， 汇点为 _Graph.Vertex+1
    network := TWeightedGraph.Create(_Graph.Vertex + 2, true);
    for v := 0 to _Graph.Vertex - 1 do
    begin
      case colors[v] of
        TColor.red: network.AddEdge(_Graph.Vertex, v, 1);
        TColor.green: network.AddEdge(v, _Graph.Vertex + 1, 1);
        else;
      end;

      for w in _Graph.Adj(v) do
      begin
        if v < w then
        begin
          case colors[v] of
            TColor.red: network.AddEdge(v, w, 1);
            TColor.green: network.AddEdge(w, v, 1);
            else;
          end;
        end;
      end;
    end;

    maxFlow := TMaxFlow.Create(network, _Graph.Vertex, _Graph.Vertex + 1);
    try
      _MaxMatching := maxFlow.Return;
    finally
      FreeAndNil(maxFlow);
    end;
  finally
    FreeAndNil(bd);
  end;
end;

destructor TBipartiteMatching.Destroy;
begin
  inherited Destroy;
end;

function TBipartiteMatching.IsPerfectMatching: boolean;
begin
  Result := MaxMatching * 2 = _Graph.Vertex;
end;

end.
