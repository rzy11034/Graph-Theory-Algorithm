unit GTA.Dijkstra;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

type
  TDijkstra = class(TObject)
  private
    _Dis: TArr_int;
    _Graph: TWeightedGraph;

  public
    constructor Create(g: IWeightedGraph; s: integer);
    destructor Destroy; override;

  end;

implementation

{ TDijkstra }

constructor TDijkstra.Create(g: IWeightedGraph; s: integer);
begin
  _Graph := g as TWeightedGraph;
  SetLength(_Dis, _Graph.Vertex);
  TArrayUtils_int.FillArray(_Dis, Integer.MaxValue);
  _Dis[s] := 0;


end;

destructor TDijkstra.Destroy;
begin
  inherited Destroy;
end;

end.
