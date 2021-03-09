unit GTA.MaxFlow;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  DeepStar.Utils;

type
  TMaxFlow = class(TObject)
  private
    _WGraph: IWeightedGraph;
    _Source: integer;
    _Target: integer;
    _MaxFlow: integer;

  public
    constructor Create(network: IWeightedGraph; s, t: integer);
    destructor Destroy; override;

  end;

implementation

{ TMaxFlow }

constructor TMaxFlow.Create(network: IWeightedGraph; s, t: integer);
var
  g: TWeightedGraph;
begin
  g := _WGraph as TWeightedGraph;

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


end;

destructor TMaxFlow.Destroy;
begin
  inherited Destroy;
end;

end.
