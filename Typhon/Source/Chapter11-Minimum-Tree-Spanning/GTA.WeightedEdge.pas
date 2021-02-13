unit GTA.WeightedEdge;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Interfaces,
  DeepStar.UString;

type
  TWeightedEdge = class(TInterfacedObject, IWeightedEdge)
  private
    _VertexV, _VertexW, _Weight: integer;

  public
    constructor Create(v, w, weight: integer);
    destructor Destroy; override;

    function VertexV: integer;
    function VertexW: integer;
    function Weight: integer;
    function ToString: UString; reintroduce;

    class function Compare(constref a, b: TWeightedEdge): integer;
  end;

implementation

{ TWeightedEdge }

constructor TWeightedEdge.Create(v, w, weight: integer);
begin
  _VertexV := v;
  _VertexW := w;
  _Weight := weight;
end;

class function TWeightedEdge.Compare(constref a, b: TWeightedEdge): integer;
begin
  Result := a._Weight - b._Weight;
end;

destructor TWeightedEdge.Destroy;
begin
  inherited Destroy;
end;

function TWeightedEdge.ToString: UString;
begin
  Result := Format('(%d-%d: %d)', [_VertexV, _VertexW, _Weight]);
end;

function TWeightedEdge.VertexV: integer;
begin
  Result := _VertexV;
end;

function TWeightedEdge.VertexW: integer;
begin
  Result := _VertexW;
end;

function TWeightedEdge.Weight: integer;
begin
  Result := _Weight;
end;

end.
