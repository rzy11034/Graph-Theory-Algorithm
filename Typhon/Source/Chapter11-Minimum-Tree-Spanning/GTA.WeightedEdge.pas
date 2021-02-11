unit GTA.WeightedEdge;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.UString;

type
  TWeightedEdge = class(TObject)
  private
    _VertexV, _VertexW, _Weight: integer;

  public
    constructor Create(v, w, weight: integer);
    destructor Destroy; override;

    function ToString: UString; reintroduce;

    property VertexV: integer read _VertexV;
    property VertexW: integer read _VertexW;

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

end.
