unit GTA.Edge;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.UString;

type
  TEdge = class(TObject)
  private
    _v, _w: integer;

  public
    constructor Create(v, w: integer);
    function ToString: UString; reintroduce;
  end;

implementation

{ TEdge }

constructor TEdge.Create(v, w: integer);
begin
  _v := v;
  _w := w;
end;

function TEdge.ToString: UString;
begin
  Result := UnicodeFormat('%d-%d', [_v, _w]);
end;

end.
