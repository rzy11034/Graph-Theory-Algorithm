unit GTA.EulerLoop;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.CC;

type
  TEulerLoop = class(TObject)
  private
    _Graph: IGraph;

  public
    constructor Create(g: IGraph);
    destructor Destroy; override;

    function HasEulerLoop: boolean;
  end;

implementation

{ TEulerLoop }

constructor TEulerLoop.Create(g: IGraph);
begin
  _Graph := g;
end;

destructor TEulerLoop.Destroy;
begin
  inherited Destroy;
end;

function TEulerLoop.HasEulerLoop: boolean;
var
  cc: TCC;
begin
  cc := TCC.Create(_Graph);
  if cc.Count > 1 then
  begin
    cc.Free;
    Exit(false);
  end;

  for v := 0 to _Graph.V - 1 do
  begin
    if Odd(_Graph.Degree(v)) then
      Exit(false);
  end;

  Result := true;
end;

end.
