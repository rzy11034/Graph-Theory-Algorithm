unit GTA.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {%H-}DeepStar.UString,
  {%H-}DeepStar.Utils,
  {%H-}GTA.Utils;

procedure Run;

implementation

uses
  GTA.WeightedEdge;

procedure Test;
var
  a, b: TWeightedEdge;
  &var: integer;
begin
  a := TWeightedEdge.Create(0, 0, 0);
  b := TWeightedEdge.Create(0, 0, 1);

  if b.Compare(a, b) < 1 then
    WriteLn(' a < b ');

  &var := -4;
  &var := &var >> 2;
  WriteLn(&var);

end;

procedure Run;
begin
  Test;
  //Main;
end;

end.
