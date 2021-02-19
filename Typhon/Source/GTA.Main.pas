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
  GTA.Dijkstra_More_about_Dijkstra;

procedure Test;
var
  t: TTreeMap_int_int;
begin
  t := TTreeMap_int_int.Create;
  t.Add(1, 1);
  t.Add(1, 2);

  TArrayUtils_int.Print(t.Keys);
  TArrayUtils_int.Print(t.Values);
end;

procedure Run;
begin
  Test;
  Main;
end;

end.
