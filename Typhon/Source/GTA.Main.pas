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
  GTA.Kruskal;

procedure Test;
var
  a: TKruskal;
  g: TWeightedGraph;
begin
  g := TWeightedGraph.Create(FileName('Chapter11-Minimum-Tree-Spanning', 'g.txt'));
  a := TKruskal.Create(g);
  a.ToString;
end;

procedure Run;
begin
  Test;
  Main;
end;

end.
