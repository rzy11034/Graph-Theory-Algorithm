unit GTA.DWeightedGraph_BellmanFord;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.BellmanFord_More_about_BellmanFord,
  DeepStar.Utils;

procedure Main;

implementation

procedure Main;
var
  g: IWeightedGraph;
  v: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg_1.txt'), true);
  with TBellmanFord.Create(g, 0) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
        Write(DistTo(v), ' ');
      WriteLn;
    end
    else
      WriteLn('Exist negative cycle.');

    TArrayUtils_int.Print(PathTo(1));
    Free;
  end;
  DrawLineBlockEnd;

  //======================================

  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg_2.txt'), true);
  with TBellmanFord.Create(g, 0) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
        Write(DistTo(v), ' ');
      WriteLn;
    end
    else
      WriteLn('Exist negative cycle.');

    TArrayUtils_int.Print(PathTo(1));
    Free;
  end;
end;

end.
