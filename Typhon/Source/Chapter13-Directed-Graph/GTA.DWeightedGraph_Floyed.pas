unit GTA.DWeightedGraph_Floyed;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  GTA.Utils,
  GTA.Floyed,
  DeepStar.Utils;

procedure Main;

implementation

procedure Main;
var
  g: IWeightedGraph;
  v, w: integer;
begin
  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg_1.txt'), true);
  with TFloyed.Create(g) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
      begin
        for w := 0 to g.Vertex - 1 do
          Write(DistTo(v, w), ' ');
        WriteLn;
      end;
      WriteLn;
    end
    else
      WriteLn('exist negative cycle.');

    Free;
  end;
  DrawLineBlockEnd;

  //======================================

  g := TWeightedGraph.Create(FileName('Chapter13-Directed-Graph', 'wg_1.txt'), true);
  with TFloyed.Create(g) do
  begin
    if not HasNegativeCycle then
    begin
      for v := 0 to g.Vertex - 1 do
        for w := 0 to g.Vertex - 1 do
          WriteLn(format(' v:%d --> w:%d = %d', [v, w, DistTo(v, w)]));
      WriteLn;
    end
    else
      WriteLn('Exist negative cycle.');

    Free;
  end;
end;

end.
