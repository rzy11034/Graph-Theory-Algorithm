﻿unit GTA.Main;

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
begin
  Exit;
end;

procedure Run;
begin
  Test;
  Main;
end;

end.
