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
  GTA.TopoSort2;

procedure Test;
begin

end;

procedure Run;
begin
  Test;
  Main;
end;

end.
