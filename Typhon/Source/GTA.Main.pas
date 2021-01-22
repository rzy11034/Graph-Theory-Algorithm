unit GTA.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils, DeepStar.Utils;

procedure Run;

implementation

uses
  GTA.Leetcode_752;

procedure Run;
var
  s: string;
  c: Char;
  n: integer;
begin
  s := '0';
  n:= ord(s.Chars[0]);
  TryStrToInt(s.Chars[0],n);
  Main;
end;

end.
