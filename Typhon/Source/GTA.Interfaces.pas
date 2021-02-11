unit GTA.Interfaces;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  DeepStar.UString;

type
  IGraph = interface
    ['{A4B7DD24-63BD-4F9C-B710-A4FE8D7F3536}']
    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);
    procedure RemoveEdge(v, w: integer);
    function Vertex: integer;
    function Edge: integer;
  end;

  IWeightedGraph = interface
    ['{B93B15AF-D611-4561-8F0B-AC2369CBD626}']
    function Adj(v: integer): TArr_int;
    function Degree(v: integer): integer;
    function HasEdge(v, w: integer): boolean;
    function ToString: UString; reintroduce;
    procedure ValidateVertex(v: integer);
    procedure RemoveEdge(v, w: integer);
    function Vertex: integer;
    function Edge: integer;
    function GetWeight(v, w: integer): integer;
  end;

implementation

end.
