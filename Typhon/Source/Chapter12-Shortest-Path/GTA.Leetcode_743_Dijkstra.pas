unit GTA.Leetcode_743_Dijkstra;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils;

type
  //743. 网络延迟时间
  //有 n 个网络节点，标记为 1 到 n。
  //
  //给你一个列表 times，表示信号经过 有向 边的传递时间。
  //times[i] = (ui, vi, wi)，其中 ui 是源节点，vi 是目标节点，
  //wi 是一个信号从源节点传递到目标节点的时间。
  //
  //现在，从某个节点 K 发出一个信号。
  //需要多久才能使所有节点都收到信号？
  //如果不能使所有节点收到信号，返回 -1 。
  //
  //示例 1：
  //输入：times = [[2,1,1],[2,3,1],[3,4,1]], n = 4, k = 2
  //输出：2
  //示例 2：
  //
  //输入：times = [[1,2,1]], n = 2, k = 1
  //输出：1
  //示例 3：
  //
  //输入：times = [[1,2,1]], n = 2, k = 2
  //输出：-1
  TSolution = class(TObject)
  private type
    TArr_TreeMap_int_int = array of TTreeMap_int_int;

    TWeightGraph = class(TObject)
    private
      _Adj: TArr_TreeMap_int_int;
      _Vertex: integer;

    public
      constructor Create(n: integer);
      destructor Destroy; override;
      procedure AddEdge(v, w, weight: integer);

      property Vertex: integer read _Vertex;
    end;

  public
    function NetworkDelayTime(times: TArr2D_int; n, k: integer): integer;

  end;

procedure Main;

implementation

procedure Main;
begin

end;

{ TSolution.TWeightGraph }

constructor TSolution.TWeightGraph.Create(n: integer);
var
  i: Integer;
begin
  SetLength(_Adj, n);
  for i := 0 to high(_Adj) do
    _Adj[i] := TTreeMap_int_int.Create;
end;

procedure TSolution.TWeightGraph.AddEdge(v, w, weight: integer);
begin
  _Adj[v].Add(w, weight);
end;

destructor TSolution.TWeightGraph.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_Adj) do
    _Adj[i].Free;

  inherited Destroy;
end;

{ TSolution }

function TSolution.NetworkDelayTime(times: TArr2D_int; n, k: integer): integer;
begin

end;

end.
