unit GTA.Leetcode_743_Dijkstra;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes,
  SysUtils,
  Math,
  DeepStar.DSA.Tree.PriorityQueue,
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
      function Adj(v: integer): TArr_int;
      function GetWeight(v, w: integer): integer;

      property Vertex: integer read _Vertex;
    end;

    TPair = record
      Vertex: integer;
      Weight: integer;
      class function Create(newVertex, newWeight: integer): TPair; static;
      class function Comparer(constref a, b: TPair): integer; static;
    end;

    TQueue_TPair = specialize TPriorityQueue<TPair>;

  private
    _Dis: TArr_int;
    _Visited: TArr_bool;

    procedure __Dijkstra(g: TWeightGraph; k: integer);

  public
    function NetworkDelayTime(times: TArr2D_int; n, k: integer): integer;

  end;

procedure Main;

implementation

procedure Main;
begin
  //示例 1：
  //输入：times = [[2,1,1],[2,3,1],[3,4,1]], n = 4, k = 2
  //输出：2
  with TSolution.Create do
  begin
    WriteLn(NetworkDelayTime([[2, 1, 1], [2, 3, 1], [3, 4, 1]], 4, 2));
    Free;
  end;

  //示例 2：
  //输入：times = [[1,2,1]], n = 2, k = 1
  //输出：1
  with TSolution.Create do
  begin
    WriteLn(NetworkDelayTime([[1, 2, 1]], 2, 1));
    Free;
  end;

  //示例 3：
  //输入：times = [[1,2,1]], n = 2, k = 2
  //输出：-1
  with TSolution.Create do
  begin
    WriteLn(NetworkDelayTime([[1, 2, 1]], 2, 2));
    Free;
  end;
end;

{ TSolution.TPair }

class function TSolution.TPair.Comparer(constref a, b: TPair): integer;
begin
  Result := a.Weight - b.Weight;
end;

class function TSolution.TPair.Create(newVertex, newWeight: integer): TPair;
var
  res: TPair;
begin
  res.Vertex := newVertex;
  res.Weight := newWeight;
  Result := res;
end;

{ TSolution.TWeightGraph }

constructor TSolution.TWeightGraph.Create(n: integer);
var
  i: integer;
begin
  SetLength(_Adj, n);
  for i := 0 to High(_Adj) do
    _Adj[i] := TTreeMap_int_int.Create;
end;

procedure TSolution.TWeightGraph.AddEdge(v, w, weight: integer);
begin
  _Adj[v].Add(w, weight);
  //_Adj[w].Add(v, weight);
end;

function TSolution.TWeightGraph.Adj(v: integer): TArr_int;
begin
  Result := _Adj[v].Keys;
end;

destructor TSolution.TWeightGraph.Destroy;
var
  i: integer;
begin
  for i := 0 to High(_Adj) do
    _Adj[i].Free;

  inherited Destroy;
end;

function TSolution.TWeightGraph.GetWeight(v, w: integer): integer;
begin
  Result := _Adj[v][w];
end;

{ TSolution }

function TSolution.NetworkDelayTime(times: TArr2D_int; n, k: integer): integer;
var
  g: TWeightGraph;
  res: integer;
  time: TArr_int;
begin
  TArrayUtils_int.SetLengthAndFill(_Dis, n, integer.MaxValue);
  TArrayUtils_bool.SetLengthAndFill(_Visited, n, false);

  g := TWeightGraph.Create(n);
  try
    for time in times do
      g.AddEdge(time[0] - 1, time[1] - 1, time[2]);

    __Dijkstra(g, k - 1);
    res := Math.MaxIntValue(_Dis);
    Result := IfThen(res = integer.MaxValue, -1, res);
  finally
    g.Free;
  end;
end;

procedure TSolution.__Dijkstra(g: TWeightGraph; k: integer);
var
  queue: TQueue_TPair;
  v, w, tempDis: integer;
begin
  _Dis[k] := 0;
  queue := TQueue_TPair.Create(TQueue_TPair.TCmp.Construct(@TPair.Comparer));
  try
    queue.EnQueue(TPair.Create(k, 0));

    while not queue.IsEmpty do
    begin
      v := TPair(queue.DeQueue).Vertex;
      _Visited[v] := true;

      for w in g.Adj(v) do
      begin
        tempDis := _Dis[v] + g.GetWeight(v, w);

        if (not _Visited[w]) and (_Dis[w] > tempDis) then
        begin
          _Dis[w] := tempDis;
          queue.EnQueue(TPair.Create(w, _Dis[w]));
        end;
      end;
    end;
  finally
    queue.Free;
  end;
end;

end.
