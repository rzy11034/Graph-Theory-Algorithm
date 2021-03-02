unit GTA.Leetcode_210;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  DeepStar.Utils,
  GTA.Utils;

//210. 课程表 II
//现在你总共有 n 门课需要选，记为 0 到 n-1。
//
//在选修某些课程之前需要一些先修课程。 例如，想要学习课程 0 ，你需要先完成课程 1 ，
//我们用一个匹配来表示他们: [0,1]
//
//给定课程总量以及它们的先决条件，返回你为了学完所有课程所安排的学习顺序。
//
//可能会有多个正确的顺序，你只要返回一种就可以了。如果不可能完成所有课程，返回一个空数组。
//
//示例 1:
//输入: 2, [[1,0]]
//输出: [0,1]
//解释: 总共有 2 门课程。要学习课程 1，你需要先完成课程 0。因此，正确的课程顺序为 [0,1] 。
//
//示例 2:
//输入: 4, [[1,0],[2,0],[3,1],[3,2]]
//输出: [0,1,2,3] or [0,2,1,3]
//解释: 总共有 4 门课程。要学习课程 3，你应该先完成课程 1 和课程 2。
//并且课程 1 和课程 2 都应该排在课程 0 之后。
//因此，一个正确的课程顺序是 [0,1,2,3] 。另一个正确的排序是 [0,2,1,3] 。
type
  TSolution = class(TObject)
  public
    function FindOrder(numCourses: integer; prerequisites: TArr2D_int): TArr_int;
  end;

procedure Main;

implementation

procedure Main;
begin
  with TSolution.Create do
  begin
    TArrayUtils_int.Print(FindOrder(2, [[1, 0]]));
    Free;
  end;

  with TSolution.Create do
  begin
    TArrayUtils_int.Print(FindOrder(4, [[1, 0], [2, 0], [3, 1], [3, 2]]));
    Free;
  end;
end;

{ TSolution }

function TSolution.FindOrder(numCourses: integer; prerequisites: TArr2D_int): TArr_int;
var
  list: IList_int;
  queue: IQueue_int;
  g: IGraph;
  inDegrees: TArr_int;
  i, v: integer;
  cur, Next: integer;
begin
  g := TGraph.Create(numCourses, true);
  for i := 0 to High(prerequisites) do
    (g as TGraph).AddEdge(prerequisites[i, 0], prerequisites[i, 1]);

  queue := TQueue_int.Create;
  SetLength(inDegrees, g.Vertex);
  for v := 0 to g.Vertex - 1 do
  begin
    inDegrees[v] := (g as TGraph).InDegree(v);

    if inDegrees[v] = 0 then
      queue.EnQueue(v);
  end;

  list := TLinkedList_int.Create;
  while not queue.IsEmpty do
  begin
    cur := queue.DeQueue;
    list.AddFirst(cur);

    for Next in g.Adj(cur) do
    begin
      inDegrees[Next] -= 1;

      if inDegrees[Next] = 0 then
        queue.EnQueue(Next);
    end;
  end;

  Result := list.ToArray;
end;

end.
