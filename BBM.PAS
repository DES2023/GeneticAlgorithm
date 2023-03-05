unit BBM;

{$MODE Delphi}

interface
 uses
  VarType,
  Solver,
  Solution,
  Classes,
  SetEvaluation,
  Sysutils;
 type

  TSegment = record
   VarNum   : TInt;
   Segment  :TSetEvaluation;
  end;

  PTBBM = ^TBBM;
  TBBM = class(TSolver)
  private
   function Up :TBool;
  public
   Oder                          :TIntArray;
   StatN                         :TIntArray;
   StatY                         :TIntArray;
   KnownRecordValue              :TReal;

   History                       :array of TSegment;
   hisIndex                      :TInt;

   Destructor Destroy;override;

   Procedure GetMemory;override;
   Procedure FreeMemory;override;

   Procedure Load(var f : TStringList);

   Procedure DeepBranchBoundMethod;

   Procedure Iteration;override;
   Procedure FirstApproximation;override;

   Procedure SaveStatistics(Name:TString);

   Procedure ShowResults;override;
   Procedure ShowRecord;override;
  end;
implementation
 uses
  Math,
  FOrder;
{******************************************************************************}
 Destructor TBBM.Destroy;
 begin
  FreeMemory;
  inherited;
 end;
{******************************************************************************}
 Procedure TBBM.GetMemory;
 var i:TInt;
     l:TInt;

 begin
  inherited;
  RunQuantity:=1;
  TerminationCondition:=1;
  TerminationParameter:=1;
  Results.Quantity:=1;
  SetLength(Oder,Task.Size);
  SetLength(StatN,Task.Size);
  SetLength(StatY,Task.Size);

  l:=0;
  for i:=0 to Task.Size_ do
  begin
   if Task.UpBoundary[i]<>0 then
   begin
    l:=l+Round(Int(Log2(Task.UpBoundary[i]))+2);
   end;
  end;

  SetLength(History,l);
  for i:=0 to l-1 do
  begin
   History[i].Segment:=TSetEvaluation.Create;
   History[i].Segment.Size:=Task.Size;
  end;

 end;
{******************************************************************************}
 Procedure TBBM.FreeMemory;
 begin
  Finalize(Oder);
  Finalize(StatN);
  Finalize(StatY);
  inherited;
 end;
{******************************************************************************}
 Procedure TBBM.Load(var f : TStringList);
 var
  i,j,t,cod,rez  :TInt;
  s,k    :string;
  p:TReal;

 begin
  inherited;

  for i:=0 to f.Count-1 do
  begin

   s:=f.Strings[i];
   while pos(' ',s) = 1 do delete(s,1,1);
   j:=pos(' ',s);
   if j = 0 then k:=s
   else k:=copy(s,1,j-1);

{______________________________________________________________________________}

   if k = 'Record' then
   begin
    s:=copy(s,j+1,length(s));
    while pos(' ',s) = 1 do delete(s,1,1);
    j:=pos(' ',s);
    if j=0 then k:=copy(s,1,length(s))
    else k:=copy(s,1,j-1);
    val (K,KnownRecordValue,cod);
   end;
{______________________________________________________________________________}
  end;
 end;
{******************************************************************************}
 Procedure TBBM.FirstApproximation;
 var i,j,k:TInt;
     a:TIntArray;
 begin
  SetLength(a,Task.Size);
  for i:=0 to Task.Size_ do
  begin
   a[i]:=Task.UpBoundary[i];
   Oder[i]:=i;
  end;

  for i:=0 to Task.Size_ do
   for j:=i+1 to Task.Size_ do
    if a[i]>a[j] then
    begin
     k:=a[i];
     a[i]:=a[j];
     a[j]:=k;

     k:=Oder[i];
     Oder[i]:=Oder[j];
     Oder[j]:=k;
    end;

  Finalize(a);
 end;
{******************************************************************************}
 Function TBBM.Up:TBool;
 var b:TBool;
     i:TInt;
 begin

  while true do
  begin

   if hisIndex=0 then
   begin
    Up:=false;
    break;
   end;

   if History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]
    < History[hisIndex-1].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]] then
   begin

    for i:=0 to History[hisIndex].VarNum-1 do
     History[hisIndex].Segment.BuffersEmpty.Element[Oder[i]]:= History[hisIndex].Segment.BuffersFull.Element[Oder[i]];

    History[hisIndex].Segment.BuffersEmpty.Element[Oder[History[hisIndex].VarNum]]:=
    min(
        History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]+1,
        History[hisIndex-1].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]);

    Task.GetSolutionValue(History[hisIndex].Segment.BuffersEmpty);
    History[hisIndex].Segment.BuffersFull.Assign(History[hisIndex-1].Segment.BuffersFull);

    Up:=true;

    break;
   end
   else
   begin
    if History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]
     = Task.UpBoundary[Oder[History[hisIndex].VarNum]] then
    begin
     if History[hisIndex].VarNum=0 then
     begin
      Up:=false;
      break;
     end;
     hisIndex:=hisIndex-1;
    end
    else
    begin
     if History[hisIndex].VarNum <> History[hisIndex-1].VarNum then
     begin
      History[hisIndex].Segment.BuffersEmpty.Assign(History[hisIndex].Segment.BuffersFull);
      History[hisIndex].Segment.BuffersFull.Assign(History[hisIndex-1].Segment.BuffersFull);


      History[hisIndex].Segment.BuffersEmpty.Element[Oder[History[hisIndex].VarNum]]:=
      min(
          History[hisIndex].Segment.BuffersEmpty.Element[Oder[History[hisIndex].VarNum]]+1,
          Task.UpBoundary[Oder[History[hisIndex].VarNum]]
      );

      History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]:=
      Task.UpBoundary[Oder[History[hisIndex].VarNum]];

      Task.GetSolutionValue(History[hisIndex].Segment.BuffersFull);
      Task.GetSolutionValue(History[hisIndex].Segment.BuffersEmpty);

      Up:=true;

      break;
     end;
     hisIndex:=hisIndex-1;
    end;
   end;
  end;

 end;
{******************************************************************************}
 Procedure TBBM.DeepBranchBoundMethod;
 var difference:TReal;
     i:TInt;s:TSTring;
     b:TBool;
     exp:TIntArray;
 begin
{$O-}
//  Task.GetSolutionValue(NowRecord);
{  SetLength(exp,8);
  exp[0]:=20;
  exp[1]:=20;
  exp[2]:=18;
  exp[3]:=20;
  exp[4]:=19;
  exp[5]:=20;
  exp[6]:=16;
  exp[7]:=0;}

  hisIndex:=0;
  History[hisIndex].VarNum:=0;

  for i:=0 to Task.Size_ do
  begin
   History[hisIndex].Segment.BuffersFull.Element[i]:=Task.UpBoundary[i];
   History[hisIndex].Segment.BuffersEmpty.Element[i]:=0;
  end;

  Task.GetSolutionValue(History[hisIndex].Segment.BuffersFull);
  Task.GetSolutionValue(History[hisIndex].Segment.BuffersEmpty);

  while true do
  begin
//   ShowResults;
   if History[hisIndex].Segment.Evaluation < KnownRecordValue then
   begin
{b:=true;
for i:=0 to Task.Size_ do
if (exp[i]<History[hisIndex].Segment.BuffersEmpty.Element[i]) or
(exp[i]>History[hisIndex].Segment.BuffersFull.Element[i]) then b:=false;}

{if b then
  FormBBM.Memo.Lines.Add('Oops!!!');}

    StatN[Oder[History[hisIndex].VarNum]]:=StatN[Oder[History[hisIndex].VarNum]]+1;
    if not Up then break;
   end
   else
   begin
    StatY[Oder[History[hisIndex].VarNum]]:=StatY[Oder[History[hisIndex].VarNum]]+1;

    if History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]
     - History[hisIndex].Segment.BuffersEmpty.Element[Oder[History[hisIndex].VarNum]] = 0 then
    begin

     b:=True;
     for i:=History[hisIndex].VarNum+1 to Task.Size_ do
      if Task.UpBoundary[Oder[i]]<>0 then
      begin
       b:=false;
       break;
      end;

     if b then
     begin
      NowRecord.Assign(History[hisIndex].Segment.BuffersFull);
      KnownRecordValue:=NowRecord.GetValue;
      if not Up then break;
     end
     else
     begin
      hisIndex:=hisIndex+1;

      History[hisIndex].VarNum:=i;

      History[hisIndex].Segment.BuffersEmpty.Assign(History[hisIndex-1].Segment.BuffersEmpty);
      History[hisIndex].Segment.BuffersFull.Assign(History[hisIndex-1].Segment.BuffersFull);

      History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]:=Round(Int(Task.UpBoundary[Oder[History[hisIndex].VarNum]]*0.5));

      Task.GetSolutionValue(History[hisIndex].Segment.BuffersFull);
     end;
    end
    else
    begin
     difference:=Int((
       History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]
     - History[hisIndex].Segment.BuffersEmpty.Element[Oder[History[hisIndex].VarNum]]
     )*0.5);

     hisIndex:=hisIndex+1;

     History[hisIndex].VarNum:=History[hisIndex-1].VarNum;

     History[hisIndex].Segment.BuffersEmpty.Assign(History[hisIndex-1].Segment.BuffersEmpty);
     History[hisIndex].Segment.BuffersFull.Assign(History[hisIndex-1].Segment.BuffersFull);

     History[hisIndex].Segment.BuffersFull.Element[Oder[History[hisIndex].VarNum]]:=
     History[hisIndex].Segment.BuffersEmpty.Element[Oder[History[hisIndex].VarNum]]+
     Round(difference);

     Task.GetSolutionValue(History[hisIndex].Segment.BuffersFull);
//     Task.GetSolutionValue(History[hisIndex].Segment.BuffersEmpty);
    end;

   end;
  end;
//  ShowRecord;
{$O+}
 end;
{******************************************************************************}
 procedure TBBM.ShowRecord;
 var
  i:TInt;
  s:TString;
  p:PTSolution;
  Hour, Min, Sec, MSec   :Word;
 begin
{  if FormBBM.Memo.Lines.Count>250 then FormBBM.Memo.Lines.Delete(0);
  s:='Solution: ';
  for i:=0 to Task.Size_ do
   s:=s+IntToStr(nowRecord.Element[i])+' ';
  s:=s+' Value: '+FloatToStr(nowRecord.GetValue);
  DecodeTime(Time-begintime,Hour, Min, Sec, MSec);
  s:=s+' Time: '+FloatToStr(sec+60*Min+3600*Hour+msec/1000);
  FormBBM.Memo.Lines.Add(s);
  for i:=0 to Task.Size_ do
   FormBBM.Memo.Lines.Add(IntToStr(Stat[oder[i]]));}
 end;
{******************************************************************************}
 procedure TBBM.ShowResults;
 var
  i:TInt;
  s,s1,s2:TString;
  p:PTSolution;
  Hour, Min, Sec, MSec   :Word;
 begin
  for i:=0 to Task.Size_ do
  begin
   str(History[hisIndex].Segment.BuffersFull.Element[i]:4,s1);
   str(History[hisIndex].Segment.BuffersEmpty.Element[i]:4,s2);
   s:=s+'['+s1
       +','+s2
       +'] ';
  end;
  s:=s+FloatToStr(History[hisIndex].Segment.Evaluation);
  writeln(s);
 end;
{******************************************************************************}
 Procedure TBBM.Iteration;
 begin
  DeepBranchBoundMethod;
 end;
{******************************************************************************}
 Procedure TBBM.SaveStatistics(Name:TString);
 var
  f:TStringList;
  i:TInt;
 begin
 f:=TStringList.Create;
 f.Duplicates:=dupIgnore;
 f.Sorted:=false;

  for i:=0 to Task.Size_ do
   f.Add(IntToStr(StatN[i])+'/'+IntToStr(StatY[i]));

 f.SaveToFile(Name);
 f.Free;
 end;
{******************************************************************************}
end.

