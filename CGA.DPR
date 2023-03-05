program CGA;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  windows,
  Messages,
  Problem,
  VarType,
  GA, LocalSearch;

var
 f1,f2,f3:TStringList;
 GA_:TGA;
 s:string;
 p:TProblem;
 v:TIntArray;
 h1,h2:TReal;
 i,j,x:Integer;
begin
 try
  if ParamStr(1) = '' then
   begin
      WriteLn('Error: Task file not found');
      readln;
      exit;
   end;
  Randomize;
 f1:=TStringList.Create;
 f1.Duplicates:=dupIgnore;
 f1.Sorted:=false;


 f2:=TStringList.Create;
 f2.Duplicates:=dupIgnore;
 f2.Sorted:=false;

 f3:=TStringList.Create;
 f3.Duplicates:=dupIgnore;
 f3.Sorted:=false;

 f1.LoadFromFile(ParamStr(1));
 f2.LoadFromFile(ParamStr(2));

 GA_:=TGA.Create();
 GA_.Task.Load(f1);
 GA_.Load(f2);

 GA_.NameResultFile:=ParamStr(3);
 if ParamStr(4) <> '' then GA_.NamePopulationFile:=ParamStr(4);

 GA_.Algoritm;

 if not GA_.PopulationSaved then GA_.SavePopulation(True);

 //GA_.Statistic.RewriteFile(GA_.NameResultFile, False);

 GA_.Destroy;

 f1.Free;
 f2.Free;
 f3.Free;

 WriteLn('Done...');
// readln;
 except
   on e: exception do
      begin
           WriteLn('Error: '+E.ToString);
           readln;
      end;

 end;
end.

