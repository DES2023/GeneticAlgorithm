program LocalSearchConcole;

{$APPTYPE CONSOLE}

uses
  Problem,
  VarType,
  LocalSearch,
  IterR;

var
 f1:TStringList;
 LS:TLS;
 s:string;

procedure WriteRecord(var s :TIterationResult; alg_name:TString);
var
 str:TString;
 i:TInt;
begin
   WriteLn('***********************'+alg_name+'***********************');
   str:='';
   for i:=0 to s.Size-1 do
     str:=str+IntToStr(s.Element[i])+' ';
   writeln('Solution: ' + str);
   writeln('Throughput: ' + FloatToStr(s.Throughput));
   writeln('Volume: ' + FloatToStr(s.Volume));
   writeln('StorageCost: ' + FloatToStr(s.StorageCost));
   WriteLn('______________________________________________________________');
   writeln('Value: ' + FloatToStr(s.GetValue));
   WriteLn();

end;

begin
try
 if ParamStr(1) = '' then
  begin
     WriteLn('Error: Input file not found');
     readln;
     exit;
  end;
 Randomize;

 f1:=TStringList.Create;
 f1.Duplicates:=dupIgnore;
 f1.Sorted:=false;
 f1.LoadFromFile(ParamStr(1));


 LS:=TLS.Create();
 LS.RunQuantity := 1;
 try
  LS.Task.Load(f1);
  LS.GetMemory();
  except
   on e: exception do
      begin
           WriteLn('Parsing error of input file: '+E.ToString);
           readln;
           exit;
      end;
  end;
 LS.Algoritm;
// GA_.Results.RewriteFile(GetCurrentDir+'\ga_result.txt');
// WriteRecord((GA_.Results.RunResult[GA_.Results.BestRecordNumber]), 'Genetic algorithm' );

 LS.Statistic.RewriteFile('LocalSearchResult',True);
 LS.Destroy;


 f1.Free;

 WriteLn('Done...');
 readln;
 except
   on e: exception do
      begin
           WriteLn('Error: '+E.ToString);
           readln;
      end;

 end;
end.
