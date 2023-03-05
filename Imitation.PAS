{$O+,F+}
unit Imitation;
 interface
uses
  Trnsl,
  TPString,
  TPCrt,
  TPMenu,
  TPedit,
  TPdos;

 type
  ms=array[1..am] of word ;
  ls=array[1..an] of real ;
  la=array[1..an] of boolean ;
  sspis=^spis ;
  spis=record
           NumTrSp: 0..an ;
           TimeSp:real ;
           Avant:sspis ;
           Arrier:sspis
           end;
  stroka    =string; 
 
  PTImitation = ^TImitation;
  TImitation = class(TImitation)
  private

  public

   Constructor Create;overload;
   Destructor Destroy;overload;

   Procedure GetMemory;override;
   Procedure FreeMemory;override;

   Procedure Load(var f : TStringList);
   Procedure imit ;
 (*------------------------------------------------------------------
      подпрограмма для имитации параметризованной
      сети Петри
   ------------------------------------------------------------------ *)

   label
       Me01,me21,me24,me27,
       me30,me31,me32,me33,me34,me35,me37,
       me40,me42,Prer,Pokaz,mexit;

(*--------------------------------------------------------------------
  типы
         ms  - массивы начальной и текущей маркировок
         ls  - массивы наработок до отказа для переходов
         la  - массивы для отметки занятости переходов
       Spis  - элемент статического и динамического списков
    Numtrsp  - номер перехода помещенного в список
     TimeSp  - временная отметка перехода
     Avant   - ссылка на предедущую запись списка
     Arrier  - ссылка на последующуу запись списка
  --------------------------------------------------------------------*)

(*--------------------------------------------------------------------
    переменные

     Numtran  - номер рассматриваемого перехода
     TimeCour - текущее модельное время
     Time     - время
     DlitMod  - длительность моделирования
     NumIter  - номер текущей итерации
     NmbIter  - число итераций
     FinPos   - номер позиции для сбора статистики
     Numpos   - номер маркировки
     nmr,i,j  - индексные переменные
     EntrStr1 - строка для ввода начальной маркировки
     Produc   - производительность моделируемой системы
     SspEntr  - ссылка на начало статического списка
     SspSort  - ссылка на конец статического списка
     DspEntr  - ссылка на начало динамического списка
     DspSort  - ссылка на конец динамического списка
     ZspEntr  - ссылка на начало списка задержанных переходов
     ZspSort  - ссылка на конец  списка задержанных переходов
     PredTran - множество предшествующих переходов
  At,Ap,Bt,Bp - вспомогательные ссылочные переменные
MarkNul,MarkCour- начальная и текущая маркировка
     OcupTran -массив для отметки занятости переходов
     OtkTran  -массив наработок до отказа
     cod      - kod возврата процедуры VAL

     переменные, описанные в уните trnsl:
     w,b,b1,s,s1    - ссылочные переменные на дуги
     str5           - строка содержащая имя файла модели
     et,st          - массивы ссылок на дуги инцидентные переходам
     ep,sp          - массивы ссылок на дуги инцидентные позициям
     tr             - массивы ссылок на параметры переходов
     pl             - массивы ссылок на параметры позиций
     m,n            - число позиций и переходов
     am,an          - максимально возможное число позиций и переходов
     mx             - константа 0.1E+38
   --------------------------------------------------------------------*)

 var
      TimeCour,Time,DlitMod                  : real;
      NumIter,NmbIter,FinPos,Numpos,Numtran  : word ;
      nmr,j,i                                : word ;
      EntrStr1,EntrStr2                      : Stroka;
      Produc                                 : real;
      SspEntr,SspSort                        : sspis;
      DspEntr,DspSort                        : sspis;
      ZspEntr,ZspSort                        : sspis;
      At,Ap,Bt,Bp                            : sspis;
      MarkNul,MarkCour                       : ms;
      OtkTran                                : ls;
      OcupTran                               : la;
      r1,A                                   : char;
      PredTran                               : set of 0..an;
      Cod                                    : integer;
      Expr                                   : file of real ;
      Produc1                                : real ;
      sqo                                    : real ;
      PosEntr,err                            : word ;
      Main                                   : Menu ;
      Key                                    : MenuKey ;
      Esc                                    : boolean ;
      Buf                                    : pointer ;
      Kode                                   : word ;
      bs,c                                   : string ;
      WC,wcfull                              : WindowCoordinates ;



begin

   window(1,1,80,25);
   TextColor(15);
   TextBackGround(1);
   clrscr;
   HiddenCursor;
   SetFrameChars(#186,#205,#188,#187,#200,#201);
   FrameWindow(1,1,80,25,$1F,$4E,' ИМИТАЦИЯ ');
   Window(2,3,79,23);
   StoreWindowCoordinates(WCfull);
   InitMenu(Main);

{ ---------  начальные значения переменных (по умолчанию)  ---------  }

   FinPos:=m;         { позиция для сбора статистики       }
   DlitMod:=5;      { длительность моделирования         }
   NmbIter:=50;        { число прогонов модели              }
   Time:=0;           { модельное время (не исп.)          }
   NumTran:=1;        { номер стартового перехода(не исп.) }

{  -----------  начальная маркировка (по умолчанию) ----------------  }

   for i:=1 to m do
     if i<= round(m/2) then
       begin
         MarkNul[i]:=50;
         PosEntr:=i;  { последняя позиция с ненулевой начальной маркировкой }
                      { ( только не знаю, почему именно она )               }
       end
      else
        MarkNul[i]:=0;

 repeat

   Key:=MenuChoice(Main,r1);
   if r1=#27 then goto mexit;
         if not savewindow(1,1,80,25,true,buf) then
            begin
              GoToXY(20,18);
              write('Не хватает памяти');
              ClrEol;
              delay(1000);
              Exit;
            end;
    if key in[2..4] then
       begin
         FrameWindow(47,8,58,13,$0E,15,'');
         StoreWindowCoordinates(WC);
         window(48,9,57,12);
         TextBackGround(0);
         clrscr;
         GoToXYABS(49,9);
         write(FinPos);GoToXYABS(49,10);
         write(DlitMod:5:3);GoToXYABS(49,11);
         write(NmbIter);
         RestoreWindowCoordinates(WC);
         TextBackGround(1);
       end;

    case Key of
      7:
       begin

{  ----------------- просмотр значений -------------------  }

         FrameWindow(47,8,58,13,$0E,15,'');
         StoreWindowCoordinates(WC);
         window(48,9,57,12);
         TextBackGround(0);
         clrscr;
         GoToXYABS(49,9);
         write(FinPos);GoToXYABS(49,10);
         write(DlitMod:5:3);GoToXYABS(49,11);
         write(NmbIter);
         A:=readkey;
         RestoreWindowCoordinates(WC);
         TextBackGround(1);
       end;


      1:
begin

{  ----------  ввод (изменение) начальной маркировки  ----------  }

          err:=1;
          FrameWindow(11,7,79,20,$2f,$30,' НЕНУЛЕВАЯ МАРКИРОВКА ');
          StoreWindowCoordinates(WC);
          Window(12,8,78,19);

Pokaz:
          GoToXYABS(10,21);
          ClrEol;
          GoToXYABS(10,23);
          ClrEol;
          textattr:=black+green*16;
          ClrScr;GoToXY(2,1);
          for i:=1 to m do
           if i in mpl then
            if marknul[i]<>0 then
              begin
                str(i:2,bs);
                str(marknul[i]:2,C);
                write(Pad(('p'+bs+'='+c),10));
                if ((WhereY=12) and (WhereX>60)) then
                  begin
                    GoToXYABS(20,21);
                    textattr:=white+blue*16;
                    write('          Нажмите любую клавишу          ');
                    a:=readkey;
                    textattr:=black+green*16;
                    ClrScr;
                    GoToXY(2,1);
                  end
                 else if WhereY=12 then
                  GoToXY(WhereX+1,1)
                 else
                   GoToXY(WhereX-10,WhereY+1);
              end;
     textattr:=white+blue*16;
     GoToXYABS(15,24);
     write('"P" - просмотреть маркировку, esc - закончить ввод');
        repeat
          EntrStr1:='p00=00';
    ReadString(' Введите строку по маске ',22,25,10,15,$30,$07,Esc,EntrStr1);
          if EntrStr1='p' then
              GoTo Pokaz;
            nmr:=pos('=',EntrStr1);
	 if ((nmr<>0) and (EntrStr1<>'p00=00')) then
               begin
                      err:=0;
		      EntrStr2:=copy(EntrStr1,1,nmr-1);
                      i:=1;
		      while i<=length(EntrStr2) do
		      if (copy(EntrStr2,i,1)<'0') or (copy(EntrStr2,i,1)>'9')
					   then delete(EntrStr2,i,1)
					   else i:=i+1;
		      val(EntrStr2,j,cod);
               if ((cod<>0) or (not(j in mpl))) then
                          begin
                            GoToXYAbs(24,21);
                            write('        Ошибка ввода');ClrEol;
                            delay(1000);
                            GoToXYABS(24,22);
                            ClrEol;
                            inc(err);
                          end
                           else
                             PosEntr:=j ;
		      EntrStr2:=copy(EntrStr1,nmr+1,length(EntrStr1)-nmr);
                      i:=1;
		      while i<=length(EntrStr2) do
		      if copy(EntrStr2,i,1)=' ' then delete(EntrStr2,i,1)
					   else i:=i+1;

		      val(EntrStr2,i,cod);
                      if ((cod<>0) or (i<0)) then
                        begin
                            GoToXYAbs(24,21);
                            write('        Ошибка ввода');ClrEol;
                            delay(1000);
                            GoToXYABS(24,22);
                            ClrEol;
                            inc(err);
                          end;
                      if err=0 then MarkNul[j]:=i;
               end;
    until  Esc;
    RestoreWindowCoordinates(WC);

{  ---------------------  маркировка введена  ----------------  }

        end;
     2:

(*-------------------------------------------------------------------
    Ввод  Homepa позиции для сбора статистики
  -------------------------------------------------------------------*)

       begin
        repeat
         err:=0;
         ReadWord(' Введите целое число ',20,23,10,15,$30,1,m,Esc,FinPos);
         if not (Finpos in mpl) then
           begin
                            GoToXYAbs(24,21);
                            write('        Ошибка ввода');ClrEol;
                            delay(1000);
                            GoToXYABS(24,22);
                            ClrEol;
                            inc(err);
           end;
         until ((err=0) or Esc);
         GoToXY(22,18);
         ClrEol;
       end;
     3:
(*---------------------------------------------------------------------
    Ввод  отрезка времени, на котором необходимо
    исследовать модель и числа прогонов
  ---------------------------------------------------------------------*)

       begin
  ReadReal(' Введите веществ. число ',20,20,10,15,$30,3,0,1E+38,Esc,DlitMod);
         GoToXY(19,18);
         ClrEol;
       end;
     4:
       begin
     ReadWord(' Введите целое число ',20,23,10,15,$30,0,10000,Esc,NmbIter);
         GoToXY(22,18);
         ClrEol;
       end;

(*
      5:
       begin
         ReadWord(' Введите целое число ',20,23,10,15,$30,0,an,Esc,NumTran);
         GoToXY(22,18);
         ClrEol;
       end;
     6:
       begin
  ReadReal(' Введите веществ. число ',20,20,10,15,$30,3,0,1E+38,Esc,Time);
         GoToXY(19,18);
         ClrEol;
          New(At);
          At^.Avant:=SspSort;
          At^.Arrier:=nil;
          At^.NumTrSp:=NumTran;
          At^.TimeSp:=Time;
          SspSort^.Arrier:=At;
          SspSort:=At;
       end;
*)

     5:
       begin
         if YesOrNo('  Все данные введены ?',20,25,15,'N') then

{  -------------------- начинается расчет ----------------  }

           begin
             HiddenCursor;
             RestoreWindow(1,1,80,25,false,buf);
             Window(2,3,79,23);
             GoToXY(22,18);
             write('        Ждите, идет расчет');
             ClrEol;

(*------------------------------------------------------------------
   начальные значения переменнных
  ------------------------------------------------------------------ *)
  assign(Expr,ForceExtension(JustFileName(str5),'sta')) ;
  if ExistFile(ForceExtension(JustFileName(str5),'sta')) then Erase(Expr);
  rewrite(Expr) ; Produc1:=0 ;
 NumIter:=0; Numpos:=1;TimeCour:=0;
 i:=0; j:=0;
 EntrStr1:='  0';  Produc:=0;
 SspEntr:=nil; SspSort:=nil;
 DspEntr:=nil; DspSort:=nil;
 ZspEntr:=nil; ZspSort:=nil;
 At:=nil; Ap:=nil; Bt:=nil; Bp:=nil;
 MarkCour:=MarkNul;
(*------------------------------------------------------------------
    занесение стартового перехода в статический список
  ------------------------------------------------------------------*)
          New(SspEntr);
          SspEntr^.NumTrSp:=0;
          SspSort:=SspEntr;
          SspEntr^.Avant:=nil; SspEntr^.Arrier:=nil;
          SspEntr^.TimeSp:=DlitMod;


   for i:=1 to n do  begin
                     randomize;
                 if tr[i]<>nil then OtkTran[i]:=-ln(1-random)/tr[i]^.l
                               else OtkTran[i]:=mx;
                     end;

   for i:=1 to n do OcupTran[i]:=False;
     w:=nil; b:=nil; s:=nil; s1:=nil; b1:=nil;




(*-------------------------------------------------------------------
         обращение к статическому списку для выполнения
         следующего шага сценария
  ------------------------------------------------------------------- *)

Me21:      if SspEntr^.NumTrSp=0 then goto me40;

(*-----------------------------------------------------
         проверить осуществимо ли заданное управление
  ----------------------------------------------------- *)

       s:=et[SspEntr^.NumTrSp];

 Me24:

         if s=nil then goto me27;
         if MarkCour[s^.pos]>0 then begin
                                          s:=s^.pat;
                                          goto me24;
                                    end
                                else begin
                     GoToXY(24,18);
     write('сценарий не может быть осуществлен');
     Delay(1000);
     goto me01;                     end;

(*------------------------------------------------------------
    снять у принудительно запускаемого перехода входную
    маркировку и отметить занятость перехода
  ------------------------------------------------------------ *)
       s:=et[Sspentr^.NumTrSp];
       while s<>nil do begin
       MarkCour[s^.pos]:=MarkCour[s^.pos]-1;
       s:=s^.pat;
                       end;
       OcupTran[SspEntr^.NumTrSp]:=True;

(*---------------------------------------------------------------
      переслать номер перехода из статического списка
      в динамический
  --------------------------------------------------------------- *)

 me27:

      New(DspEntr);
      DspEntr^.NumTrSp:=SspEntr^.NumTrSp;
      DspEntr^.Avant:=nil;
      DspEntr^.Arrier:=nil;
     if SspEntr^.TimeSp<=TimeCour then DspEntr^.TimeSp:=TimeCour
                                else DspEntr^.TimeSp:=SspEntr^.TimeSp;

     DspSort:=DspEntr;

(*------------------------------------------------------------------
     поместить в статическом списке первый переход на последнее
     место
  ------------------------------------------------------------------ *)

 if SspEntr^.Arrier<>nil then    begin
              SspEntr^.Avant:=SspSort;
              SspSort^.Arrier:=SspEntr;
              SspSort:=SspEntr;
              SspEntr^.Arrier^.Avant:=nil;
        SspEntr:=SspEntr^.Arrier;  end;

 me30:

(*------------------------------------------------------------
       расмотреть очередное событие в динамическом списке
  ------------------------------------------------------------*)

     if DspEntr=nil then goto me21;

(*-------------------------------------------------------------
     проверка условий завершения очередного прогона
--------------------------------------------------------------- *)

     if DspEntr^.TimeSp>=DlitMod then goto me40;

(*---------------------------------------------------------------
    продвинуться к следующему моменту времени
    и удалить первый элемент динамического списка
 ----------------------------------------------------------------*)

     NumTran:=DspEntr^.NumTrSp;

   if TimeCour<DspEntr^.TimeSp then TimeCour:=DspEntr^.TimeSp;
{gotoxy(24,18) ;
write(' модельное время :',TimeCour:10:4) ;
GoToXY(24,20);
write('   Нажмите любую клавишу ');
A:=readkey;GoToXY(24,18);ClrEol; GoToXY(24,20); ClrEol;
}
     Bp:=DspEntr^.Arrier;
   if Bp<>nil then Bp^.Avant:=nil;
     dispose(DspEntr);
     DspEntr:=Bp;
     if DspEntr=nil Then DspSort:=nil;

(*----------------------------------------------------------------
   проверить не является ли данный переход задержанным
  ---------------------------------------------------------------*)

     b:=st[NumTran];
         While b<>nil do begin
               if pl[b^.pos]^.h>=32000 then goto me31;
              if MarkCour[b^.pos]>=round(pl[b^.pos]^.h+1) then goto me32;
me31:                  b:=b^.pat
                         end;

(*-----------------------------------------------------------------
   выставить выходную маркировку и снять занятость
 -----------------------------------------------------------------*)

      s:=st[NumTran];
            while s<>nil do begin
            MarkCour[s^.pos]:=MarkCour[s^.pos]+1;
            s:=s^.pat
            end;

      OcupTran[NumTran]:=False;

(*------------------------------------------------------------------
     реализовать правило диспетчирования- выбрать последовательность
     активизации переходов и поместить их в динамический
     список
 -------------------------------------------------------------------*)

 Me33:

      { DISP  } i:=m; While i<>0 do begin {a}
                     if MarkCour[i]>0 then begin {b}
                                 s:=sp[i];
                                while s<>nil do begin {c}
                                if OcupTran[s^.tr]=True then goto me35;
                                         s1:=et[s^.tr];
                                        while s1<>nil do
                                          if MarkCour[s1^.pos]>0 then
                                 s1:=s1^.pat  else goto me35;
                                         s1:=et[s^.tr];
           OcupTran[s^.tr]:=True;

                                 While s1<>nil do begin
                                 MarkCour[s1^.pos]:=MarkCour[s1^.pos]-1;
                                         s1:=s1^.pat
                                                  end;

                                 if (1/s^.vit)>=OtkTran[s^.tr] then begin
                                                Randomize;
             Time:=TimeCour+1/s^.vit-ln(1-Random)/tr[s^.tr]^.m;
 OtkTran[s^.tr]:=OtkTran[s^.tr]-ln(1-Random)/tr[s^.tr]^.l-1/s^.vit  end
                                                              else begin
             OtkTran[s^.tr]:=OtkTran[s^.tr]-1/s^.vit;
             Time:=TimeCour+1/s^.vit;                              end;



(*---------------------------------------------------------------------
       вставить в список активизированный переход
  ---------------------------------------------------------------------*)
        Bp:=DspEntr;
        While Bp<>nil do  begin
                    if Bp^.TimeSp>Time then begin
                    new(Ap);
                    Ap^.Avant:=Bp^.Avant;
                    Ap^.Arrier:=Bp;
               if Ap^.Avant<>nil then Ap^.Avant^.Arrier:=Ap;
                    Bp^.Avant:=Ap;
                    Ap^.TimeSp:=Time;
                    Ap^.NumTrSp:=s^.tr;
                 if Bp=DspEntr then DspEntr:=Ap;
                       goto me34;            end;
             Bp:=Bp^.Arrier; end;
(*------------------------------------------
 вставить в конец списка или в пустой список
 -------------------------------------------*)
             new(Ap);
             Bp:=DspSort;
         if  Bp<>nil then Bp^.Arrier:=Ap;
             Ap^.Avant:=Bp;
             Ap^.Arrier:=nil;
             AP^.TimeSp:=Time;
             Ap^.NumTrSp:=s^.tr;
             DspSort:=Ap;
            if DspEntr=nil then DspEntr:=Ap;

(*-------------------------------------------
   проверить нет ли задержаных переходов
   изменивших свой статус
 -------------------------------------------*)

me34:

  if ZspSort<>nil then begin  {сформировать мн-во предшествующих пер.}
                       PredTran:=[];
                       s1:=et[s^.tr];
                  While s1<>nil do begin
                                  w:=ep[s1^.pos];
                            While w<>nil do begin
                                            PredTran:=PredTran+[W^.tr];
                                            w:=w^.pap
                                            end;
                                  s1:=s1^.pat
                                  end;
  if PredTran=[] then goto me35 ;
{ просмотр списка задержанных переходов }
                    Bt:=ZspSort;
                    While Bt<>nil do
                                  if Bt^.NumTrSp in PredTran then
                                  begin
                         {  gotoxy(15,18) ;
                           write(' извлечен переход :',Bt^.NumTrSp) ;}
{вставить первым элементом списка Dt }
                                  New(Ap);
                                  DspEntr^.Avant:=Ap;
                                  Ap^.Arrier:=DspEntr;
                                  Ap^.Avant:=nil;
                                  Ap^.NumTrSp:=Bt^.NumTrSp;
                                  Ap^.TimeSp:=Bt^.TimeSp;
                                  DspEntr:=Ap;
{удалить элемент из списка Zt }

            if Bt=ZspSort then ZspSort:=Bt^.Avant;
            if Bt=ZspEntr then ZspEntr:=Bt^.Arrier;
        if Bt^.Avant<>nil then Bt^.Avant^.Arrier:=Bt^.Arrier;
       if Bt^.arrier<>nil then Bt^.Arrier^.Avant:=Bt^.Avant;
               Dispose(Bt) ;
               Bt:=nil
                                  end
                              else Bt:=Bt^.Avant;
                PredTran:=[]
                     end;
 me35:

(*------------------------
 перейти к следующей дуге
 -------------------------*)
                             s:=s^.pap;           end{c};
                                          end{b};
 Me37:
                 i:=i-1            end{a};
                                          goto me30;


me32:
(*-------------------------------------------------------------------
      поместить переход в список задержанных -Zt
---------------------------------------------------------------------*)
{gotoxy(15,17) ;
write(' задержан переход :',NumTran) ;}

       New(Ap);
       Bp:=ZspSort;
    if Bp<>nil then Bp^.Arrier:=Ap;
       Ap^.Avant:=Bp;
       Ap^.Arrier:=nil;
       Ap^.TimeSp:=TimeCour;
       Ap^.NumTrSp:=NumTran;
       ZspSort:=Ap;
    if ZspEntr=nil then ZspEntr:=Ap;

       goto me30;

(*------------------------------------------------------------------
       обработка результатов прогона модели
  ------------------------------------------------------------------ *)

 me40:

        if NumIter=0 then Produc:=Produc-MarkCour[FinPos]
                     else produc1:=produc;
                          Produc:=Produc+MarkCour[FinPos];
                          produc1:=produc-produc1 ;
{gotoxy(24,17)  ;
write(' число  деталей  :',produc1:10:4) ;
gotoxy(24,18)  ;
write(' номер итерации  : ',numIter:9) ;
ClrEol;
GoToXY(24,20);
write('   Нажмите любую клавишу ');
A:=readkey;
GoToXY(24,17);ClrEol;
GoToXY(24,18);ClrEol;
GoToXY(24,20);ClrEol;}
         if NumIter=NmbIter then goto me42
                            else             begin
                                    SQO:=Produc1/Dlitmod ;
                                    write(Expr,SQO);
                                    NumIter:=NumIter+1;
(*----------------------------
 очистить динамический список
 ----------------------------*)
                                    while DspEntr<>nil do
                                                      begin
                                                     Bt:=DspEntr;
                                                     DspEntr:=Bt^.Arrier;
                                                     Dispose(Bt);
                                                      end;
                                     DspSort:=nil; DspEntr:=nil;
(*----------------------------
 очистить список задержанных
 ----------------------------*)
                                   while ZspEntr<>nil do
                                                      begin
                                                     Bt:=ZspEntr;
                                                     ZspEntr:=Bt^.Arrier;
                                                     Dispose(Bt);
                                                      end;
                                     ZspSort:=nil; ZspEntr:=nil;
(*---------------------------
  установить начальное время
  --------------------------- *)
                                     TimeCour:=0;

(*---------------------------------------
 установить начальные наработки до отказа
  и сбросить занятость переходов
  ---------------------------------------*)


Prer:
       Esc:=false;
  if (CheckKbd(Kode)) then
    if((Hi(Kode)=27) and (Lo(Kode)=60)) then
         begin
           Esc:=true;
           goto me42;
         end;


   for i:=1 to n do  begin
                     randomize;
                 if tr[i]<>nil then OtkTran[i]:=-ln(1-random)/tr[i]^.l
                               else OtkTran[i]:=mx;
                     end;

   for i:=1 to n do OcupTran[i]:=False;

(*--------------------------------------
  установить начальную маркировку
  --------------------------------------*)

  MarkCour:=MarkNul;

                if SspEntr^.NumTrSp<>0 then begin
                GoToXY(24,18);
 write('сценарий не выполним');
   ClrEol;
   delay(1000);
   goto me01
                                             end;
                            goto Me33;
                             end;

 me42:

(*---------------------------------------
 расчитать среднюю производительность
 ----------------------------------------*)
            Close(expr);

{   0000000000000000000000000000000000000000000000000000
первый прочитанный элемент не используется.}

            SQO:=0 ;
            Produc:=Produc/NumIter;
            reset(expr) ; read(Expr,Produc1) ;
            for i:=1 to filesize(expr)-1 do begin
                                            read(Expr,Produc1) ;
            SQO:=(Produc1-Produc/DlitMod)*(Produc1-Produc/DlitMod)+SQO
                                            end ;
  SQO:=SQO/(NumIter-1) ;
             GoToXY(24,18);
             ClrEol;
    {         if not SaveWindow(2,3,79,23,true,buf) then
               begin
                 GoToXY(24,18);
                 write('Не хватает памяти для экрана');
                 delay(1500);
                 Close(Expr);
                 Erase(Expr);
                 DisposeMenu(Main);
                 Exit;
               end;}

 FrameWindow(5,6,76,24,$1F,$30,' ИМИТАЦИЯ ЗАКОНЧЕНА ');
 RestoreWindowCoordinates(WCfull);
 GoToXY(15,7);
 TextBackGround(3);
 TextColor(0);
write('     Завершена интерпретация файла  ',Pad(JustFileName(str5),14));
GoToXY(15,8);
write('   ____________________________________________   '); GoToXY(15,9);
write('                                                  '); GoToXY(15,10);
write('   Объем запуска                     ',MarkNul[PosEntr]:10,'   ');
GoToXY(15,11);
write('   Число прогонов                    ',NumIter:10,'   ');
GoToXY(15,12);
write('   Длительность смены                ',DlitMod:10:4,'   ');
GoToXY(15,13);
write('   Среднее число обработанных деталей',Produc:10:4,'   ');
GoToXY(15,14);
write('   Средняя пропускная способность    ',(Produc/DlitMod):10:4,'   ');
GoToXY(15,15);
write('   Среднеквадратическое отклонение                ');
GoToXY(15,16);
write('   пропускной способности            ',SQO:10:4,'   ');
GoToXY(15,17);
write('   ____________________________________________   ');
GoToXY(15,18);
write('                                                  ');

TextBackGround(1);
textcolor(15);
{ if YesOrNo(' Считать дальше ?',20,25,15,'Y') and Esc then
GoTo Prer;}
GoToXY(15,20);
write('      Нажмите любую клавишу для продолжения');
             A:=readkey;
Me01:
             Close(Expr);
             Erase(Expr);
           end
        {  else
            begin
              GoToXY(20,18);
              ClrEol;
            end;}
       end;
     6:
       begin
mexit:
         DisposeMenu(Main);
         Exit;
       end;
   end;    { от case  }
   RestoreWindow(1,1,80,25,true,buf);
   HiddenCursor;
 until false;

end;  {  IMIT  }

  end;  




 procedure imit ;
 (*------------------------------------------------------------------
      подпрограмма для имитации параметризованной
      сети Петри
   ------------------------------------------------------------------ *)

 label
       Me01,me21,me24,me27,
       me30,me31,me32,me33,me34,me35,me37,
       me40,me42,Prer,Pokaz,mexit;

(*--------------------------------------------------------------------
  типы
         ms  - массивы начальной и текущей маркировок
         ls  - массивы наработок до отказа для переходов
         la  - массивы для отметки занятости переходов
       Spis  - элемент статического и динамического списков
    Numtrsp  - номер перехода помещенного в список
     TimeSp  - временная отметка перехода
     Avant   - ссылка на предедущую запись списка
     Arrier  - ссылка на последующуу запись списка
  --------------------------------------------------------------------*)

(*--------------------------------------------------------------------
    переменные

     Numtran  - номер рассматриваемого перехода
     TimeCour - текущее модельное время
     Time     - время
     DlitMod  - длительность моделирования
     NumIter  - номер текущей итерации
     NmbIter  - число итераций
     FinPos   - номер позиции для сбора статистики
     Numpos   - номер маркировки
     nmr,i,j  - индексные переменные
     EntrStr1 - строка для ввода начальной маркировки
     Produc   - производительность моделируемой системы
     SspEntr  - ссылка на начало статического списка
     SspSort  - ссылка на конец статического списка
     DspEntr  - ссылка на начало динамического списка
     DspSort  - ссылка на конец динамического списка
     ZspEntr  - ссылка на начало списка задержанных переходов
     ZspSort  - ссылка на конец  списка задержанных переходов
     PredTran - множество предшествующих переходов
  At,Ap,Bt,Bp - вспомогательные ссылочные переменные
MarkNul,MarkCour- начальная и текущая маркировка
     OcupTran -массив для отметки занятости переходов
     OtkTran  -массив наработок до отказа
     cod      - kod возврата процедуры VAL

     переменные, описанные в уните trnsl:
     w,b,b1,s,s1    - ссылочные переменные на дуги
     str5           - строка содержащая имя файла модели
     et,st          - массивы ссылок на дуги инцидентные переходам
     ep,sp          - массивы ссылок на дуги инцидентные позициям
     tr             - массивы ссылок на параметры переходов
     pl             - массивы ссылок на параметры позиций
     m,n            - число позиций и переходов
     am,an          - максимально возможное число позиций и переходов
     mx             - константа 0.1E+38
   --------------------------------------------------------------------*)

 var
      TimeCour,Time,DlitMod                  : real;
      NumIter,NmbIter,FinPos,Numpos,Numtran  : word ;
      nmr,j,i                                : word ;
      EntrStr1,EntrStr2                      : Stroka;
      Produc                                 : real;
      SspEntr,SspSort                        : sspis;
      DspEntr,DspSort                        : sspis;
      ZspEntr,ZspSort                        : sspis;
      At,Ap,Bt,Bp                            : sspis;
      MarkNul,MarkCour                       : ms;
      OtkTran                                : ls;
      OcupTran                               : la;
      r1,A                                   : char;
      PredTran                               : set of 0..an;
      Cod                                    : integer;
      Expr                                   : file of real ;
      Produc1                                : real ;
      sqo                                    : real ;
      PosEntr,err                            : word ;
      Main                                   : Menu ;
      Key                                    : MenuKey ;
      Esc                                    : boolean ;
      Buf                                    : pointer ;
      Kode                                   : word ;
      bs,c                                   : string ;
      WC,wcfull                              : WindowCoordinates ;

procedure InitMenu(var M : Menu);
const
  Color1 : MenuColorArray = ($70, $2E, $71, $1E, $70, $0E, $19, $07);
  Frame1 : FrameArray = '┌└┐┘─│';

begin
  {Customize this call for special exit characters and custom item displays}
  M := NewMenu([], nil);

  SubMenu(8,3,0,Horizontal,Frame1,Color1,'');
    MenuMode(True, False, False);
    MenuItem(' Данные ',7,2,0,'');
    SubMenu(9,5,0,Vertical,Frame1,Color1,'');
      MenuMode(True, False, False);
      MenuItem('Начальная маркировка',1,1,1,'');
      MenuItem('Данные для расчета',2,1,0,'');
      SubMenu(10,8,0,Vertical,Frame1,Color1,'');
        MenuMode(True, False, False);
        MenuItem('Номер позиции для сбора статистики',1,1,2,'');
        MenuItem('Длительность моделирования',2,1,3,'');
        MenuItem('Число прогонов модели',3,1,4,'');
        MenuItem('Просмотр значений',4,1,7,'');
      PopSublevel;
     Popsublevel;
    MenuItem(' Расчет ',29,2,5,'');
    MenuItem(' Выход ',55,2,6,'');
    PopSublevel;

  ResetMenu(M);
  SetMenuDelay(M, 10);
end;

begin

   window(1,1,80,25);
   TextColor(15);
   TextBackGround(1);
   clrscr;
   HiddenCursor;
   SetFrameChars(#186,#205,#188,#187,#200,#201);
   FrameWindow(1,1,80,25,$1F,$4E,' ИМИТАЦИЯ ');
   Window(2,3,79,23);
   StoreWindowCoordinates(WCfull);
   InitMenu(Main);

{ ---------  начальные значения переменных (по умолчанию)  ---------  }

   FinPos:=m;         { позиция для сбора статистики       }
   DlitMod:=5;      { длительность моделирования         }
   NmbIter:=50;        { число прогонов модели              }
   Time:=0;           { модельное время (не исп.)          }
   NumTran:=1;        { номер стартового перехода(не исп.) }

{  -----------  начальная маркировка (по умолчанию) ----------------  }

   for i:=1 to m do
     if i<= round(m/2) then
       begin
         MarkNul[i]:=50;
         PosEntr:=i;  { последняя позиция с ненулевой начальной маркировкой }
                      { ( только не знаю, почему именно она )               }
       end
      else
        MarkNul[i]:=0;

 repeat

   Key:=MenuChoice(Main,r1);
   if r1=#27 then goto mexit;
         if not savewindow(1,1,80,25,true,buf) then
            begin
              GoToXY(20,18);
              write('Не хватает памяти');
              ClrEol;
              delay(1000);
              Exit;
            end;
    if key in[2..4] then
       begin
         FrameWindow(47,8,58,13,$0E,15,'');
         StoreWindowCoordinates(WC);
         window(48,9,57,12);
         TextBackGround(0);
         clrscr;
         GoToXYABS(49,9);
         write(FinPos);GoToXYABS(49,10);
         write(DlitMod:5:3);GoToXYABS(49,11);
         write(NmbIter);
         RestoreWindowCoordinates(WC);
         TextBackGround(1);
       end;

    case Key of
      7:
       begin

{  ----------------- просмотр значений -------------------  }

         FrameWindow(47,8,58,13,$0E,15,'');
         StoreWindowCoordinates(WC);
         window(48,9,57,12);
         TextBackGround(0);
         clrscr;
         GoToXYABS(49,9);
         write(FinPos);GoToXYABS(49,10);
         write(DlitMod:5:3);GoToXYABS(49,11);
         write(NmbIter);
         A:=readkey;
         RestoreWindowCoordinates(WC);
         TextBackGround(1);
       end;


      1:
begin

{  ----------  ввод (изменение) начальной маркировки  ----------  }

          err:=1;
          FrameWindow(11,7,79,20,$2f,$30,' НЕНУЛЕВАЯ МАРКИРОВКА ');
          StoreWindowCoordinates(WC);
          Window(12,8,78,19);

Pokaz:
          GoToXYABS(10,21);
          ClrEol;
          GoToXYABS(10,23);
          ClrEol;
          textattr:=black+green*16;
          ClrScr;GoToXY(2,1);
          for i:=1 to m do
           if i in mpl then
            if marknul[i]<>0 then
              begin
                str(i:2,bs);
                str(marknul[i]:2,C);
                write(Pad(('p'+bs+'='+c),10));
                if ((WhereY=12) and (WhereX>60)) then
                  begin
                    GoToXYABS(20,21);
                    textattr:=white+blue*16;
                    write('          Нажмите любую клавишу          ');
                    a:=readkey;
                    textattr:=black+green*16;
                    ClrScr;
                    GoToXY(2,1);
                  end
                 else if WhereY=12 then
                  GoToXY(WhereX+1,1)
                 else
                   GoToXY(WhereX-10,WhereY+1);
              end;
     textattr:=white+blue*16;
     GoToXYABS(15,24);
     write('"P" - просмотреть маркировку, esc - закончить ввод');
        repeat
          EntrStr1:='p00=00';
    ReadString(' Введите строку по маске ',22,25,10,15,$30,$07,Esc,EntrStr1);
          if EntrStr1='p' then
              GoTo Pokaz;
            nmr:=pos('=',EntrStr1);
	 if ((nmr<>0) and (EntrStr1<>'p00=00')) then
               begin
                      err:=0;
		      EntrStr2:=copy(EntrStr1,1,nmr-1);
                      i:=1;
		      while i<=length(EntrStr2) do
		      if (copy(EntrStr2,i,1)<'0') or (copy(EntrStr2,i,1)>'9')
					   then delete(EntrStr2,i,1)
					   else i:=i+1;
		      val(EntrStr2,j,cod);
               if ((cod<>0) or (not(j in mpl))) then
                          begin
                            GoToXYAbs(24,21);
                            write('        Ошибка ввода');ClrEol;
                            delay(1000);
                            GoToXYABS(24,22);
                            ClrEol;
                            inc(err);
                          end
                           else
                             PosEntr:=j ;
		      EntrStr2:=copy(EntrStr1,nmr+1,length(EntrStr1)-nmr);
                      i:=1;
		      while i<=length(EntrStr2) do
		      if copy(EntrStr2,i,1)=' ' then delete(EntrStr2,i,1)
					   else i:=i+1;

		      val(EntrStr2,i,cod);
                      if ((cod<>0) or (i<0)) then
                        begin
                            GoToXYAbs(24,21);
                            write('        Ошибка ввода');ClrEol;
                            delay(1000);
                            GoToXYABS(24,22);
                            ClrEol;
                            inc(err);
                          end;
                      if err=0 then MarkNul[j]:=i;
               end;
    until  Esc;
    RestoreWindowCoordinates(WC);

{  ---------------------  маркировка введена  ----------------  }

        end;
     2:

(*-------------------------------------------------------------------
    Ввод  Homepa позиции для сбора статистики
  -------------------------------------------------------------------*)

       begin
        repeat
         err:=0;
         ReadWord(' Введите целое число ',20,23,10,15,$30,1,m,Esc,FinPos);
         if not (Finpos in mpl) then
           begin
                            GoToXYAbs(24,21);
                            write('        Ошибка ввода');ClrEol;
                            delay(1000);
                            GoToXYABS(24,22);
                            ClrEol;
                            inc(err);
           end;
         until ((err=0) or Esc);
         GoToXY(22,18);
         ClrEol;
       end;
     3:
(*---------------------------------------------------------------------
    Ввод  отрезка времени, на котором необходимо
    исследовать модель и числа прогонов
  ---------------------------------------------------------------------*)

       begin
  ReadReal(' Введите веществ. число ',20,20,10,15,$30,3,0,1E+38,Esc,DlitMod);
         GoToXY(19,18);
         ClrEol;
       end;
     4:
       begin
     ReadWord(' Введите целое число ',20,23,10,15,$30,0,10000,Esc,NmbIter);
         GoToXY(22,18);
         ClrEol;
       end;

(*
      5:
       begin
         ReadWord(' Введите целое число ',20,23,10,15,$30,0,an,Esc,NumTran);
         GoToXY(22,18);
         ClrEol;
       end;
     6:
       begin
  ReadReal(' Введите веществ. число ',20,20,10,15,$30,3,0,1E+38,Esc,Time);
         GoToXY(19,18);
         ClrEol;
          New(At);
          At^.Avant:=SspSort;
          At^.Arrier:=nil;
          At^.NumTrSp:=NumTran;
          At^.TimeSp:=Time;
          SspSort^.Arrier:=At;
          SspSort:=At;
       end;
*)

     5:
       begin
         if YesOrNo('  Все данные введены ?',20,25,15,'N') then

{  -------------------- начинается расчет ----------------  }

           begin
             HiddenCursor;
             RestoreWindow(1,1,80,25,false,buf);
             Window(2,3,79,23);
             GoToXY(22,18);
             write('        Ждите, идет расчет');
             ClrEol;

(*------------------------------------------------------------------
   начальные значения переменнных
  ------------------------------------------------------------------ *)
  assign(Expr,ForceExtension(JustFileName(str5),'sta')) ;
  if ExistFile(ForceExtension(JustFileName(str5),'sta')) then Erase(Expr);
  rewrite(Expr) ; Produc1:=0 ;
 NumIter:=0; Numpos:=1;TimeCour:=0;
 i:=0; j:=0;
 EntrStr1:='  0';  Produc:=0;
 SspEntr:=nil; SspSort:=nil;
 DspEntr:=nil; DspSort:=nil;
 ZspEntr:=nil; ZspSort:=nil;
 At:=nil; Ap:=nil; Bt:=nil; Bp:=nil;
 MarkCour:=MarkNul;
(*------------------------------------------------------------------
    занесение стартового перехода в статический список
  ------------------------------------------------------------------*)
          New(SspEntr);
          SspEntr^.NumTrSp:=0;
          SspSort:=SspEntr;
          SspEntr^.Avant:=nil; SspEntr^.Arrier:=nil;
          SspEntr^.TimeSp:=DlitMod;


   for i:=1 to n do  begin
                     randomize;
                 if tr[i]<>nil then OtkTran[i]:=-ln(1-random)/tr[i]^.l
                               else OtkTran[i]:=mx;
                     end;

   for i:=1 to n do OcupTran[i]:=False;
     w:=nil; b:=nil; s:=nil; s1:=nil; b1:=nil;




(*-------------------------------------------------------------------
         обращение к статическому списку для выполнения
         следующего шага сценария
  ------------------------------------------------------------------- *)

Me21:      if SspEntr^.NumTrSp=0 then goto me40;

(*-----------------------------------------------------
         проверить осуществимо ли заданное управление
  ----------------------------------------------------- *)

       s:=et[SspEntr^.NumTrSp];

 Me24:

         if s=nil then goto me27;
         if MarkCour[s^.pos]>0 then begin
                                          s:=s^.pat;
                                          goto me24;
                                    end
                                else begin
                     GoToXY(24,18);
     write('сценарий не может быть осуществлен');
     Delay(1000);
     goto me01;                     end;

(*------------------------------------------------------------
    снять у принудительно запускаемого перехода входную
    маркировку и отметить занятость перехода
  ------------------------------------------------------------ *)
       s:=et[Sspentr^.NumTrSp];
       while s<>nil do begin
       MarkCour[s^.pos]:=MarkCour[s^.pos]-1;
       s:=s^.pat;
                       end;
       OcupTran[SspEntr^.NumTrSp]:=True;

(*---------------------------------------------------------------
      переслать номер перехода из статического списка
      в динамический
  --------------------------------------------------------------- *)

 me27:

      New(DspEntr);
      DspEntr^.NumTrSp:=SspEntr^.NumTrSp;
      DspEntr^.Avant:=nil;
      DspEntr^.Arrier:=nil;
     if SspEntr^.TimeSp<=TimeCour then DspEntr^.TimeSp:=TimeCour
                                else DspEntr^.TimeSp:=SspEntr^.TimeSp;

     DspSort:=DspEntr;

(*------------------------------------------------------------------
     поместить в статическом списке первый переход на последнее
     место
  ------------------------------------------------------------------ *)

 if SspEntr^.Arrier<>nil then    begin
              SspEntr^.Avant:=SspSort;
              SspSort^.Arrier:=SspEntr;
              SspSort:=SspEntr;
              SspEntr^.Arrier^.Avant:=nil;
        SspEntr:=SspEntr^.Arrier;  end;

 me30:

(*------------------------------------------------------------
       расмотреть очередное событие в динамическом списке
  ------------------------------------------------------------*)

     if DspEntr=nil then goto me21;

(*-------------------------------------------------------------
     проверка условий завершения очередного прогона
--------------------------------------------------------------- *)

     if DspEntr^.TimeSp>=DlitMod then goto me40;

(*---------------------------------------------------------------
    продвинуться к следующему моменту времени
    и удалить первый элемент динамического списка
 ----------------------------------------------------------------*)

     NumTran:=DspEntr^.NumTrSp;

   if TimeCour<DspEntr^.TimeSp then TimeCour:=DspEntr^.TimeSp;
{gotoxy(24,18) ;
write(' модельное время :',TimeCour:10:4) ;
GoToXY(24,20);
write('   Нажмите любую клавишу ');
A:=readkey;GoToXY(24,18);ClrEol; GoToXY(24,20); ClrEol;
}
     Bp:=DspEntr^.Arrier;
   if Bp<>nil then Bp^.Avant:=nil;
     dispose(DspEntr);
     DspEntr:=Bp;
     if DspEntr=nil Then DspSort:=nil;

(*----------------------------------------------------------------
   проверить не является ли данный переход задержанным
  ---------------------------------------------------------------*)

     b:=st[NumTran];
         While b<>nil do begin
               if pl[b^.pos]^.h>=32000 then goto me31;
              if MarkCour[b^.pos]>=round(pl[b^.pos]^.h+1) then goto me32;
me31:                  b:=b^.pat
                         end;

(*-----------------------------------------------------------------
   выставить выходную маркировку и снять занятость
 -----------------------------------------------------------------*)

      s:=st[NumTran];
            while s<>nil do begin
            MarkCour[s^.pos]:=MarkCour[s^.pos]+1;
            s:=s^.pat
            end;

      OcupTran[NumTran]:=False;

(*------------------------------------------------------------------
     реализовать правило диспетчирования- выбрать последовательность
     активизации переходов и поместить их в динамический
     список
 -------------------------------------------------------------------*)

 Me33:

      { DISP  } i:=m; While i<>0 do begin {a}
                     if MarkCour[i]>0 then begin {b}
                                 s:=sp[i];
                                while s<>nil do begin {c}
                                if OcupTran[s^.tr]=True then goto me35;
                                         s1:=et[s^.tr];
                                        while s1<>nil do
                                          if MarkCour[s1^.pos]>0 then
                                 s1:=s1^.pat  else goto me35;
                                         s1:=et[s^.tr];
           OcupTran[s^.tr]:=True;

                                 While s1<>nil do begin
                                 MarkCour[s1^.pos]:=MarkCour[s1^.pos]-1;
                                         s1:=s1^.pat
                                                  end;

                                 if (1/s^.vit)>=OtkTran[s^.tr] then begin
                                                Randomize;
             Time:=TimeCour+1/s^.vit-ln(1-Random)/tr[s^.tr]^.m;
 OtkTran[s^.tr]:=OtkTran[s^.tr]-ln(1-Random)/tr[s^.tr]^.l-1/s^.vit  end
                                                              else begin
             OtkTran[s^.tr]:=OtkTran[s^.tr]-1/s^.vit;
             Time:=TimeCour+1/s^.vit;                              end;



(*---------------------------------------------------------------------
       вставить в список активизированный переход
  ---------------------------------------------------------------------*)
        Bp:=DspEntr;
        While Bp<>nil do  begin
                    if Bp^.TimeSp>Time then begin
                    new(Ap);
                    Ap^.Avant:=Bp^.Avant;
                    Ap^.Arrier:=Bp;
               if Ap^.Avant<>nil then Ap^.Avant^.Arrier:=Ap;
                    Bp^.Avant:=Ap;
                    Ap^.TimeSp:=Time;
                    Ap^.NumTrSp:=s^.tr;
                 if Bp=DspEntr then DspEntr:=Ap;
                       goto me34;            end;
             Bp:=Bp^.Arrier; end;
(*------------------------------------------
 вставить в конец списка или в пустой список
 -------------------------------------------*)
             new(Ap);
             Bp:=DspSort;
         if  Bp<>nil then Bp^.Arrier:=Ap;
             Ap^.Avant:=Bp;
             Ap^.Arrier:=nil;
             AP^.TimeSp:=Time;
             Ap^.NumTrSp:=s^.tr;
             DspSort:=Ap;
            if DspEntr=nil then DspEntr:=Ap;

(*-------------------------------------------
   проверить нет ли задержаных переходов
   изменивших свой статус
 -------------------------------------------*)

me34:

  if ZspSort<>nil then begin  {сформировать мн-во предшествующих пер.}
                       PredTran:=[];
                       s1:=et[s^.tr];
                  While s1<>nil do begin
                                  w:=ep[s1^.pos];
                            While w<>nil do begin
                                            PredTran:=PredTran+[W^.tr];
                                            w:=w^.pap
                                            end;
                                  s1:=s1^.pat
                                  end;
  if PredTran=[] then goto me35 ;
{ просмотр списка задержанных переходов }
                    Bt:=ZspSort;
                    While Bt<>nil do
                                  if Bt^.NumTrSp in PredTran then
                                  begin
                         {  gotoxy(15,18) ;
                           write(' извлечен переход :',Bt^.NumTrSp) ;}
{вставить первым элементом списка Dt }
                                  New(Ap);
                                  DspEntr^.Avant:=Ap;
                                  Ap^.Arrier:=DspEntr;
                                  Ap^.Avant:=nil;
                                  Ap^.NumTrSp:=Bt^.NumTrSp;
                                  Ap^.TimeSp:=Bt^.TimeSp;
                                  DspEntr:=Ap;
{удалить элемент из списка Zt }

            if Bt=ZspSort then ZspSort:=Bt^.Avant;
            if Bt=ZspEntr then ZspEntr:=Bt^.Arrier;
        if Bt^.Avant<>nil then Bt^.Avant^.Arrier:=Bt^.Arrier;
       if Bt^.arrier<>nil then Bt^.Arrier^.Avant:=Bt^.Avant;
               Dispose(Bt) ;
               Bt:=nil
                                  end
                              else Bt:=Bt^.Avant;
                PredTran:=[]
                     end;
 me35:

(*------------------------
 перейти к следующей дуге
 -------------------------*)
                             s:=s^.pap;           end{c};
                                          end{b};
 Me37:
                 i:=i-1            end{a};
                                          goto me30;


me32:
(*-------------------------------------------------------------------
      поместить переход в список задержанных -Zt
---------------------------------------------------------------------*)
{gotoxy(15,17) ;
write(' задержан переход :',NumTran) ;}

       New(Ap);
       Bp:=ZspSort;
    if Bp<>nil then Bp^.Arrier:=Ap;
       Ap^.Avant:=Bp;
       Ap^.Arrier:=nil;
       Ap^.TimeSp:=TimeCour;
       Ap^.NumTrSp:=NumTran;
       ZspSort:=Ap;
    if ZspEntr=nil then ZspEntr:=Ap;

       goto me30;

(*------------------------------------------------------------------
       обработка результатов прогона модели
  ------------------------------------------------------------------ *)

 me40:

        if NumIter=0 then Produc:=Produc-MarkCour[FinPos]
                     else produc1:=produc;
                          Produc:=Produc+MarkCour[FinPos];
                          produc1:=produc-produc1 ;
{gotoxy(24,17)  ;
write(' число  деталей  :',produc1:10:4) ;
gotoxy(24,18)  ;
write(' номер итерации  : ',numIter:9) ;
ClrEol;
GoToXY(24,20);
write('   Нажмите любую клавишу ');
A:=readkey;
GoToXY(24,17);ClrEol;
GoToXY(24,18);ClrEol;
GoToXY(24,20);ClrEol;}
         if NumIter=NmbIter then goto me42
                            else             begin
                                    SQO:=Produc1/Dlitmod ;
                                    write(Expr,SQO);
                                    NumIter:=NumIter+1;
(*----------------------------
 очистить динамический список
 ----------------------------*)
                                    while DspEntr<>nil do
                                                      begin
                                                     Bt:=DspEntr;
                                                     DspEntr:=Bt^.Arrier;
                                                     Dispose(Bt);
                                                      end;
                                     DspSort:=nil; DspEntr:=nil;
(*----------------------------
 очистить список задержанных
 ----------------------------*)
                                   while ZspEntr<>nil do
                                                      begin
                                                     Bt:=ZspEntr;
                                                     ZspEntr:=Bt^.Arrier;
                                                     Dispose(Bt);
                                                      end;
                                     ZspSort:=nil; ZspEntr:=nil;
(*---------------------------
  установить начальное время
  --------------------------- *)
                                     TimeCour:=0;

(*---------------------------------------
 установить начальные наработки до отказа
  и сбросить занятость переходов
  ---------------------------------------*)


Prer:
       Esc:=false;
  if (CheckKbd(Kode)) then
    if((Hi(Kode)=27) and (Lo(Kode)=60)) then
         begin
           Esc:=true;
           goto me42;
         end;


   for i:=1 to n do  begin
                     randomize;
                 if tr[i]<>nil then OtkTran[i]:=-ln(1-random)/tr[i]^.l
                               else OtkTran[i]:=mx;
                     end;

   for i:=1 to n do OcupTran[i]:=False;

(*--------------------------------------
  установить начальную маркировку
  --------------------------------------*)

  MarkCour:=MarkNul;

                if SspEntr^.NumTrSp<>0 then begin
                GoToXY(24,18);
 write('сценарий не выполним');
   ClrEol;
   delay(1000);
   goto me01
                                             end;
                            goto Me33;
                             end;

 me42:

(*---------------------------------------
 расчитать среднюю производительность
 ----------------------------------------*)
            Close(expr);

{   0000000000000000000000000000000000000000000000000000
первый прочитанный элемент не используется.}

            SQO:=0 ;
            Produc:=Produc/NumIter;
            reset(expr) ; read(Expr,Produc1) ;
            for i:=1 to filesize(expr)-1 do begin
                                            read(Expr,Produc1) ;
            SQO:=(Produc1-Produc/DlitMod)*(Produc1-Produc/DlitMod)+SQO
                                            end ;
  SQO:=SQO/(NumIter-1) ;
             GoToXY(24,18);
             ClrEol;
    {         if not SaveWindow(2,3,79,23,true,buf) then
               begin
                 GoToXY(24,18);
                 write('Не хватает памяти для экрана');
                 delay(1500);
                 Close(Expr);
                 Erase(Expr);
                 DisposeMenu(Main);
                 Exit;
               end;}

 FrameWindow(5,6,76,24,$1F,$30,' ИМИТАЦИЯ ЗАКОНЧЕНА ');
 RestoreWindowCoordinates(WCfull);
 GoToXY(15,7);
 TextBackGround(3);
 TextColor(0);
write('     Завершена интерпретация файла  ',Pad(JustFileName(str5),14));
GoToXY(15,8);
write('   ____________________________________________   '); GoToXY(15,9);
write('                                                  '); GoToXY(15,10);
write('   Объем запуска                     ',MarkNul[PosEntr]:10,'   ');
GoToXY(15,11);
write('   Число прогонов                    ',NumIter:10,'   ');
GoToXY(15,12);
write('   Длительность смены                ',DlitMod:10:4,'   ');
GoToXY(15,13);
write('   Среднее число обработанных деталей',Produc:10:4,'   ');
GoToXY(15,14);
write('   Средняя пропускная способность    ',(Produc/DlitMod):10:4,'   ');
GoToXY(15,15);
write('   Среднеквадратическое отклонение                ');
GoToXY(15,16);
write('   пропускной способности            ',SQO:10:4,'   ');
GoToXY(15,17);
write('   ____________________________________________   ');
GoToXY(15,18);
write('                                                  ');

TextBackGround(1);
textcolor(15);
{ if YesOrNo(' Считать дальше ?',20,25,15,'Y') and Esc then
GoTo Prer;}
GoToXY(15,20);
write('      Нажмите любую клавишу для продолжения');
             A:=readkey;
Me01:
             Close(Expr);
             Erase(Expr);
           end
        {  else
            begin
              GoToXY(20,18);
              ClrEol;
            end;}
       end;
     6:
       begin
mexit:
         DisposeMenu(Main);
         Exit;
       end;
   end;    { от case  }
   RestoreWindow(1,1,80,25,true,buf);
   HiddenCursor;
 until false;

end;  {  IMIT  }


end.