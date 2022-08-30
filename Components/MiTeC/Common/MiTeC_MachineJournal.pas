{$INCLUDE Compilers.inc}

unit MiTeC_MachineJournal;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Classes, System.SysUtils,
     {$ELSE}
     Windows, Classes, SysUtils,
     {$ENDIF}
     MiTeC_EventLogNT;

type
  TEventKind = (ekNone, ekStartup, ekShutdown, ekSleep, ekWakeup, ekLogin, ekLogoff, ekLoginFail, ekCrash, ekUpdate, ekLock, ekUnlock);

  TEventRecord = record
    ID: Cardinal;
    Kind: TEventKind;
    Timestamp: TDateTime;
    Source,
    Description,
    Details: string;
  end;

  TMachineJournal = class
  private
    dl: TStringList;
    FEL: TEventLog;
    FRecords: array of TEventRecord;
    FMachine: string;
    FHost: string;
    procedure Add(AID: Cardinal; AKind: TEventKind; ATimestamp: TDateTime; const ASource,ADesc,ADetails: string);
    //procedure Delete(AIndex: integer);
    procedure Sort;
    function GetCount: integer;
    function GetRecord(AIndex: integer): TEventRecord;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Find(AKind: TEventKind; ATimestamp: TDateTime): Integer;
    function FindEx(AKind: TEventKind; ATimestamp: TDateTime): Integer;
    procedure Save(const AFilename: string);
    function RefreshData: Cardinal;

    property Host: string read FHost write FHost;
    property Machine: string read FMachine write FMachine;
    property Count: integer read GetCount;
    property Records[AIndex: integer]: TEventRecord read GetRecord;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.DateUtils,
     {$ELSE}
     DateUtils,
     {$ENDIF}
     MiTeC_Routines, MiTeC_StrUtils;

{ TMachineJournal }

procedure TMachineJournal.Add(AID: Cardinal; AKind: TEventKind;
  ATimestamp: TDateTime; const ASource, ADesc, ADetails: string);
begin
  SetLength(FRecords,Length(FRecords)+1);
  with FRecords[High(FRecords)] do begin
    ID:=AID;
    Kind:=AKind;
    Timestamp:=ATimestamp;
    Source:=ASource;
    Description:=ADesc;
    Details:=ADetails;
  end;
end;

procedure TMachineJournal.Clear;
begin
  Finalize(FRecords);
end;

constructor TMachineJournal.Create;
begin
  FMachine:=MachineName;
  FHost:=MachineName;
  dl:=TStringList.Create;
  FEL:=TEventLog.Create;
end;

{procedure TMachineJournal.Delete(AIndex: integer);
var
  i: integer;
begin
  for i:=AIndex to High(FRecords)-1 do
    FRecords[i]:=FRecords[i+1];
  SetLength(FRecords,High(FRecords));
end;}

destructor TMachineJournal.Destroy;
begin
  Clear;
  FEL.Free;
  dl.Free;
  inherited;
end;

function TMachineJournal.Find(AKind: TEventKind;
  ATimestamp: TDateTime): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FRecords) do
    if (FRecords[i].Kind=AKind) and SameDateTime(FRecords[i].Timestamp,ATimestamp) then begin
      Result:=i;
      Break;
    end;
end;

function TMachineJournal.FindEx(AKind: TEventKind;
  ATimestamp: TDateTime): Integer;
var
  i: Integer;
  k: set of TEventKind;
begin
  Result:=-1;
  k:=[AKind];
  if AKind=ekLogin then
    k:=k+[ekLogoff];
  if AKind=ekLogoff then
    k:=k+[ekLogin];
  for i:=0 to High(FRecords) do
    if (FRecords[i].Kind in k) and SameDateTime(FRecords[i].Timestamp,ATimestamp) then begin
      Result:=i;
      Break;
    end;
end;

function TMachineJournal.GetCount: integer;
begin
  Result:=Length(FRecords);
end;

function TMachineJournal.GetRecord(AIndex: integer): TEventRecord;
begin
  Result:=FRecords[AIndex];
end;

function TMachineJournal.RefreshData: Cardinal;
var
  dt: TDateTime;
  i,lt: Integer;
  s,d: string;
  sleep: Boolean;
  k: TEventKind;
begin
  sleep:=False;
  Clear;

  FEL.ReverseOrder:=False;
  FEL.Machine:=FHost;
  FEL.ConvertTimeToLocal:=True;
  FEL.ExpandMessages:=False;
  FEL.SourceFilter:='Microsoft-Windows-Kernel-General,Microsoft-Windows-Kernel-Power,EventLog,Microsoft-Windows-WindowsUpdateClient'; //,Microsoft-Windows-Power-Troubleshooter;
  FEL.SourceName:='System';
  FEL.RefreshData(True);
  //FEL.Sort;
  for i:=0 to FEL.RecordCount-1 do begin
    if (CompareDate(FEL.Records[i].DateTime,Date)<>1) and (
       ((FEL.Records[i].EventID in [12,13]) and SameText(FEL.Records[i].Source,'Microsoft-Windows-Kernel-General')) or
       //((FEL.Records[i].EventID=1) and SameText(FEL.Records[i].Source,'Microsoft-Windows-Power-Troubleshooter')) or
       ((FEL.Records[i].EventID=42) and SameText(FEL.Records[i].Source,'Microsoft-Windows-Kernel-Power')) or
       (((FEL.Records[i].EventID=1) and SameText(FEL.Records[i].Source,'Microsoft-Windows-Kernel-General')) and sleep) or
       ((FEL.Records[i].EventID=6008) and SameText(FEL.Records[i].Source,'EventLog')) or
       ((FEL.Records[i].EventID=6006) and SameText(FEL.Records[i].Source,'EventLog') and (OS<=osVista)) or
       ((FEL.Records[i].EventID=6009) and SameText(FEL.Records[i].Source,'EventLog') and (OS<=osVista)) or
       ((FEL.Records[i].EventID=19) and SameText(FEL.Records[i].Source,'Microsoft-Windows-WindowsUpdateClient'))
       )
    then begin
      sleep:=(FEL.Records[i].EventID=42) and SameText(FEL.Records[i].Source,'Microsoft-Windows-Kernel-Power');
      dt:=FEL.Records[i].DateTime;
      s:='';
      d:='';
      dl.Text:=FEL.Records[i].Description;
      if (FEL.Records[i].EventID in [1,12]) or (FEL.Records[i].EventID=6009) then begin
        if (FEL.Records[i].EventID=1) then begin
          s:='The system has resumed from sleep';
          k:=ekWakeup;
        end else begin
          s:='The operating system started';
          k:=ekStartup;
        end;
      end else begin
        if (FEL.Records[i].EventID=6008) then begin
          s:=FastStringReplace(GetCSVData(FEL.Records[i].Description,1,#$D),'?','')+' '+
             FastStringReplace(GetCSVData(FEL.Records[i].Description,0,#$D),'?','');
          s:=FastStringReplace(s,#$200E,'');
          dt:=StrToDateTimeDef(s,FEL.Records[i].DateTime);
          s:='Unexpected shutdown';
          k:=ekCrash;
        end else if (FEL.Records[i].EventID=42) then begin
          s:='The system entered sleep';
          k:=ekSleep;
        end else if (FEL.Records[i].EventID=19) then begin
          s:='The operating system was succesfully updated';
          k:=ekUpdate;
          if (dl.Count>0) then
            d:=dl[0];
        end else begin
          s:='The operating system was shut down';
          k:=ekShutdown;
        end;
      end;
      if (k<>ekNone) then
        Add(FEL.Records[i].EventID,k,dt,FEL.Records[i].Source,s,d);
    end;
  end;

  FEL.ClearRecords;
  FEL.ConvertTimeToLocal:=True;
  FEL.ExpandMessages:=False;
  FEL.SourceFilter:='Microsoft-Windows-Security-Auditing';
  FEL.SourceName:='Security';
  Result:=FEL.RefreshData(True);
  //FEL.Sort;
  for i:=0 to FEL.RecordCount-1 do begin
    if (FEL.Records[i].EventID=4624) or
       (FEL.Records[i].EventID=4647) or
       (FEL.Records[i].EventID=4634) or
       (FEL.Records[i].EventID=4625) or
       (FEL.Records[i].EventID=4800) or
       (FEL.Records[i].EventID=4801) then begin
      s:='';
      d:='';
      k:=ekNone;
      dl.Text:=FEL.Records[i].Description;
      if (FEL.Records[i].EventID=4624) and (dl.Count>18) then begin
        lt:=StrToIntDef(dl[8],0);
        case lt of
          2: s:='locally';
          10: s:='via RDP';
          3: s:='for a shared resource';
          7: s:='as unlock';
        end;
        s:=Trim(Format('User %s logged in %s',[dl[5],s]));
        d:=Format('Source IP: %s',[dl[18]]);
        if not(lt in [4,5,8,9,11]) and (dl[18]<>'-') and SameText(dl[6],FMachine) and (dl[5]<>'') then
          k:=ekLogin;
      end else if ((FEL.Records[i].EventID=4647) or
                   (FEL.Records[i].EventID=4634))
                  and (dl.Count>4) then begin
        if dl.Count>4 then begin
          lt:=StrToIntDef(dl[4],0);
          case lt of
            2: s:='locally';
            10: s:='via RDP';
            3: s:='for a shared resource';
          end;
        end else
          lt:=0;
        s:=Trim(Format('User %s logged off %s',[dl[1],s]));
        if not(lt in [4,5,8,9,11]) and SameText(dl[2],FMachine) then
          k:=ekLogoff;
      end else if (FEL.Records[i].EventID=4800) then begin
        s:=Format('User %s locked workstation',[dl[1]]);
        d:=Format('Workstation: %s',[dl[2]]);
        k:=ekLock;
      end else if (FEL.Records[i].EventID=4801) then begin
        s:=Format('User %s unlocked workstation',[dl[1]]);
        d:=Format('Workstation: %s',[dl[2]]);
        k:=ekUnlock;
      end else if (FEL.Records[i].EventID=4625) and (dl.Count>13) then begin
        s:=Format('User %s login failed',[dl[5]]);
        d:=Format('Workstation: %s',[dl[13]]);
        k:=ekLoginFail;
      end;
      {if (k<>ekNone) then begin
        idx:=FindEx(k,FEL.Records[i].DateTime);
        if idx=-1 then
          Add(FEL.Records[i].EventID,k,FEL.Records[i].DateTime,FEL.Records[i].Source,s,d)
        else if (k in [ekLogin,ekLogoff]) and (k<>Records[idx].Kind) then
          Delete(idx);
      end;}
      if (k<>ekNone) and (Find(k,FEL.Records[i].DateTime)=-1) then
        Add(FEL.Records[i].EventID,k,FEL.Records[i].DateTime,FEL.Records[i].Source,s,d);
    end;
  end;

  FEL.ClearRecords;

  Sort;
end;

procedure TMachineJournal.Save(const AFilename: string);
var
  sl: TStringList;
  r: TEventRecord;
begin
  sl:=TStringList.Create;
  try
    sl.Add('ID;Kind;Timestamp;Source;Description;Details');
    for r in FRecords do
      sl.Add(Format('%d;%d;%s;%s;%s;%s',[r.ID,Integer(r.Kind),DateTimeToStr(r.Timestamp),r.Source,r.Description,r.Details]));
    sl.SaveToFile(AFilename);
  finally
    sl.Free;
  end;
end;

procedure TMachineJournal.Sort;

  procedure QuickSort(ALo, AHi: integer);
  var
    Lo,Hi,Mid: Integer;
    r: TEventRecord;
  begin
    repeat
      Lo:=ALo;
      Hi:=AHi;
      Mid:=(Lo+Hi) div 2;
      repeat
        while CompareDatetime(FRecords[Lo].Timestamp,FRecords[Mid].Timestamp)<0 do
          Inc(Lo);
        while CompareDatetime(FRecords[Hi].Timestamp,FRecords[Mid].Timestamp)>0 do
          Dec(Hi);
        if Lo<=Hi then begin
          if Lo<>Hi then begin
            r:=FRecords[Lo];
            FRecords[Lo]:=FRecords[Hi];
            FRecords[Hi]:=r;
            if Mid=Lo then
              Mid:=Hi
            else if Mid=Hi then
              Mid:=Lo;
          end;
          Inc(Lo);
          Dec(Hi);
        end;
      until Lo>Hi;
      if ALo<Hi then
        QuickSort(ALo,Hi);
      ALo:=Lo;
    until Lo>=AHi;
  end;

begin
  if Length(FRecords)>0 then
    QuickSort(0,High(FRecords));
end;

end.
