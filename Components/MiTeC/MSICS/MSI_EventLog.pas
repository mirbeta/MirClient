{*******************************************************}
{       MiTeC System Information Component Suite        }
{                EventLog component                     }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_EventLog;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_EventLogNT;

const
  StorageFolderName = 'EventLog';

type
  TMiTeC_EventLog = class(TMiTeC_Component)
  private
    FEL: TEventLog;
    FOnReadEventLog: TOnReadEventLog;
    function GetCont(Index: Integer): TLogContainer;
    function GetContCount: Cardinal;
    function GetRecCount: Cardinal;
    function GetRecord(Index: Integer): TLogRecord;
    procedure SetExpand(const Value: Boolean);
    procedure SetSourceName(const Value: string);
    function GetExpand: Boolean;
    function GetSourceName: string;
    function GetSourceFilter: string;
    procedure SetSourceFilter(const Value: string);
    function GetLookupSID: Boolean;
    procedure SetLookupSID(const Value: Boolean);
    procedure SetOnReadEventLog(const Value: TOnReadEventLog);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    procedure Sort;

    property Containers[Index: Integer]: TLogContainer read GetCont;
    property Records[Index: Integer]: TLogRecord read GetRecord;
  published
    property SourceName: string read GetSourceName write SetSourceName;
    property SourceFilter: string read GetSourceFilter write SetSourceFilter;
    property ContainerCount: Cardinal read GetContCount;
    property RecordCount: Cardinal read GetRecCount;
    property ExpandMessages: Boolean read GetExpand write SetExpand;
    property LookupSID: Boolean read GetLookupSID write SetLookupSID;

    property OnReadEventLog: TOnReadEventLog read FOnReadEventLog write SetOnReadEventLog;
  end;

implementation

uses
  MiTeC_Routines, MiTeC_CRC, MiTeC_StrUtils;

{ TMiTeC_EventLog }

procedure TMiTeC_EventLog.Clear;
begin
  FEL.ClearRecords;
  FEL.ClearContainers;
end;

constructor TMiTeC_EventLog.Create(AOwner: TComponent);
begin
  inherited;
  FEL:=TEventLog.Create;
  FEL.ConvertTimeToLocal:=False;
end;

destructor TMiTeC_EventLog.Destroy;
begin
  FEL.Free;
  inherited;
end;

function TMiTeC_EventLog.GetCont(Index: Integer): TLogContainer;
begin
  Result:=FEL.Containers[Index];
end;

function TMiTeC_EventLog.GetContCount: Cardinal;
begin
  Result:=FEL.ContainerCount;
end;

function TMiTeC_EventLog.GetExpand: Boolean;
begin
  Result:=FEL.ExpandMessages;
end;

function TMiTeC_EventLog.GetLookupSID: Boolean;
begin
  Result:=FEL.LookupSID;
end;

function TMiTeC_EventLog.GetRecCount: Cardinal;
begin
  Result:=FEL.RecordCount;
end;

function TMiTeC_EventLog.GetRecord(Index: Integer): TLogRecord;
begin
  Result:=FEL.Records[Index];
end;

function TMiTeC_EventLog.GetSourceFilter: string;
begin
  Result:=FEL.SourceFilter;
end;

function TMiTeC_EventLog.GetSourceName: string;
begin
  Result:=FEL.SourceName;
end;

function TMiTeC_EventLog.LoadFromStorage;

procedure ParseRecord(ASource: string; var ARecord: TLogRecord);
var
  p: Integer;
begin
  Finalize(Arecord);
  ARecord.BinaryData:='';
  ARecord.CharData:='';
  p:=Pos(';',ASource);
  if p=0 then
    Exit;
  ARecord.EventType:=TEventType(StrToIntDef(Copy(ASource,1,p-1),0));
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.DateTime:=StrToFloatDef(Copy(ASource,1,p-1),0);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Source:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Category:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.EventID:=StrToIntDef(Copy(ASource,1,p-1),0);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Username:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Domain:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);
  p:=Pos(';',ASource);
  ARecord.Computer:=Copy(ASource,1,p-1);
  Delete(ASource,1,p);

  ARecord.Description:=ASource;
  ARecord.Description:=StringReplace(ARecord.Description,'<CR>',#13,[rfReplaceAll,rfIgnoreCase]);
  ARecord.Description:=StringReplace(ARecord.Description,'<LF>',#10,[rfReplaceAll,rfIgnoreCase]);
  ARecord.Description:=StringReplace(ARecord.Description,'<SEMI>',';',[rfReplaceAll,rfIgnoreCase]);
end;

var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  i: Integer;
  sl: TStringList;
  r: TLogRecord;
  c: TLogContainer;
  ds: char;
  s: string;
  st: TStringStream;
begin
  Sub:=nil;
  ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
  {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  sl:=TStringList.Create;
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then begin
      for i:=0 to Sub.ElementCount-1 do begin
        strm:=Sub.OpenStream(Sub.Elements[i].Name,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            if sl.Count>0 then begin
              if Pos(';',sl[0])=0 then
                c.Name:=sl[0]
              else
                c.Name:=Sub.Elements[i].Name;
              c.FileName:='';
              FEL.AddContainer(c);
            end;
          finally
            strm.Free;
          end;
      end;
      if (FEL.SourceName='') then
        Exit;
      s:=FEL.SourceName;
      if not Sub.ElementExists(s) then begin
        st:=TStringStream.Create(FEL.SourceName);
        try
          s:=Format('%x',[CRC32Stream(st)]);
        finally
         st.Free;
        end;
        if not Sub.ElementExists(s) then
          Exit;
      end;
      strm:=Sub.OpenStream(s,STG_READ_INSTORAGE,False);
      if strm=nil then
        Exit;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        if sl.Count>0 then begin
          if Pos(';',sl[0])=0 then
            sl.Delete(0);
          for i:=0 to sl.Count-1 do begin
            ParseRecord(sl[i],r);
            if (FEL.SourceFilter='') or (PosText(FEL.SourceFilter+',',r.Source+',')>0) then
              FEL.AddRecord(r);
          end;
        end;
        SetDataAvail(True);
      finally
        strm.Free;
      end;
    end;
  finally
    {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
    sl.Free;
    if Sub<>nil then
      Sub.Free;
    SS.Free;
  end;
end;

procedure TMiTeC_EventLog.RefreshData;
begin
  inherited;
  FEL.RefreshData;
  SetDataAvail(True);
end;

procedure TMiTeC_EventLog.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  s: string;
  i: Integer;
  ds: char;
  st: TStringStream;
begin
  Sub:=nil;
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    st:=TStringStream.Create(Sourcename);
    try
      s:=Format('%x',[CRC32Stream(st)]);
    finally
      st.Free;
    end;
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    Sub.DeleteElement(s);
    strm:=Sub.OpenStream(s,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
      try
        sl.Add(Sourcename);
        for i:=0 to FEL.RecordCount-1 do begin
          s:=StringReplace(FEL.Records[i].Description,#13,'<CR>',[rfReplaceAll,rfIgnoreCase]);
          s:=StringReplace(s,#10,'<LF>',[rfReplaceAll,rfIgnoreCase]);
          s:=StringReplace(s,';','<SEMI>',[rfReplaceAll,rfIgnoreCase]);
          try
            sl.Add(Format('%d;%1.10f;%s;%s;%d;%s;%s;%s;%s',[Integer(FEL.Records[i].EventType),
                                   FEL.Records[i].DateTime,
                                   FEL.Records[i].Source,
                                   FEL.Records[i].Category,
                                   FEL.Records[i].EventID,
                                   FEL.Records[i].Username,
                                   FEL.Records[i].Domain,
                                   FEL.Records[i].Computer,
                                   s
                                  ]));
          except on e: exception do
            sl.Add(Format('[%d] %s',[i,e.Message]));
          end;
        end;
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
        sl.Free;
      end;
    finally
      strm.Free;
    end;
  finally
    Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

procedure TMiTeC_EventLog.SetExpand(const Value: Boolean);
begin
  FEL.ExpandMessages:=Value;
end;

procedure TMiTeC_EventLog.SetLookupSID(const Value: Boolean);
begin
  FEL.LookupSID:=Value;
end;

procedure TMiTeC_EventLog.SetOnReadEventLog(const Value: TOnReadEventLog);
begin
  FOnReadEventLog:=Value;
  if Assigned(FEL) then
    FEL.OnReadEventLog:=Value;
end;

procedure TMiTeC_EventLog.SetSourceFilter(const Value: string);
begin
  FEL.SourceFilter:=Value;
end;

procedure TMiTeC_EventLog.SetSourceName(const Value: string);
var
  rh: Boolean;
begin
  FEL.SourceName:=Value;
  FEL.ConvertTimeToLocal:=ConvertTimeToLocal;
  if LiveData then
    RefreshData
  else begin
    rh:=True;
    LoadFromStorage(StorageFilename,rh,StreamCodeProc);
  end;
end;

procedure TMiTeC_EventLog.Sort;
begin
  FEL.Sort;
end;

end.




