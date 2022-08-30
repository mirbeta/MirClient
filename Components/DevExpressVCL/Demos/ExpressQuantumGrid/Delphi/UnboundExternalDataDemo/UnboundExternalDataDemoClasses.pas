unit UnboundExternalDataDemoClasses;

{$I cxVer.inc}

interface

uses
  Windows, Classes, SysUtils, cxCustomData, Variants;

type

  TUserIniFile = class(TObject)
  private
    FFileName: string;
    FModified: Boolean;
    FSections: TStringList;
    FOnModify: TNotifyEvent;
    function GetSectionName(AIndex: Integer): string;
    procedure ReleaseSection(AIndex: Integer);
    procedure SetModified(AValue: Boolean);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure ChangeSectionName(AIndex: Integer; const ASection: string);
    procedure Clear;
    procedure DeleteSection(AIndex: Integer);
    function InsertSection(AIndex: Integer; const ASection: string;
      AAppend: Boolean = False): TStrings;
    procedure LoadValues; overload;
    procedure LoadValues(AList: TStrings); overload;
    procedure Rename(const AFileName: string; AReload: Boolean);
    procedure SaveAs(const AFileName: string);
    procedure SaveValues; overload;
    procedure SaveValues(AList: TStrings); overload;
    function SectionCount: Integer;
    property FileName: string read FFileName;
    property Modified: Boolean read FModified write SetModified;
    property SectionNames[AIndex: Integer]: string read GetSectionName;
    property Sections: TStringList read FSections;
    property OnModify: TNotifyEvent read FOnModify write FOnModify;
  end;

  TUserDataSource = class(TcxCustomDataSource)
  private
    FIniFile: TUserIniFile;
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(AIniFile: TUserIniFile);
    function AppendRecord: TcxDataRecordHandle; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    property DataIniFile: TUserIniFile read FIniFile;
  end;

  TUserDetailDataSource = class(TcxCustomDataSource)
  private
    FDataSource: TUserDataSource; // master
  protected
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
  public
    constructor Create(ADataSource: TUserDataSource);
    function AppendRecord: TcxDataRecordHandle; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetMasterRecordIndex: Integer;
    property MasterDataSource: TUserDataSource read FDataSource;
  end;

implementation

{ TUserIniFile }

constructor TUserIniFile.Create(const AFileName: string);
begin
  inherited Create;
  FSections := TStringList.Create;
  FFileName := AFileName;
end;

destructor TUserIniFile.Destroy;
begin
  Clear;
  FSections.Free;
  inherited;
end;

procedure TUserIniFile.ChangeSectionName(AIndex: Integer;
  const ASection: string);
begin
  Sections[AIndex] := ASection;
end;

procedure TUserIniFile.Clear;
var
 I: Integer;
begin
  for I := 0 to SectionCount - 1 do
    ReleaseSection(I);
  Sections.Clear;
end;

procedure TUserIniFile.DeleteSection(AIndex: Integer);
begin
  ReleaseSection(AIndex);
  Sections.Delete(AIndex);
end;

function TUserIniFile.InsertSection(AIndex: Integer; const ASection: string;
  AAppend: Boolean = False): TStrings;
begin
  if ASection = '' then
  begin
    Result := nil;
    Exit;
  end;
  Result := TStringList.Create;
  try
    if AAppend then
      Sections.AddObject(ASection, Result)
    else
      Sections.InsertObject(AIndex, ASection, Result);
  except
    FreeAndNil(Result);
  end;
end;

procedure TUserIniFile.LoadValues;
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    if FileExists(FileName) then
      AList.LoadFromFile(FileName);
    LoadValues(AList);
  finally
    AList.Free;
  end;
end;

procedure TUserIniFile.LoadValues(AList: TStrings);
var
  I: Integer;
  S: string;
  ASection: TStrings;
  AEqualityPos: Integer;
begin
  Clear;
  ASection := nil;
  for I := 0 to AList.Count - 1 do
  begin
    S := Trim(AList[I]);
    if (S <> '') and (S[1] <> ';') then
      if (S[1] = '[') and (S[Length(S)] = ']') then
        ASection := InsertSection(-1, Trim(Copy(S, 2, Length(S) - 2)), True)
      else
        if ASection <> nil then
        begin
          AEqualityPos := Pos('=', S);
          if AEqualityPos > 0 then
            ASection.Add(TrimRight(Copy(S, 1, AEqualityPos - 1)) + '=' + TrimLeft(Copy(S, AEqualityPos + 1, MaxInt)))
          else
            ASection.Add(S);
        end;
  end;
  Modified := False;
end;

procedure TUserIniFile.Rename(const AFileName: string; AReload: Boolean);
begin
  FFileName := AFileName;
  if AReload then
    LoadValues;
end;

procedure TUserIniFile.SaveAs(const AFileName: string);
begin
  Rename(AFileName, False);
  SaveValues;
end;

procedure TUserIniFile.SaveValues;
var
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    SaveValues(AList);
    AList.SaveToFile(FileName);
  finally
    AList.Free;
  end;
end;

procedure TUserIniFile.SaveValues(AList: TStrings);
var
  I, J: Integer;
  ASection: TStrings;
begin
  for I := 0 to SectionCount - 1 do
  begin
    AList.Add('[' + SectionNames[I] + ']');
    ASection := TStrings(FSections.Objects[I]);
    for J := 0 to ASection.Count - 1 do
      AList.Add(ASection[J]);
    if I < SectionCount - 1 then
      AList.Add('');
  end;
  Modified := False;
end;

function TUserIniFile.SectionCount: Integer;
begin
  Result := Sections.Count;
end;

function TUserIniFile.GetSectionName(AIndex: Integer): string;
begin
  Result := Sections[AIndex];
end;

procedure TUserIniFile.ReleaseSection(AIndex: Integer);
begin
  Sections.Objects[AIndex].Free;
end;

procedure TUserIniFile.SetModified(AValue: Boolean);
begin
  if Assigned(OnModify) then
    FOnModify(Self);
  if FModified <> AValue then
    FModified := AValue;
end;

{ TUserDataSource }

constructor TUserDataSource.Create(AIniFile: TUserIniFile);
begin
  inherited Create;
  FIniFile := AIniFile;
end;

procedure TUserDataSource.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
begin
 with DataIniFile do
 begin
   DeleteSection(Integer(ARecordHandle));
   Modified := True;
 end;
 DataChanged;
end;


function TUserDataSource.GetRecordCount: Integer;
begin
  Result := DataIniFile.Sections.Count;
end;

function TUserDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
begin
  Result := DataIniFile.Sections[Integer(ARecordHandle)];
end;

function TUserDataSource.AppendRecord: TcxDataRecordHandle;
begin
 with DataIniFile do
 begin
   InsertSection(-1 , '...', True);
   Modified := True;
   Result := TcxDataRecordHandle(Sections.Count - 1);
 end;
 DataChanged;
end;

function TUserDataSource.InsertRecord(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
 with DataIniFile do
 begin
   InsertSection(Integer(ARecordHandle), '...');
   Modified := True;
   Result := ARecordHandle;
 end;
 DataChanged;
end;

procedure TUserDataSource.SetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle; const AValue: Variant);
begin
  with DataIniFile do
  begin
    ChangeSectionName(Integer(ARecordHandle), VarAsType(AValue, varOLEStr));
    Modified := True;
  end;
end;

{ TUserDetailDataSource }

function TUserDetailDataSource.AppendRecord: TcxDataRecordHandle;
var
  AStrings: TStrings;
begin
  with MasterDataSource.DataIniFile do
  begin
    AStrings := TStrings(Sections.Objects[GetMasterRecordIndex]);
    Result := TcxDataRecordHandle(AStrings.Add(''));
    Modified := True;
  end;
  DataChanged;
end;

constructor TUserDetailDataSource.Create(ADataSource: TUserDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
end;

procedure TUserDetailDataSource.DeleteRecord(
  ARecordHandle: TcxDataRecordHandle);
var
  AStrings: TStrings;
begin
  with MasterDataSource.DataIniFile do
  begin
    AStrings := TStringList(Sections.Objects[GetMasterRecordIndex]);
    AStrings.Delete(Integer(ARecordHandle));
    Modified := True;
  end;
  DataChanged;
end;

function TUserDetailDataSource.GetMasterRecordIndex: Integer;
begin
  Result := DataController.GetMasterRecordIndex;
end;

function TUserDetailDataSource.GetRecordCount: Integer;
begin
  Result := 0;
  if GetMasterRecordIndex >= 0 then
    Result := TStrings(MasterDataSource.DataIniFile.Sections.Objects[GetMasterRecordIndex]).Count
end;

function TUserDetailDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  AStrings: TStrings;
  AColumnId: Integer;
begin
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  AStrings := TStrings(MasterDataSource.DataIniFile.Sections.Objects[GetMasterRecordIndex]);
  if AColumnId = 0 then
    Result := AStrings.Names[Integer(ARecordHandle)]
  else
    Result := AStrings.Values[AStrings.Names[Integer(ARecordHandle)]];
end;

function TUserDetailDataSource.InsertRecord(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
var
  AStrings: TStrings;
begin
  with MasterDataSource.DataIniFile do
  begin
    AStrings := TStrings(Sections.Objects[GetMasterRecordIndex]);
    AStrings.Insert(Integer(ARecordHandle), '');
    Modified := True;
    Result := ARecordHandle;
  end;
  DataChanged;
end;

procedure TUserDetailDataSource.SetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle;
  const AValue: Variant);
var
  AStrings: TStrings;
  AColumnId: Integer;
  S1, S2: string;
begin
  AColumnId := GetDefaultItemID(Integer(AItemHandle));
  with MasterDataSource.DataIniFile do
  begin
    AStrings := TStrings(Sections.Objects[GetMasterRecordIndex]);
    if AColumnId = 0 then  // set name
    begin
      if VarIsNull(AValue) then
        S1 := ''
      else S1 := AValue;
      S2 := AStrings.Values[AStrings.Names[Integer(ARecordHandle)]]
    end
    else // set value
    begin
      S1 := AStrings.Names[Integer(ARecordHandle)];
      if VarIsNull(AValue) then
        S2 := ''
      else S2 := AValue;
    end;
    AStrings.Strings[Integer(ARecordHandle)]:= Format('%S=%S', [S1,S2]);
    Modified := True;
   end;
end;

end.
