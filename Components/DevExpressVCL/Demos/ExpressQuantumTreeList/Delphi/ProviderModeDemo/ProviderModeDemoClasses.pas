unit ProviderModeDemoClasses;

{$I cxVer.inc}

interface

uses
  Classes, cxTL, cxTLData, cxCustomData;

type
  { TcxProviderRecordHandle }

  TcxProviderRecordHandle = class
  private
    FChildLevelCount: Integer;
    FChildList: TList;
    FDate: TDateTime;
    FDataLoaded: Boolean;
    FDeletion: Boolean;
    FIntValue: Integer;
    FParent: TcxProviderRecordHandle;
    FText: string;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxProviderRecordHandle;
    function GetIndex: Integer;
    function GetLevel: Integer;
    function GetTotalCount: Integer;
  protected
    property Deletion: Boolean read FDeletion;
    property Parent: TcxProviderRecordHandle read FParent;
  public
    constructor Create(AParent: TcxProviderRecordHandle);
    destructor Destroy; override;
    function Add: TcxProviderRecordHandle;
    function AddChild: TcxProviderRecordHandle;
    procedure DeleteChildren;
    procedure NodeMoveTo(AttachRecordHandle: TcxProviderRecordHandle;
      AttachMode: TcxTreeListNodeAttachMode; IsCopy: Boolean);
    property ChildLevelCount: Integer read FChildLevelCount;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property IntValue: Integer read FIntValue write FIntValue;
    property Items[Index: Integer]: TcxProviderRecordHandle read GetItem; default;
    property Date: TDateTime read FDate write FDate;
    property Level: Integer read GetLevel;
    property Text: string read FText write FText;
    property TotalCount: Integer read GetTotalCount;
  end;

  { TcxCustomDemoDataSource}

  TcxCustomDemoDataSource = class(TcxTreeListCustomDataSource)
  private
    FID: Integer; 
    FRootHandle: TcxProviderRecordHandle;
    function InsertRecordHandle(AParentHandle: TcxProviderRecordHandle;
      AIsChild: Boolean): TcxProviderRecordHandle; virtual;
    procedure GenerateChildRecords(AParentHandle: TcxProviderRecordHandle);
  protected
    procedure NodeMoveTo(ARecordHandle, AttachRecordHandle: TcxDataRecordHandle;
      AttachMode: TcxTreeListNodeAttachMode; IsCopy: Boolean); override;
    function AppendRecord: TcxDataRecordHandle; override;
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;
    function InsertRecord(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    procedure SetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle; const AValue: Variant); override;
    property RootHandle: TcxProviderRecordHandle read FRootHandle;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TcxCustomDemoDataSourceClass  = class of TcxCustomDemoDataSource;

  { TcxSmartLoadDemoDataSource }

  TcxSmartLoadDemoDataSource = class(TcxCustomDemoDataSource)
  protected
    function AppendRecord: TcxDataRecordHandle; override;
    function GetChildCount(AParentHandle: TcxDataRecordHandle): Integer; override;
    function GetChildRecordHandle(AParentHandle: TcxDataRecordHandle;
      AChildIndex: Integer): TcxDataRecordHandle; override;
    function GetRootRecordHandle: TcxDataRecordHandle; override;
  public
    constructor Create; override;
  end;

  { TcxLoadAllRecordsDemoDataSource }

  TcxLoadAllRecordsDemoDataSource = class(TcxCustomDemoDataSource)
  private
    FRecordsList: TList;
    procedure CreateAllRecords;
    function InsertRecordHandle(AParentHandle: TcxProviderRecordHandle;
      AIsChild: Boolean): TcxProviderRecordHandle; override;
  protected
    procedure DeleteRecord(ARecordHandle: TcxDataRecordHandle); override;
    function GetParentRecordHandle(ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle; override;
    function GetRecordHandle(ARecordIndex: Integer): TcxDataRecordHandle; override;
    function GetRecordCount: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure RecreateDemoDataSource(ATreeList: TcxVirtualTreeList);

implementation

uses
  SysUtils, Dialogs;

const
  cxProviderDemoLevelCount = 5;
  cxProviderDemoRecordsPerLevel = 10;


procedure RecreateDemoDataSource(ATreeList: TcxVirtualTreeList);
var
  ADataSource: TObject;
const
  AClasses: array[Boolean] of TClass =
    (TcxLoadAllRecordsDemoDataSource, TcxSmartLoadDemoDataSource);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  ADataSource := ATreeList.CustomDataSource;
  ATreeList.CustomDataSource := nil;
  FreeAndNil(ADataSource);
  ATreeList.CustomDataSource :=
    TcxCustomDemoDataSourceClass(AClasses[ATreeList.OptionsData.SmartLoad]).Create;
//}

end;


{ TcxProviderRecordHandle }

constructor TcxProviderRecordHandle.Create(
  AParent: TcxProviderRecordHandle);
begin
  FParent := AParent;
  FChildList := TList.Create;
  if AParent <> nil then
    Parent.FChildList.Add(Self);
  if AParent <> nil then
    FChildLevelCount := AParent.ChildLevelCount - 1
  else
    FChildLevelCount := cxProviderDemoLevelCount;
end;

destructor TcxProviderRecordHandle.Destroy;
begin
  try
    DeleteChildren;
  finally
    FChildList.Free;
    if (Parent <> nil) and not Parent.Deletion then
      Parent.FChildList.Remove(Self);
    inherited Destroy;
  end;
end;

function TcxProviderRecordHandle.Add: TcxProviderRecordHandle;
begin
  Result := TcxProviderRecordHandle.Create(Parent);
end;

function TcxProviderRecordHandle.AddChild: TcxProviderRecordHandle;
begin
  Result := TcxProviderRecordHandle.Create(Self);
end;

procedure TcxProviderRecordHandle.DeleteChildren;
var
  I: Integer;
begin
  FDeletion := True;
  try
    for I := 0 to Count - 1 do
      TObject(FChildList.List[I]).Free;
  finally
    FChildList.Clear;
    FDeletion := False;
  end;
end;

procedure TcxProviderRecordHandle.NodeMoveTo(
  AttachRecordHandle: TcxProviderRecordHandle;
  AttachMode: TcxTreeListNodeAttachMode; IsCopy: Boolean);
  procedure ChangeParent(AParent: TcxProviderRecordHandle);
  begin
    if FParent <> AParent then
    begin
      FParent.FChildList.Remove(Self);
      AParent.FChildList.Add(Self);
      FParent := AParent;
    end;
  end;
begin
  case AttachMode of
    tlamAdd, tlamAddFirst, tlamInsert:
      ChangeParent(AttachRecordHandle.Parent);
    tlamAddChild, tlamAddChildFirst:
      ChangeParent(AttachRecordHandle);
  end;
end;

function TcxProviderRecordHandle.GetCount: Integer;
begin
  Result := FChildList.Count;
end;

function TcxProviderRecordHandle.GetItem(Index: Integer): TcxProviderRecordHandle;
begin
  Result := TcxProviderRecordHandle(FChildList[Index]);
end;

function TcxProviderRecordHandle.GetIndex: Integer;
begin
  if Parent <> nil then
    Result := Parent.FChildList.IndexOf(Self)
  else
    Result := -1; 
end;

function TcxProviderRecordHandle.GetLevel: Integer;
var
  AParent: TcxProviderRecordHandle;
begin
  Result := -1;
  AParent := Parent;
  while AParent <> nil do
  begin
    AParent := AParent.Parent;
    Inc(Result);
  end;
end;

function TcxProviderRecordHandle.GetTotalCount: Integer;
var
  I: Integer;
begin
  Result := Count;
  for I := 0 to Count - 1 do
    Inc(Result, Items[I].TotalCount);
end;

{ TcxCustomDemoDataSource }

constructor TcxCustomDemoDataSource.Create;
begin
  FRootHandle := TcxProviderRecordHandle.Create(nil);
end;

destructor TcxCustomDemoDataSource.Destroy;
begin
  FRootHandle.Free;
  inherited Destroy;
end;

function TcxCustomDemoDataSource.AppendRecord: TcxDataRecordHandle;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := InsertRecordHandle(RootHandle, True);
  TcxProviderRecordHandle(Result).FDataLoaded := True;
  DataChanged;

//}
end;

procedure TcxCustomDemoDataSource.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  TcxProviderRecordHandle(ARecordHandle).Free;
  DataChanged;

//}
end;

function TcxCustomDemoDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  with TcxProviderRecordHandle(ARecordHandle) do
    case Integer(AItemHandle) of
      0: Result := IntValue;
      1: Result := Text;
      2: Result := Date;
      3:
        if Parent = nil then
          Result := -1
        else
          Result := Parent.IntValue;
    end;

//}
end;

function TcxCustomDemoDataSource.InsertRecord(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := InsertRecordHandle(ARecordHandle, False);
  TcxProviderRecordHandle(Result).FDataLoaded := True;
  DataChanged;

//}
end;

procedure TcxCustomDemoDataSource.SetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle;
  const AValue: Variant);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  with TcxProviderRecordHandle(ARecordHandle) do
    case Integer(AItemHandle) of
      0: IntValue := AValue;
      1: Text := AValue;
      2: Date := AValue;
    end;

//
end;

procedure TcxCustomDemoDataSource.NodeMoveTo(ARecordHandle,
  AttachRecordHandle: TcxDataRecordHandle;
  AttachMode: TcxTreeListNodeAttachMode; IsCopy: Boolean);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  if IsCopy then
    with InsertRecordHandle(TcxProviderRecordHandle(AttachRecordHandle),
      AttachMode in [tlamAddChild, tlamAddChildFirst]) do
    begin
      IntValue := TcxProviderRecordHandle(ARecordHandle).IntValue;
      Date := TcxProviderRecordHandle(ARecordHandle).Date;
      Text := TcxProviderRecordHandle(ARecordHandle).Text;
    end
  else
    TcxProviderRecordHandle(ARecordHandle).NodeMoveTo(
      TcxProviderRecordHandle(AttachRecordHandle), AttachMode, IsCopy);
  DataChanged;

//}
end;

function TcxCustomDemoDataSource.InsertRecordHandle(
  AParentHandle: TcxProviderRecordHandle; AIsChild: Boolean): TcxProviderRecordHandle;
begin
  Inc(FID);
  if AIsChild then
    Result := AParentHandle.AddChild
  else
    Result := AParentHandle.Add
end;

procedure TcxCustomDemoDataSource.GenerateChildRecords(
  AParentHandle: TcxProviderRecordHandle);
var
  I: Integer;
begin
  for I := 0 to cxProviderDemoRecordsPerLevel - 1 do
    with InsertRecordHandle(AParentHandle, True) do
    begin
      IntValue := FID;
      Text := 'Text' + IntToStr(FIntValue);
      Date := Now + FIntValue * 0.001;
    end;
  AParentHandle.FDataLoaded := True;
end;

{ TcxSmartLoadDemoDataSource }

constructor TcxSmartLoadDemoDataSource.Create;
begin
  inherited Create;
  GenerateChildRecords(FRootHandle);
end;

function TcxSmartLoadDemoDataSource.AppendRecord: TcxDataRecordHandle;
var
  AIndex: Integer;
  AHandle: TcxDataRecordHandle;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  AIndex := DataController.FocusedRecordIndex;
  if AIndex = -1 then
    AHandle := RootHandle
  else
    AHandle := TcxProviderRecordHandle(GetRecordHandleByIndex(AIndex)).Parent;
  Result := InsertRecordHandle(AHandle, True);
  TcxProviderRecordHandle(Result).FDataLoaded := True;
  DataChanged;

//}
end;

function TcxSmartLoadDemoDataSource.GetChildCount(
  AParentHandle: TcxDataRecordHandle): Integer;

  function GetCountFromItem(AItem: TcxProviderRecordHandle): Integer;
  begin
    Result := AItem.Count;
    if not AItem.FDataLoaded then
    begin
      if (AItem.ChildLevelCount > 0) then
        Result := Result + cxProviderDemoRecordsPerLevel
      else
        AItem.FDataLoaded := True;
    end;
  end;

begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := GetCountFromItem(TcxProviderRecordHandle(AParentHandle));

//}
end;

function TcxSmartLoadDemoDataSource.GetChildRecordHandle(
  AParentHandle: TcxDataRecordHandle; AChildIndex: Integer): TcxDataRecordHandle;

  function GetChildItemHandle(AItem: TcxProviderRecordHandle): TcxDataRecordHandle;
  begin
    if not AItem.FDataLoaded then
      GenerateChildRecords(AItem);
    Result := AItem.Items[AChildIndex]
  end;

begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := GetChildItemHandle(TcxProviderRecordHandle(AParentHandle));

//}
end;

function TcxSmartLoadDemoDataSource.GetRootRecordHandle: TcxDataRecordHandle;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := FRootHandle;

//}
end;

{ TcxLoadAllRecordsDemoDataSource }

constructor TcxLoadAllRecordsDemoDataSource.Create;
begin
  inherited Create;
  FRecordsList := TList.Create;
  CreateAllRecords; 
end;

destructor TcxLoadAllRecordsDemoDataSource.Destroy;
begin
  FRecordsList.Free;
  inherited Destroy;
end;

procedure TcxLoadAllRecordsDemoDataSource.DeleteRecord(ARecordHandle: TcxDataRecordHandle);
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  FRecordsList.Remove(ARecordHandle);
  inherited DeleteRecord(ARecordHandle);

//}
end;

function TcxLoadAllRecordsDemoDataSource.GetParentRecordHandle(
  ARecordHandle: TcxDataRecordHandle): TcxDataRecordHandle;
begin
  Result := TcxProviderRecordHandle(ARecordHandle).Parent; 
end;

function TcxLoadAllRecordsDemoDataSource.GetRecordHandle(
  ARecordIndex: Integer): TcxDataRecordHandle;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := FRecordsList[ARecordIndex];

//}
end;

function TcxLoadAllRecordsDemoDataSource.GetRecordCount: Integer;
begin
{ remove/add the closing brace on this line to disable/enable the following code}

  Result := FRecordsList.Count;

//}
end;

function TcxLoadAllRecordsDemoDataSource.InsertRecordHandle(
  AParentHandle: TcxProviderRecordHandle; AIsChild: Boolean): TcxProviderRecordHandle;
begin
  Result := inherited InsertRecordHandle(AParentHandle, AIsChild);
  FRecordsList.Add(Result);
end;

procedure TcxLoadAllRecordsDemoDataSource.CreateAllRecords;

   procedure DoCreateRecords(AParent: TcxProviderRecordHandle; ALevel: Integer);
   var
     I: Integer;
   begin
     if ALevel > cxProviderDemoLevelCount then Exit;
     GenerateChildRecords(AParent);
     for I := 0 to AParent.Count - 1 do
       DoCreateRecords(AParent.Items[I], ALevel + 1);
   end;
begin
  DoCreateRecords(RootHandle, 1);
end;

end.
