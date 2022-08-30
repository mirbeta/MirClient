unit uPackageManager;

interface
  uses Windows, Classes, Sysutils, Graphics, ActnListXE;

  type
  PComponentInfo  = ^TComponentInfo;
  TComponentInfo  = record
    PageName: String[255];
    ComponentClass: TComponentClass;
    ComponentBitmap: TBitmap;
  end;

  TPackage  = class
  private
    FPackageHandle: Longint;
    FContainsUnit,
    FRequires: TStrings;
    FComponentList: TList;
    FPackageFile: String;
    FDescription: String;
    function GetComponentInfo(index: Integer): PComponentInfo;
    function GetCount: Integer;
    procedure RegisterUnit(const UnitName: String);
  public
    constructor Create(const PackageFile: String);
    destructor Destroy; override;
    function ContrainComponentClass(ComponentClass: TComponentClass; out Info: PComponentInfo): boolean; overload;
    function ContrainComponentClass(ComponentClass: TComponentClass): boolean; overload;
    property PackageFile: String read FPackageFile;
    property Description: String read FDescription;
    property ComponentCount: Integer read GetCount;
    property Component[index: Integer]: PComponentInfo read GetComponentInfo;
  end;

  TPackageManager = class
  private
    FCustomPackage: TPackage;
    FPackageList: TList;
    FNoIconList: TList;
    function GetPkgCount: Integer;
    function GetPackage(index: Integer): TPackage;
    function PackgeIsRegisted(const PackageFile: String): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function GetComponentICON(const AComponent: TComponent): TBitmap;
    function ComponentISReg(const AComponentClass: TComponentClass): Boolean;
    function ISNoIcon(ACls: TClass): Boolean;
    procedure RegisterComponents(const PageName: String; Components: array of TComponentClass);
    procedure RegisterPackage(const PackageFile: String);
    property PackageCount: Integer read GetPkgCount;
    property Package[index: Integer]: TPackage read GetPackage;
  end;

var
  PackageManager: TPackageManager;
  ISRunTime: Boolean;

implementation

const UNKNOW_ICO  = 'UNKNOW_COMPONENT';
var
  _CurrentPackage:  TPackage  = nil;

function CurrentPackage:  TPackage;
begin
  Result  :=  _CurrentPackage;
  if not Assigned(Result) then
    Result  :=  PackageManager.FCustomPackage;
end;

{ TPackage }
procedure PackageInfoProc(const Name: string; NameType: TNameType; Flags: Byte; Param: Pointer);
begin
  with TPackage(Param) do
    case NameType of
      ntContainsUnit:
      begin
        FContainsUnit.Add(Name);
        RegisterUnit(Name);
      end;
      ntRequiresPackage:
        FRequires.Add(Name);
    end;
end;

procedure RegComponentsProc(const Page: string;
  const ComponentClasses: array of TComponentClass);
var
  I: Integer;
  ComponentInfo: PComponentInfo;
begin
  for i := 0 to High(ComponentClasses) do
    if not PackageManager.ComponentISReg(ComponentClasses[I]) then
    begin
      RegisterClass(TPersistentClass(ComponentClasses[I]));
      New(ComponentInfo);
      ComponentInfo^.PageName :=  Page;
      ComponentInfo^.ComponentClass   :=  ComponentClasses[I];
      ComponentInfo^.ComponentBitmap  :=  TBitmap.Create;
      if FindResource(CurrentPackage.FPackageHandle, PChar(UpperCase(ComponentClasses[I].ClassName)),RT_BITMAP)<>0 then
      begin
        ComponentInfo^.ComponentBitmap.LoadFromResourceName(CurrentPackage.FPackageHandle, PChar(UpperCase(ComponentClasses[I].ClassName)));
      end;
      CurrentPackage.FComponentList.Add(ComponentInfo);
    end;
end;

procedure RegisterNoIcon(const ComponentClasses: array of TComponentClass);
var
  I: Integer;
begin
  for I := Low(ComponentClasses) to High(ComponentClasses) do
  begin
    RegisterClass(ComponentClasses[I]);
    PackageManager.FNoIconList.Add(ComponentClasses[I]);
  end;
end;

procedure RegisterNonActiveXP(const ComponentClasses: array of TComponentClass;
    AxRegType: TActiveXRegType);
var
  I: Integer;
begin
  for I := Low(ComponentClasses) to High(ComponentClasses) do
    RegisterClass(ComponentClasses[I]);
end;

procedure RegisterActions(const CategoryName: string;
    const AClasses: array of TBasicActionClass; Resource: TComponentClass);
begin
end;

function TPackage.ContrainComponentClass(ComponentClass: TComponentClass;
  out Info: PComponentInfo): boolean;
var
  I: Integer;
begin
  Result  :=  False;
  for I := 0 to ComponentCount - 1 do
    if ComponentClass=Component[I].ComponentClass then
    begin
      Info    :=  Component[I];
      Result  :=  True;
      Exit;
    end;
  Info    :=  nil;
  Result  :=  False;
end;

function TPackage.ContrainComponentClass(
  ComponentClass: TComponentClass): boolean;
var
  I: Integer;
begin
  Result  :=  False;
  for I := 0 to ComponentCount - 1 do
    if ComponentClass=Component[I].ComponentClass then
    begin
      Result  :=  True;
      Exit;
    end;
  Result  :=  False;
end;

constructor TPackage.Create(const PackageFile: String);
var
  Flags: Integer;
begin
  FPackageFile    :=  PackageFile;
  FComponentList  :=  TList.Create;
  FContainsUnit   :=  TStringList.Create;
  FRequires       :=  TStringList.Create;
  if (PackageFile<>'') and FileExists(PackageFile) then
  begin
    FDescription    := GetPackageDescription(PChar(PackageFile));
    FPackageHandle  :=  LoadPackage(PackageFile);
    if FPackageHandle>0 then
      GetPackageInfo(FPackageHandle, Pointer(Self), Flags, PackageInfoProc);
  end;
end;

destructor TPackage.Destroy;
begin
  UnLoadPackage(FPackageHandle);
  FreeAndNil(FContainsUnit);
  FreeAndNil(FRequires);
  FreeAndNil(FComponentList);
  inherited;
end;

function TPackage.GetComponentInfo(index: Integer): PComponentInfo;
begin
  Result  :=  FComponentList.Items[index];
end;

function TPackage.GetCount: Integer;
begin
  Result  :=  FComponentList.Count;
end;

procedure TPackage.RegisterUnit(const UnitName: String);
var
  RegProc:  procedure();
  RegProcName: string;
begin
  RegProc     := nil;
  _CurrentPackage :=  Self;
  RegProcName := '@'  + UpCase(UnitName[1])
                  + LowerCase(Copy(UnitName, 2, Length(UnitName)))
                  + '@Register$qqrv';

  @RegProc := GetProcAddress(FPackageHandle, PChar(RegProcName));
  if Assigned(RegProc) then
    try
      if not SameText(UnitName, 'SvcReg') then
        RegProc;
      except
      end;
  @RegProc := GetProcAddress(FPackageHandle, PChar('@' + UnitName + '@RuntimeRegister$qqrv'));
  if Assigned(RegProc) then
  try
    RegProc;
  except
  end;
  _CurrentPackage :=  nil;
end;

{ TPackageManager }

function TPackageManager.ComponentISReg(
  const AComponentClass: TComponentClass): Boolean;
var
  I: Integer;
begin
  for I := 0 to FPackageList.Count - 1 do
    if GetPackage(I).ContrainComponentClass(AComponentClass) then
    begin
      Result  :=  True;
      Exit;
    end;

  if FCustomPackage.ContrainComponentClass(AComponentClass) then
  begin
    Result  :=  True;
    Exit;
  end;
  Result  :=  False;
end;

constructor TPackageManager.Create;
begin
  FPackageList  :=  TList.Create;
  FCustomPackage:=  TPackage.Create('');
  FCustomPackage.FPackageHandle :=  HInstance;
  FNoIconList   :=  TList.Create;
  FPackageList.Add(FCustomPackage);
end;

destructor TPackageManager.Destroy;
begin
  FreeAndNil(FPackageList);
  FreeAndNil(FNoIconList);
  inherited;
end;

function TPackageManager.GetComponentICON(const AComponent: TComponent): TBitmap;
var
  I: Integer;
  ComponentInfo: PComponentInfo;
begin
  Result  :=  nil;
  for I := 0 to PackageCount - 1 do
    if Package[I].ContrainComponentClass(TComponentClass(AComponent.ClassType), ComponentInfo) then
    begin
      Result  :=  ComponentInfo^.ComponentBitmap;
      Break;
    end;
end;

function TPackageManager.GetPackage(index: Integer): TPackage;
begin
  Result  :=  FPackageList.Items[index];
end;

function TPackageManager.GetPkgCount: Integer;
begin
  Result  :=  FPackageList.Count;
end;

function TPackageManager.ISNoIcon(ACls: TClass): Boolean;
var
  I: Integer;
begin
  Result  :=  True;
  for I := 0 to FNoIconList.Count - 1 do
    if ACls.InheritsFrom(TClass(FNoIconList.Items[I])) then
      Exit;
  Result  :=  False;
end;

function TPackageManager.PackgeIsRegisted(const PackageFile: String): boolean;
var
  I: Integer;
begin
  for I := 0 to FPackageList.Count - 1 do
    if SameText(PackageFile, TPackage(FPackageList.Items[I]).FPackageFile) then
    begin
      Result  :=  True;
      Exit;
    end;
  Result  :=  False;
end;

procedure TPackageManager.RegisterComponents(const PageName: String;
  Components: array of TComponentClass);
var
  I: Integer;
  ComponentInfo: PComponentInfo;

  procedure LoadComponentICO;
  var
    CompCls: TComponentClass;
  begin
    CompCls :=  ComponentInfo.ComponentClass;
    repeat
      if FindResource(HInstance, PChar(UpperCase(CompCls.ClassName)),RT_BITMAP)<>0 then
        ComponentInfo^.ComponentBitmap.LoadFromResourceName(HInstance, PChar(UpperCase(CompCls.ClassName)));
      if CompCls=TComponent then Break;
      CompCls :=  TComponentClass(CompCls.ClassParent);
    until (CompCls=TComponent) or not ComponentInfo^.ComponentBitmap.Empty;
    if ComponentInfo^.ComponentBitmap.Empty and (FindResource(HInstance, PChar(UNKNOW_ICO) ,RT_BITMAP)<>0) then
      ComponentInfo^.ComponentBitmap.LoadFromResourceName(HInstance, PChar(UNKNOW_ICO));
  end;

begin
  for I := Low(Components) to High(Components) do
    if not ComponentISReg(Components[I]) then
    begin
      RegisterClass(Components[I]);
      New(ComponentInfo);
      ComponentInfo^.PageName         :=  PageName;
      ComponentInfo^.ComponentClass   :=  Components[I];
      ComponentInfo^.ComponentBitmap  :=  TBitmap.Create;
      LoadComponentICO;
      FCustomPackage.FComponentList.Add(ComponentInfo);
    end;
end;

procedure TPackageManager.RegisterPackage(const PackageFile: String);
begin
  if not PackgeIsRegisted(PackageFile) then
  begin
    _CurrentPackage :=  TPackage.Create(PackageFile);
    FPackageList.Add(_CurrentPackage);
    _CurrentPackage :=  nil;
  end;
end;

initialization
  ISRunTime       :=  not Assigned(RegisterComponentsProc);
  PackageManager  :=  TPackageManager.Create;
  if ISRunTime then
  begin
    Classes.RegisterComponentsProc  :=  RegComponentsProc;
    Classes.RegisterNoIconProc      :=  RegisterNoIcon;
    Classes.RegisterNonActiveXProc  :=  RegisterNonActiveXP;
    ActnListXE.RegisterActionsProc    :=  RegisterActions;
  end;

finalization
  FreeAndNil(PackageManager);

end.
