unit uComponentDesignerImpl;

interface

uses Windows, Classes, Controls, ActiveX, SysUtils, uDesignIntf, inifiles,
     uDesignerTypes, uComponentDesigner, uToolsApi, uEvents;

type
  TuCRoot = class(TInterfacedObject, IRoot)
  protected
    function FormFileOpen: IStream;
    function GetFileName: string;
    function GetTimeAtLoad: integer;
    function GetModifyTime: integer;
    function CheckFileDate: Boolean;
    procedure Rename(const NewFileName: string);
    procedure Save;
    property FileName: string read GetFileName;
    property TimeAtLoad: integer read GetTimeAtLoad;
    property ModifyTime: integer read GetModifyTime;
    procedure Close;
    procedure CreateComponent(Item: IInternalPaletteItem);

    procedure CreateComponentPos(Item: IInternalPaletteItem; X,Y: integer);
    function GetCompType(Index: integer):string;
    function FindCompClass(const CompName: string): string;
    function GetAncestorName: string;
    function GetCompCount: integer;
    procedure GetDependentRoots(Proc: TGetRootProc);
    function GetDesignClassName: string;
    procedure GetDependencies(Proc: TGetRootProc);
    function GetCompInfo(Index: integer): ICompInfo;
    function GetModule: IDesignerModule;
    function GetCompName(Index: Integer): string;
    function GetFileSystem: string;
    function GetRoot: TComponent;
    function GetRootName: string;
    procedure GetUnits(Proc: Classes.TGetStrProc);
    function GetState: TDesignerState;
    procedure Hide;
    procedure GoDormant;
    procedure RenameRootMethod(const CurName, NewName: string);
    function RenameComponent(const CurName, NewName: string): Boolean;
    procedure RemoveDependentLinks;
    procedure SetFileSystem(const FileSystem: string);
    procedure SetRootName(const AName: string);
    procedure SetSelection(const Name: string);
    procedure Show;
    procedure ShowAs(ShowState: TShowState);
    procedure ShowComponentHelp;
    function SpecialPropertyHelp(const Member: string; out HelpFile, Context: string;
             out HelpType: THelpType): Boolean;
    function GetDesigner: IDesigner;
    function GetFormEditor: IOTAFormEditor;
    property Root: TComponent read GetRoot;
    property Module: IDesignerModule read GetModule;
  end;

  TuCDesigners = class(TInterfacedObject, IComponentDesigners)
  public
    constructor Create;
    destructor Destroy; override;
  protected
    function DesignerActive: Boolean;
    procedure DesignerOptionsChanged;
    function FindFile(const Filename: string): IFile;
    function GetActiveDesigner: IComponentDesigner;
    procedure HideWindows;
    procedure ShowWindows;
    procedure SetProjectName(const Name: string);
    procedure SetControlsLocked(Value: Boolean);
    function GetControlsLocked: Boolean;
    function GetAlignable: IAlignable;
    procedure GetSelection(const ASelection: IDesignerSelections);
    procedure SetSelection(const Designer: IDesigner; const DesignWindow: IDesignWindow; const ASelection: IDesignerSelections);
    procedure ShowComponentHelp;
    function SpecialPropertyHelp(const Member: string; out HelpFile, Context: string; out HelpType: THelpType): Boolean;
    procedure GetProperties(Proc: TGetPropProc);
    function GetFirstSelectionType: string;
    function RegisteredExtensions: TStringArray;
    function DesignerFromExtension(const Extension: string): IComponentDesigner;
    function RootComponentClass(ClassRef: TComponentClass): Boolean;

    function GetFirstSelectionUnit: string;
    function GetCustomModuleIdentifier(const CustomModule: ICustomModule): string;
    procedure DeactivateActiveDesigner;
  end;

  TuCComponentDesigner = class(TObject,IUnknown, IComponentDesigner)
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure CopySelectionToStream(S: TMemoryStream; UnitDependencies: TStrings);
    function CreateRoot(const AModule: IDesignerModule; const AFileName: string;
        Existing: Boolean; const ARootName, AAncestor, AFileSystem: string): IRoot;
    function CreateFromStream(const AModule: IDesignerModule; const AFileName, AFileSystem:
        string; const Stream: IDatedStream): IRoot;
    function CreateNewRoot(const AModule: IDesignerModule; const AFileName: string;
        const Creator: IInterface): IRoot;
    function Active: Boolean;
    procedure DesignerOptionsChanged;
    function EditAction(Action: TEditAction): Boolean;
    function FindFile(const FileName: string): IFile;
    function FindRoot(Component: TComponent): IRoot; overload;
    function FindRoot(const RootName: string): IRoot; overload;
    function GetActiveRoot: IRoot;
    procedure GetClassUnits(const ClassName: string; Proc: TGetStrProc);
    function GetEditState: TEditState;
    function GetFirstSelectionType: string;
    procedure GetRootNames(Proc: TGetStrProc);
    procedure GetProperties(Proc: TGetPropProc); overload;
    procedure GetProperties(const ASelections: IDesignerSelections; const ADesigner: IDesigner; Proc: TGetPropProc); overload;
    function GetSelectionName: string;
    function GetSelectionType: string;
    procedure HideWindows;
    procedure ShowWindows;
    procedure ModalEditDone(const ReturnWindow: IActivatable);
    function OpenRootClass(const ClassName: string): IRoot;
    procedure PasteSelectionFromStream(S: TMemoryStream; Parent: TComponent; const Rect: TRect);
    procedure GetSelection(const ASelection: IDesignerSelections);
    procedure SetSelection(const Designer: IDesigner; const DesignWindow: IDesignWindow;
                               const ASelection: IDesignerSelections);
    procedure CancelModes;
    function GetControlsLocked: Boolean;
    procedure SetControlsLocked(Value: Boolean);
    function GetRoots(Index: integer): IRoot;
    function GetRootsCount: integer;
    function GetFirstSelectionClass: TClass;
    function IsNestable(const ClassName: string): Boolean;
    function GetEnvironment: IDesignEnvironment;
    function GetExtension: string;
    function GetActiveClassGroup: TPersistentClass;
    function GetFirstSelectionUnit: string;
  end;

  TuCDesignEnvironment = class(TObject,IUnknown, IDesignEnvironment)
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure ActiveRootModified;
    procedure ComponentRenamed(const CurName, NewName: string);
    function FindComponent(const Name: string): TComponent;
    procedure ExecDesignDialog(DesignDialog: TDesignDialog);
    procedure RootActivated;
    function GetPathAndBaseExeName: string;
    function GetPrivateDirectory: string;
    function GetBaseRegKey: string;
    function GetIDEOptions: TCustomIniFile;
    function GetToolSelected: Boolean;
    function GetCurCompClass: IInternalPaletteItem;
    function GetPaletteItem(ComponentClass: TComponentClass):IInternalPaletteItem;
    function GetCurTime: integer;
    procedure GetDesignerOptions(var Options: TDesignerOptions);
    function GetMainWindowSize: TRect;
    function GetWorkspaceOrigin: TPoint;
    procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);
    procedure OpenRoot(const RootName: string; Show: Boolean);
    procedure ResetCompClass;
    procedure SelectionChanged;
    procedure ShowContextHelp(const HelpFile: string; Command, Data: integer);
    procedure SelectItemName(const PropertyName: string);
    function CreateBackupFile: Boolean;
    procedure RequestTemplate(const UnitName: string; const CompName: string; Stream: TStream; Dependencies: TStrings);
    procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);

    procedure RootDeactivated;
    function GetComponentClass(const ClassName: string): TComponentClass;
    procedure LoadCustomModuleClass(const ClassName: string; const Designer: IComponentDesigner);
    function MakeBackupFileName(const FileName: string): string;
    function ShowClassHelp(const ClassName: string): Boolean;
    function GetTemplateDirectory: String;
    function GetAppDataDirectory(Local: Boolean = False): string;
    function GetPackagesEvInstalled: TEvent;
    function GetPackagesEvUninstalling: TEvent;

    property PackagesEvUninstalling: TEvent read GetPackagesEvUninstalling;
    property PackagesEvInstalled: TEvent read GetPackagesEvInstalled;
  end;

procedure BuildDesigners(const ToteBPL: Boolean);
procedure FreeDesigners(const ToteBPL: Boolean);

var
  CompnentDesigner: TuCComponentDesigner;
  DesignEnvironment: TuCDesignEnvironment;
  _Designers: IComponentDesigners;

implementation

uses Forms;

const
  _Offset = $90;
//IDA¼ì²é£º
//D2010: BorlandIDEServices¦a§}$20E0F108
//       ComponentDesigner.Designers¦a§}:$20E0E198

procedure BuildDesigners(const ToteBPL: Boolean);
begin
  _Designers := TuCDesigners.Create;
  if ToteBPL then
    PLongWord(LongWord(@BorlandIDEServices) + _Offset)^ := LongWord(_Designers);
end;

procedure FreeDesigners(const ToteBPL: Boolean);
begin
  if ToteBPL then
    PLongWord(LongWord(@BorlandIDEServices) + _Offset)^ := 0;
end;

{: TuCComponentDesigner }

function TuCComponentDesigner.Active: Boolean;
begin
  Result := True;
end;

procedure TuCComponentDesigner.CancelModes;
begin

end;

procedure TuCComponentDesigner.CopySelectionToStream(S: TMemoryStream;
  UnitDependencies: TStrings);
begin

end;

function TuCComponentDesigner.CreateFromStream(
  const AModule: IDesignerModule; const AFileName, AFileSystem: string;
  const Stream: IDatedStream): IRoot;
begin

end;

function TuCComponentDesigner.CreateNewRoot(const AModule: IDesignerModule;
  const AFileName: string; const Creator: IInterface): IRoot;
begin

end;

function TuCComponentDesigner.CreateRoot(const AModule: IDesignerModule;
  const AFileName: string; Existing: Boolean; const ARootName, AAncestor,
  AFileSystem: string): IRoot;
begin

end;

procedure TuCComponentDesigner.DesignerOptionsChanged;
begin

end;

function TuCComponentDesigner.EditAction(Action: TEditAction): Boolean;
begin
  Result := False;
end;

function TuCComponentDesigner.FindFile(const FileName: string): IFile;
begin
  Result := nil;
end;

function TuCComponentDesigner.FindRoot(Component: TComponent): IRoot;
begin
  Result := nil;
end;

function TuCComponentDesigner.FindRoot(const RootName: string): IRoot;
begin
  Result := nil;
end;

function TuCComponentDesigner.GetActiveClassGroup: TPersistentClass;
begin

end;

function TuCComponentDesigner.GetActiveRoot: IRoot;
begin
  Result := TuCRoot.Create;
end;

procedure TuCComponentDesigner.GetClassUnits(const ClassName: string;
  Proc: TGetStrProc);
begin

end;

function TuCComponentDesigner.GetControlsLocked: Boolean;
begin
  Result := False;
end;

function TuCComponentDesigner.GetEditState: TEditState;
begin
 Result := [esCanUndo..esCanCreateTemplate];
end;

function TuCComponentDesigner.GetEnvironment: IDesignEnvironment;
begin
  Result := DesignEnvironment;
end;

function TuCComponentDesigner.GetExtension: string;
begin
  Result := '';
end;

function TuCComponentDesigner.GetFirstSelectionClass: TClass;
begin
  Result := nil;
end;

function TuCComponentDesigner.GetFirstSelectionType: string;
begin
  Result := '';
end;

function TuCComponentDesigner.GetFirstSelectionUnit: string;
begin

end;

procedure TuCComponentDesigner.GetProperties(
  const ASelections: IDesignerSelections; const ADesigner: IDesigner;
  Proc: TGetPropProc);
begin

end;

procedure TuCComponentDesigner.GetProperties(Proc: TGetPropProc);
begin

end;

procedure TuCComponentDesigner.GetRootNames(Proc: TGetStrProc);
begin

end;

function TuCComponentDesigner.GetRoots(Index: integer): IRoot;
begin
  Result := nil;
end;

function TuCComponentDesigner.GetRootsCount: integer;
begin
  Result := 0;
end;

procedure TuCComponentDesigner.GetSelection(
  const ASelection: IDesignerSelections);
begin

//  if DsnManager.ActiveDesigner <> nil then
//   DsnManager.ActiveDesigner.GetSelections(ASelection);
end;

function TuCComponentDesigner.GetSelectionName: string;
begin
  Result := '';
end;

function TuCComponentDesigner.GetSelectionType: string;
begin
  Result := '';
end;

procedure TuCComponentDesigner.HideWindows;
begin
end;

function TuCComponentDesigner.IsNestable(const ClassName: string): Boolean;
begin
  Result := False;
end;

procedure TuCComponentDesigner.ModalEditDone(const ReturnWindow: IActivatable);
begin
end;

function TuCComponentDesigner.OpenRootClass(const ClassName: string): IRoot;
begin
end;

procedure TuCComponentDesigner.PasteSelectionFromStream(S: TMemoryStream;
  Parent: TComponent; const Rect: TRect);
begin

end;

function TuCComponentDesigner.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
 if GetInterface(IID, Obj) then Result := S_OK
    else Result := E_NOINTERFACE;
end;

procedure TuCComponentDesigner.SetControlsLocked(Value: Boolean);
begin

end;

procedure TuCComponentDesigner.SetSelection(const Designer: IDesigner;
  const DesignWindow: IDesignWindow;
  const ASelection: IDesignerSelections);
begin
  if Designer <> nil then
   Designer.SetSelections(ASelection);
end;

procedure TuCComponentDesigner.ShowWindows;
begin

end;

function TuCComponentDesigner._AddRef: Integer;
begin
 Result := 1;
end;

function TuCComponentDesigner._Release: Integer;
begin
 Result := 1;
end;

{: TuCDesignEnvironment }

procedure TuCDesignEnvironment.ActiveRootModified;
begin

end;

procedure TuCDesignEnvironment.ComponentRenamed(const CurName,
  NewName: string);
begin

end;

function TuCDesignEnvironment.CreateBackupFile: Boolean;
begin
  Result := False;
end;

procedure TuCDesignEnvironment.ExecDesignDialog(
  DesignDialog: TDesignDialog);
begin

end;

function TuCDesignEnvironment.FindComponent(const Name: string): TComponent;
begin
  Result := nil;
end;

function TuCDesignEnvironment.GetBaseRegKey: string;
begin
  Result := '';
end;

function TuCDesignEnvironment.GetCurCompClass: IInternalPaletteItem;
begin

end;

function TuCDesignEnvironment.GetCurTime: integer;
begin
  Result := 0;
end;

procedure TuCDesignEnvironment.GetDesignerOptions(
  var Options: TDesignerOptions);
begin

end;

function TuCDesignEnvironment.GetIDEOptions: TCustomIniFile;
begin
  Result := nil;
end;

function TuCDesignEnvironment.GetMainWindowSize: TRect;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := Rect(0, 0, P.X, P.Y);
end;

function TuCDesignEnvironment.GetPackagesEvInstalled: TEvent;
begin
  Result := nil;
end;

function TuCDesignEnvironment.GetPackagesEvUninstalling: TEvent;
begin
  Result := nil;
end;

function TuCDesignEnvironment.GetPaletteItem(ComponentClass: TComponentClass): IInternalPaletteItem;
begin
  Result := nil;
end;

function TuCDesignEnvironment.GetPathAndBaseExeName: string;
begin
  Result := '';
end;

function TuCDesignEnvironment.GetPrivateDirectory: string;
begin
  Result := '';
end;

function TuCDesignEnvironment.GetTemplateDirectory: String;
begin

end;

function TuCDesignEnvironment.GetToolSelected: Boolean;
begin
  Result := False;
end;

function TuCDesignEnvironment.GetWorkspaceOrigin: TPoint;
begin
// Result := DsnManager.GetWorkspaceOrigin;
end;

procedure TuCDesignEnvironment.ItemDeleted(const ADesigner: IDesigner;
  AItem: TPersistent);
begin

end;

procedure TuCDesignEnvironment.ModalEdit(EditKey: Char;
  const ReturnWindow: IActivatable);
begin

end;

procedure TuCDesignEnvironment.OpenRoot(const RootName: string;
  Show: Boolean);
begin

end;

function TuCDesignEnvironment.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
 if GetInterface(IID, Obj) then Result := S_OK
    else Result := E_NOINTERFACE;
end;

procedure TuCDesignEnvironment.RequestTemplate(const UnitName: string; const CompName: string;
  Stream: TStream; Dependencies: TStrings);
begin
end;

procedure TuCDesignEnvironment.ResetCompClass;
begin
end;

procedure TuCDesignEnvironment.RootActivated;
begin

end;

procedure TuCDesignEnvironment.SelectionChanged;
begin

end;

procedure TuCDesignEnvironment.SelectItemName(const PropertyName: string);
begin

end;

procedure TuCDesignEnvironment.ShowContextHelp(const HelpFile: string;
  Command, Data: integer);
begin

end;

function TuCDesignEnvironment._AddRef: Integer;
begin
  Result := 1;
end;

function TuCDesignEnvironment._Release: Integer;
begin
  Result := 1;
end;

procedure TuCDesignEnvironment.RootDeactivated;
begin

end;

function TuCDesignEnvironment.GetComponentClass(const ClassName: string): TComponentClass;
begin
  Result := nil;
end;

procedure TuCDesignEnvironment.LoadCustomModuleClass(const ClassName: string; const Designer: IComponentDesigner);
begin

end;

function TuCDesignEnvironment.MakeBackupFileName(const FileName: string): string;
begin
  Result := '';
end;

function TuCDesignEnvironment.ShowClassHelp(const ClassName: string): Boolean;
begin
  Result := False;
end;

function TuCDesignEnvironment.GetAppDataDirectory(Local: Boolean = False): string;
begin
  Result := ExtractFileDir(Application.ExeName);
end;

{: TuCDesigners }

constructor TuCDesigners.Create;
begin
  CompnentDesigner := TuCComponentDesigner.Create;
  DesignEnvironment := TuCDesignEnvironment.Create;
end;

destructor TuCDesigners.Destroy;
begin
  CompnentDesigner.Free;
  DesignEnvironment.Free;
  inherited;
end;

procedure TuCDesigners.DeactivateActiveDesigner;
begin

end;

function TuCDesigners.DesignerActive: Boolean;
begin
//  Result := DsnManager.ActiveDesigner <> nil;
end;

function TuCDesigners.DesignerFromExtension(
  const Extension: string): IComponentDesigner;
begin
  Result := CompnentDesigner;
end;

procedure TuCDesigners.DesignerOptionsChanged;
begin

end;

function TuCDesigners.FindFile(const Filename: string): IFile;
begin

end;

function TuCDesigners.GetActiveDesigner: IComponentDesigner;
begin
  Result := CompnentDesigner;
end;

function TuCDesigners.GetAlignable: IAlignable;
begin

end;

function TuCDesigners.GetControlsLocked: Boolean;
begin
  Result := False;
end;

function TuCDesigners.GetCustomModuleIdentifier(
  const CustomModule: ICustomModule): string;
begin

end;

function TuCDesigners.GetFirstSelectionType: string;
begin

end;

function TuCDesigners.GetFirstSelectionUnit: string;
begin

end;

procedure TuCDesigners.GetProperties(Proc: TGetPropProc);
begin

end;

procedure TuCDesigners.GetSelection(const ASelection: IDesignerSelections);
begin

end;

procedure TuCDesigners.HideWindows;
begin

end;

function TuCDesigners.RegisteredExtensions: TStringArray;
begin
  SetLength(Result, 0);
end;

function TuCDesigners.RootComponentClass(ClassRef: TComponentClass): Boolean;
begin
  Result := False;
end;

procedure TuCDesigners.SetControlsLocked(Value: Boolean);
begin

end;

procedure TuCDesigners.SetProjectName(const Name: string);
begin

end;

procedure TuCDesigners.SetSelection(const Designer: IDesigner;
  const DesignWindow: IDesignWindow;
  const ASelection: IDesignerSelections);
begin

end;

procedure TuCDesigners.ShowComponentHelp;
begin

end;

procedure TuCDesigners.ShowWindows;
begin

end;

function TuCDesigners.SpecialPropertyHelp(const Member: string;
  out HelpFile, Context: string; out HelpType: THelpType): Boolean;
begin
  Result := False;
end;

{: TuCRoot }

function TuCRoot.CheckFileDate: Boolean;
begin
 Result := False;
end;

procedure TuCRoot.Close;
begin
end;

procedure TuCRoot.CreateComponent(Item: IInternalPaletteItem);
begin

end;

procedure TuCRoot.CreateComponentPos(Item: IInternalPaletteItem; X, Y: integer);
begin

end;

function TuCRoot.GetCompType(Index: integer):string;
begin
  Result := '';
end;

function TuCRoot.FindCompClass(const CompName: string): string;
begin
  Result := ''
end;

function TuCRoot.FormFileOpen: IStream;
begin
  Result := nil;
end;

function TuCRoot.GetAncestorName: string;
begin
  Result := '';
end;

function TuCRoot.GetCompCount: integer;
begin
  Result := 0;
end;

function TuCRoot.GetCompInfo(Index: integer): ICompInfo;
begin
  Result := nil;
end;

function TuCRoot.GetCompName(Index: Integer): string;
begin
  Result := '';
end;

procedure TuCRoot.GetDependencies(Proc: TGetRootProc);
begin

end;

procedure TuCRoot.GetDependentRoots(Proc: TGetRootProc);
begin

end;

function TuCRoot.GetDesignClassName: string;
begin
  Result := '';
end;

function TuCRoot.GetDesigner: IDesigner;
begin
//  Result := DsnManager.ActiveDesigner;
end;

function TuCRoot.GetFileName: string;
begin
  Result := '';
end;

function TuCRoot.GetFileSystem: string;
begin
  Result := '';
end;

function TuCRoot.GetFormEditor: IOTAFormEditor;
begin
  Result := nil;
end;

function TuCRoot.GetModifyTime: integer;
begin
  Result := 0;
end;

function TuCRoot.GetModule: IDesignerModule;
begin
  Result := nil;
end;

function TuCRoot.GetRoot: TComponent;
begin
  Result := nil;
end;

function TuCRoot.GetRootName: string;
begin
  Result := '';
end;

function TuCRoot.GetState: TDesignerState;
begin

end;

function TuCRoot.GetTimeAtLoad: integer;
begin
  Result := 0;
end;

procedure TuCRoot.GetUnits(Proc: TGetStrProc);
begin

end;

procedure TuCRoot.GoDormant;
begin

end;

procedure TuCRoot.Hide;
begin

end;

procedure TuCRoot.RemoveDependentLinks;
begin

end;

procedure TuCRoot.Rename(const NewFileName: string);
begin

end;

function TuCRoot.RenameComponent(const CurName, NewName: string): Boolean;
begin
  Result := False;
end;

procedure TuCRoot.RenameRootMethod(const CurName, NewName: string);
begin

end;

procedure TuCRoot.Save;
begin

end;

procedure TuCRoot.SetFileSystem(const FileSystem: string);
begin

end;

procedure TuCRoot.SetRootName(const AName: string);
begin

end;

procedure TuCRoot.SetSelection(const Name: string);
begin

end;

procedure TuCRoot.Show;
begin

end;

procedure TuCRoot.ShowAs(ShowState: TShowState);
begin

end;

procedure TuCRoot.ShowComponentHelp;
begin

end;

function TuCRoot.SpecialPropertyHelp(const Member: string; out HelpFile,
  Context: string; out HelpType: THelpType): Boolean;
begin
  Result := False;
end;

end.
