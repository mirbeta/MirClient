unit uComponentDesigner;

interface

uses Classes, Graphics, Windows, ActiveX, TypInfo, uDesignIntf, uToolsApi,
     inifiles, SyncObjs, uEvents;

type

{$IFDEF VER210}
  TNotifyCode = (ncModuleDeleted, ncModuleRenamed, ncEditorModified,
    ncFormModified, ncEditorSelected, ncFormSelected, ncBeforeSave,
    ncAfterSave, ncFormSaving, ncProjResModified);
{$ENDIF}

  TPaintItemStyles = ( piDown, piSelected, piFramed, piGhosted );
  TPaintItemStyle = set of TPaintItemStyles;

  IPalettePaint = interface
    ['{D9BAD01A-99D9-4661-A470-90C7BC743DC9}']
    procedure Paint(Canvas: TCanvas; X, Y: integer; AStyle: TPaintItemStyle = [] );
  end;

  IPaletteBits = interface
    ['{E99AAAD9-8686-49FE-9129-13606772BDD9}']
    function GetBits(AStyle: TPaintItemStyle = []): TStream;
    procedure GetID;
  end;

  TFormState = set of (fsVisible, fsIconic, fsZoomed);

  IDatedStream = interface
    ['{A8613563-3389-4BA7-9A8A-5A8FF324F317}']
    function GetModifyTime: integer;
    procedure SetModifyTime(Time: integer);
  end;

  IFile = interface
    ['{346E7BA0-D47E-11D3-BA96-0080C78ADCDB}']
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
  end;

  TAffect = ( afTop, afLeft, afBottom, afRight, afHCenter, afVCenter, afHSpace, afVSpace, afHWinCenter, afVWinCenter, afNothing );
  TSizeAffect = ( asHGrow, asHShrink, asHAbsolute, asVGrow, asVShrink, asVAbsolute, asNothing );
  TShowState = (ssNormal, ssMinimized, ssMaximized);
  TDesignerState = set of (dsVisible, dsIconic, dsZoomed);

  IRoot = interface;
  TGetRootProc = procedure(AForm: IRoot) of object;

  IDesignerModule = interface;

  IPaletteItem = interface
    ['{F9D448F1-50BC-11D1-9FB5-0020AF3D82DA}']
    function CreateComponent(const Owner, Parent: TComponent; const Module: IDesignerModule; const Rect: TRect): TComponent;
  end;
  IInternalPaletteItem = IPaletteItem;

  ICompInfo = interface
    ['{AF859551-7401-11D1-9FBC-0020AF3D82DA}']
    procedure ClearEvent(Index: integer);
    function GetClassName: string;
    function GetEventCount: integer;
    function GetEventInfo(Index: integer): PPropInfo;
    function GetEventName(Index: integer): string;
    function GetEventValue(Index: integer): string;
    function GetComponentHandle: pointer;
    function GetNamePath: string;
    function GetSubInfoCount: integer;
    function GetSubInfo(Index: integer): ICompInfo;
  end;

  IAlignable = interface
    ['{346E7BA3-D47E-11D3-BA96-0080C78ADCDB}']
    procedure Align(Affect: TAffect);
    procedure Size(Affect: TSizeAffect; Value: integer);
  end;

  IScaleable = interface
    ['{346E7BA6-D47E-11D3-BA96-0080C78ADCDB}']
    procedure Scale(Factor: integer);
  end;

  ITabOrderable = interface
    ['{346E7BA4-D47E-11D3-BA96-0080C78ADCDB}']
    function GetTabCompCount: integer;
    function GetTabCompInfo(Order: integer; var Name: string; var Comp: Pointer): Boolean;
    procedure SetTabCompOrder(Comp: Pointer; Order: integer);
  end;

  ICreateOrderable = interface
    ['{346E7BA5-D47E-11D3-BA96-0080C78ADCDB}']
    function GetCompCount: integer;
    function GetCompName(Index: integer): string;
    function GetNVComp(Index: integer): pointer;
    procedure SetNVComp(Comp: Pointer; Order: integer);
  end;

  IRoot = interface(IFile)
     ['{346E7BA1-D47E-11D3-BA96-0080C78ADCDB}']
     procedure Close;
     procedure CreateComponent(Item: IPaletteItem);
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

  IDesignWindowActions = IDesignWindow;

  IDesignEnvironment = interface;

  {$IFDEF VER210}
  IBaseComponentDesigner = interface
    ['{6EBEF997-E986-41B3-8E70-3112112F1AB7}']
     function CreateRoot(const AModule: IDesignerModule; const AFileName: string;
        Existing: Boolean; const ARootName, AAncestor, AFileSystem: string): IRoot;
     function CreateFromStream(const AModule: IDesignerModule; const AFileName, AFileSystem:
        string; const Stream: IDatedStream): IRoot;
     function CreateNewRoot(const AModule: IDesignerModule; const AFileName: string;
        const Creator: IInterface): IRoot;
     function GetExtension: string;
  end;
  {$ENDIF}

  IComponentDesigner = interface{$IFDEF VER210}(IBaseComponentDesigner){$ENDIF}
     ['{7ED7BF25-E349-11D3-AB4A-00C04FB17A72}']
     procedure CopySelectionToStream(S: TMemoryStream; UnitDependencies: TStrings);
     {$IFNDEF VER210}
     function CreateRoot(const AModule: IDesignerModule; const AFileName: string;
        Existing: Boolean; const ARootName, AAncestor, AFileSystem: string): IRoot;
     function CreateFromStream(const AModule: IDesignerModule; const AFileName, AFileSystem:
        string; const Stream: IDatedStream): IRoot;
     function CreateNewRoot(const AModule: IDesignerModule; const AFileName: string;
        const Creator: IInterface): IRoot;
     {$ENDIF}
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
     {$IFDEF VER210}
     function GetFirstSelectionUnit: string;
     {$ENDIF}
     procedure GetRootNames(Proc: TGetStrProc);
     procedure GetProperties(Proc: TGetPropProc); {$IFDEF VER210}overload; {$ENDIF}
     {$IFDEF VER210}
     procedure GetProperties(const ASelections: IDesignerSelections; const ADesigner: IDesigner; Proc: TGetPropProc); overload;
     {$ENDIF}
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
     {$IFDEF VER210}
     function GetActiveClassGroup: TPersistentClass;
     {$ELSE}
     function GetExtension: string;
     {$ENDIF}
     property ControlsLocked: Boolean read GetControlsLocked write SetControlsLocked;
     property Roots[Index: integer]: IRoot read GetRoots;
     property Environment: IDesignEnvironment read GetEnvironment;
   end;

   TStringArray = array of string;

   IComponentDesigners = interface
     ['{82B1BC83-2E27-49D8-BF00-09E60D8BCC20}']
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
     function SpecialPropertyHelp(const Member: string; out HelpFile, Context: string;
         out HelpType: THelpType): Boolean;
     procedure GetProperties(Proc: TGetPropProc);
     function GetFirstSelectionType: string;
     {$IFDEF VER210}
     function GetFirstSelectionUnit: string;
     {$ENDIF}
     function RegisteredExtensions: TStringArray;
     function DesignerFromExtension(const Extension: string): IComponentDesigner;
     function RootComponentClass(ClassRef: TComponentClass): Boolean;
     {$IFDEF VER210}
     function GetCustomModuleIdentifier(const CustomModule: ICustomModule): string;
     procedure DeactivateActiveDesigner;
     {$ENDIF}
   end;

  IComponentPaletteItem = interface
    ['{707992A9-F11A-11D2-AAD2-00C04FB16FBC}']
    function GetComponentClass: TComponentClass;
  end;

  IInternalComponentDesigner = interface(IComponentDesigner)
    ['{7ED7BF31-E349-11D3-AB4A-00C04FB17A72}']
    procedure RemoveDependenciesOn(const Component: TComponent);
    procedure RootListAdd(const ARoot: IRoot);
    procedure RootListRemove(const ARoot: IRoot);
    procedure RootGoingDormant(const ARoot: IRoot);
    procedure RootActivated(const ARoot: IRoot);
    procedure RootModified(const ARoot: IRoot);
    procedure UpdateRootDependents;
    procedure UpdateSelections;
    function FileSystemFileOpen(const FileSystem, FileName: string; Mode: integer): TStream;
    function FileSystemFileAge(const FileSystem, FileName: string): integer;
    function FileSystemFileExists(const FileSystem, FileName: string): Boolean;
    function FileSystemRenameFile(const FileSystem, OldFileName, NewFileName: string): Boolean;
    function FileSystemGetTempFileName(const FileSystem, FileName: string): string;
    function FileSystemGetBackupName(const FileSystem, FileName: string): string;
    function FileSystemIsReadOnly(const FileSystem, FileName: string): Boolean;
    procedure FileSystemDeleteFile(const FileSystem, FileName: string);
    procedure FileSystemSetDate(const FileSystem: string; FileStream: TStream; Age: integer);
    function FindBaseClass(const AClassName: string): string;
    function FindCustomModuleClass(const AClassName: string): TCustomModuleClass;
    function IsRootBaseClass(const AClassName: string): Boolean;
    function ValidBaseClass(ComponentClass: TComponentClass): Boolean;
  end;

  IInternalComponentDesigners = interface(IComponentDesigners)
    ['{E1EC0A1E-1F1F-4167-8CF9-F628DD2D31F9}']
    procedure AddDesigner(const Designer: IInternalComponentDesigner);
    procedure RemoveDesigner(const Designer: IInternalComponentDesigner);
    procedure ActivateDesigner(const Designer: IInternalComponentDesigner);
    procedure RemoveRoot(const ARoot: IRoot);
  end;

  TDesignDialog = ( ddAlign, ddSize, ddScale, ddTabOrder, ddCreationOrder, ddSaveTemplate );
  TDesignCommand = ( dmAlignToGrid, dmFlipAllChildren, dmFlipSelectedChildren, dmTextDFM, dmViewAsText, dmRevert );

  TDesignerOptions = record
    DisplayGrid: Boolean;
    SnapToGrid: Boolean;
    GridSizeX: integer;
    GridSizeY: integer;
    ShowComponentCaptions: Boolean;
    ShowDesignerHints: Boolean;
    DFMFormat: Boolean;
    ShowNonVisualComponents: Boolean;
    ShowExtendedControlHints: Boolean;
  end;

   IDesignEnvironment = interface
     ['{D47416A5-9DC4-47E8-8D9C-6AD6B1C5F746}']
     procedure ActiveRootModified;
     procedure ComponentRenamed(const CurName, NewName: string);
     function FindComponent(const Name: string): TComponent;
     procedure ExecDesignDialog(DesignDialog: TDesignDialog);
     procedure RootActivated;
     procedure RootDeactivated;
     function GetPathAndBaseExeName: string;
     function GetPrivateDirectory: string;
     function GetBaseRegKey: string;
     function GetIDEOptions: TCustomIniFile;
     function GetToolSelected: Boolean;
     function GetCurCompClass: IPaletteItem;
     function GetPaletteItem(ComponentClass: TComponentClass): IPaletteItem;
     function GetCurTime: integer;
     procedure GetDesignerOptions(var Options: TDesignerOptions);
     function GetMainWindowSize: TRect;
     function GetWorkspaceOrigin: TPoint;
     function GetPackagesEvInstalled: TEvent;
     function GetPackagesEvUninstalling: TEvent;
     function GetComponentClass(const ClassName: string): TComponentClass;
     procedure LoadCustomModuleClass(const ClassName: string; const Designer: IComponentDesigner);
     procedure ModalEdit(EditKey: Char; const ReturnWindow: IActivatable);
     procedure OpenRoot(const RootName: string; Show: Boolean);
     procedure ResetCompClass;
     procedure SelectionChanged;
     function ShowClassHelp(const ClassName: string): Boolean;
     procedure ShowContextHelp(const HelpFile: string; Command, Data: integer);
     procedure SelectItemName(const PropertyName: string);
     function MakeBackupFileName(const FileName: string): string;
     function CreateBackupFile: Boolean;
     procedure RequestTemplate(const UnitName: string; const CompName: string; Stream: TStream; Dependencies: TStrings);
     procedure ItemDeleted(const ADesigner: IDesigner; AItem: TPersistent);
     property PackagesEvUninstalling: TEvent read GetPackagesEvUninstalling;
     property PackagesEvInstalled: TEvent read GetPackagesEvInstalled;
   end;

  IDesignerModule = interface
    ['{88442F57-A264-4A42-B734-D772147FF120}']
    procedure Activate(IsVisual: Boolean);
    procedure CreateMethod(const Name: string; TypeData: PTypeData);
    procedure RootModified;
    function GetAncestorClassName(const ClassName: string): string;
    procedure GetMethods( TypeData: PTypeData; Proc: TGetStrProc);
    function GetRootIsTopmost: Boolean;
    procedure RootResurrected;
    function MethodExists(const Name: string): Boolean;
    procedure RenameComponent(const CompInfo: ICompInfo; const CurName, NewName: string);
    procedure RenameRoot(const CompInfo: ICompInfo; const NewName: string);
    procedure RenameMethod(const CurName, NewName: string);
    procedure ShowMethod(const Name: string);
    procedure GetImportedRoots(Proc: TGetStrProc);
    procedure ImportRoot(const RootName: string);
    procedure RootSaving;
//    procedure Notify( NotifyCode: TNotifyCode);
    procedure SwapSourceVisualView;
    function GetMethod(const MethodName: string): string;
    procedure AddMethods(Methods: string; Replacements, MethodNames: TStrings);
    procedure GetProjectModules(Proc: TGetModuleProc);
    function GetReadOnly: Boolean;
    function GetModuleName: string;
    procedure ChainCall(const MethodName, InstanceName, InstanceMethod: string; TypeData: PTypeData);
    function GetUnnamed: Boolean;
    function SaveModule: Boolean;
    procedure ValidateEdit;
    property RootIsTopmost: Boolean read GetRootIsTopmost;
    property ReadOnly: Boolean read GetReadOnly;
    property Unnamed: Boolean read GetUnnamed;
  end;

  IInterfaceDesigner = interface
    ['{7E351A77-0AC5-4FD4-908B-5E259D418F81}']
    function HasInterface: Boolean;
    function InterfaceMemberExists(const Name: string): Boolean;
    procedure AddToInterface(InvKind: integer; const Name: string; VT: WORD; const TypeInfo: string);
  end;

function ActiveDesigner: IComponentDesigner;
function Designers: IComponentDesigners;
function ActiveRoot: IRoot;

implementation
  uses uComponentDesignerImpl;

function Designers: IComponentDesigners;
begin
  Result  :=  uComponentDesignerImpl._Designers;
end;

function ActiveDesigner: IComponentDesigner;
begin
  Result  :=  uComponentDesignerImpl.CompnentDesigner;
end;

function ActiveRoot: IRoot;
begin
  Result  :=  ActiveDesigner.GetActiveRoot;
end;

end.
