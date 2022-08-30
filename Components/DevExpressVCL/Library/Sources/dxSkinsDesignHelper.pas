{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSkins Library                                     }
{                                                                    }
{           Copyright (c) 2006-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSSKINS AND ALL ACCOMPANYING     }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxSkinsDesignHelper;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, ToolsApi, CheckLst, ExtCtrls,
  Menus, IniFiles, Registry, StdCtrls, dxCoreClasses, cxLookAndFeels, cxLookAndFeelPainters, cxClasses, cxLibraryReg;

type
  TdxSkinsUnitStateList = class;
  TdxSkinsUnitsState = (susDisabled, susEnabled, susUndefined);

  { TdxSkinsUnitStateListItem }

  TdxSkinsUnitStateListItem = class(TObject)
  private
    FName: string;
    FOwner: TdxSkinsUnitStateList;
    FSkinUnitName: string;
    FState: TdxSkinsUnitsState;
    function GetEnabled: Boolean;
    procedure SetState(AState: TdxSkinsUnitsState);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TdxSkinsUnitStateList; const ASkinUnitName, AName: string);
    procedure LoadFromIni(AIniFile: TCustomIniFile; const ASection: string);
    procedure SaveToIni(AIniFile: TCustomIniFile; const ASection: string);

    property Enabled: Boolean read GetEnabled;
    property Name: string read FName;
    property Owner: TdxSkinsUnitStateList read FOwner;
    property SkinUnitName: string read FSkinUnitName;
    property State: TdxSkinsUnitsState read FState write SetState;
  end;

  { TdxSkinsUnitStateList }

  TdxSkinsUnitStateList = class
  strict private
    FList: TcxObjectList;

    FOnChanged: TNotifyEvent;

    function GetCount: Integer;
    function GetHasItemsWithState(AState: TdxSkinsUnitsState): Boolean;
    function GetItem(AIndex: Integer): TdxSkinsUnitStateListItem;
  protected
    procedure Changed; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddItem(const AUnitName, AName: string): TdxSkinsUnitStateListItem;
    function FindItemByName(const AName: string; var AItem: TdxSkinsUnitStateListItem): Boolean;
    procedure Clear;
    procedure Reset;

    procedure LoadSettings(AIniFile: TCustomIniFile; const ASection: string);
    procedure SaveSettings(AIniFile: TCustomIniFile; const ASection: string);

    property Count: Integer read GetCount;
    property HasDisabledItems: Boolean index susDisabled read GetHasItemsWithState;
    property HasEnabledItems: Boolean index susEnabled read GetHasItemsWithState;
    property HasUndefinedItems: Boolean index susUndefined read GetHasItemsWithState;
    property Item[Index: Integer]: TdxSkinsUnitStateListItem read GetItem;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  end;

  { TdxSkinsProjectSettings }

  TdxSkinsProjectSettings = class(TcxIUnknownObject, IcxLookAndFeelPainterListener)
  private
    FCheckDefaultSettingsNeeded: Boolean;
    FEnabled: Boolean;
    FInitialized: Boolean;
    FIsDefaultSettings: Boolean;
    FShowNotification: Boolean;
    FUnitStateList: TdxSkinsUnitStateList;

    function GetCurrentProjectFileName: string;
    function GetHasDefaultSettings: Boolean;
    function GetIsCurrentProjectExists: Boolean;
    function GetIsDefaultSettings: Boolean;
    function GetNeedShowConfirmation: Boolean;
    function GetSkinsConfigFileName: string;
    procedure SetEnabled(AValue: Boolean);
    procedure SetShowNotification(AValue: Boolean);
  protected
    function CalcIsDefaultSettings: Boolean;
    function CheckDefaultsForUndefinedItems: Boolean;
    function CreateRegIniFile: TRegistryIniFile;
    procedure ActiveProjectChanged;
    procedure DoUnitStateListChanged(Sender: TObject);
    procedure InternalLoadSettings(AConfig: TCustomIniFile; const ASection: string);
    procedure InternalSaveSettings(AConfig: TCustomIniFile; const ASection: string);
    procedure SettingsChanged;
    // IcxLookAndFeelPainterListener
    procedure IcxLookAndFeelPainterListener.PainterAdded = PainterChanged;
    procedure IcxLookAndFeelPainterListener.PainterRemoved = PainterChanged;
    procedure PainterChanged(APainter: TcxCustomLookAndFeelPainter);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    //
    procedure Finalize;
    procedure Initialize;
    procedure LoadSettings;
    procedure ReloadUnitsList;
    procedure SaveSettings;
    procedure UpdateActiveProjectSettings;

    procedure LoadDefaultSettings;
    procedure RemoveDefaultSettings;
    procedure SaveDefaultSettings;

    property CurrentProjectFileName: string read GetCurrentProjectFileName;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property HasDefaultSettings: Boolean read GetHasDefaultSettings;
    property Initialized: Boolean read FInitialized;
    property IsCurrentProjectExists: Boolean read GetIsCurrentProjectExists;
    property IsDefaultSettings: Boolean read GetIsDefaultSettings;
    property NeedShowConfirmation: Boolean read GetNeedShowConfirmation;
    property ShowNotification: Boolean read FShowNotification write SetShowNotification;
    property SkinsConfigFileName: string read GetSkinsConfigFileName;
    property UnitStateList: TdxSkinsUnitStateList read FUnitStateList;
  end;

  { TdxSkinsProjectOptionsMenuExpert }

  TdxSkinsProjectOptionsMenuExpert = class(TObject)
  private
    FMenuItem: TMenuItem;
    function GetProjectMenuItem: TMenuItem;
    procedure DoMenuItemClick(Sender: TObject);
  protected
    function CalcMenuItemPosition(AParent: TMenuItem): Integer;
    function CreateMenuItem(AParent: TMenuItem): TMenuItem;
    function FindMenuItemByName(AParent: TMenuItem; const AName: string): TMenuItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property MenuItem: TMenuItem read FMenuItem;
    property ProjectMenuItem: TMenuItem read GetProjectMenuItem;
  end;

  { TdxSkinsDesignHelper }

  TdxSkinsDesignHelper = class(TcxIUnknownObject,
    IOTAModuleNotifier, IOTANotifier, IOTAIDENotifier)
  private
    FActiveProject: IOTAProject;
    FActiveProjectNotifierID: Integer;
    FMenuExpert: TdxSkinsProjectOptionsMenuExpert;
    FServicesNotifierID: Integer;
    procedure SetActiveProject(AProject: IOTAProject);
  protected
    function RegisterModuleNotifier(AModule: IOTAModule): Integer;
    procedure RegisterIDENotifier;
    procedure UnregisterIDENotifier;
    procedure UnregisterModuleNotifier(AModule: IOTAModule; ID: Integer);
    procedure UpdateMenuItemState;
    // IOTAModuleNotifier
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);
    // IOTANotifier
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    // IOTAIDENotifier
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property ActiveProject: IOTAProject read FActiveProject write SetActiveProject;
    property MenuExpert: TdxSkinsProjectOptionsMenuExpert read FMenuExpert;
  end;

  { TdxSkinsDesignHelperForm }

  TdxSkinsDesignHelperForm = class(TForm)
    bCancel: TButton;
    bOk: TButton;
    bSelectAll: TButton;
    bSelectNone: TButton;
    bvFrame: TBevel;
    cbSkinsAutoFilling: TCheckBox;
    CheckListBoxHolder: TBevel;
    Image: TImage;
    lbNotes: TLabel;
    lbSkins: TLabel;
    pbFrame: TPaintBox;
    plNotes: TPanel;
    cbDefault: TCheckBox;
    cbShowNotifications: TCheckBox;
    procedure bSelectAllClick(Sender: TObject);
    procedure cbSkinsAutoFillingClick(Sender: TObject);
    procedure pbFramePaint(Sender: TObject);
    procedure cbShowNotificationsClick(Sender: TObject);
  private
    CheckListBox: TCheckListBox;
    FIsDefaultSettings: Boolean;
    function GetHasChanges: Boolean;
    procedure ApplySettings(AResetToDefaults: Boolean);
    procedure DoCheckListBoxClickCheck(Sender: TObject);
    procedure PopulateList;
    procedure UpdateDefaultCheckBoxState;
  public
    constructor Create(AOwner: TComponent); override;
    class procedure Execute;
    function IsShortCut(var Message: TWMKey): Boolean; override;
    //
    property HasChanges: Boolean read GetHasChanges;
  end;

function dxSkinsListFilter(const ASkinName: string): Boolean;
function dxSkinsProjectSettings: TdxSkinsProjectSettings;
procedure dxSkinsShowProjectOptionsDialog;
implementation

{$R *.dfm}

{$R dxSkinsDesignHelper.res}

uses
  dxSkinsLookAndFeelPainter, dxSkinsStrs, dxDesignHelpers, dxSkinsReg;

const
  //don't localize!
  sdxSkinsCfgExt = '.skincfg';
  sdxSkinsCfgSection = 'ExpressSkins';
  sdxSkinsMenuItemGlyphResName = 'DXSKINSDESIGNHELPERICON';
  sdxSkinsProjectState = 'Enabled';
  sdxSkinsProjectShowNotifications = 'ShowNotifications';
  sdxSkinsDefault = 'Default';
  sdxSkinsDefaultRegistryPath = 'Software\Developer Express\ExpressSkins\OptionsEditor';

  sdxSkinCheckListBoxHint =
    'Select skins in this list to make them available' + #13#10 +
    'within the project. Selecting skins automatically adds' + #13#10 +
    'corresponding skin units to the ''uses'' clause.' + #13#10 +
    'New skins added to the project are highlighted in bold.';
  sdxEnableSkinSupportHint =
    'Check this option to enable skins within the current project.' + #13#10 +
    'If enabled, all required skin painter units will be automatically' + #13#10 +
    'added to the ''uses'' clause.';
  sdxSkinsMenuItemCaption = '&Modify Skin Options';

  sdxSkinNotifyBoxHint = 'If enabled, this Editor is automatically invoked when ' + #13#10 +
    'the installed ExpressSkins Library contains new skins. Otherwise, the ' + #13#10 +
    'Editor is not shown, and new skins are automatically enabled within the ' + #13#10 +
    'project, if all skins are enabled in a .SKINCFG file. If one or more skins ' + #13#10 +
    'are disabled in this file, new skins will be automatically disabled in your projects.';

  sdxSkinDefaultHint = 'Enable this option to store skin options to the registry, ' + #13#10 +
    'and make them default. These options will be applied, by default, to all new projects.';

  BoolToUnitState: array[Boolean] of TdxSkinsUnitsState = (susDisabled, susEnabled);

type

  { TdxSkinsCheckListBox }

  TdxSkinsCheckListBox = class(TCheckListBox)
  private
    FAllowBoldSelection: Boolean;
    function GetItemState(Index: Integer): TdxSkinsUnitsState;
  protected
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    //
    property ItemState[Index: Integer]: TdxSkinsUnitsState read GetItemState;
    property AllowBoldSelection: Boolean read FAllowBoldSelection write FAllowBoldSelection;
  end;

var
  SkinsDesignHelper: TdxSkinsDesignHelper;
  SkinsProjectSettings: TdxSkinsProjectSettings;

function dxSkinsProjectSettings: TdxSkinsProjectSettings;
begin
  if SkinsProjectSettings = nil then
    SkinsProjectSettings := TdxSkinsProjectSettings.Create;
  Result := SkinsProjectSettings;
end;

procedure dxSkinsShowProjectOptionsDialog;
begin
  TdxSkinsDesignHelperForm.Execute;
end;

function dxSkinsListFilter(const ASkinName: string): Boolean;
var
  AItem: TdxSkinsUnitStateListItem;
begin
  Result := dxSkinsProjectSettings.Enabled and
    dxSkinsProjectSettings.UnitStateList.FindItemByName(ASkinName, AItem) and
    AItem.Enabled;
end;

{ TdxSkinsUnitStateList }

constructor TdxSkinsUnitStateList.Create;
begin
  FList := TcxObjectList.Create;
end;

destructor TdxSkinsUnitStateList.Destroy;
begin
  FList.Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxSkinsUnitStateList.AddItem(const AUnitName, AName: string): TdxSkinsUnitStateListItem;
begin
  Result := TdxSkinsUnitStateListItem.Create(Self, AUnitName, AName);
  FList.Add(Result);
end;

procedure TdxSkinsUnitStateList.Clear;
begin
  FList.Clear;
  Changed;
end;

procedure TdxSkinsUnitStateList.Changed;
begin
  if Assigned(OnChanged) then OnChanged(Self);
end;

function TdxSkinsUnitStateList.FindItemByName(
  const AName: string; var AItem: TdxSkinsUnitStateListItem): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := SameText(AName, Item[I].Name);
    if Result then
    begin
      AItem := Item[I];
      Break;
    end;
  end;
end;

function TdxSkinsUnitStateList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TdxSkinsUnitStateList.GetHasItemsWithState(AState: TdxSkinsUnitsState): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Item[I].State = AState;
    if Result then
      Break;
  end;
end;

function TdxSkinsUnitStateList.GetItem(AIndex: Integer): TdxSkinsUnitStateListItem;
begin
  Result := TdxSkinsUnitStateListItem(FList.Items[AIndex]);
end;

procedure TdxSkinsUnitStateList.Reset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].State := susUndefined;
end;

procedure TdxSkinsUnitStateList.LoadSettings(AIniFile: TCustomIniFile; const ASection: string);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Item[I].LoadFromIni(AIniFile, ASection);
end;

procedure TdxSkinsUnitStateList.SaveSettings(AIniFile: TCustomIniFile; const ASection: string);
var
  I: Integer;
begin
  try
    for I := 0 to Count - 1 do
      Item[I].SaveToIni(AIniFile, ASection);
  except
  end;
end;

{ TdxSkinsUnitStateListItem }

constructor TdxSkinsUnitStateListItem.Create(
  AOwner: TdxSkinsUnitStateList; const ASkinUnitName, AName: string);
begin
  FName := AName;
  FOwner := AOwner;
  FSkinUnitName := ASkinUnitName;
  FState := susUndefined;
end;

procedure TdxSkinsUnitStateListItem.Changed;
begin
  Owner.Changed;
end;

procedure TdxSkinsUnitStateListItem.LoadFromIni(AIniFile: TCustomIniFile; const ASection: string);
begin
  if AIniFile.ValueExists(ASection, SkinUnitName) then
    State := BoolToUnitState[AIniFile.ReadBool(ASection, SkinUnitName, True)]
  else
    State := susUndefined;
end;

procedure TdxSkinsUnitStateListItem.SaveToIni(
  AIniFile: TCustomIniFile; const ASection: string);
begin
  AIniFile.WriteBool(ASection, SkinUnitName, Enabled);
end;

function TdxSkinsUnitStateListItem.GetEnabled: Boolean;
begin
  Result := State <> susDisabled;
end;

procedure TdxSkinsUnitStateListItem.SetState(AState: TdxSkinsUnitsState);
begin
  if AState <> FState then
  begin
    FState := AState;
    Changed;
  end;
end;

{ TdxSkinsCheckListBox }

procedure TdxSkinsCheckListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  Canvas.Font.Assign(Font);
  if (ItemState[Index] = susUndefined) and AllowBoldSelection then
    Canvas.Font.Style := [fsBold];
  if odSelected in State then
    Canvas.Font.Color := clHighlightText;
  inherited DrawItem(Index, Rect, State);
end;

function TdxSkinsCheckListBox.GetItemState(Index: Integer): TdxSkinsUnitsState;
begin
  Result := dxSkinsProjectSettings.UnitStateList.Item[Index].State;
end;

{ TdxSkinsDesignHelperForm }

constructor TdxSkinsDesignHelperForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  dxSkinsProjectSettings.LoadSettings;

  CheckListBox := TdxSkinsCheckListBox.Create(Self);
  CheckListBox.Parent := Self;
  CheckListBox.Hint := sdxSkinCheckListBoxHint;
  CheckListBox.BoundsRect := CheckListBoxHolder.BoundsRect;
  CheckListBox.TabOrder := 1;
  CheckListBox.OnClickCheck := DoCheckListBoxClickCheck;
  TdxSkinsCheckListBox(CheckListBox).AllowBoldSelection := FileExists(dxSkinsProjectSettings.SkinsConfigFileName);

  cbSkinsAutoFilling.Hint := sdxEnableSkinSupportHint;
  cbSkinsAutoFilling.Checked := dxSkinsProjectSettings.Enabled;
  cbSkinsAutoFillingClick(nil);

  cbShowNotifications.Checked := dxSkinsProjectSettings.ShowNotification;
  cbShowNotifications.Hint := sdxSkinNotifyBoxHint;

  cbDefault.Hint := sdxSkinDefaultHint;
  lbNotes.Color := clInfoText;

  FIsDefaultSettings := dxSkinsProjectSettings.IsDefaultSettings;
  PopulateList;
end;

class procedure TdxSkinsDesignHelperForm.Execute;
begin
  with TdxSkinsDesignHelperForm.Create(nil) do
  try
    dxSkinsProjectSettings.Initialize;
    ApplySettings(ShowModal <> mrOk);
    dxSkinsProjectSettings.SaveSettings;
  finally
    Free;
  end;
end;

procedure TdxSkinsDesignHelperForm.ApplySettings(AResetToDefaults: Boolean);
var
  AItem: TdxSkinsUnitStateListItem;
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
  begin
    AItem := dxSkinsProjectSettings.UnitStateList.Item[I];
    if AResetToDefaults then
      AItem.State := BoolToUnitState[AItem.Enabled]
    else
      AItem.State := BoolToUnitState[CheckListBox.Checked[I]];
  end;

  if not AResetToDefaults then
  begin
    dxSkinsProjectSettings.Enabled := cbSkinsAutoFilling.Checked;
    dxSkinsProjectSettings.ShowNotification := cbShowNotifications.Checked;
    case cbDefault.State of
      cbChecked:
        dxSkinsProjectSettings.SaveDefaultSettings;
      cbUnchecked:
        dxSkinsProjectSettings.RemoveDefaultSettings;
    end;
  end;
end;

procedure TdxSkinsDesignHelperForm.DoCheckListBoxClickCheck(Sender: TObject);
begin
  UpdateDefaultCheckBoxState;
end;

function TdxSkinsDesignHelperForm.IsShortCut(var Message: TWMKey): Boolean;
begin
  Result := Message.CharCode = VK_ESCAPE;
  if Result then
    PostMessage(Handle, WM_CLOSE, 0, 0)
  else
    Result := inherited IsShortCut(Message);
end;

procedure TdxSkinsDesignHelperForm.PopulateList;
var
  AItem: TdxSkinsUnitStateListItem;
  I: Integer;
begin
  CheckListBox.Items.BeginUpdate;
  try
    CheckListBox.Items.Clear;
    for I := 0 to dxSkinsProjectSettings.UnitStateList.Count - 1 do
    begin
      AItem := dxSkinsProjectSettings.UnitStateList.Item[I];
      CheckListBox.Items.AddObject(AItem.Name, TObject(AItem.Enabled));
      CheckListBox.Checked[I] := AItem.Enabled;
    end;
  finally
    CheckListBox.Items.EndUpdate;
  end;
  UpdateDefaultCheckBoxState;
end;

function TdxSkinsDesignHelperForm.GetHasChanges: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to CheckListBox.Count - 1 do
  begin
    Result := CheckListBox.Checked[I] <> Boolean(CheckListBox.Items.Objects[I]);
    if Result then
      Break;
  end;
  Result := Result or (dxSkinsProjectSettings.Enabled <> cbSkinsAutoFilling.Checked);
  Result := Result or (dxSkinsProjectSettings.ShowNotification <> cbShowNotifications.Checked);
end;

procedure TdxSkinsDesignHelperForm.UpdateDefaultCheckBoxState;
begin
  if not dxSkinsProjectSettings.HasDefaultSettings then
    cbDefault.State := cbUnchecked
  else
    if not FIsDefaultSettings or HasChanges then
      cbDefault.State := cbGrayed
    else
      cbDefault.State := cbChecked;
end;

procedure TdxSkinsDesignHelperForm.cbSkinsAutoFillingClick(Sender: TObject);
begin
  bSelectAll.Enabled := cbSkinsAutoFilling.Checked;
  bSelectNone.Enabled := cbSkinsAutoFilling.Checked;
  CheckListBox.Enabled := cbSkinsAutoFilling.Checked;
  cbShowNotifications.Enabled := cbSkinsAutoFilling.Checked;
  UpdateDefaultCheckBoxState;
end;

procedure TdxSkinsDesignHelperForm.bSelectAllClick(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    CheckListBox.Checked[I] := TComponent(Sender).Tag = 1;
  UpdateDefaultCheckBoxState;
end;

procedure TdxSkinsDesignHelperForm.pbFramePaint(Sender: TObject);
begin
  pbFrame.Canvas.Pen.Color := clBtnShadow;
  pbFrame.Canvas.Brush.Color := clInfoBk;
  pbFrame.Canvas.Rectangle(pbFrame.ClientRect);
end;

procedure TdxSkinsDesignHelperForm.cbShowNotificationsClick(Sender: TObject);
begin
  UpdateDefaultCheckBoxState
end;

{ TdxSkinsProjectOptionsMenuExpert }

constructor TdxSkinsProjectOptionsMenuExpert.Create;
begin
  FMenuItem := CreateMenuItem(ProjectMenuItem);
end;

destructor TdxSkinsProjectOptionsMenuExpert.Destroy;
begin
  FreeAndNil(FMenuItem);
  inherited Destroy;
end;

function TdxSkinsProjectOptionsMenuExpert.CalcMenuItemPosition(AParent: TMenuItem): Integer;
var
  AItem: TMenuItem;
begin
  AItem := FindMenuItemByName(AParent, 'ProjectOptionsItem');
  if AItem = nil then
    Result := AParent.Count - 1
  else
    Result := AParent.IndexOf(AItem);
end;

function TdxSkinsProjectOptionsMenuExpert.CreateMenuItem(AParent: TMenuItem): TMenuItem;
var
  ABitmap: TBitmap;
begin
  Result := nil;
  if Assigned(AParent) then
  begin
    Result := TMenuItem.Create(nil);
    Result.Caption := sdxSkinsMenuItemCaption;
    Result.OnClick := DoMenuItemClick;

    if AParent.GetImageList <> nil then
    begin
      ABitmap := TBitmap.Create;
      try
        ABitmap.LoadFromResourceName(HInstance, sdxSkinsMenuItemGlyphResName);
        Result.ImageIndex := AParent.GetImageList.AddMasked(ABitmap, clFuchsia)
      finally
        ABitmap.Free;
      end;
    end
    else
      Result.Bitmap.LoadFromResourceName(HInstance, sdxSkinsMenuItemGlyphResName);

    AParent.Insert(CalcMenuItemPosition(AParent), Result);
  end;
end;

procedure TdxSkinsProjectOptionsMenuExpert.DoMenuItemClick(Sender: TObject);
begin
  dxSkinsShowProjectOptionsDialog;
end;

function TdxSkinsProjectOptionsMenuExpert.FindMenuItemByName(
  AParent: TMenuItem; const AName: string): TMenuItem;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to AParent.Count - 1 do
    if SameText(AParent.Items[I].Name, AName) then
    begin
      Result := AParent.Items[I];
      Break;
    end;
end;

function TdxSkinsProjectOptionsMenuExpert.GetProjectMenuItem: TMenuItem;
var
  AServices: INTAServices;
begin
  if Supports(BorlandIDEServices, INTAServices, AServices) then
    Result := FindMenuItemByName(AServices.MainMenu.Items, 'ProjectMenu')
  else
    Result := nil;
end;

{ TdxSkinsDesignHelper }

constructor TdxSkinsDesignHelper.Create;
begin
  FMenuExpert := TdxSkinsProjectOptionsMenuExpert.Create;
  RegisterIDENotifier;
  ActiveProject := dxGetActiveProject;
end;

destructor TdxSkinsDesignHelper.Destroy;
begin
  ActiveProject := nil;
  UnregisterIDENotifier;
  FreeAndNil(FMenuExpert);
  inherited Destroy;
end;

// IOTAIDENotifier
procedure TdxSkinsDesignHelper.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TdxSkinsDesignHelper.BeforeCompile(
  const Project: IOTAProject; var Cancel: Boolean);
begin
end;

procedure TdxSkinsDesignHelper.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
begin
  if NotifyCode = ofnActiveProjectChanged then
    ActiveProject := dxGetActiveProject;
end;

// IOTAModuleNotifier
function TdxSkinsDesignHelper.CheckOverwrite: Boolean;
begin
  Result := True;
end;

procedure TdxSkinsDesignHelper.ModuleRenamed(const NewName: string);
begin
end;

// IOTANotifier
procedure TdxSkinsDesignHelper.AfterSave;
begin
  dxSkinsProjectSettings.SaveSettings;
end;

procedure TdxSkinsDesignHelper.BeforeSave;
begin
end;

procedure TdxSkinsDesignHelper.Destroyed;
begin
  ActiveProject := nil;
end;

procedure TdxSkinsDesignHelper.Modified;
begin
end;

procedure TdxSkinsDesignHelper.SetActiveProject(AProject: IOTAProject);
begin
  if AProject <> FActiveProject then
  begin
    UnregisterModuleNotifier(ActiveProject, FActiveProjectNotifierID);
    FActiveProject := AProject;
    FActiveProjectNotifierID := RegisterModuleNotifier(ActiveProject);
    dxSkinsProjectSettings.ActiveProjectChanged;
    UpdateMenuItemState;
  end;
end;

procedure TdxSkinsDesignHelper.RegisterIDENotifier;
var
  AServices: IOTAServices;
begin
  if Supports(BorlandIDEServices, IOTAServices, AServices) then
    FServicesNotifierID := AServices.AddNotifier(Self)
  else
    FServicesNotifierID := -1;
end;

procedure TdxSkinsDesignHelper.UnregisterIDENotifier;
var
  AServices: IOTAServices;
begin
  if FServicesNotifierID >= 0 then
  begin
    if Supports(BorlandIDEServices, IOTAServices, AServices) then
      AServices.RemoveNotifier(FServicesNotifierID);
    FServicesNotifierID := -1;
  end;
end;

function TdxSkinsDesignHelper.RegisterModuleNotifier(AModule: IOTAModule): Integer;
begin
  if AModule = nil then
    Result := -1
  else
    Result := AModule.AddNotifier(Self);
end;

procedure TdxSkinsDesignHelper.UnregisterModuleNotifier(AModule: IOTAModule; ID: Integer);
begin
  if Assigned(AModule) then
    AModule.RemoveNotifier(ID);
end;

procedure TdxSkinsDesignHelper.UpdateMenuItemState;
begin
  if Assigned(MenuExpert.MenuItem) then
    MenuExpert.MenuItem.Visible := Assigned(ActiveProject);
end;

{ TdxSkinsProjectSettings }

constructor TdxSkinsProjectSettings.Create;
begin
  inherited Create;
  FEnabled := True;
  FShowNotification := True;
  FUnitStateList := TdxSkinsUnitStateList.Create;
  FUnitStateList.OnChanged := DoUnitStateListChanged;
  cxLookAndFeelPaintersManager.AddListener(Self);
  ActiveProjectChanged;
end;

destructor TdxSkinsProjectSettings.Destroy;
begin
  Finalize;
  cxLookAndFeelPaintersManager.RemoveListener(Self);
  FreeAndNil(FUnitStateList);
  inherited Destroy;
end;

function TdxSkinsProjectSettings.CalcIsDefaultSettings: Boolean;

  function CheckValue(AReg: TRegistryIniFile; const AName: string; ATestValue: Boolean): Boolean;
  begin
    Result := AReg.ValueExists(sdxSkinsDefaultRegistryPath, AName) and
      (AReg.ReadBool(sdxSkinsDefaultRegistryPath, AName, ATestValue) = ATestValue);
  end;

var
  AItem: TdxSkinsUnitStateListItem;
  ARegistry: TRegistryIniFile;
  I: Integer;
begin
  ARegistry := TRegistryIniFile.Create('');
  try
    ARegistry.RegIniFile.RootKey := HKEY_CURRENT_USER;
    Result := CheckValue(ARegistry, sdxSkinsProjectState, Enabled);
    if Result then
    begin
      for I := 0 to UnitStateList.Count - 1 do
      begin
        AItem := UnitStateList.Item[I];
        Result := CheckValue(ARegistry, AItem.SkinUnitName, AItem.Enabled);
        if not Result then
          Break;
      end;
    end;
  finally
    ARegistry.Free;
  end;
end;

function TdxSkinsProjectSettings.CheckDefaultsForUndefinedItems: Boolean;
const
  DefaultStatesMap: array[Boolean] of TdxSkinsUnitsState = (susEnabled, susDisabled);
var
  ADefaultState: TdxSkinsUnitsState;
  I: Integer;
begin
  Result := UnitStateList.HasUndefinedItems and not ShowNotification;
  if Result then
  begin
    ADefaultState := DefaultStatesMap[UnitStateList.HasDisabledItems];
    for I := 0 to UnitStateList.Count - 1 do
    begin
      if UnitStateList.Item[I].State = susUndefined then
        UnitStateList.Item[I].State := ADefaultState;
    end;
  end;
end;

function TdxSkinsProjectSettings.CreateRegIniFile: TRegistryIniFile;
begin
  Result := TRegistryIniFile.Create('');
  Result.RegIniFile.RootKey := HKEY_CURRENT_USER;
end;

procedure TdxSkinsProjectSettings.DoUnitStateListChanged(Sender: TObject);
begin
  SettingsChanged;
end;

procedure TdxSkinsProjectSettings.InternalLoadSettings(AConfig: TCustomIniFile; const ASection: string);
begin
  UnitStateList.Reset;
  UnitStateList.LoadSettings(AConfig, ASection);
  Enabled := AConfig.ReadBool(ASection, sdxSkinsProjectState, True);
  ShowNotification := AConfig.ReadBool(ASection, sdxSkinsProjectShowNotifications, True);
end;

procedure TdxSkinsProjectSettings.InternalSaveSettings(AConfig: TCustomIniFile; const ASection: string);
begin
  AConfig.WriteBool(ASection, sdxSkinsProjectShowNotifications, ShowNotification);
  AConfig.WriteBool(ASection, sdxSkinsProjectState, Enabled);
  UnitStateList.SaveSettings(AConfig, ASection);
end;

function TdxSkinsProjectSettings.GetHasDefaultSettings: Boolean;
var
  ARegistry: TRegistry;
begin
  ARegistry := TRegistry.Create;
  try
    ARegistry.RootKey := HKEY_CURRENT_USER;
    Result := ARegistry.KeyExists(sdxSkinsDefaultRegistryPath);
  finally
    ARegistry.Free;
  end;
end;

function TdxSkinsProjectSettings.GetIsCurrentProjectExists: Boolean;
begin
  Result := FileExists(CurrentProjectFileName);
end;

function TdxSkinsProjectSettings.GetIsDefaultSettings: Boolean;
begin
  if FCheckDefaultSettingsNeeded then
  begin
    FIsDefaultSettings := CalcIsDefaultSettings;
    FCheckDefaultSettingsNeeded := False;
  end;
  Result := FIsDefaultSettings;
end;

function TdxSkinsProjectSettings.GetNeedShowConfirmation: Boolean;
begin
  Result := Enabled and UnitStateList.HasUndefinedItems;
end;

function TdxSkinsProjectSettings.GetSkinsConfigFileName: string;
begin
  Result := ChangeFileExt(CurrentProjectFileName, sdxSkinsCfgExt);
end;

procedure TdxSkinsProjectSettings.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    SettingsChanged;
  end;
end;

procedure TdxSkinsProjectSettings.SetShowNotification(AValue: Boolean);
begin
  if AValue <> FShowNotification then
  begin
    FShowNotification := AValue;
    SettingsChanged;
  end;
end;

function TdxSkinsProjectSettings.GetCurrentProjectFileName: string;
begin
  Result := dxGetActiveProjectFileName;
end;

procedure TdxSkinsProjectSettings.LoadDefaultSettings;
var
  AConfig: TCustomIniFile;
begin
  AConfig := CreateRegIniFile;
  try
    InternalLoadSettings(AConfig, sdxSkinsDefaultRegistryPath);
    FCheckDefaultSettingsNeeded := False;
    FIsDefaultSettings := True;
  finally
    AConfig.Free;
  end;
end;

procedure TdxSkinsProjectSettings.LoadSettings;
var
  AConfig: TIniFile;
  AHasDefaultSettings: Boolean;
  ARestoreDefaultsNeeded: Boolean;
begin
  if IsCurrentProjectExists and FileExists(SkinsConfigFileName) then
  begin
    Initialize;
    ARestoreDefaultsNeeded := False;
    AConfig := TIniFile.Create(SkinsConfigFileName);
    try
      AHasDefaultSettings := HasDefaultSettings;
      FIsDefaultSettings := AConfig.ReadBool(sdxSkinsCfgSection, sdxSkinsDefault, False);
      if not (FIsDefaultSettings and AHasDefaultSettings) then
      begin
        InternalLoadSettings(AConfig, sdxSkinsCfgSection);
        ARestoreDefaultsNeeded := FIsDefaultSettings and not AHasDefaultSettings;
      end;
      if CheckDefaultsForUndefinedItems then
        ARestoreDefaultsNeeded := ARestoreDefaultsNeeded or FIsDefaultSettings;
      if ARestoreDefaultsNeeded then
        SaveDefaultSettings;
      FCheckDefaultSettingsNeeded := True;
    finally
      AConfig.Free;
    end;
  end;
end;

procedure TdxSkinsProjectSettings.PainterChanged(APainter: TcxCustomLookAndFeelPainter);
begin
  if not TdxSkinsPackageManager.IsPackageLoading then
  begin
    ReloadUnitsList;
    LoadDefaultSettings;
    LoadSettings;
  end;
end;

procedure TdxSkinsProjectSettings.ReloadUnitsList;
begin
  UnitStateList.Clear;
  dxSkinsEnumSkins(
    procedure (const ASkinName, AUnitName: string)
    begin
      UnitStateList.AddItem(AUnitName, ASkinName);
    end);
end;

procedure TdxSkinsProjectSettings.RemoveDefaultSettings;
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := CreateRegIniFile;
  try
    ARegIniFile.EraseSection(sdxSkinsDefaultRegistryPath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TdxSkinsProjectSettings.SaveDefaultSettings;
var
  ARegIniFile: TRegistryIniFile;
begin
  ARegIniFile := CreateRegIniFile;
  try
    FIsDefaultSettings := True;
    FCheckDefaultSettingsNeeded := False;
    InternalSaveSettings(ARegIniFile, sdxSkinsDefaultRegistryPath);
  finally
    ARegIniFile.Free;
  end;
end;

procedure TdxSkinsProjectSettings.SaveSettings;
var
  AConfig: TIniFile;
begin
  if Initialized and IsCurrentProjectExists then
  begin
    AConfig := TIniFile.Create(SkinsConfigFileName);
    try
      AConfig.WriteBool(sdxSkinsCfgSection, sdxSkinsDefault, IsDefaultSettings);
      InternalSaveSettings(AConfig, sdxSkinsCfgSection);
    finally
      AConfig.Free;
    end;
  end;
end;

procedure TdxSkinsProjectSettings.ActiveProjectChanged;
begin
  ReloadUnitsList;
  LoadDefaultSettings;
  LoadSettings;
end;

procedure TdxSkinsProjectSettings.SettingsChanged;
begin
  FCheckDefaultSettingsNeeded := True;
end;

procedure TdxSkinsProjectSettings.UpdateActiveProjectSettings;
begin
  if NeedShowConfirmation then
    dxSkinsShowProjectOptionsDialog;
end;

procedure TdxSkinsProjectSettings.Finalize;
begin
  FInitialized := False;
end;

procedure TdxSkinsProjectSettings.Initialize;
begin
  FInitialized := True;
end;

initialization
  SkinsDesignHelper := TdxSkinsDesignHelper.Create;

finalization
  FreeAndNil(SkinsDesignHelper);
  FreeAndNil(SkinsProjectSettings);

end.
