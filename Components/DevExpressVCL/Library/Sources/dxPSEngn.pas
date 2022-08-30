{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSEngn;

interface

{$I cxVer.inc}

uses
  Types, Classes, Graphics, SysUtils, IniFiles, cxControls, cxLookAndFeels, dxPSESys,
  dxPSGlbl, dxPSSngltn, cxClasses;

type
  TdxPSStorageType = (dxstNone, dxstRegistry, dxstIniFile);

  { TdxPSOptionsStoring }

  TdxPSOptionsStoring = class(TPersistent)
  private
    FIniFileName: string;
    FOnChanged: TNotifyEvent;
    FRegistryPath: string;
    FSaveFormsPosition: Boolean;
    FStorageType: TdxPSStorageType;
    procedure SetIniFileName(const AValue: string);
    procedure SetRegistryPath(const AValue: string);
    procedure SetSaveFormsPosition(AValue: Boolean);
    procedure SetStorageType(AValue: TdxPSStorageType);
  protected
    function CanStoreToIniFile: Boolean;
    function CanStoreToRegistry: Boolean;
    procedure Changed; virtual;
    //
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property IniFileName: string read FIniFileName write SetIniFileName;
    property RegistryPath: string read FRegistryPath write SetRegistryPath;
    property SaveFormsPosition: Boolean read FSaveFormsPosition write SetSaveFormsPosition default True;
    property StorageType: TdxPSStorageType read FStorageType write SetStorageType default dxstNone;
  end;

  { TdxPSEngine }

  TdxPSEngine = class(TBasedxPSSingleton)
  strict private
    FDesignTimeRegistryPath: string;
    FDialogsLookAndFeel: TcxLookAndFeel;
    FHelpFile: string;

    function GetAppName: string;
    function GetIsNativeStyle: Boolean;
    function GetIsSkinsStyle: Boolean;
    function GetIsStandardStyle: Boolean;
    function GetPreviewDialogStyle: TdxPSPreviewDialogStyle;
    function GetRealRegistryPath: string;
    function GetRegistryPath: string;
    function GetSaveFormsPosition: Boolean;
    function GetThemesEnabled: Boolean;
    procedure SetDialogsLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetPreviewDialogStyle(const AValue: TdxPSPreviewDialogStyle);
    procedure SetRegistryPath(const AValue: string);
    procedure SetSaveFormsPosition(AValue: Boolean);
  protected
    procedure FinalizeInstance; override;
    procedure InitializeInstance; override;
  public
    class function Instance: TdxPSEngine; reintroduce; overload;

    property AppName: string read GetAppName;
    property DesignTimeRegistryPath: string read FDesignTimeRegistryPath;
    property DialogsLookAndFeel: TcxLookAndFeel read FDialogsLookAndFeel write SetDialogsLookAndFeel;
    property HelpFile: string read FHelpFile write FHelpFile;
    property IsNativeStyle: Boolean read GetIsNativeStyle;
    property IsSkinsStyle: Boolean read GetIsSkinsStyle;
    property IsStandardStyle: Boolean read GetIsStandardStyle;
    property PreviewDialogStyle: TdxPSPreviewDialogStyle read GetPreviewDialogStyle write SetPreviewDialogStyle;
    property RealRegistryPath: string read GetRealRegistryPath;
    property RegistryPath: string read GetRegistryPath write SetRegistryPath;
    property SaveFormsPosition: Boolean read GetSaveFormsPosition write SetSaveFormsPosition;
    property ThemesEnabled: Boolean read GetThemesEnabled;
  end;

  { TdxPSEngineController }

  TdxPSEngineController = class(TcxCustomComponent, IdxSkinSupport)
  strict private
    FDialogsLookAndFeel: TcxLookAndFeel;
    FHelpFile: string;
    FOptionsStoring: TdxPSOptionsStoring;
    FPreviewDialogStyle: TdxPSPreviewDialogStyle;

    function GetActive: Boolean;
    function GetPreviewDialogStyle: TdxPSPreviewDialogStyle;
    function IsDesigning: Boolean;
    function IsPreviewDialogStyleStored: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetDialogsLookAndFeel(Value: TcxLookAndFeel);
    procedure SetHelpFile(const Value: string);
    procedure SetOptionsStoring(AValue: TdxPSOptionsStoring);
    procedure SetPreviewDialogStyle(const AValue: TdxPSPreviewDialogStyle);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
    procedure StoringOptionsChanged(Sender: TObject);
    procedure UpdateEngineParams;
    //
    procedure ReadLookAndFeelProperty(Reader: TReader);
    procedure ReadRegistryPath(Reader: TReader);
    procedure ReadSaveFormsPosition(Reader: TReader);
    procedure ReadUseNativeLookAndFeelProperty(Reader: TReader);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Activate;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property DialogsLookAndFeel: TcxLookAndFeel read FDialogsLookAndFeel write SetDialogsLookAndFeel;
    property HelpFile: string read FHelpFile write SetHelpFile;
    property OptionsStoring: TdxPSOptionsStoring read FOptionsStoring write SetOptionsStoring;
    property PreviewDialogStyle: TdxPSPreviewDialogStyle read GetPreviewDialogStyle write SetPreviewDialogStyle stored IsPreviewDialogStyleStored;
  end;

  { TdxPSStoringManager }

  TdxPSStoringManager = class(TObject)
  private
    FIniFile: TCustomIniFile;
    FLockCount: Integer;
    FOptionsStoring: TdxPSOptionsStoring;
    procedure SetOptionsStoring(AValue: TdxPSOptionsStoring);
  protected
    function CreateIniFile: TCustomIniFile;
    function GetRealIniFileName: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function BeginStoring(var AIniFile: TCustomIniFile): Boolean;
    procedure EndStoring(var AIniFile: TCustomIniFile);
    //
    property OptionsStoring: TdxPSOptionsStoring read FOptionsStoring write SetOptionsStoring;
  end;

function dxPSStoringManager: TdxPSStoringManager;
function dxPSEngine: TdxPSEngine;
implementation

uses
  Forms, Themes, dxPSRes, dxPSUtl, dxPSCore, Registry;

type

  { TdxPSEngineControllerList }

  TdxPSEngineControllerList = class(TObject)
  private
    FItems: TList;
    function GetActiveController: TdxPSEngineController;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxPSEngineController;
    procedure SetActiveController(Value: TdxPSEngineController);
  protected
    procedure ActiveControllerChanged;
  public
    destructor Destroy; override;

    function IndexOf(Value: TdxPSEngineController): Integer;
    procedure Add(Value: TdxPSEngineController);
    procedure Remove(Value: TdxPSEngineController);

    property ActiveController: TdxPSEngineController read GetActiveController write SetActiveController;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxPSEngineController read GetItem; default;
  end;

var
  FEngineControllerList: TdxPSEngineControllerList = nil;
  FConfigManager: TdxPSStoringManager = nil;

function dxPSStoringManager: TdxPSStoringManager;
begin
  if FConfigManager = nil then
    FConfigManager := TdxPSStoringManager.Create;
  Result := FConfigManager;
end;

function dxPSEngine: TdxPSEngine;
begin
  Result := TdxPSEngine.Instance;
end;

function dxPSEngineControllerList: TdxPSEngineControllerList;
begin
  if FEngineControllerList = nil then
    FEngineControllerList := TdxPSEngineControllerList.Create;
  Result := FEngineControllerList;
end;

{ TdxPSEngine }

class function TdxPSEngine.Instance: TdxPSEngine;
begin
  Result := inherited Instance as TdxPSEngine;
end;

procedure TdxPSEngine.InitializeInstance;
begin
  inherited InitializeInstance;
  FDialogsLookAndFeel := TcxLookAndFeel.Create(nil);
  FDialogsLookAndFeel.MasterLookAndFeel := RootLookAndFeel;
  FDesignTimeRegistryPath := dxPSGlbl.sdxPSRegPathDesignTime;
end;

procedure TdxPSEngine.FinalizeInstance;
begin
  FreeAndNil(FDialogsLookAndFeel);
  inherited FinalizeInstance;
end;

function TdxPSEngine.GetIsNativeStyle: Boolean;
begin
  Result := DialogsLookAndFeel.NativeStyle;
end;

function TdxPSEngine.GetIsSkinsStyle: Boolean;
begin
  Result := Assigned(DialogsLookAndFeel.SkinPainter);
end;

function TdxPSEngine.GetIsStandardStyle: Boolean;
begin
  Result := DialogsLookAndFeel.Kind = lfStandard;
end;

function TdxPSEngine.GetAppName: string;
begin
  Result := ChangeFileExt(ExtractFileName(Application.ExeName), '');
end;

function TdxPSEngine.GetPreviewDialogStyle: TdxPSPreviewDialogStyle;
begin
  Result := dxPSPreviewDialogManager.CurrentPreviewDialogStyle;
end;

function TdxPSEngine.GetRealRegistryPath: string;
begin
  if IsDesignTime then
    Result := DesignTimeRegistryPath
  else
    Result := RegistryPath;
end;

function TdxPSEngine.GetRegistryPath: string;
begin
  Result := dxPSStoringManager.OptionsStoring.RegistryPath;
end;

function TdxPSEngine.GetSaveFormsPosition: Boolean;
begin
  Result := dxPSStoringManager.OptionsStoring.SaveFormsPosition;
end;

function TdxPSEngine.GetThemesEnabled: Boolean;
begin
{$IFDEF DELPHI7}
  Result := cxIsVCLThemesEnabled;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TdxPSEngine.SetDialogsLookAndFeel(AValue: TcxLookAndFeel);
begin
  FDialogsLookAndFeel.Assign(AValue);
end;

procedure TdxPSEngine.SetPreviewDialogStyle(const AValue: TdxPSPreviewDialogStyle);
begin
  dxPSPreviewDialogManager.CurrentPreviewDialogStyle := AValue;
end;

procedure TdxPSEngine.SetRegistryPath(const AValue: string);
begin
  dxPSStoringManager.OptionsStoring.RegistryPath := AValue;
end;

procedure TdxPSEngine.SetSaveFormsPosition(AValue: Boolean);
begin
  dxPSStoringManager.OptionsStoring.SaveFormsPosition := AValue;
end;

{ TdxPSEngineControllerList }

destructor TdxPSEngineControllerList.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxPSEngineControllerList.ActiveControllerChanged;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].UpdateEngineParams;
end;

procedure TdxPSEngineControllerList.Add(Value: TdxPSEngineController);
begin
  if FItems = nil then
    FItems := TList.Create;
  FItems.Add(Value);
  ActiveControllerChanged;
end;

function TdxPSEngineControllerList.IndexOf(Value: TdxPSEngineController): Integer;
begin
  if FItems = nil then
    Result := -1
  else
    Result := FItems.IndexOf(Value);
end;

procedure TdxPSEngineControllerList.Remove(Value: TdxPSEngineController);
begin
  if IndexOf(Value) > -1 then
  begin
    FItems.Remove(Value);
    if Count = 0 then
      FreeAndNil(FItems);
    ActiveControllerChanged;
  end;
end;

function TdxPSEngineControllerList.GetActiveController: TdxPSEngineController;
begin
  if FItems = nil then
    Result := nil
  else
    Result := Items[Count - 1];
end;

function TdxPSEngineControllerList.GetCount: Integer;
begin
  if FItems = nil then
    Result := 0
  else
    Result := FItems.Count;
end;

function TdxPSEngineControllerList.GetItem(Index: Integer): TdxPSEngineController;
begin
  Result := TdxPSEngineController(FItems.Items[Index]);
end;

procedure TdxPSEngineControllerList.SetActiveController(Value: TdxPSEngineController);
begin
  if (ActiveController <> Value) and (IndexOf(Value) > -1) then
  begin
    FItems.Remove(Value);
    FItems.Add(Value);
    ActiveControllerChanged;
  end;
end;

{ TdxPSEngineController }

constructor TdxPSEngineController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionsStoring := TdxPSOptionsStoring.Create;
  FOptionsStoring.OnChanged := StoringOptionsChanged;
  FDialogsLookAndFeel := TcxLookAndFeel.Create(Self);
  FDialogsLookAndFeel.MasterLookAndFeel := RootLookAndFeel;
  FDialogsLookAndFeel.OnChanged := LookAndFeelChanged;
  dxPSEngineControllerList.Add(Self);
end;

destructor TdxPSEngineController.Destroy;
begin
  dxPSEngineControllerList.Remove(Self);
  FreeAndNil(FDialogsLookAndFeel);
  FreeAndNil(FOptionsStoring);
  inherited Destroy;
end;

procedure TdxPSEngineController.Activate;
begin
  dxPSEngineControllerList.ActiveController := Self;
end;

procedure TdxPSEngineController.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('RegistryPath', ReadRegistryPath, nil, False);
  Filer.DefineProperty('LookAndFeel', ReadLookAndFeelProperty, nil, False);
  Filer.DefineProperty('SaveFormsPosition', ReadSaveFormsPosition, nil, False);
  Filer.DefineProperty('UseNativeLookAndFeel', ReadUseNativeLookAndFeelProperty, nil, False);
end;

procedure TdxPSEngineController.Loaded;
begin
  inherited Loaded;
  if not IsDesigning then
    UpdateEngineParams;
end;

procedure TdxPSEngineController.LookAndFeelChanged(
  Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  UpdateEngineParams;
end;

function TdxPSEngineController.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TdxPSEngineController.IsPreviewDialogStyleStored: Boolean;
begin
  Result := FPreviewDialogStyle <> '';
end;

procedure TdxPSEngineController.ReadLookAndFeelProperty(Reader: TReader);
var
  AIdentStr: string;
begin
  AIdentStr := Reader.ReadIdent;
  DialogsLookAndFeel.AssignedValues := [];
  if AIdentStr = 'pslfFlat' then
    DialogsLookAndFeel.Kind := lfFlat;
  if AIdentStr = 'pslfOffice11' then
    DialogsLookAndFeel.Kind := lfOffice11;
  if AIdentStr = 'pslfStandard' then
    DialogsLookAndFeel.Kind := lfStandard;
  if AIdentStr = 'pslfWinXP' then
    DialogsLookAndFeel.NativeStyle := True;
end;

procedure TdxPSEngineController.ReadRegistryPath(Reader: TReader);
begin
  OptionsStoring.RegistryPath := Reader.ReadString;
  if OptionsStoring.RegistryPath <> '' then
    OptionsStoring.StorageType := dxstRegistry;
end;

procedure TdxPSEngineController.ReadSaveFormsPosition(Reader: TReader);
begin
  OptionsStoring.SaveFormsPosition := Reader.ReadBoolean;
end;

procedure TdxPSEngineController.ReadUseNativeLookAndFeelProperty(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TdxPSEngineController.StoringOptionsChanged(Sender: TObject);
begin
  UpdateEngineParams;
end;

procedure TdxPSEngineController.UpdateEngineParams;
begin
  if Active then
  begin
    dxPSEngine.HelpFile := HelpFile;
    dxPSEngine.DialogsLookAndFeel := DialogsLookAndFeel;
    dxPSPreviewDialogManager.CurrentPreviewDialogStyle := FPreviewDialogStyle;
    dxPSStoringManager.OptionsStoring.Assign(OptionsStoring);
  end;
end;

function TdxPSEngineController.GetActive: Boolean;
begin
  Result := dxPSEngineControllerList.ActiveController = Self;
end;

function TdxPSEngineController.GetPreviewDialogStyle: TdxPSPreviewDialogStyle;
begin
  Result := FPreviewDialogStyle;
  if Result = '' then
    Result := dxPSEngine.PreviewDialogStyle;
end;

procedure TdxPSEngineController.SetActive(AValue: Boolean);
begin
  if AValue then
    Activate;
end;

procedure TdxPSEngineController.SetPreviewDialogStyle(const AValue: TdxPSPreviewDialogStyle);
begin
  FPreviewDialogStyle := AValue;
  if Active then
    dxPSEngine.PreviewDialogStyle := FPreviewDialogStyle;
end;

procedure TdxPSEngineController.SetHelpFile(const Value: string);
begin
  FHelpFile := Value;
  UpdateEngineParams;
end;

procedure TdxPSEngineController.SetDialogsLookAndFeel(Value: TcxLookAndFeel);
begin
  FDialogsLookAndFeel.Assign(Value);
end;

procedure TdxPSEngineController.SetOptionsStoring(AValue: TdxPSOptionsStoring);
begin
  FOptionsStoring.Assign(AValue);
end;

{ TdxPSStoringManager }

constructor TdxPSStoringManager.Create;
begin
  inherited Create;
  FOptionsStoring := TdxPSOptionsStoring.Create;
end;

destructor TdxPSStoringManager.Destroy;
begin
  FreeAndNil(FOptionsStoring);
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

function TdxPSStoringManager.BeginStoring(var AIniFile: TCustomIniFile): Boolean;
begin
  if FLockCount = 0 then
  try
    FIniFile := CreateIniFile;
  except
    FIniFile := nil;
  end;

  Result := FIniFile <> nil;
  if Result then
  begin
    AIniFile := FIniFile;
    Inc(FLockCount);
  end;
end;

function TdxPSStoringManager.CreateIniFile: TCustomIniFile;
begin
  if OptionsStoring.CanStoreToRegistry then
    Result := TRegistryIniFile.Create(OptionsStoring.RegistryPath)
  else
    if OptionsStoring.CanStoreToIniFile then
      Result := TMemIniFile.Create(GetRealIniFileName)
    else
      Result := nil;
end;

procedure TdxPSStoringManager.EndStoring(var AIniFile: TCustomIniFile);
begin
  if AIniFile = FIniFile then
  begin
    Dec(FLockCount);
    if FLockCount = 0 then
    begin
      try
        FIniFile.UpdateFile;
      except
        // do nothing
      end;
      FreeAndNil(FIniFile);
      AIniFile := nil;
    end;
  end;
end;

function TdxPSStoringManager.GetRealIniFileName: string;
begin
  Result := OptionsStoring.IniFileName;
  if ExtractFileDrive(Result) = '' then
    Result := ExtractFilePath(Application.ExeName) + Result;
end;

procedure TdxPSStoringManager.SetOptionsStoring(AValue: TdxPSOptionsStoring);
begin
  FOptionsStoring.Assign(AValue);
end;

{ TdxPSOptionsStoring }

constructor TdxPSOptionsStoring.Create;
begin
  inherited Create;
  FSaveFormsPosition := True;
end;

procedure TdxPSOptionsStoring.Assign(Source: TPersistent);
begin
  if Source is TdxPSOptionsStoring then
  begin
    FIniFileName := TdxPSOptionsStoring(Source).IniFileName;
    FStorageType := TdxPSOptionsStoring(Source).FStorageType;
    FRegistryPath := TdxPSOptionsStoring(Source).RegistryPath;
    FSaveFormsPosition := TdxPSOptionsStoring(Source).FSaveFormsPosition;
    Changed;
  end;
end;

function TdxPSOptionsStoring.CanStoreToIniFile: Boolean;
begin
  Result := (StorageType = dxstIniFile) and (IniFileName <> '');
end;

function TdxPSOptionsStoring.CanStoreToRegistry: Boolean;
begin
  Result := (StorageType = dxstRegistry) and (RegistryPath <> '');
end;

procedure TdxPSOptionsStoring.Changed;
begin
  if Assigned(OnChanged) then OnChanged(Self);
end;

procedure TdxPSOptionsStoring.SetIniFileName(const AValue: string);
begin
  if AValue <> FIniFileName then
  begin
    FIniFileName := AValue;
    if AValue <> '' then
      FStorageType := dxstIniFile;
    Changed;
  end;
end;

procedure TdxPSOptionsStoring.SetRegistryPath(const AValue: string);
begin
  if AValue <> FRegistryPath then
  begin
    FRegistryPath := AValue;
    if AValue <> '' then
      FStorageType := dxstRegistry;
    Changed;
  end;
end;

procedure TdxPSOptionsStoring.SetSaveFormsPosition(AValue: Boolean);
begin
  if AValue <> FSaveFormsPosition then
  begin
    FSaveFormsPosition := AValue;
    Changed;
  end;
end;

procedure TdxPSOptionsStoring.SetStorageType(AValue: TdxPSStorageType);
begin
  if AValue <> FStorageType then
  begin
    FStorageType := AValue;
    Changed;
  end;
end;

initialization

finalization
  FreeAndNil(FEngineControllerList);
  FreeAndNil(FConfigManager);
end.
