{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressCore Library                                      }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL           }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxCoreReg;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, Menus, TypInfo, Windows, VCLEditors, Graphics, Forms, Controls,
  DesignIntf, DesignEditors, DesignConst, DesignMenus, dxCore, Registry, Generics.Defaults, Generics.Collections;

const
  dxCoreLibraryProductPage = 'DevExpress';
  dxCompanyName = 'Developer Express Inc.';
  dxCompanyURL = 'www.devexpress.com';

type
  TDesignMenuItem = DesignMenus.IMenuItem;

  { TdxComponentEditor }

  TdxComponentEditor = class(TComponentEditor)
  private
    function GetBaseVerbCount: Integer;

    procedure LinkToClick(Sender: TObject);
    procedure CheckObjectsForLinkTo(const AObjectName: string);
    procedure UpdateLinkableList;
  protected
    FLinkableObjects: TStringList;

    procedure DoLinkTo(AObject: TObject); virtual;
    function GetLinkToItemCaption: string; virtual;
    function GetLinkToTypeClass: TClass; virtual;
    function IsLinkable: Boolean; virtual;
    function IsObjectLinkable(AObject: TObject): Boolean; virtual;
    procedure PrepareLinkableSubItem(const AItem: TDesignMenuItem); virtual;

    function GetProductVersion: string; virtual;
    function GetProductName: string; virtual;
    procedure InternalExecuteVerb(AIndex: Integer); virtual;
    function InternalGetVerb(AIndex: Integer): string; virtual;
    function InternalGetVerbCount: Integer; virtual;

  public
    constructor Create(AComponent: TComponent; ADesigner: IDesigner); override;
    destructor Destroy; override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;

    procedure PrepareItem(Index: Integer; const AItem: TDesignMenuItem); override;
  end;

  TdxDefaultBooleanPropertyEditor = class(TEnumProperty,
    ICustomPropertyDrawing,
    ICustomPropertyDrawing80,
    ICustomPropertyMessage
    )
  protected
    function GetCheckBoxBounds(const ARect: TRect): TRect;
    function GetDefaultValue: Boolean; virtual; abstract;
    function GetDisplayText(Value: TdxDefaultBoolean): string;
    procedure PaintCheckbox(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
  public
    procedure PropDrawName(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    procedure PropDrawValue(ACanvas: TCanvas; const ARect: TRect; ASelected: Boolean);
    function PropDrawNameRect(const ARect: TRect): TRect;
    function PropDrawValueRect(const ARect: TRect): TRect;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer;
      InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
    procedure HintShow(var HintInfo: THintInfo; InNameRect: Boolean;
      const ItemRect: TRect; var Handled: Boolean);
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure Register;
procedure dxHideClassesFromStructureView(const AClasses: array of TClass);

implementation

{$R dxSplash.res}

uses
  StrUtils, ToolsApi, Themes, StdCtrls, ShellApi, TreeIntf;


type
{ TdxRegisterInfo }

  TdxRegisterInfo = class
  public
    class function GetSplashBitmap: HBITMAP;
    class function GetIsUnRegistered: Boolean;
    class function GetLicenseStatus: string;
  end;

{ TdxRegisterInfo }

class function TdxRegisterInfo.GetIsUnRegistered: Boolean;
begin
  Result := False;
end;

class function TdxRegisterInfo.GetLicenseStatus: string;
begin
  Result := 'Registered';
end;

class function TdxRegisterInfo.GetSplashBitmap: HBITMAP;
begin
  Result := LoadBitmap(HInstance, 'DXSPLASH');
end;

type
  { TdxHiddenSprig }

  TdxHiddenSprig = class(TAbstractSprig)
  public
    function Hidden: Boolean; override;
  end;

{ TdxHiddenSprig }

function TdxHiddenSprig.Hidden: Boolean;
begin
  Result := True;
end;

procedure dxHideClassesFromStructureView(const AClasses: array of TClass);
var
  I: Integer;
begin
  for I := Low(AClasses) to High(AClasses) do
    RegisterSprigType(AClasses[I], TdxHiddenSprig);
end;

{ TdxComponentEditor }

constructor TdxComponentEditor.Create(AComponent: TComponent; ADesigner: IDesigner);
begin
  inherited Create(AComponent, ADesigner);
  FLinkableObjects := TStringList.Create;
  FLinkableObjects.Sorted := True;
end;

destructor TdxComponentEditor.Destroy;
begin
  FreeAndNil(FLinkableObjects);
  inherited Destroy;
end;

function TdxComponentEditor.GetVerb(Index: Integer): string;
begin
  if Index < InternalGetVerbCount then
    Result := InternalGetVerb(Index)
  else
  begin
    Index := Index - InternalGetVerbCount;
    if FLinkableObjects.Count = 0 then
      Inc(Index);
    case Index of
      0: Result := GetLinkToItemCaption;
      1: Result := '-';
      2: Result := GetProductName + ' ' + GetProductVersion;
      3: Result := dxCompanyName;
      4: Result := dxCompanyURL;
    end;
  end;
end;

function TdxComponentEditor.GetVerbCount: Integer;
begin
  if IsLinkable then
    UpdateLinkableList;

  Result := GetBaseVerbCount + InternalGetVerbCount;
end;

procedure TdxComponentEditor.ExecuteVerb(Index: Integer);
begin
  if Index < InternalGetVerbCount then
    InternalExecuteVerb(Index)
  else
  begin
    Dec(Index, InternalGetVerbCount);
    if FLinkableObjects.Count = 0 then
      Inc(Index);
    case Index of
      4: dxShellExecute('http://' + dxCompanyURL, SW_SHOWMAXIMIZED);
    end;
  end;
end;

procedure TdxComponentEditor.PrepareItem(Index: Integer; const AItem: TDesignMenuItem);
var
  I: Integer;
begin
  inherited;
  if (FLinkableObjects.Count > 0) and (Index = InternalGetVerbCount) then
  begin
    for I := 0 to FLinkableObjects.Count - 1 do
      PrepareLinkableSubItem(AItem.AddItem(FLinkableObjects[I], 0, False, True, LinkToClick));
  end;
end;

procedure TdxComponentEditor.DoLinkTo(AObject: TObject);
begin
end;

function TdxComponentEditor.GetLinkToItemCaption: string;
begin
  Result := 'Assign From';
end;

function TdxComponentEditor.GetLinkToTypeClass: TClass;
begin
  Result := nil;
end;

function TdxComponentEditor.IsLinkable: Boolean;
begin
  Result := False;
end;

function TdxComponentEditor.IsObjectLinkable(AObject: TObject): Boolean;
begin
  Result := AObject <> Component;
end;

procedure TdxComponentEditor.PrepareLinkableSubItem(const AItem: TDesignMenuItem);
begin
// do nothing
end;

function TdxComponentEditor.GetProductVersion: string;
begin
  Result := dxGetBuildNumberAsString;
end;

function TdxComponentEditor.GetProductName: string;
begin
  Result := '';
end;

procedure TdxComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
// do nothing
end;

function TdxComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := '';
end;

function TdxComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 0;
end;

function TdxComponentEditor.GetBaseVerbCount: Integer;
begin
  Result := 4;
  if FLinkableObjects.Count > 0 then
    Inc(Result);
end;

procedure TdxComponentEditor.LinkToClick(Sender: TObject);
begin
  DoLinkTo(FLinkableObjects.Objects[((Sender as TMenuItem).MenuIndex)]);
end;

procedure TdxComponentEditor.CheckObjectsForLinkTo(const AObjectName: string);
var
  AObject: TObject;
begin
  AObject := Designer.GetComponent(AObjectName);
  if IsObjectLinkable(AObject) then
    FLinkableObjects.AddObject(AObjectName, AObject);
end;

procedure TdxComponentEditor.UpdateLinkableList;
begin
  FLinkableObjects.Clear;
  Designer.GetComponentNames(GetTypeData(GetLinkToTypeClass.ClassInfo), CheckObjectsForLinkTo);
end;

procedure RegisterSplashItem;
begin
  SplashScreenServices.AddPluginBitmap(Format('DevExpress VCL %s', [dxGetBuildNumberAsString]), TdxRegisterInfo.GetSplashBitmap,
    TdxRegisterInfo.GetIsUnRegistered, TdxRegisterInfo.GetLicenseStatus);
end;

procedure DrawCheckbox(ACanvas: TCanvas; const ARect: TRect; AState: TCheckBoxState;
  AEnabled: Boolean = True);
const
  ThemeStyles : array[TCheckBoxState] of array[Boolean] of TThemedButton = (
    (tbCheckBoxUncheckedDisabled, tbCheckBoxUnCheckedNormal),
    (tbCheckBoxCheckedDisabled, tbCheckBoxCheckedNormal),
    (tbCheckBoxMixedDisabled, tbCheckBoxMixedNormal)
  );
  UnThemedStyles : array[TCheckBoxState] of array[Boolean] of Cardinal = (
    (DFCS_BUTTONCHECK or DFCS_INACTIVE, DFCS_BUTTONCHECK),
    (DFCS_CHECKED or DFCS_INACTIVE, DFCS_CHECKED),
    (DFCS_BUTTON3STATE or DFCS_INACTIVE, DFCS_CHECKED or DFCS_INACTIVE{DFCS_BUTTON3STATE})
  );
begin
{$IFDEF DELPHI16}
  if StyleServices.Enabled then
    StyleServices.DrawElement(ACanvas.Handle,
      StyleServices.GetElementDetails(ThemeStyles[AState][AEnabled]), ARect)
{$ELSE}
  if ThemeServices.ThemesEnabled then
    ThemeServices.DrawElement(ACanvas.Handle,
      ThemeServices.GetElementDetails(ThemeStyles[AState][AEnabled]), ARect)
{$ENDIF}
  else
    DrawFrameControl(ACanvas.Handle, ARect,
      DFC_BUTTON, UnThemedStyles[AState][AEnabled]);
end;

function TdxDefaultBooleanPropertyEditor.GetCheckBoxBounds(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Right + 2, ARect.Top, ARect.Right + ARect.Bottom - ARect.Top + 2, ARect.Bottom);
end;

function TdxDefaultBooleanPropertyEditor.GetDisplayText(Value: TdxDefaultBoolean): string;
begin
  case Value of
    bFalse:
      Result := BoolToStr(False, True);
    bTrue:
      Result := BoolToStr(True, True);
  else
    Result := 'Default (' + BoolToStr(GetDefaultValue, True) + ')';
  end;
end;

function TdxDefaultBooleanPropertyEditor.GetValue: string;
begin
  Result := GetDisplayText(TdxDefaultBoolean(GetOrdValue));
end;

procedure TdxDefaultBooleanPropertyEditor.GetValues(Proc: TGetStrProc);
begin
  Proc(GetDisplayText(bDefault));
  Proc(GetDisplayText(bTrue));
  Proc(GetDisplayText(bFalse));
end;

procedure TdxDefaultBooleanPropertyEditor.SetValue(const Value: string);
begin
  if SameText(Value, GetDisplayText(bTrue)) then
    SetOrdValue(Ord(bTrue))
  else
    if SameText(Value, GetDisplayText(bFalse)) then
      SetOrdValue(Ord(bFalse))
    else
      SetOrdValue(Ord(bDefault));
end;

procedure TdxDefaultBooleanPropertyEditor.PropDrawName(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TdxDefaultBooleanPropertyEditor.PropDrawValue(ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
begin
  PaintCheckbox(ACanvas, ARect, ASelected);
end;

function TdxDefaultBooleanPropertyEditor.PropDrawNameRect(const ARect: TRect): TRect;
begin
  Result := ARect;
end;

function TdxDefaultBooleanPropertyEditor.PropDrawValueRect(const ARect: TRect): TRect;
begin
  Result := Rect(ARect.Left, ARect.Top, (ARect.Bottom - ARect.Top) + ARect.Left, ARect.Bottom);
end;

procedure TdxDefaultBooleanPropertyEditor.HintShow(var HintInfo: THintInfo;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TdxDefaultBooleanPropertyEditor.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer; InNameRect: Boolean; const ItemRect: TRect;
  var Handled: Boolean);
begin
  Handled := False;
end;

procedure TdxDefaultBooleanPropertyEditor.MouseMove(Shift: TShiftState; X, Y: Integer;
  InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
begin
  Handled := False;
end;

procedure TdxDefaultBooleanPropertyEditor.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer; InNameRect: Boolean; const ItemRect: TRect; var Handled: Boolean);
const
  NextValue: array[TdxDefaultBoolean] of TdxDefaultBoolean = (bDefault, bFalse, bTrue);
begin
  Handled := False;
  if paReadOnly in GetAttributes then Exit;
  if PtInRect(GetCheckBoxBounds(ItemRect), Point(x,y)) then
  begin
    SetOrdValue(Ord(NextValue[TdxDefaultBoolean(GetOrdValue)]));
    Handled := True;
  end;
end;

procedure TdxDefaultBooleanPropertyEditor.PaintCheckbox(ACanvas: TCanvas;
  const ARect: TRect; ASelected: Boolean);
begin
  if not AllEqual then
    DrawCheckbox(ACanvas, ARect, cbGrayed)
  else
  begin
    case TdxDefaultBoolean(GetOrdValue) of
      bFalse:
        DrawCheckbox(ACanvas, ARect, cbUnchecked);
      bTrue:
        DrawCheckbox(ACanvas, ARect, cbChecked);
    else
      DrawCheckbox(ACanvas, ARect, cbGrayed);
    end;
  end;
  if ASelected then
    ExcludeClipRect(ACanvas.Handle, ARect.Left, ARect.Top, ARect.Right + 2, ARect.Bottom);
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterSplashItem;
end;

{$IFDEF DELPHIXE8}
type
  TdxIDEMenuHelpItemClickHandler = class
  private
    FItem: TMenuItem;
    FHelpFilePath: string;
    procedure ItemClick(Sender: TObject);
  public
    constructor Create(AItem: TMenuItem; const APath: string);
  end;

  TdxIDEMenuHelpLoader = class
  private
    FHandlers: TObjectList<TdxIDEMenuHelpItemClickHandler>;
    FDXRootHelpMenuItem: TMenuItem;
  public
    constructor Create;
    destructor Destroy; override;
  end;

{ TdxIDEMenuHelpLoader }

constructor TdxIDEMenuHelpLoader.Create;
var
  AServices: IOTAServices;
  ANTAServices: INTAServices;
  ARegistry: TRegistry;
  AProducts: TStringList;
  AProductHelpPath, ADXRootPath: string;
  I: Integer;
  AMenuItem: TMenuItem;
  AHandler: TdxIDEMenuHelpItemClickHandler;
begin
  FHandlers := TObjectList<TdxIDEMenuHelpItemClickHandler>.Create;
  ARegistry := TRegistry.Create;
  try
    ARegistry.RootKey := HKEY_LOCAL_MACHINE;
    if ARegistry.OpenKeyReadOnly('Software\Developer Express VCL Products') and
      ARegistry.ValueExists('Help') and ARegistry.ReadBool('Help') then
    begin
      ADXRootPath := ARegistry.ReadString('RootDirectory');
      ARegistry.RootKey := HKEY_CURRENT_USER;
      if Supports(BorlandIDEServices, IOTAServices, AServices) and
        Supports(BorlandIDEServices, INTAServices, ANTAServices) and
        ARegistry.OpenKeyReadOnly(AServices.GetBaseRegistryKey + '\Help\HtmlHelp1Files') then
      begin
        FDXRootHelpMenuItem := TMenuItem.Create(nil);
        FDXRootHelpMenuItem.Name := 'DeveloperExpressVCLHelp';
        FDXRootHelpMenuItem.Caption := 'Developer Express VCL Help';
        FDXRootHelpMenuItem.ImageIndex := 16;
        try
          ANTAServices.AddActionMenu('HelpThirdPartyMenuItem', nil, FDXRootHelpMenuItem, False, True);
        except
          FreeAndNil(FDXRootHelpMenuItem);
        end;
        if FDXRootHelpMenuItem <> nil then
        begin
          AProducts := TStringList.Create;
          try
            ARegistry.GetValueNames(AProducts);
            AProducts.Sort;
            for I := 0 to AProducts.Count - 1 do
            begin
              AProductHelpPath := ARegistry.ReadString(AProducts[I]);
              if Pos(ADXRootPath, AProductHelpPath) <> 0 then
              begin
                AMenuItem := TMenuItem.Create(FDXRootHelpMenuItem);
                AMenuItem.Caption := AProducts[I];
                AMenuItem.ImageIndex := 16;
                AHandler := TdxIDEMenuHelpItemClickHandler.Create(AMenuItem, AProductHelpPath);
                FHandlers.Add(AHandler);
                ANTAServices.AddActionMenu('DeveloperExpressVCLHelp', nil, AMenuItem, True, True);
              end;
            end;
          finally
            AProducts.Free;
          end;
        end;
      end;
    end;
  finally
    ARegistry.Free;
  end;
end;

destructor TdxIDEMenuHelpLoader.Destroy;
begin
  FHandlers.Free;
  FDXRootHelpMenuItem.Free;
end;

{ TdxIDEMenuHelpItemClickHandler }

constructor TdxIDEMenuHelpItemClickHandler.Create(AItem: TMenuItem;
  const APath: string);
begin
  inherited Create;
  FItem := AItem;
  FItem.OnClick := ItemClick;
  FHelpFilePath := APath;
end;

procedure TdxIDEMenuHelpItemClickHandler.ItemClick(Sender: TObject);
begin
  dxShellExecute(FHelpFilePath);
end;

var
  FIDEMenuHelpLoader: TdxIDEMenuHelpLoader;
{$ENDIF}

var
  FAboutInfoIndex: Integer;

procedure AddAboutInfo;
var
  AServices: IOTAAboutBoxServices;
  ATitle: string;
begin
  FAboutInfoIndex := -1;
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AServices) then
  begin
    ATitle := 'Developer Express VCL Products ' + dxGetBuildNumberAsString;
    FAboutInfoIndex := AServices.AddPluginInfo(ATitle, ATitle + dxCRLF + dxCRLF +
      'Copyright ?1998-' + FormatDateTime('YYYY', Now) + ' ' + dxCompanyName + dxCRLF + 'All rights reserved.',
      TdxRegisterInfo.GetSplashBitmap, TdxRegisterInfo.GetIsUnRegistered, TdxRegisterInfo.GetLicenseStatus);
  end;
end;

procedure RemoveAboutInfo;
var
  AServices: IOTAAboutBoxServices;
begin
  if Supports(BorlandIDEServices, IOTAAboutBoxServices, AServices) and
    (FAboutInfoIndex <> -1) then
    AServices.RemovePluginInfo(FAboutInfoIndex);
end;

initialization
  dxIsDesignTime := True;
{$IFDEF DELPHIXE8}
  FIDEMenuHelpLoader := TdxIDEMenuHelpLoader.Create;
{$ENDIF}
  AddAboutInfo;

finalization
  RemoveAboutInfo;
{$IFDEF DELPHIXE8}
  FreeAndNil(FIDEMenuHelpLoader);
{$ENDIF}
end.

