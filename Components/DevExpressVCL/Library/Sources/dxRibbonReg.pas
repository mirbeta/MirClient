{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressBars components                                   }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSBARS AND ALL ACCOMPANYING VCL  }
{   CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.                  }
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

unit dxRibbonReg;

{$I cxVer.inc}

interface

uses
  ColnEdit, DesignEditors, DesignIntf,
  SysUtils, Classes, Graphics, cxLibraryReg, dxBarReg, dxRibbon, ComCtrls,
  cxClasses, cxDesignWindows, dxBarSkin, dxRibbonSkins, dxStatusBar,
  dxRibbonStatusBar, TypInfo, dxBar, cxComponentCollectionEditor, cxPropEditors,
  dxRibbonMiniToolbar, dxBarPopupMenuEd, dxRibbonRadialMenu, dxRibbonUIGenerator;

const
  EmptyContext = '<none>';

type
  { TdxRibbonDesignEditor }

  TdxRibbonDesignEditor = class(TdxBarComponentEditor)
  private
    function GetRibbon: TdxCustomRibbon;
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  public
    property Ribbon: TdxCustomRibbon read GetRibbon;
  end;

  { TdxRibbonColorSchemeNameProperty }

  TdxRibbonColorSchemeNameProperty = class(TStringProperty)
  protected
    function CanAddSkin(ASkin: TdxCustomRibbonSkin): Boolean; virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxRibbonApplicationMenuProperty }

  TdxRibbonApplicationMenuProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxRibbonQuickAccessGroupButtonToolbarProperty }

  TdxRibbonQuickAccessGroupButtonToolbarProperty = class(TComponentProperty)
  private
    function GetRibbon: TdxCustomRibbon;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxRibbonStatusBarEditor }

  TdxRibbonStatusBarEditor = class(TdxBarComponentEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TdxRibbonSelectionEditor }

  TdxRibbonSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxBarProperty }

  TdxBarProperty = class(TComponentProperty)
  private
    FProc: TGetPropProc;
    procedure GetPropProc(const Prop: IProperty);
  public
    procedure GetProperties(Proc: TGetPropProc); override;
  end;

  { TdxExtraPaneEventEditor }

  TdxExtraPaneEventEditor = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  public
    function GetName: string; override;
  end;

  { TdxRibbonContextProperty }

  TdxRibbonContextProperty = class(TClassProperty)
  private
    function GetRibbon: TdxCustomRibbon;
    function GetTab: TdxRibbonTab;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
    function GetValue: string; override;

    property Ribbon: TdxCustomRibbon read GetRibbon;
    property Tab: TdxRibbonTab read GetTab;
  end;

  { TdxRibbonMiniToolbarEditor }

  TdxRibbonMiniToolbarEditor = class(TdxBarComponentEditor)
  protected
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
  end;

  { TdxRadialMenuEditor }

  TdxRibbonRadialMenuEditor = class(TdxBarComponentEditor)
  protected
    procedure DoLinkTo(AObject: TObject); override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;
    function IsLinkable: Boolean; override;

    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
  end;

procedure Register;

implementation

uses
  dxDPIAwareUtils, dxSkinsdxRibbonPainter;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterComponents(dxBarProductPage, [TdxRibbon, TdxRibbonPopupMenu, TdxBarApplicationMenu,
    TdxRibbonStatusBar, TdxRibbonMiniToolbar, TdxRibbonRadialMenu]);
  RegisterNoIcon([TdxRibbonGroupsDockControl, TdxRibbonQuickAccessGroupButton, TdxRibbonTab]);
  RegisterSelectionEditor(TdxCustomRibbon, TdxRibbonSelectionEditor);
  RegisterComponentEditor(TdxCustomRibbon, TdxRibbonDesignEditor);
  RegisterComponentEditor(TdxRibbonStatusBar, TdxRibbonStatusBarEditor);
  RegisterComponentEditor(TdxRibbonMiniToolbar, TdxRibbonMiniToolbarEditor);
  RegisterComponentEditor(TdxRibbonRadialMenu, TdxRibbonRadialMenuEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxCustomRibbon, 'ColorSchemeName',
    TdxRibbonColorSchemeNameProperty);
  RegisterPropertyEditor(TypeInfo(TdxBar), TdxRibbonQuickAccessGroupButton, 'Toolbar',
    TdxRibbonQuickAccessGroupButtonToolbarProperty);
  RegisterPropertyEditor(TypeInfo(TComponent), TdxRibbonApplicationButton, 'Menu', TdxRibbonApplicationMenuProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxRibbonApplicationButton, 'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TBitmap), TdxRibbonHelpButton, 'Glyph', TcxBitmapProperty);
  RegisterPropertyEditor(TypeInfo(TdxBar), nil, '', TdxBarProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxBarApplicationMenu, 'ExtraPaneEvents', TdxExtraPaneEventEditor);

  RegisterPropertyEditor(TypeInfo(TdxRibbonContext), TdxRibbonTab, 'Context', TdxRibbonContextProperty);

    // ExtraPane
  HideClassProperties(TdxBarApplicationMenu, ['ExtraPaneWidthRatio',
    'ExtraPaneSize', 'ExtraPaneItems', 'ExtraPaneHeader', 'OnExtraPaneItemClick']);

  HideClassProperties(TdxRibbonQuickAccessGroupButton, ['Action', 'Align',
    'Caption', 'Category', 'Description', 'Hint', 'MergeKind', 'MergeOrder',
    'Style']);

  HideClassProperties(TdxRibbon, ['HelpButtonScreenTip']);

  HideClassProperties(TdxRibbonTabGroup, ['CanCollapse']);
end;

{ TdxRibbonColorSchemeNameProperty }

function TdxRibbonColorSchemeNameProperty.CanAddSkin(ASkin: TdxCustomRibbonSkin): Boolean;
begin
  Result := (ASkin.TargetDPI = dxDefaultDPI) and (ASkin.Style = TdxRibbon(GetComponent(0)).Style);
  if Result and (ASkin is TdxSkinRibbonPainter) then
    Result := dxSkinListCanUseSkin(ASkin.Name);
end;

function TdxRibbonColorSchemeNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paValueList, paRevertable] - [paReadOnly];
end;

procedure TdxRibbonColorSchemeNameProperty.GetValues(Proc: TGetStrProc);
var
  ASkin: TdxCustomRibbonSkin;
  AValues: TStringList;
  I: Integer;
begin
  AValues := TStringList.Create;
  try
    for I := 0 to dxRibbonSkinsManager.SkinCount - 1 do
    begin
      ASkin := dxRibbonSkinsManager[I];
      if CanAddSkin(ASkin) then
        AValues.Add(ASkin.Name);
    end;

    dxSkinsEnumSkins(
      procedure (const ASkinName, AUnitName: string)
      begin
        if AValues.IndexOf(ASkinName) < 0 then
        begin
          if dxSkinListCanUseSkin(ASkinName) then
            AValues.Add(ASkinName);
        end;
      end);

    AValues.Sort;
    for I := 0 to AValues.Count - 1 do
      Proc(AValues[I]);
  finally
    AValues.Free;
  end;
end;

procedure TdxRibbonColorSchemeNameProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
    TdxCustomRibbon(GetComponent(I)).ColorSchemeName := Value;
end;

{ TdxRibbonDesignEditor }

function TdxRibbonDesignEditor.InternalGetVerb(AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := 'Tabs Editor...'
  else
    Result := 'Contexts Editor...';
end;

function TdxRibbonDesignEditor.InternalGetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TdxRibbonDesignEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
    ShowFormEditorClass(Designer, Component, Ribbon.Tabs, 'Tabs', TfrmComponentCollectionEditor)
  else
    ShowCollectionEditor(Designer, Ribbon, Ribbon.Contexts, 'Contexts');
end;

function TdxRibbonDesignEditor.GetRibbon: TdxCustomRibbon;
begin
  Result := Component as TdxCustomRibbon;
end;

{ TdxRibbonQuickAccessGroupButtonToolbarProperty }

function TdxRibbonQuickAccessGroupButtonToolbarProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paMultiSelect];
end;

procedure TdxRibbonQuickAccessGroupButtonToolbarProperty.GetValues(Proc: TGetStrProc);
var
  AGroupButton: TdxRibbonQuickAccessGroupButton;
  AList: TStringList;
  ARibbon: TdxCustomRibbon;
  AToolbar: TdxBar;
  I, J: Integer;
begin
  ARibbon := GetRibbon;
  if ARibbon = nil then
    Exit;
  AGroupButton := TdxRibbonQuickAccessGroupButton(GetComponent(0));
  AList := TStringList.Create;
  try
    for I := 0 to ARibbon.TabCount - 1 do
      for J := 0 to ARibbon.Tabs[I].Groups.Count - 1 do
      begin
        AToolbar := ARibbon.Tabs[I].Groups[J].Toolbar;
        if (AToolbar <> nil) and (AList.IndexOf(AToolbar.Name) = -1) and AGroupButton.IsToolbarAcceptable(AToolbar) then
          AList.Add(AToolbar.Name);
      end;
    if (AGroupButton.Toolbar <> nil) and (AList.IndexOf(AGroupButton.Toolbar.Name) = -1) then
      AList.Add(AGroupButton.Toolbar.Name);
    for I := 0 to AList.Count - 1 do
      Proc(AList[I]);
  finally
    AList.Free;
  end;
end;

function TdxRibbonQuickAccessGroupButtonToolbarProperty.GetRibbon: TdxCustomRibbon;
var
  AGroupButton: TdxRibbonQuickAccessGroupButton;
begin
  AGroupButton := TdxRibbonQuickAccessGroupButton(GetComponent(0));
  if AGroupButton.LinkCount = 0 then
    Result := nil
  else
    Result := TdxRibbonQuickAccessToolbarBarControl(AGroupButton.Links[0].BarControl).Ribbon;
end;

{ TdxRibbonApplicationMenuProperty }

procedure TdxRibbonApplicationMenuProperty.GetValues(Proc: TGetStrProc);
var
  AComponent: TComponent;
  I: Integer;
begin
  for I := 0 to Designer.Root.ComponentCount - 1 do
  begin
    AComponent := Designer.Root.Components[I];
    if Supports(AComponent, IdxRibbonApplicationMenu) then
      Proc(AComponent.Name);
  end;
end;

{ TdxRibbonStatusBarEditor }

function TdxRibbonStatusBarEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'Panels Editor...';
end;

function TdxRibbonStatusBarEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxRibbonStatusBarEditor.InternalExecuteVerb(AIndex: Integer);
begin
  ShowCollectionEditor(Designer, Component, (Component as TdxRibbonStatusBar).Panels, 'Panels');
end;

{ TdxRibbonSelectionEditor }

{ TdxBarProperty }

procedure TdxBarProperty.GetProperties(Proc: TGetPropProc);
begin
  FProc := Proc;
  inherited GetProperties(GetPropProc);
end;

procedure TdxBarProperty.GetPropProc(const Prop: IProperty);
const
  PropertiesToHide =
    ' AllowClose AllowCustomizing AllowQuickCustomizing AllowReset ' +
    'BorderStyle Color DockControl DockedDockControl DockedDockingStyle ' +
    'DockedLeft DockedTop DockingStyle FloatLeft FloatTop FloatClientWidth ' +
    'FloatClientHeight Font Hidden IsMainMenu MultiLine NotDocking OldName ' +
    'OneOnRow RotateWhenVertical Row ShowMark SizeGrip UseOwnFont ' +
    'UseRecentItems UseRestSpace WholeRow BackgroundBitmap AlphaBlendValue ';
var
  I: Integer;
begin
  if (GetComponent(0) is TdxRibbonQuickAccessToolbar) or
    (GetComponent(0) is TdxStatusBarToolbarPanelStyle) or
    (GetComponent(0) is TdxRibbonTabGroup) then
  begin
    for I := 0 to PropCount - 1 do
      if Pos(' ' + Prop.GetName + ' ', PropertiesToHide) > 0 then Exit;
  end;
  FProc(Prop);
end;

{ TdxExtraPaneEventEditor }

function TdxExtraPaneEventEditor.GetName: string;
begin
  Result := 'ExtraPane';
end;

function TdxExtraPaneEventEditor.GetInstance: TPersistent;
begin
  Result := TdxBarApplicationMenu(GetComponent(0)).ExtraPane;
end;

{ TdxRibbonContextProperty }

function TdxRibbonContextProperty.GetAttributes: TPropertyAttributes;
begin
  Result := {inherited GetAttributes +} [paValueList, paRevertable];
end;

function TdxRibbonContextProperty.GetValue: string;
begin
  if Tab.Context = nil then
    Result := EmptyContext
  else
    Result := Tab.Context.DisplayName;
end;

procedure TdxRibbonContextProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  if Ribbon = nil then Exit;
  Proc(EmptyContext);
  for I := 0 to Ribbon.Contexts.Count - 1 do
    Proc(Ribbon.Contexts[I].DisplayName);
end;

procedure TdxRibbonContextProperty.SetValue(const Value: string);
var
  I: Integer;
begin
  if Ribbon = nil then Exit;
  if Value = EmptyContext then
    Tab.Context := nil
  else
    for I := 0 to Ribbon.Contexts.Count - 1 do
      if Ribbon.Contexts[I].DisplayName = Value then
        Tab.Context := Ribbon.Contexts[I];
end;

function TdxRibbonContextProperty.GetRibbon: TdxCustomRibbon;
begin
  if Tab <> nil then
    Result := Tab.Ribbon
  else
    Result := nil;
end;

function TdxRibbonContextProperty.GetTab: TdxRibbonTab;
begin
  Result := TdxRibbonTab(GetComponent(0));
end;

{ TdxRibbonSelectionEditor }

procedure TdxRibbonSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxRibbonSkins');
  dxSkinsRequiresAdditionalUnits(TdxCustomRibbon, Proc);
end;

{ TdxBarPopupMenuEditor }

function TdxRibbonMiniToolbarEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'MiniToolbar Editor...';
end;

function TdxRibbonMiniToolbarEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdxRibbonMiniToolbarEditor.InternalExecuteVerb(AIndex: Integer);
begin
  ShowdxBarSubMenuEditor((Component as TdxRibbonCustomMiniToolbar).ItemLinks);
end;

{ TdxRibbonRadialMenuEditor }

procedure TdxRibbonRadialMenuEditor.DoLinkTo(AObject: TObject);
var
  ARadialMenu: TdxRibbonRadialMenu;
begin
  ARadialMenu := Component as TdxRibbonRadialMenu;
  ARadialMenu.ItemLinks.Assign((AObject as TdxBarCustomPopupMenu).ItemLinks);
  Designer.Modified;
end;

function TdxRibbonRadialMenuEditor.GetLinkToItemCaption: string;
begin
  Result := 'Assign ItemLinks From';
end;

function TdxRibbonRadialMenuEditor.GetLinkToTypeClass: TClass;
begin
  Result := TdxBarCustomPopupMenu;
end;

function TdxRibbonRadialMenuEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

procedure TdxRibbonRadialMenuEditor.InternalExecuteVerb(AIndex: Integer);
begin
  ShowdxBarSubMenuEditor((Component as TdxRibbonRadialMenu).ItemLinks);
end;

function TdxRibbonRadialMenuEditor.InternalGetVerb(AIndex: Integer): string;
begin
  Result := 'RadialMenu Editor...';
end;

function TdxRibbonRadialMenuEditor.InternalGetVerbCount: Integer;
begin
  Result := 1;
end;

end.
